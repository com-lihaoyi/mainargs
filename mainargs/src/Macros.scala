package mainargs

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

/**
 * More or less a minimal version of Autowire's Server that lets you generate
 * a set of "routes" from the methods defined in an object, and call them
 * using passing in name/args/kwargs via Java reflection, without having to
 * generate/compile code or use Scala reflection. This saves us spinning up
 * the Scala compiler and greatly reduces the startup time of cached scripts.
 */
class Macros(val c: Context) {
  def generateBareEntryPoints[B: c.WeakTypeTag]: c.Expr[BareEntryPoints[B]] = {
    import c.universe._
    val allRoutes = getAllRoutesForClass(weakTypeOf[B])
    c.Expr[BareEntryPoints[B]](
      q"_root_.mainargs.BareEntryPoints(_root_.scala.Seq(..$allRoutes))"
    )
  }
  def generateEntryPoints[B: c.WeakTypeTag]: c.Expr[EntryPoints[B]] = {
    import c.universe._
    val allRoutes = getAllRoutesForClass(weakTypeOf[B])
    val obj = weakTypeOf[B].termSymbol
    c.Expr[EntryPoints[B]](
      q"_root_.mainargs.EntryPoints(_root_.scala.Seq(..$allRoutes), () => $obj)"
    )
  }
  def genereateClassEntryPoints[T: c.WeakTypeTag]: c.Expr[ClassEntryPoint[T]] = {
    import c.universe._

    val cls = weakTypeOf[T].typeSymbol.asClass
    val companionObj = weakTypeOf[T].typeSymbol.companion
    val constructor = cls.primaryConstructor.asMethod
    val route = extractMethod(
      "apply",
      constructor.paramLists.flatten,
      constructor.pos,
      cls.annotations.find(_.tpe =:= typeOf[MainAnnotation]).head,
      companionObj.typeSignature
    )

    c.Expr[ClassEntryPoint[T]](
      q"_root_.mainargs.ClassEntryPoint[${weakTypeOf[T]}]($route.asInstanceOf[_root_.mainargs.EntryPoint[Any]], () => $companionObj)"
    )
  }
  import c.universe._
  def getValsOrMeths(curCls: Type): Iterable[MethodSymbol] = {
    def isAMemberOfAnyRef(member: Symbol) = {
      // AnyRef is an alias symbol, we go to the real "owner" of these methods
      val anyRefSym = c.mirror.universe.definitions.ObjectClass
      member.owner == anyRefSym
    }
    val extractableMembers = for {
      member <- curCls.members.toList.reverse
      if !isAMemberOfAnyRef(member)
      if !member.isSynthetic
      if member.isPublic
      if member.isTerm
      memTerm = member.asTerm
      if memTerm.isMethod
      if !memTerm.isModule
    } yield memTerm.asMethod

    extractableMembers flatMap { case memTerm =>
      if (memTerm.isSetter || memTerm.isConstructor || memTerm.isGetter) Nil
      else Seq(memTerm)
    }
  }

  def extractMethod(methodName: String,
                    flattenedArgLists: Seq[Symbol],
                    methodPos: Position,
                    mainAnnotation: Annotation,
                    curCls: c.universe.Type): c.universe.Tree = {

    val baseArgSym = TermName(c.freshName())

    def hasDefault(i: Int) = {
      val defaultName = s"${methodName}$$default$$${i + 1}"
      if (curCls.members.exists(_.name.toString == defaultName)) Some(defaultName)
      else None
    }

    val argListSymbol = q"${c.fresh[TermName](TermName("argsList"))}"
    val extrasSymbol = q"${c.fresh[TermName](TermName("extras"))}"
    val defaults = for ((arg, i) <- flattenedArgLists.zipWithIndex) yield {
      val arg = TermName(c.freshName())
      hasDefault(i).map(defaultName =>
        q"($arg: $curCls) => $arg.${newTermName(defaultName)}"
      )
    }

    def unwrapVarargType(arg: Symbol) = {
      val vararg = arg.typeSignature.typeSymbol == definitions.RepeatedParamClass
      val unwrappedType =
        if (!vararg) arg.typeSignature
        else arg.typeSignature.asInstanceOf[TypeRef].args(0)

      (vararg, unwrappedType)
    }

    val readArgSigs = for((arg, defaultOpt) <- flattenedArgLists.zip(defaults)) yield {

      val (vararg, varargUnwrappedType) = unwrapVarargType(arg)

      val default =
        if (vararg) q"scala.Some(scala.Nil)"
        else defaultOpt match {
          case Some(defaultExpr) => q"scala.Some($defaultExpr($baseArgSym))"
          case None => q"scala.None"
        }
      val argAnnotation = arg.annotations.find(_.tpe =:= typeOf[ArgAnnotation]).headOption

      val instantiateArg = argAnnotation match{
        case Some(annot) => q"new ${annot.tree.tpe}(..${annot.tree.children.tail})"
        case _ => q"new _root_.mainargs.arg()"
      }
      val argVal = TermName(c.freshName("arg"))
      val argSigVal = TermName(c.freshName("argSig"))
      val argSig = q"""
        val $argSigVal = {
          val $argVal = $instantiateArg
          _root_.mainargs.ArgSig[$curCls](
            scala.Option($argVal.name).getOrElse(${arg.name.toString}),
            $argVal.short match{ case '\u0000' => None; case c => Some(c)},
            _root_.mainargs.MacroHelpers.getShortName[$varargUnwrappedType],
            scala.Option($argVal.doc),
            $defaultOpt,
            $vararg,
            $argVal.flag,
          )
        }
      """

      val reader =
        if(vararg) q"""
          _root_.mainargs.MacroHelpers.makeReadVarargsCall[$varargUnwrappedType](
            $argSigVal,
            $extrasSymbol
          )
        """ else q"""
          _root_.mainargs.MacroHelpers.makeReadCall[$varargUnwrappedType](
            $argListSymbol,
            $default,
            $argSigVal
          )
        """
      c.internal.setPos(reader, methodPos)
      (reader, argSig, (argSigVal, vararg))
    }

    val (readArgs, argSigs, argSigValVarargs) = readArgSigs.unzip3
    val (argSigVals, varargs) = argSigValVarargs.unzip
    val (argNames, argNameCasts) = flattenedArgLists.map { arg =>
      val (vararg, unwrappedType) = unwrapVarargType(arg)
      (
        pq"${arg.name.toTermName}",
        if (!vararg) q"${arg.name.toTermName}.value.asInstanceOf[$unwrappedType]"
        else q"${arg.name.toTermName}.value.asInstanceOf[Seq[$unwrappedType]]: _*"

      )
    }.unzip


    val methVal = TermName(c.freshName("arg"))
    val res = q"""{
    val $methVal = new ${mainAnnotation.tree.tpe}(..${mainAnnotation.tree.children.tail})
    ..$argSigs
    _root_.mainargs.EntryPoint[$curCls](
      _root_.scala.Option($methVal.name).getOrElse($methodName),
      _root_.scala.Seq(..$argSigVals),
      _root_.scala.Option($methVal.doc),
      ${varargs.contains(true)},
      ($baseArgSym: $curCls, $argListSymbol: Map[String, String], $extrasSymbol: Seq[String]) =>
        _root_.mainargs.MacroHelpers.validate(Seq(..$readArgs)).flatMap{
          case _root_.scala.List(..$argNames) =>
            _root_.mainargs.Result.Success(
              _root_.mainargs.Computed($baseArgSym.${TermName(methodName)}(..$argNameCasts))
            )
        }
    )
    }"""
//    println(res)
    res
  }

  def hasMainAnnotation(t: MethodSymbol) = t.annotations.exists(_.tpe =:= typeOf[MainAnnotation])
  def getAllRoutesForClass(curCls: Type,
                           pred: MethodSymbol => Boolean = hasMainAnnotation)
                            : Iterable[c.universe.Tree] = {
    for(t <- getValsOrMeths(curCls) if pred(t))
    yield {
      extractMethod(
        t.name.toString,
        t.paramss.flatten,
        t.pos,
        t.annotations.find(_.tpe =:= typeOf[MainAnnotation]).head,
        curCls
      )
    }
  }
}
