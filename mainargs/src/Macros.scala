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
  import c.universe._

  def parserForMethods[B: c.WeakTypeTag](base: c.Expr[B]): c.Expr[ParserForMethods[B]] = {
    val allRoutes = getAllRoutesForClass(weakTypeOf[B])
    c.Expr[ParserForMethods[B]](q"""
      new _root_.mainargs.ParserForMethods(
        _root_.mainargs.BasedMains[${weakTypeOf[B]}](_root_.scala.List(..$allRoutes), () => $base)
      )
    """)
  }

  def parserForClass[T: c.WeakTypeTag]: c.Expr[ParserForClass[T]] = {

    val cls = weakTypeOf[T].typeSymbol.asClass
    val companionObj = weakTypeOf[T].typeSymbol.companion
    val constructor = cls.primaryConstructor.asMethod
    val route = extractMethod(
      "apply",
      constructor.paramLists.flatten,
      constructor.pos,
      cls.annotations.find(_.tpe =:= typeOf[main]).head,
      companionObj.typeSignature
    )

    c.Expr[ParserForClass[T]](q"""
      new _root_.mainargs.ParserForClass(
        _root_.mainargs.ClassMains[${weakTypeOf[T]}](
          $route.asInstanceOf[_root_.mainargs.MainData[Any]],
          () => $companionObj
        )
      )
    """)
  }
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

      val argAnnotation = arg.annotations.find(_.tpe =:= typeOf[arg]).headOption

      val instantiateArg = argAnnotation match{
        case Some(annot) => q"new ${annot.tree.tpe}(..${annot.tree.children.tail})"
        case _ => q"new _root_.mainargs.arg()"
      }
      val argVal = TermName(c.freshName("arg"))
      val argSigVal = TermName(c.freshName("argSig"))
      val argSig = q"""
        val $argSigVal: _root_.mainargs.AnyArgSig[$curCls] = implicitly[_root_.mainargs.AnyArgParser[$varargUnwrappedType]] match{
          case _root_.mainargs.AnyArgParser.Simple(parser) =>
            val $argVal = $instantiateArg
            _root_.mainargs.ArgSig[$curCls](
              scala.Option($argVal.name).getOrElse(${arg.name.toString}),
              $argVal.short match{ case '\u0000' => None; case c => Some(c)},
              scala.Option($argVal.doc),
              $defaultOpt,
              $vararg,
              $argVal.flag,
              parser
            )
          case _root_.mainargs.AnyArgParser.Class(parser) =>
            _root_.mainargs.ClassArgSig(parser)
        }
      """

      c.internal.setPos(argSig, methodPos)
      (argSig, argSigVal)
    }

    val (argSigs, argSigVals) = readArgSigs.unzip
    val argNameCasts = flattenedArgLists.zipWithIndex.map { case (arg, i) =>
      val (vararg, unwrappedType) = unwrapVarargType(arg)
      if (!vararg) q"$argListSymbol($i).value.asInstanceOf[$unwrappedType]"
      else q"$argListSymbol($i).value.asInstanceOf[Seq[$unwrappedType]]: _*"
    }


    val methVal = TermName(c.freshName("arg"))
    val res = q"""{
    val $methVal = new ${mainAnnotation.tree.tpe}(..${mainAnnotation.tree.children.tail})
    ..$argSigs
    _root_.mainargs.MainData[$curCls](
      _root_.scala.Option($methVal.name).getOrElse($methodName),
      _root_.scala.Seq(..$argSigVals),
      _root_.scala.Option($methVal.doc),
      ($baseArgSym: $curCls, $argListSymbol: _root_.scala.Seq[_root_.mainargs.Computed[_root_.scala.Any]]) => {
        _root_.mainargs.Computed($baseArgSym.${TermName(methodName)}(..$argNameCasts))
      }
    )
    }"""
//    println(res)
    res
  }

  def hasmain(t: MethodSymbol) = t.annotations.exists(_.tpe =:= typeOf[main])
  def getAllRoutesForClass(curCls: Type,
                           pred: MethodSymbol => Boolean = hasmain)
                            : Iterable[c.universe.Tree] = {
    for(t <- getValsOrMeths(curCls) if pred(t))
    yield {
      extractMethod(
        t.name.toString,
        t.paramss.flatten,
        t.pos,
        t.annotations.find(_.tpe =:= typeOf[main]).head,
        curCls
      )
    }
  }
}
