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

  def parserForMethods[B: c.WeakTypeTag](base: c.Expr[B]): c.Tree = {
    val allRoutes = getAllRoutesForClass(weakTypeOf[B])
    q"""
      new _root_.mainargs.ParserForMethods(
        _root_.mainargs.MethodMains[${weakTypeOf[B]}](_root_.scala.List(..$allRoutes), () => $base)
      )
    """
  }

  def parserForClass[T: c.WeakTypeTag]: c.Tree = {

    val cls = weakTypeOf[T].typeSymbol.asClass
    val companionObj = weakTypeOf[T].typeSymbol.companion
    val constructor = cls.primaryConstructor.asMethod
    val route = extractMethod(
      TermName("apply"),
      constructor.paramLists.flatten,
      constructor.pos,
      cls.annotations.find(_.tpe =:= typeOf[main]),
      companionObj.typeSignature,
      weakTypeOf[T]
    )

    q"""
      new _root_.mainargs.ParserForClass(
        $route.asInstanceOf[_root_.mainargs.MainData[${weakTypeOf[T]}, Any]],
        () => $companionObj
      )
    """
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

  def unwrapVarargType(arg: Symbol) = {
    val vararg = arg.typeSignature.typeSymbol == definitions.RepeatedParamClass
    val unwrappedType =
      if (!vararg) arg.typeSignature
      else arg.typeSignature.asInstanceOf[TypeRef].args(0)

    (vararg, unwrappedType)
  }

  def extractMethod(
      methodName: TermName,
      flattenedArgLists: Seq[Symbol],
      methodPos: Position,
      mainAnnotation: Option[Annotation],
      curCls: c.universe.Type,
      returnType: c.universe.Type
  ): c.universe.Tree = {

    val baseArgSym = TermName(c.freshName())

    // Somehow this is necessary to make default
    // method discovery work on Scala 2.12.1 -> 2.12.7
    curCls.members.foreach(_.name)
    def hasDefault(i: Int) = {
      val defaultName = s"${methodName}$$default$$${i + 1}"
      if (curCls.members.exists(_.name.toString == defaultName)) Some(defaultName)
      else None
    }

    val argListSymbol = q"${c.fresh[TermName](TermName("argsList"))}"

    val defaults = for ((arg0, i) <- flattenedArgLists.zipWithIndex) yield {
      val arg = TermName(c.freshName())
      hasDefault(i) match {
        case None => q"_root_.scala.None"
        case Some(defaultName) =>
          q"_root_.scala.Some[$curCls => ${arg0.info}](($arg: $curCls) => $arg.${newTermName(defaultName)}: ${arg0.info})"
      }
    }

    val argSigs = for ((arg, defaultOpt) <- flattenedArgLists.zip(defaults)) yield {

      val (vararg, varargUnwrappedType) = unwrapVarargType(arg)
      val argAnnotation = arg.annotations.find(_.tpe =:= typeOf[arg])

      val instantiateArg = argAnnotation match {
        case Some(annot) => q"new ${annot.tree.tpe}(..${annot.tree.children.tail})"
        case _ => q"new _root_.mainargs.arg()"
      }
      val argSig = if (vararg) q"""
        _root_.mainargs.ArgSig.create[_root_.mainargs.Leftover[$varargUnwrappedType], $curCls](
                ${arg.name.decoded},
                $instantiateArg,
                $defaultOpt
              )
      """ else q"""
        _root_.mainargs.ArgSig.create[$varargUnwrappedType, $curCls](
          ${arg.name.decoded},
          $instantiateArg,
          $defaultOpt
        )
      """

      c.internal.setPos(argSig, methodPos)
      argSig
    }

    val argNameCasts = flattenedArgLists.zipWithIndex.map { case (arg, i) =>
      val (vararg, unwrappedType) = unwrapVarargType(arg)
      val baseTree = q"$argListSymbol($i)"
      if (!vararg) q"$baseTree.asInstanceOf[$unwrappedType]"
      else q"$baseTree.asInstanceOf[_root_.mainargs.Leftover[$unwrappedType]].value: _*"
    }

    val mainInstance = mainAnnotation match {
      case Some(m) => q"new ${m.tree.tpe}(..${m.tree.children.tail})"
      case None => q"new _root_.mainargs.main()"
    }

    val res = q"""{
    _root_.mainargs.MainData.create[$returnType, $curCls](
      ${methodName.decoded},
      $mainInstance,
      _root_.scala.Seq(..$argSigs),
      ($baseArgSym: $curCls, $argListSymbol: _root_.scala.Seq[_root_.scala.Any]) => {
        $baseArgSym.$methodName(..$argNameCasts)
      }
    )
    }"""
//    println(res)
    res
  }

  def hasmain(t: MethodSymbol) = t.annotations.exists(_.tpe =:= typeOf[main])
  def getAllRoutesForClass(
      curCls: Type,
      pred: MethodSymbol => Boolean = hasmain
  ): Iterable[c.universe.Tree] = {
    for (t <- getValsOrMeths(curCls) if pred(t))
      yield {
        extractMethod(
          t.name,
          t.paramss.flatten,
          t.pos,
          t.annotations.find(_.tpe =:= typeOf[main]),
          curCls,
          weakTypeOf[Any]
        )
      }
  }
}
