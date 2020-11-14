package mainargs
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object RouterMacros{
  def generateRoutesImpl[T: c.WeakTypeTag](c: Context): c.Expr[Seq[EntryPoint[T]]] = {
    import c.universe._
    val r = new RouterMacros(c)
    val allRoutes = r.getAllRoutesForClass(
      weakTypeOf[T].asInstanceOf[r.c.Type]
    ).asInstanceOf[Iterable[c.Tree]]

    c.Expr[Seq[EntryPoint[T]]](q"_root_.scala.Seq(..$allRoutes)")
  }
}
class RouterMacros [C <: Context](val c: C) {
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

  def extractMethod(meth: MethodSymbol, curCls: c.universe.Type): c.universe.Tree = {
    val baseArgSym = TermName(c.freshName())
    val flattenedArgLists = meth.paramss.flatten
    def hasDefault(i: Int) = {
      val defaultName = s"${meth.name}$$default$$${i + 1}"
      if (curCls.members.exists(_.name.toString == defaultName)) Some(defaultName)
      else None
    }

    val argListSymbol = q"${c.fresh[TermName](TermName("argsList"))}"
    val extrasSymbol = q"${c.fresh[TermName](TermName("extras"))}"
    val defaults = for ((arg, i) <- flattenedArgLists.zipWithIndex) yield {
      val arg = TermName(c.freshName())
      hasDefault(i).map(defaultName => q"($arg: $curCls) => $arg.${newTermName(defaultName)}")
    }

    def getDocAnnotation(annotations: List[Annotation]) = {
      val (docTrees, remaining) = annotations.partition(_.tpe =:= typeOf[doc])
      val docValues = for {
        doc <- docTrees
        if doc.scalaArgs.head.isInstanceOf[Literal]
        l =  doc.scalaArgs.head.asInstanceOf[Literal]
        if l.value.value.isInstanceOf[String]
      } yield l.value.value.asInstanceOf[String]
      (remaining, docValues.headOption)
    }
    def getShortAnnotation(annotations: List[Annotation]) = {
      val (docTrees, remaining) = annotations.partition(_.tpe =:= typeOf[short])
      val docValues = for {
        doc <- docTrees
        if doc.scalaArgs.head.isInstanceOf[Literal]
        l =  doc.scalaArgs.head.asInstanceOf[Literal]
        if l.value.value.isInstanceOf[Char]
      } yield l.value.value.asInstanceOf[Char]
      (remaining, docValues.headOption)
    }

    def unwrapVarargType(arg: Symbol) = {
      val vararg = arg.typeSignature.typeSymbol == definitions.RepeatedParamClass
      val unwrappedType =
        if (!vararg) arg.typeSignature
        else arg.typeSignature.asInstanceOf[TypeRef].args(0)

      (vararg, unwrappedType)
    }


    val (_, methodDoc) = getDocAnnotation(meth.annotations)
    val readArgSigs = for(
      ((arg, defaultOpt), i) <- flattenedArgLists.zip(defaults).zipWithIndex
    ) yield {

      val (vararg, varargUnwrappedType) = unwrapVarargType(arg)

      val default =
        if (vararg) q"scala.Some(scala.Nil)"
        else defaultOpt match {
          case Some(defaultExpr) => q"scala.Some($defaultExpr($baseArgSym))"
          case None => q"scala.None"
        }

      val (_, docOpt) = getDocAnnotation(arg.annotations)
      val (_, shortOpt) = getShortAnnotation(arg.annotations)

      val docTree = docOpt match{
        case None => q"scala.None"
        case Some(s) => q"scala.Some($s)"
      }
      val shortTree = shortOpt match{
        case None => q"scala.None"
        case Some(c) => q"scala.Some($c)"
      }

      val argSig = q"""
        mainargs.ArgSig(
          ${arg.name.toString},
          $shortTree,
          ${varargUnwrappedType.toString + (if(vararg) "*" else "")},
          $docTree,
          $defaultOpt,
          $vararg
        )
      """

      val reader =
        if(vararg) q"""
          mainargs.Router.makeReadVarargsCall[$varargUnwrappedType](
            $argSig,
            $extrasSymbol
          )
        """ else q"""
        mainargs.Router.makeReadCall[$varargUnwrappedType](
          $argListSymbol,
          $default,
          $argSig
        )
        """
      c.internal.setPos(reader, meth.pos)
      (reader, argSig, vararg)
    }

    val (readArgs, argSigs, varargs) = readArgSigs.unzip3
    val (argNames, argNameCasts) = flattenedArgLists.map { arg =>
      val (vararg, unwrappedType) = unwrapVarargType(arg)
      (
        pq"${arg.name.toTermName}",
        if (!vararg) q"${arg.name.toTermName}.asInstanceOf[$unwrappedType]"
        else q"${arg.name.toTermName}.asInstanceOf[Seq[$unwrappedType]]: _*"

      )
    }.unzip


    q"""
    mainargs.EntryPoint(
      ${meth.name.toString},
      scala.Seq(..$argSigs),
      ${methodDoc match{
        case None => q"scala.None"
        case Some(s) => q"scala.Some($s)"
      }},
      ${varargs.contains(true)},
      ($baseArgSym: $curCls, $argListSymbol: Map[String, String], $extrasSymbol: Seq[String]) =>
        mainargs.Router.validate(Seq(..$readArgs)) match{
          case mainargs.Result.Success(List(..$argNames)) =>
            mainargs.Result.Success(
              $baseArgSym.${meth.name.toTermName}(..$argNameCasts)
            )
          case x: mainargs.Result.Error => x
        }
    )
    """
  }

  def hasMainAnnotation(t: MethodSymbol) = t.annotations.exists(_.tpe =:= typeOf[main])
  def getAllRoutesForClass(curCls: Type,
                           pred: MethodSymbol => Boolean = hasMainAnnotation)
                            : Iterable[c.universe.Tree] = {
    for(t <- getValsOrMeths(curCls) if pred(t))
    yield extractMethod(t, curCls)
  }
}

