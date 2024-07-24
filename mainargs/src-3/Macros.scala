package mainargs

import scala.quoted._

object Macros {
  private def mainAnnotation(using Quotes) = quotes.reflect.Symbol.requiredClass("mainargs.main")
  private def argAnnotation(using Quotes) = quotes.reflect.Symbol.requiredClass("mainargs.arg")
  def parserForMethods[B](base: Expr[B])(using Quotes, Type[B]): Expr[ParserForMethods[B]] = {
    import quotes.reflect._
    val allMethods = TypeRepr.of[B].typeSymbol.memberMethods
    val annotatedMethodsWithMainAnnotations = allMethods.flatMap { methodSymbol =>
      methodSymbol.getAnnotation(mainAnnotation).map(methodSymbol -> _)
    }.sortBy(_._1.pos.map(_.start))
    val mainDatas = Expr.ofList(annotatedMethodsWithMainAnnotations.map { (annotatedMethod, mainAnnotationInstance) =>
      createMainData[Any, B](annotatedMethod, mainAnnotationInstance)
    })

    '{
      new ParserForMethods[B](
        MethodMains[B]($mainDatas, () => $base)
      )
    }
  }

  def parserForClass[B](using Quotes, Type[B]): Expr[ParserForClass[B]] = {
    import quotes.reflect._
    val typeReprOfB = TypeRepr.of[B]
    val companionModule = typeReprOfB match {
      case TypeRef(a,b) => TermRef(a,b)
    }
    val typeSymbolOfB = typeReprOfB.typeSymbol
    val companionModuleType = typeSymbolOfB.companionModule.tree.asInstanceOf[ValDef].tpt.tpe.asType
    val companionModuleExpr = Ident(companionModule).asExpr
    val mainAnnotationInstance = typeSymbolOfB.getAnnotation(mainAnnotation).getOrElse {
      report.throwError(
        s"cannot find @main annotation on ${companionModule.name}",
        typeSymbolOfB.pos.get
      )
    }
    val annotatedMethod = TypeRepr.of[B].typeSymbol.companionModule.memberMethod("apply").head
    companionModuleType match
      case '[bCompanion] =>
        val mainData = createMainData[B, Any](
          annotatedMethod,
          mainAnnotationInstance,
          // Somehow the `apply` method parameter annotations don't end up on
          // the `apply` method parameters, but end up in the `<init>` method
          // parameters, so use those for getting the annotations instead
          TypeRepr.of[B].typeSymbol.primaryConstructor.paramSymss
        )
        '{ new ParserForClass[B](${ mainData }, () => ${ Ident(companionModule).asExpr }) }
  }

  def createMainData[T: Type, B: Type](using Quotes)
                                      (method: quotes.reflect.Symbol,
                                       mainAnnotation: quotes.reflect.Term): Expr[MainData[T, B]] = {
    createMainData[T, B](method, mainAnnotation, method.paramSymss)
  }

  def createMainData[T: Type, B: Type](using Quotes)
                                      (method: quotes.reflect.Symbol,
                                       mainAnnotation: quotes.reflect.Term,
                                       annotatedParamsLists: List[List[quotes.reflect.Symbol]]): Expr[MainData[T, B]] = {

    import quotes.reflect.*
    val params = method.paramSymss.headOption.getOrElse(report.throwError("Multiple parameter lists not supported"))
    val defaultParams = getDefaultParams(method)
    val argSigsExprs = params.zip(annotatedParamsLists.flatten).map { paramAndAnnotParam =>
      val param = paramAndAnnotParam._1
      val annotParam = paramAndAnnotParam._2
      val paramTree = param.tree.asInstanceOf[ValDef]
      val paramTpe = paramTree.tpt.tpe
      val arg = annotParam.getAnnotation(argAnnotation).map(_.asExprOf[mainargs.arg]).getOrElse('{ new mainargs.arg() })
      val paramType = paramTpe.asType
      paramType match
        case '[t] =>
          val defaultParam: Expr[Option[B => t]] = defaultParams.get(param) match {
            case Some('{ $v: `t`}) => '{ Some(((_: B) => $v)) }
            case None => '{ None }
          }
          val tokensReader = Expr.summon[mainargs.TokensReader[t]].getOrElse {
            report.throwError(
              s"No mainargs.ArgReader found for parameter ${param.name}",
              param.pos.get
            )
          }
          '{ (ArgSig.create[t, B](${ Expr(param.name) }, ${ arg }, ${ defaultParam })(using ${ tokensReader })) }
    }
    val argSigs = Expr.ofList(argSigsExprs)

    val invokeRaw: Expr[(B, Seq[Any]) => T] = {

      def callOf(methodOwner: Expr[Any], args: Expr[Seq[Any]]) =
        call(methodOwner, method, '{ Seq($args) }).asExprOf[T]

      '{ (b: B, params: Seq[Any]) => ${ callOf('b, 'params) } }
    }
    '{ MainData.create[T, B](${ Expr(method.name) }, ${ mainAnnotation.asExprOf[mainargs.main] }, ${ argSigs }, ${ invokeRaw }) }
  }

  /** Call a method given by its symbol.
    *
    * E.g.
    *
    * assuming:
    *
    *   def foo(x: Int, y: String)(z: Int)
    *
    *   val argss: List[List[Any]] = ???
    *
    * then:
    *
    *   call(<symbol of foo>, '{argss})
    *
    * will expand to:
    *
    *   foo(argss(0)(0), argss(0)(1))(argss(1)(0))
    *
    */
  private def call(using Quotes)(
      methodOwner: Expr[Any],
      method: quotes.reflect.Symbol,
      argss: Expr[Seq[Seq[Any]]]
  ): Expr[_] = {
    // Copy pasted from Cask.
    // https://github.com/com-lihaoyi/cask/blob/65b9c8e4fd528feb71575f6e5ef7b5e2e16abbd9/cask/src-3/cask/router/Macros.scala#L106
    import quotes.reflect._
    val paramss = method.paramSymss

    if (paramss.isEmpty) {
      report.throwError("At least one parameter list must be declared.", method.pos.get)
    }

    val accesses: List[List[Term]] = for (i <- paramss.indices.toList) yield {
      for (j <- paramss(i).indices.toList) yield {
        val tpe = paramss(i)(j).tree.asInstanceOf[ValDef].tpt.tpe
        tpe.asType match
          case '[t] => '{ $argss(${Expr(i)})(${Expr(j)}).asInstanceOf[t] }.asTerm
      }
    }

    methodOwner.asTerm.select(method).appliedToArgss(accesses).asExpr
  }
    

  /** Lookup default values for a method's parameters. */
  private def getDefaultParams(using Quotes)(method: quotes.reflect.Symbol): Map[quotes.reflect.Symbol, Expr[Any]] = {
    // Copy pasted from Cask.
    // https://github.com/com-lihaoyi/cask/blob/65b9c8e4fd528feb71575f6e5ef7b5e2e16abbd9/cask/src-3/cask/router/Macros.scala#L38
    import quotes.reflect._

    val params = method.paramSymss.flatten
    val defaults = collection.mutable.Map.empty[Symbol, Expr[Any]]

    val Name = (method.name + """\$default\$(\d+)""").r
    val InitName = """\$lessinit\$greater\$default\$(\d+)""".r

    val idents = method.owner.tree.asInstanceOf[ClassDef].body

    idents.foreach{
      case deff @ DefDef(Name(idx), _, _, _) =>
        val expr = Ref(deff.symbol).asExpr
        defaults += (params(idx.toInt - 1) -> expr)

      // The `apply` method re-uses the default param factory methods from `<init>`,
      // so make sure to check if those exist too
      case deff @ DefDef(InitName(idx), _, _, _) if method.name == "apply" =>
        val expr = Ref(deff.symbol).asExpr
        defaults += (params(idx.toInt - 1) -> expr)

      case _ =>
    }

    defaults.toMap
  }
}
