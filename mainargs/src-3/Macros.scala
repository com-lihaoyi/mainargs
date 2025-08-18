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
    val mainDatas = Expr.ofList(annotatedMethodsWithMainAnnotations.map {
      (annotatedMethod, mainAnnotationInstance) =>
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
      case TypeRef(a, b) => TermRef(a, b)
    }
    val typeSymbolOfB = typeReprOfB.typeSymbol
    val companionModuleType = typeSymbolOfB.companionModule.tree.asInstanceOf[ValDef].tpt.tpe.asType
    val companionModuleExpr = Ident(companionModule).asExpr
    val mainAnnotationInstance = typeSymbolOfB.getAnnotation(mainAnnotation).getOrElse {
      '{ new mainargs.main() }.asTerm // construct a default if not found.
    }
    val ctor = typeSymbolOfB.primaryConstructor
    val ctorParams = ctor.paramSymss.flatten
    // try to match the apply method with the constructor parameters, this is a good heuristic
    // for if the apply method is overloaded.
    val annotatedMethod = typeSymbolOfB.companionModule.memberMethod("apply").filter(p =>
      p.paramSymss.flatten.corresponds(ctorParams) { (p1, p2) =>
        p1.name == p2.name
      }
    ).headOption.getOrElse {
      report.errorAndAbort(
        s"Cannot find apply method in companion object of ${typeReprOfB.show}",
        typeSymbolOfB.companionModule.pos.getOrElse(Position.ofMacroExpansion)
      )
    }
    companionModuleType match
      case '[bCompanion] =>
        val mainData = createMainData[B, bCompanion](
          annotatedMethod,
          mainAnnotationInstance,
          // Somehow the `apply` method parameter annotations don't end up on
          // the `apply` method parameters, but end up in the `<init>` method
          // parameters, so use those for getting the annotations instead
          TypeRepr.of[B].typeSymbol.primaryConstructor.paramSymss
        )
        val erasedMainData = '{ $mainData.asInstanceOf[MainData[B, Any]] }
        '{ new ParserForClass[B]($erasedMainData, () => ${ Ident(companionModule).asExpr }) }
  }

  def createMainData[T: Type, B: Type](using
      Quotes
  )(method: quotes.reflect.Symbol, mainAnnotation: quotes.reflect.Term): Expr[MainData[T, B]] = {
    createMainData[T, B](method, mainAnnotation, method.paramSymss)
  }

  private object VarargTypeRepr {
    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] = {
      import quotes.reflect.*
      tpe match {
        case AnnotatedType(AppliedType(_, Seq(arg)), x) // Scala 3 repeated parameter
            if x.tpe =:= defn.RepeatedAnnot.typeRef => Some(arg)
        case AppliedType(
              TypeRef(pre, "<repeated>"),
              Seq(arg)
            ) // Scala 2 repeated parameter, can be read from Scala 3
            if pre =:= defn.ScalaPackage.termRef => Some(arg)
        case _ => None
      }
    }
  }

  private object AsType {
    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Some[Type[?]] = {
      Some(tpe.asType)
    }
  }

  def createMainData[T: Type, B: Type](using Quotes)(
      method: quotes.reflect.Symbol,
      mainAnnotation: quotes.reflect.Term,
      annotatedParamsLists: List[List[quotes.reflect.Symbol]]
  ): Expr[MainData[T, B]] = {

    import quotes.reflect.*
    val params = method.paramSymss.headOption.getOrElse(
      report.throwError("Multiple parameter lists not supported")
    )
    val defaultParams =
      if (params.exists(_.flags.is(Flags.HasDefault))) getDefaultParams(method) else Map.empty
    val argSigsExprs = params.zip(annotatedParamsLists.flatten).map { paramAndAnnotParam =>
      val param = paramAndAnnotParam._1
      val annotParam = paramAndAnnotParam._2
      val paramTree = param.tree.asInstanceOf[ValDef]
      val paramTpe = paramTree.tpt.tpe
      val readerTpe = paramTpe match {
        case VarargTypeRepr(AsType('[t])) => TypeRepr.of[Leftover[t]]
        case _ => paramTpe
      }
      val arg = annotParam.getAnnotation(argAnnotation).map(_.asExprOf[mainargs.arg]).getOrElse('{
        new mainargs.arg()
      })
      readerTpe.asType match {
        case '[t] =>
          def applyAndCast(f: Expr[Any] => Expr[Any], arg: Expr[B]): Expr[t] = {
            f(arg) match {
              case '{ $v: `t` } => v
              case expr => {
                // this case will be activated when the found default parameter is not of type `t`
                val recoveredType =
                  try
                    expr.asExprOf[t]
                  catch
                    case err: Exception =>
                      report.errorAndAbort(
                        s"""Failed to convert default value for parameter ${param.name},
                           |expected type: ${paramTpe.show},
                           |but default value ${expr.show} is of type: ${expr.asTerm.tpe.widen.show}
                           |while converting type caught an exception with message: ${err.getMessage}
                           |There might be a bug in mainargs.""".stripMargin,
                        param.pos.getOrElse(Position.ofMacroExpansion)
                      )
                recoveredType
              }
            }
          }
          val defaultParam: Expr[Option[B => t]] = defaultParams.get(param) match {
            case Some(f) => '{ Some((b: B) => ${ applyAndCast(f, 'b) }) }
            case None => '{ None }
          }
          val tokensReader = Expr.summon[mainargs.TokensReader[t]].getOrElse {
            report.errorAndAbort(
              s"No mainargs.TokensReader[${Type.show[t]}] found for parameter ${param.name} of method ${method.name} in ${method.owner.fullName}",
              method.pos.getOrElse(Position.ofMacroExpansion)
            )
          }
          '{
            (ArgSig.create[t, B](${ Expr(param.name) }, ${ arg }, ${ defaultParam })(using
            ${ tokensReader }))
          }
      }
    }
    val argSigs = Expr.ofList(argSigsExprs)

    val invokeRaw: Expr[(B, Seq[Any]) => T] = {

      def callOf(methodOwner: Expr[Any], args: Expr[Seq[Any]]) =
        call(methodOwner, method, args).asExprOf[T]

      '{ (b: B, params: Seq[Any]) => ${ callOf('b, 'params) } }
    }
    '{
      MainData.create[T, B](
        ${ Expr(method.name) },
        ${ mainAnnotation.asExprOf[mainargs.main] },
        ${ argSigs },
        ${ invokeRaw }
      )
    }
  }

  /**
   * Call a method given by its symbol.
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
   */
  private def call(using Quotes)(
      methodOwner: Expr[Any],
      method: quotes.reflect.Symbol,
      args: Expr[Seq[Any]]
  ): Expr[_] = {
    // Copy pasted from Cask.
    // https://github.com/com-lihaoyi/cask/blob/65b9c8e4fd528feb71575f6e5ef7b5e2e16abbd9/cask/src-3/cask/router/Macros.scala#L106
    import quotes.reflect._
    val paramss = method.paramSymss

    if (paramss.isEmpty) {
      report.errorAndAbort("At least one parameter list must be declared.", method.pos.get)
    }
    if (paramss.sizeIs > 1) {
      report.errorAndAbort("Multiple parameter lists are not supported.", method.pos.get)
    }
    val params = paramss.head

    val methodType = methodOwner.asTerm.tpe.memberType(method)

    def accesses(ref: Expr[Seq[Any]]): List[Term] =
      for (i <- params.indices.toList) yield {
        val param = params(i)
        val tpe = methodType.memberType(param)
        val untypedRef = '{ $ref(${ Expr(i) }) }
        tpe match {
          case VarargTypeRepr(AsType('[t])) =>
            Typed(
              '{ $untypedRef.asInstanceOf[Leftover[t]].value }.asTerm,
              Inferred(AppliedType(defn.RepeatedParamClass.typeRef, List(TypeRepr.of[t])))
            )
          case _ => tpe.asType match
              case '[t] => '{ $untypedRef.asInstanceOf[t] }.asTerm
        }
      }

    methodOwner.asTerm.select(method).appliedToArgs(accesses(args)).asExpr
  }

  /** Lookup default values for a method's parameters. */
  private def getDefaultParams(using
      Quotes
  )(method: quotes.reflect.Symbol): Map[quotes.reflect.Symbol, Expr[Any] => Expr[Any]] = {
    // Copy pasted from Cask.
    // https://github.com/com-lihaoyi/cask/blob/65b9c8e4fd528feb71575f6e5ef7b5e2e16abbd9/cask/src-3/cask/router/Macros.scala#L38
    import quotes.reflect._

    val params = method.paramSymss.flatten
    val defaults = collection.mutable.Map.empty[Symbol, Expr[Any] => Expr[Any]]

    val Name = (method.name + """\$default\$(\d+)""").r
    val InitName = """\$lessinit\$greater\$default\$(\d+)""".r

    val idents = method.owner.tree.asInstanceOf[ClassDef].body

    idents.foreach {
      case deff @ DefDef(Name(idx), _, _, _) =>
        val expr = (owner: Expr[Any]) => Select(owner.asTerm, deff.symbol).asExpr
        defaults += (params(idx.toInt - 1) -> expr)

      // The `apply` method re-uses the default param factory methods from `<init>`,
      // so make sure to check if those exist too
      case deff @ DefDef(InitName(idx), _, _, _) if method.name == "apply" =>
        val expr = (owner: Expr[Any]) => Select(owner.asTerm, deff.symbol).asExpr
        defaults += (params(idx.toInt - 1) -> expr)

      case _ =>
    }

    defaults.toMap
  }
}
