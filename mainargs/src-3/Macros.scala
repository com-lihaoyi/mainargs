package mainargs

import scala.quoted._

object Macros {
  def parserForMethods[B](base: Expr[B])(using Quotes, Type[B]): Expr[ParserForMethods[B]] = {
    import quotes.reflect._
    val allMethods = TypeRepr.of[B].typeSymbol.memberMethods
    val mainAnnotation = TypeRepr.of[mainargs.main].typeSymbol
    val argAnnotation = TypeRepr.of[mainargs.arg].typeSymbol
    val annotatedMethodsWithMainAnnotations = allMethods.flatMap { methodSymbol =>
      methodSymbol.getAnnotation(mainAnnotation).map(methodSymbol -> _)
    }.sortBy(_._1.pos.map(_.start))
    val mainDatasExprs: Seq[Expr[MainData[Any, B]]] = annotatedMethodsWithMainAnnotations.map { (annotatedMethod, mainAnnotation) =>
      val doc: Option[String] = mainAnnotation match {
        case Apply(_, args) => args.collectFirst {
          case NamedArg("doc", Literal(constant)) => constant.value.asInstanceOf[String]
        }
        case _ => None
      }
      val params = annotatedMethod.paramSymss.headOption.getOrElse(throw new Exception("Multiple parameter lists not supported"))
      val defaultParams = getDefaultParams(annotatedMethod)
      val argSigs = Expr.ofList(params.map { param =>
        val paramTree = param.tree.asInstanceOf[ValDef]
        val paramTpe = paramTree.tpt.tpe
        val arg = param.getAnnotation(argAnnotation).map(_.asExpr.asInstanceOf[Expr[mainargs.arg]]).getOrElse('{ new mainargs.arg() })
        val paramType = paramTpe.asType
        paramType match
          case '[t] =>
            val defaultParam: Expr[Option[B => t]] = defaultParams.get(param) match {
              case Some(v) => '{ Some(((_: B) => $v).asInstanceOf[B => t]) }
              case None => '{ None }
            }
            val argReader = Expr.summon[mainargs.ArgReader[t]].getOrElse{
              report.error(
                s"No mainargs.ArgReader of ${paramTpe.typeSymbol.fullName} found for parameter ${param.name}",
                param.pos.get
              )
              '{ ??? }
            }
            '{ ArgSig.create[t, B](${ Expr(param.name) }, ${ arg }, ${ defaultParam })(using ${ argReader }) }
      })

      val invokeRaw: Expr[(B, Seq[Any]) => Any] = {
        def callOf(args: Expr[Seq[Any]]) = call(annotatedMethod, '{ Seq( ${ args }) })
        '{ (b: B, params: Seq[Any]) =>
          ${ callOf('{ params }) }
        }
      }

      '{ MainData[Any, B](${ Expr(annotatedMethod.name) }, ${ argSigs }, ${Expr(doc)}, ${ invokeRaw }) }
    }
    val mainDatas = Expr.ofList(mainDatasExprs)

    '{
      new ParserForMethods[B](
        MethodMains[B](${ mainDatas }, () => ${ base })
      )
    }
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
    method: quotes.reflect.Symbol,
    argss: Expr[Seq[Seq[Any]]]
  ): Expr[_] = {
    // Copy pasted from Cask.
    // https://github.com/com-lihaoyi/cask/blob/65b9c8e4fd528feb71575f6e5ef7b5e2e16abbd9/cask/src-3/cask/router/Macros.scala#L106
    import quotes.reflect._
    val paramss = method.paramSymss

    if (paramss.isEmpty) {
      report.error("At least one parameter list must be declared.", method.pos.get)
      return '{???}
    }

    val fct = Ref(method)

    val accesses: List[List[Term]] = for (i <- paramss.indices.toList) yield {
      for (j <- paramss(i).indices.toList) yield {
        val tpe = paramss(i)(j).tree.asInstanceOf[ValDef].tpt.tpe
        tpe.asType match
          case '[t] => '{ $argss(${Expr(i)})(${Expr(j)}).asInstanceOf[t] }.asTerm
      }
    }

    val base = Apply(fct, accesses.head)
    val application: Apply = accesses.tail.foldLeft(base)((lhs, args) => Apply(lhs, args))
    val expr = application.asExpr
    expr
  }


  /** Lookup default values for a method's parameters. */
  private def getDefaultParams(using Quotes)(method: quotes.reflect.Symbol): Map[quotes.reflect.Symbol, Expr[Any]] = {
    // Copy pasted from Cask.
    // https://github.com/com-lihaoyi/cask/blob/65b9c8e4fd528feb71575f6e5ef7b5e2e16abbd9/cask/src-3/cask/router/Macros.scala#L38
    import quotes.reflect._

    val params = method.paramSymss.flatten
    val defaults = collection.mutable.Map.empty[Symbol, Expr[Any]]

    val Name = (method.name + """\$default\$(\d+)""").r

    val idents = method.owner.tree.asInstanceOf[ClassDef].body
    idents.foreach{
      case deff @ DefDef(Name(idx), _, _, _) =>
        val expr = Ref(deff.symbol).asExpr
        defaults += (params(idx.toInt - 1) -> expr)
      case _ =>
    }

    defaults.toMap
  }
}
