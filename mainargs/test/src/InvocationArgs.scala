package mainargs
import utest._
@main
case class LargeArgs(
    v1: String,
    v2: String = "v2-default",
    v3: Option[String] = None,
    v4: Option[String] = None,
    v5: Int,
    v6: Int = 0,
    v7: Int = 123,
    v8: Int = 3.14.toInt,
    v9: Boolean,
    v10: Boolean = true,
    v11: Boolean = false,
    v12: Option[Int] = None,
    v13: Option[Int] = None,
    v14: String = "v14-default",
    v15: String = "v15-default",
    v16: String = "v16-default",
    v17: String = "v17-default",
    v18: String = "v18-default",
    v19: String = "v19-default",
    v20: String = "v20-default",
    v21: String = "v21-default",
    v22: String = "v22-default",
    v23: String = "v23-default",
    v24: String = "v24-default",
    v25: String = "v25-default",
    v26: String = "v26-default",
    v27: String = "v27-default",
    v28: String = "v28-default",
    v29: String = "v29-default",
    v30: String = "v30-default",
    v31: String = "v31-default",
    v32: String = "v32-default"
)

object LargeClassTests extends TestSuite {
  val largeArgsParser = ParserForClass[LargeArgs]

  val tests = Tests {
    test("simple") {
      largeArgsParser.constructOrThrow(
        Seq("--v1", "v1-value", "--v5", "5", "--v9", "true", "--v23", "v23-value")
      ) ==>
        LargeArgs(v1 = "v1-value", v5 = 5, v9 = true, v23 = "v23-value")
    }
  }
}
