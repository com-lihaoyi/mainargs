package mainargs
import scala.annotation.ClassfileAnnotation


class arg(val name: String = null,
          val short: Char = 0,
          val doc: String = null,
          val noDefaultName: Boolean = false) extends ClassfileAnnotation

class main(val name: String = null, val doc: String = null) extends ClassfileAnnotation
