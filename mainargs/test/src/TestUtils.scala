package mainargs

object TestUtils {
  def scala2Only(f: => Unit): Unit = {
    if (VersionSpecific.isScala3) {} else f
  }
}
