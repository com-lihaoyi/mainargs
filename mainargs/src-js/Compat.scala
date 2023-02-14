package mainargs

object Compat {
  def exit(n: Int) = throw new Exception()
}
