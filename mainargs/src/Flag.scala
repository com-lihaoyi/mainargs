package mainargs
case class Flag(value: Boolean = false) {
  val toOption: Option[Boolean] = if (value) Some(true) else None
}
