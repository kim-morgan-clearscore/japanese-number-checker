package model

trait Rewrite[A] {
  def apply(in: List[A | Char]): Option[List[A | Char]]

  def orElse(that: Rewrite[A]): Rewrite[A] = {
    val self = this
    (in: List[A | Char]) => self(in).orElse(that(in))
  }
}
object Rewrite {
  def lift[A](
      f: PartialFunction[List[A | Char], List[A | Char]]
  ): Rewrite[A] =
    new Rewrite[A] {
      val lifted: List[A | Char] => Option[List[A | Char]] = f.lift
      def apply(in: List[A | Char]): Option[List[A | Char]] =
        lifted(in)
    }
}
