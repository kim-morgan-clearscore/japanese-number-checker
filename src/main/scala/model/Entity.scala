package model

sealed trait Entity
final case class Number(value: Long) extends Entity
sealed trait WordNumber extends Entity { val value: Long }
object WordNumber {
  def apply(value: Long): WordNumber = new WordNumber {
    val value: Long = value
  }
}
final case class WordDigit(value: Long) extends WordNumber
final case class WordTen(value: Long) extends WordNumber
case object WordHundred extends WordNumber { val value: Long = 100 }
case object WordThousand extends WordNumber { val value: Long = 1000 }
case object WordMillion extends WordNumber { val value: Long = 1000000 }
case object WordBillion extends WordNumber { val value: Long = 1000000000 }
final case class JapaneseNumber(value: Long) extends Entity
final case class Year(value: Int) extends Entity
final case class Month(value: Int) extends Entity
