package model

sealed trait Entity
final case class Number(value: Int) extends Entity
final case class JapaneseDigit(value: Int) extends Entity
final case class JapaneseNumber(value: Long) extends Entity
final case class JapaneseMultiplier(value: Char) extends Entity
final case class Year(value: Int) extends Entity
final case class Month(value: Int) extends Entity
