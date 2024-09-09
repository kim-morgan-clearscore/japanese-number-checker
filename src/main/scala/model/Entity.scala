package model

sealed trait Entity
final case class Number(value: Long) extends Entity
final case class JapaneseNumber(value: Long) extends Entity
final case class Year(value: Int) extends Entity
final case class Month(value: Int) extends Entity
