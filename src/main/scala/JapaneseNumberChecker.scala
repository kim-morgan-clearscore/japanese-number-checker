import Rules.*
import model.*

import scala.annotation.tailrec

object JapaneseNumberChecker {
  private def rewrite[A](
      text: List[Char | A],
      rule: Rewrite[A]
  ): List[Char | A] =
    text match {
      case head :: rest =>
        rule(text) match {
          case None               => head :: rewrite(rest, rule)
          case Some(head :: rest) =>
            // The conditions on rewrites ensure that if they match, the head is
            // the rewritten result and rest is the unmatched remainder that we
            // should attempt to process.
            head :: rewrite(rest, rule)
          case Some(Nil) =>
            // This case can't happen but need it to make exhaustiveness
            // checking happy (and to catch buggy rules!)
            throw new Exception(
              s"A Rewrite rule returned Nil on input $text, which should never happen."
            )
        }

      case Nil => Nil
    }

  @tailrec
  def iterate[A](text: List[Char | A], rule: Rewrite[A]): List[Char | A] = {
    println(text)

    val t = rewrite(text, rule)
    if (t == text) text
    else iterate(t, rule)
  }

  def runTests(): Unit = {
    println(rewrite("1 is something we should find".toList, singleDigit))
    println(iterate("1 is something we should find".toList, singleDigit))
    println(iterate("一と二を見つけるといいでしょう".toList, japaneseDigits))
    println(
      iterate(
        "もはや12月ですね".toList,
        singleDigit.orElse(multiDigit).orElse(japaneseMonths)
      )
    )
    println(
      iterate(
        "今年は2024年です".toList,
        singleDigit.orElse(multiDigit).orElse(japaneseYears)
      )
    )
    println(
      iterate(
        "明治3年".toList,
        singleDigit.orElse(multiDigit).orElse(japaneseYears)
      )
    )
    println(
      iterate(
        "二十億三千六百五十万千八百一".toList,
        normaliseInput
          .orElse(japaneseDigits)
          .orElse(japaneseMultipliers)
          .orElse(japaneseNumberCombination)
      )
    )
    // japanese number over 10000
    println(
      iterate(
        "四千万".toList,
        normaliseInput
          .orElse(japaneseDigits)
          .orElse(japaneseMultipliers)
          .orElse(japaneseNumberCombination)
      )
    ) // 4011200
    println(iterate("1012".toList, singleDigit.orElse(multiDigit)))
    println(iterate("65535".toList, singleDigit.orElse(multiDigit)))
    println(iterate("262144".toList, singleDigit.orElse(multiDigit)))
    println(iterate("16777215".toList, singleDigit.orElse(multiDigit)))
  }
}
