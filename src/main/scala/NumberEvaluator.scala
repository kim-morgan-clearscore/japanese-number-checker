import Rules.{singleDigit, *}
import model.*

import scala.annotation.tailrec

object NumberEvaluator {
  def rewrite[A](
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

  def orderedIterate[A](
      text: List[Char | A],
      rules: List[Rewrite[A]]
  ): List[Char | A] = {
    rules.foldLeft(text) { (t, rule) => iterate(t, rule) }
  }

  @tailrec
  def iterate[A](text: List[Char | A], rule: Rewrite[A]): List[Char | A] = {
    println(text)

    val t = rewrite(text, rule)
    if (t == text) text
    else iterate(t, rule)
  }

  val allJapaneseRules: List[Rewrite[Entity]] = List(
    singleDigit
      .orElse(multiDigit),
    japaneseDigits,
    japaneseTens,
    japaneseHundreds,
    japaneseThousands,
    japaneseTenThousands,
    japaneseHundredMillions,
    japaneseMonths
      .orElse(japaneseYears)
      .orElse(mixedNumberCombination)
      .orElse(japaneseNumberCombination)
  )

  val allEnglishRules: List[Rewrite[Entity]] = List(
    singleDigit
      .orElse(multiDigit)
      .orElse(singleWordTens)
      .orElse(singleWordDigit)
      .orElse(singleWordLargeUnits)
      .orElse(wordNumberPunctuationRemoval),
    wordDigitEvaluation,
    wordTenEvaluation,
    wordHundredEvaluation,
    wordThousandEvaluation,
    wordMillionEvaluation,
    wordBillionEvaluation
  )
}
