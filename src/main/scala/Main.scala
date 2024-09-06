import model.*
import Rules.*
import JapaneseNumberChecker.*

object Main extends App {
//  // In the previous example we worked on an expression *tree*. Text, on the
//  // other hand, has a linear structure (unless we first parse it into a parse
//  // tree, but that's a very hard problem we wish to avoid.) This means our
//  // rewrites will also have a different structure. We will define rewrites as
//  // transformations from a list of entities to a list of entities, where an
//  // entity is either a character or some more structured data. We also have
//  // some restrictions on rewrites:
//  //
//  // 1. a rewrite may match any number of entities within the list, but those
//  // must all be consecutive elements and start at the start of the list
//  //
//  // 2. if the rewrite matches it must rewrite the elements it matches to a
//  // single element at the start of its result, and leave the rest of the list
//  // alone.
//  //
//  // These two criteria allow us to ensure we make progress in rewriting and
//  // don't endlessly loop. The criteria are bit imprecisely expressed above.
//  // They will be clearer with examples.
//  //
//  // Let's define `Rewrite`
//
//  // Now let's define entities that we with to extract. For simplicity we will
//  // only look for integers for now.
//
//  // The simplest rule is to find single digit numbers.
//
//  // Now we need a method to apply rewrites
//
//  // We find single digit numbers just fine, but if we have a two or more digit
//  // number we extract it as several single digit numbers. We need to coallesce
//  // consecutive single digit numbers into a single number. How can we do this?
//  // With a rewrite rule!
//  val multiDigit: Rewrite[Entity] =
//    Rewrite.lift { case Number(a) :: Number(b) :: rest =>
//      Number(a * 10 + b) :: rest
//    }
//
//  // Now we see why we have restrictions on rewrites. If we didn't have them we
//  // couldn't safely write this rule.
//  //
//  // Does it work, though? Well, we need to apply the rules multiple times to
//  // see it taking effect, so we need to implement a method that does this.
//  def iterate[A](text: List[Char | A], rule: Rewrite[A]): List[Char | A] = {
//    println(text)
//
//    val t = rewrite(text, rule)
//    if t == text then text
//    else iterate(t, rule)
//  }
//
//  println(
//    iterate(
//      "Is 12 something we handle properly?".toList,
//      singleDigit.orElse(multiDigit)
//    )
//  )
//
//  println("Can we count to 112? What about 1012?")
//  println(
//    iterate(
//      "Can we count to 112? What about 1012?".toList,
//      singleDigit.orElse(multiDigit)
//    )
//  )
//
//  // Hmmm ... it works for 12 and 112, but not 1012. The trace of the rewrites
//  // shows why: we combine two pairs 1 and 0, and 1 and 2 to get 10 and 12, then
//  // compute (10 * 10) + 12 when we really want (10 * 100) + 12.
//  //
//  // Let's correct the implementation. The number of digits in the right hand
//  // side number tells us how many zeros we need to add to the left hand side
//  // number. We can compute this by taking the floor or the log base 10 of the
//  // right hand side number.
//
//  // Better test it!
//  println(iterate("1012".toList, singleDigit.orElse(multiDigitCorrect)))
//  println(iterate("65535".toList, singleDigit.orElse(multiDigitCorrect)))
//  println(iterate("262144".toList, singleDigit.orElse(multiDigitCorrect)))
//  println(iterate("16777215".toList, singleDigit.orElse(multiDigitCorrect)))
//
//  // It works!

//四, 千, 万, 千, 二, 百
// D(4), 千, 万, 千, D(2), 百
// D(4), 千, 万, 千, D(200)
// D(4), 千, 万, 千, D(200)
// D(4000), 万, D(1000), D(200)
// D(40 million), (1000), (200)

//二十二億三千六百五十万千八百一
// N(2) 十 N(2) 億 N(3) 千 N(6) 百 N(5) 十 万 千 N(8) 百 N(1) // evaluate digits
// N(22) 億 N(3) 千 N(6) 百 (5) 万 千 N(8) 百 N(1) // evaluate 10s
// N(22) 億 N(3) 千 N(650) 万 千 N(801) // evaluate 100s
// N(22) 億 N(3) 千 N(650) 万 千 N(801) // evaluate 1000s
// N(22) 億 N(3650) 万 N(1801) // evaluate 10,000s
// N(22) 億 N(36501801) // evaluate 100 millions
// N(2,236,511,801) //2,236,501,801
  runTests()
}
