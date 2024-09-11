import munit.FunSuite
import model.*
import Rules.*
import NumberEvaluator.*

class NumberEvaluatorTest extends FunSuite {
  test("rewrite: applies rewrite rules to a string once") {
    assertEquals(
      NumberEvaluator.rewrite(
        "1 is something we should find".toList,
        singleDigit
      ),
      List(Number(1)) ++ " is something we should find".toList
    )
  }

  test(
    "iterate: rewrite rule is applied repeatedly until no more matches (single digit)"
  ) {
    assertEquals(
      NumberEvaluator
        .orderedIterate(
          "1 is something we should find".toList,
          List(singleDigit)
        ),
      List(Number(1)) ++ " is something we should find".toList
    )
  }

  test(
    "iterate: can handle single Japanese digits"
  ) {
    assertEquals(
      NumberEvaluator
        .orderedIterate("一と二を見つけるといいでしょう".toList, List(japaneseDigits)),
      List(JapaneseNumber(1), 'と', JapaneseNumber(2)) ++ "を見つけるといいでしょう".toList
    )
  }

  test(
    "iterate: can handle japanese month"
  ) {
    assertEquals(
      NumberEvaluator
        .orderedIterate(
          "もはや12月ですね".toList,
          allJapaneseRules
        ),
      List('も', 'は', 'や', Month(12), 'で', 'す', 'ね')
    )
  }

  test(
    "iterate: can handle western year"
  ) {
    assertEquals(
      NumberEvaluator
        .orderedIterate(
          "今年は2024年です".toList,
          allJapaneseRules
        ),
      List('今', '年', 'は', Year(2024), 'で', 'す')
    )
  }

  test(
    "iterate: can handle japanese year - meiji"
  ) {
    assertEquals(
      NumberEvaluator
        .orderedIterate(
          "明治3年".toList,
          allJapaneseRules
        ),
      List(Year(1870))
    )
  }

  test(
    "iterate: can handle japanese year - showa"
  ) {
    assertEquals(
      NumberEvaluator.orderedIterate(
        "昭和60年".toList,
        allJapaneseRules
      ),
      List(Year(1985))
    )
  }

  test(
    "iterate: can handle japanese year - heisei"
  ) {
    assertEquals(
      NumberEvaluator.orderedIterate(
        "平成20年".toList,
        allJapaneseRules
      ),
      List(Year(2008))
    )
  }

  test(
    "iterate: can handle japanese year - reiwa"
  ) {
    assertEquals(
      NumberEvaluator.orderedIterate(
        "令和2年".toList,
        allJapaneseRules
      ),
      List(Year(2020))
    )
  }

  test("iterate: can handle japanese year with number in Kanji") {
    assertEquals(
      NumberEvaluator.orderedIterate(
        "平成三年".toList,
        allJapaneseRules
      ),
      List(Year(1991))
    )
  }

  test("iterate: can handle gannen year") {
    assertEquals(
      NumberEvaluator.orderedIterate(
        "令和元年".toList,
        allJapaneseRules
      ),
      List(Year(2019))
    )
  }

  test("iterate: can handle japanese kanji number over 10") {
    assertEquals(
      NumberEvaluator.orderedIterate("三十一".toList, allJapaneseRules),
      List(JapaneseNumber(31))
    )
  }

  test("iterate: can handle standalone kanji 10") {
    assertEquals(
      NumberEvaluator.orderedIterate("十".toList, allJapaneseRules),
      List(JapaneseNumber(10))
    )
  }

  test("iterate: can handle standalone kanji 100") {
    assertEquals(
      NumberEvaluator.orderedIterate("百".toList, allJapaneseRules),
      List(JapaneseNumber(100))
    )
  }

  test("iterate: can handle hundred with multiplier followed by kanji number") {
    assertEquals(
      NumberEvaluator.orderedIterate("三百二十".toList, allJapaneseRules),
      List(JapaneseNumber(320))
    )
  }

  test(
    "iterate: can handle thousand with multiplier followed by kanji number"
  ) {
    assertEquals(
      NumberEvaluator.orderedIterate("三千三百二十二".toList, allJapaneseRules),
      List(JapaneseNumber(3322))
    )
  }

  test("iterate: can handle standalone japanese thousand") {
    assertEquals(
      NumberEvaluator.orderedIterate("千".toList, allJapaneseRules),
      List(JapaneseNumber(1000))
    )
  }

  test("iterate: can handle standalone japanese ten thousand") {
    assertEquals(
      NumberEvaluator.orderedIterate("万".toList, allJapaneseRules),
      List(JapaneseNumber(10000))
    )
  }

  test("iterate: can handle kanji ten thousand followed by kanji number") {
    assertEquals(
      NumberEvaluator
        .orderedIterate("二万三千四百五十六".toList, allJapaneseRules),
      List(JapaneseNumber(23456))
    )
  }

  test("iterate: can handle kanji hundred million followed by kanji number") {
    assertEquals(
      NumberEvaluator
        .orderedIterate("三億四千五百六十七万八千九百十一".toList, allJapaneseRules),
      List(JapaneseNumber(345678911))
    )
  }

  test("iterate: can handle standalone japanese hundred million") {
    assertEquals(
      NumberEvaluator.orderedIterate("億".toList, allJapaneseRules),
      List(JapaneseNumber(100000000))
    )
  }

  test("iterate: can handle mixed arabic and kanji number") {
    assertEquals(
      NumberEvaluator.orderedIterate(
        "35万123".toList,
        List(
          singleDigit
            .orElse(multiDigit),
          japaneseTenThousands
            .orElse(mixedNumberCombination)
        )
      ),
      List(JapaneseNumber(350123))
    )
  }

  test("iterate: can handle long arabic number") {
    assertEquals(
      NumberEvaluator
        .orderedIterate("123456789".toList, allJapaneseRules),
      List(Number(123456789))
    )
  }

  test("iterate: can handle word digits in English") {
    assertEquals(
      NumberEvaluator.orderedIterate(
        "one is something we should find".toList,
        List(singleWordDigit)
      ),
      List(WordDigit(1)) ++ " is something we should find".toList
    )
  }

  test("iterate: can handle word tens in English") {
    assertEquals(
      NumberEvaluator.orderedIterate(
        "ten twenty thirty forty fifty sixty seventy eighty".toList,
        List(singleWordTens)
      ),
      List(
        WordTen(10),
        ' ',
        WordTen(20),
        ' ',
        WordTen(30),
        ' ',
        WordTen(40),
        ' ',
        WordTen(50),
        ' ',
        WordTen(60),
        ' ',
        WordTen(70),
        ' ',
        WordTen(80)
      )
    )
  }

  test("iterate: can handle word large units in English") {
    assertEquals(
      NumberEvaluator.orderedIterate(
        "hundred thousand million billion".toList,
        List(singleWordLargeUnits)
      ),
      List(
        WordHundred,
        ' ',
        WordThousand,
        ' ',
        WordMillion,
        ' ',
        WordBillion
      )
    )
  }

  test("iterate: can remove punctuation from word number") {
    assertEquals(
      NumberEvaluator.orderedIterate(
        "one million, five hundred forty-three thousand, seven hundred and sixty-five".toList,
        List(
          singleWordTens
            .orElse(singleWordDigit)
            .orElse(singleWordLargeUnits)
            .orElse(wordNumberPunctuationRemoval)
        )
      ),
      List(
        WordDigit(1),
        WordMillion,
        WordDigit(5),
        WordHundred,
        WordTen(40),
        WordDigit(3),
        WordThousand,
        WordDigit(7),
        WordHundred,
        WordTen(60),
        WordDigit(5)
      )
    )
  }

  test("iterate: can evaluate word number") {
    assertEquals(
      NumberEvaluator.orderedIterate(
        "one million, five hundred forty-three thousand, seven hundred and sixty-five".toList,
        allEnglishRules
      ),
      List(
        Number(1543765)
      )
    )
  }

  test("iterate: does not combine word digits of same magnitude") {
    assertEquals(
      NumberEvaluator.orderedIterate(
        "one, two, three".toList,
        allEnglishRules
      ),
      List(
        Number(1),
        Number(2),
        Number(3)
      )
    )
  }

  test("iterate: does not combine tens") {
    assertEquals(
      NumberEvaluator.orderedIterate(
        "twenty, thirty, forty".toList,
        allEnglishRules
      ),
      List(
        Number(20),
        Number(30),
        Number(40)
      )
    )
  }

  test("iterate: can evaluate mixed arabic numeral and word number") {
    assertEquals(
      NumberEvaluator.orderedIterate(
        "7 billion, 542 million, and 32 thousand".toList,
        allEnglishRules
      ),
      List(
        Number(7542032000L)
      )
    )
  }

}
