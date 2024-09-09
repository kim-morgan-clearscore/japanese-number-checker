import munit.FunSuite
import model.*
import Rules.*
import JapaneseNumberChecker.*

class JapaneseNumberCheckerTest extends FunSuite {
  test("rewrite: applies rewrite rules to a string once") {
    assertEquals(
      JapaneseNumberChecker.rewrite(
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
      JapaneseNumberChecker
        .orderedIterate("1 is something we should find".toList, List(singleDigit)),
      List(Number(1)) ++ " is something we should find".toList
    )
  }

  test(
    "iterate: can handle single Japanese digits"
  ) {
    assertEquals(
      JapaneseNumberChecker
        .orderedIterate("一と二を見つけるといいでしょう".toList, List(japaneseDigits)),
      List(JapaneseNumber(1), 'と', JapaneseNumber(2)) ++ "を見つけるといいでしょう".toList
    )
  }

  test(
    "iterate: can handle japanese month"
  ) {
    assertEquals(
      JapaneseNumberChecker
        .orderedIterate(
          "もはや12月ですね".toList,
          allRules
        ),
      List('も', 'は', 'や', Month(12), 'で', 'す', 'ね')
    )
  }

  test(
    "iterate: can handle western year"
  ) {
    assertEquals(
      JapaneseNumberChecker
        .orderedIterate(
          "今年は2024年です".toList,
          allRules
        ),
      List('今', '年', 'は', Year(2024), 'で', 'す')
    )
  }

  test(
    "iterate: can handle japanese year - meiji"
  ) {
    assertEquals(
      JapaneseNumberChecker
        .orderedIterate(
          "明治3年".toList,
          allRules
        ),
      List(Year(1870))
    )
  }

  test(
    "iterate: can handle japanese year - showa"
  ) {
    assertEquals(
      JapaneseNumberChecker.orderedIterate(
        "昭和60年".toList,
        allRules
      ),
      List(Year(1985))
    )
  }

  test(
    "iterate: can handle japanese year - heisei"
  ) {
    assertEquals(
      JapaneseNumberChecker.orderedIterate(
        "平成20年".toList,
        allRules
      ),
      List(Year(2008))
    )
  }

  test(
    "iterate: can handle japanese year - reiwa"
  ) {
    assertEquals(
      JapaneseNumberChecker.orderedIterate(
        "令和2年".toList,
        allRules
      ),
      List(Year(2020))
    )
  }

  test("iterate: can handle japanese year with number in Kanji") {
    assertEquals(
      JapaneseNumberChecker.orderedIterate(
        "平成三年".toList,
        allRules
      ),
      List(Year(1991))
    )
  }

  test("iterate: can handle gannen year") {
    assertEquals(
      JapaneseNumberChecker.orderedIterate(
        "令和元年".toList,
        allRules
      ),
      List(Year(2019))
    )
  }

  test("iterate: can handle japanese kanji number over 10") {
    assertEquals(
      JapaneseNumberChecker.orderedIterate("三十一".toList, allRules),
      List(JapaneseNumber(31))
    )
  }

  test("iterate: can handle standalone kanji 10") {
    assertEquals(
      JapaneseNumberChecker.orderedIterate("十".toList, allRules),
      List(JapaneseNumber(10))
    )
  }

  test("iterate: can handle standalone kanji 100") {
    assertEquals(
      JapaneseNumberChecker.orderedIterate("百".toList, allRules),
      List(JapaneseNumber(100))
    )
  }

  test("iterate: can handle hundred with multiplier followed by kanji number") {
    assertEquals(
      JapaneseNumberChecker.orderedIterate("三百二十".toList, allRules),
      List(JapaneseNumber(320))
    )
  }

  test(
    "iterate: can handle thousand with multiplier followed by kanji number"
  ) {
    assertEquals(
      JapaneseNumberChecker.orderedIterate("三千三百二十二".toList, allRules),
      List(JapaneseNumber(3322))
    )
  }

  test("iterate: can handle standalone japanese thousand") {
    assertEquals(
      JapaneseNumberChecker.orderedIterate("千".toList, allRules),
      List(JapaneseNumber(1000))
    )
  }

  test("iterate: can handle standalone japanese ten thousand") {
    assertEquals(
      JapaneseNumberChecker.orderedIterate("万".toList, allRules),
      List(JapaneseNumber(10000))
    )
  }

  test("iterate: can handle kanji ten thousand followed by kanji number") {
    assertEquals(
      JapaneseNumberChecker.orderedIterate("二万三千四百五十六".toList, allRules),
      List(JapaneseNumber(23456))
    )
  }

  test("iterate: can handle kanji hundred million followed by kanji number") {
    assertEquals(
      JapaneseNumberChecker.orderedIterate("三億四千五百六十七万八千九百十一".toList, allRules),
      List(JapaneseNumber(345678911))
    )
  }

  test("iterate: can handle standalone japanese hundred million") {
    assertEquals(
      JapaneseNumberChecker.orderedIterate("億".toList, allRules),
      List(JapaneseNumber(100000000))
    )
  }

  test("iterate: can handle mixed arabic and kanji number") {
    assertEquals(
      JapaneseNumberChecker.orderedIterate(
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
      JapaneseNumberChecker.orderedIterate("123456789".toList, allRules),
      List(Number(123456789))
    )
  }

}
