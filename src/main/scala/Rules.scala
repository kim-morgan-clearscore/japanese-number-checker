import model.*

object Rules {
  val singleDigit: Rewrite[Entity] =
    Rewrite
      .lift[Entity] { case '0' :: rest => Number(0) :: rest }
      .orElse(
        Rewrite.lift { case '1' :: rest => Number(1) :: rest }
      )
      .orElse(
        Rewrite.lift { case '2' :: rest => Number(2) :: rest }
      )
      .orElse(
        Rewrite.lift { case '3' :: rest => Number(3) :: rest }
      )
      .orElse(
        Rewrite.lift { case '4' :: rest => Number(4) :: rest }
      )
      .orElse(
        Rewrite.lift { case '5' :: rest => Number(5) :: rest }
      )
      .orElse(
        Rewrite.lift { case '6' :: rest => Number(6) :: rest }
      )
      .orElse(
        Rewrite.lift { case '7' :: rest => Number(7) :: rest }
      )
      .orElse(
        Rewrite.lift { case '8' :: rest => Number(8) :: rest }
      )
      .orElse(
        Rewrite.lift { case '9' :: rest => Number(9) :: rest }
      )

  val japaneseDigits = Rewrite
    .lift[Entity] { case '一' :: rest =>
      JapaneseNumber(1) :: rest
    }
    .orElse(
      Rewrite.lift { case '二' :: rest => JapaneseNumber(2) :: rest }
    )
    .orElse(
      Rewrite.lift { case '三' :: rest => JapaneseNumber(3) :: rest }
    )
    .orElse(
      Rewrite.lift { case '四' :: rest => JapaneseNumber(4) :: rest }
    )
    .orElse(
      Rewrite.lift { case '五' :: rest => JapaneseNumber(5) :: rest }
    )
    .orElse(
      Rewrite.lift { case '六' :: rest => JapaneseNumber(6) :: rest }
    )
    .orElse(
      Rewrite.lift { case '七' :: rest => JapaneseNumber(7) :: rest }
    )
    .orElse(
      Rewrite.lift { case '八' :: rest => JapaneseNumber(8) :: rest }
    )
    .orElse(
      Rewrite.lift { case '九' :: rest => JapaneseNumber(9) :: rest }
    )

  val japaneseTens = Rewrite
    .lift[Entity] {
      case JapaneseNumber(a) :: '十' :: JapaneseNumber(b) :: rest =>
        JapaneseNumber(a * 10 + b) :: rest
    }
    .orElse(
      Rewrite.lift { case Number(a) :: '十' :: Number(b) :: rest =>
        JapaneseNumber(a * 10 + b) :: rest
      }
    )
    .orElse(
      Rewrite.lift { case JapaneseNumber(a) :: '十' :: rest =>
        JapaneseNumber(a * 10) :: rest
      }
    )
    .orElse(
      Rewrite.lift { case Number(a) :: '十' :: rest =>
        JapaneseNumber(a * 10) :: rest
      }
    )
    .orElse(
      Rewrite.lift { case '十' :: rest =>
        JapaneseNumber(10) :: rest
      }
    )

  val japaneseHundreds = Rewrite
    .lift[Entity] {
      case JapaneseNumber(a) :: '百' :: JapaneseNumber(b) :: rest =>
        JapaneseNumber(a * 100 + b) :: rest
    }
    .orElse(
      Rewrite.lift { case JapaneseNumber(a) :: '百' :: rest =>
        JapaneseNumber(a * 100) :: rest
      }
    )
    .orElse(
      Rewrite.lift { case Number(a) :: '百' :: Number(b) :: rest =>
        JapaneseNumber(a * 100 + b) :: rest
      }
    )
    .orElse(
      Rewrite.lift { case Number(a) :: '百' :: rest =>
        JapaneseNumber(a * 100) :: rest
      }
    )
    .orElse(
      Rewrite.lift { case '百' :: rest =>
        JapaneseNumber(100) :: rest
      }
    )

  val japaneseThousands = Rewrite
    .lift[Entity] {
      case JapaneseNumber(a) :: '千' :: JapaneseNumber(b) :: rest =>
        JapaneseNumber(a * 1000 + b) :: rest
    }
    .orElse(
      Rewrite.lift { case JapaneseNumber(a) :: '千' :: rest =>
        JapaneseNumber(a * 1000) :: rest
      }
    )
    .orElse(
      Rewrite.lift { case Number(a) :: '千' :: Number(b) :: rest =>
        JapaneseNumber(a * 1000 + b) :: rest
      }
    )
    .orElse(
      Rewrite.lift { case Number(a) :: '千' :: rest =>
        JapaneseNumber(a * 1000) :: rest
      }
    )
    .orElse(
      Rewrite.lift { case '千' :: rest =>
        JapaneseNumber(1000) :: rest
      }
    )

  val japaneseTenThousands = Rewrite
    .lift[Entity] {
      case JapaneseNumber(a) :: '万' :: JapaneseNumber(b) :: rest =>
        JapaneseNumber(a * 10000 + b) :: rest
    }
    .orElse(Rewrite.lift { case JapaneseNumber(a) :: '万' :: rest =>
      JapaneseNumber(a * 10000) :: rest
    })
    .orElse(
      Rewrite.lift { case Number(a) :: '万' :: Number(b) :: rest =>
        JapaneseNumber(a * 10000 + b) :: rest
      }
    )
    .orElse(
      Rewrite.lift { case Number(a) :: '万' :: rest =>
        JapaneseNumber(a * 10000) :: rest
      }
    )
    .orElse(
      Rewrite.lift { case '万' :: rest =>
        JapaneseNumber(10000) :: rest
      }
    )

  val japaneseHundredMillions = Rewrite
    .lift[Entity] {
      case JapaneseNumber(a) :: '億' :: JapaneseNumber(b) :: rest =>
        JapaneseNumber(a * 100000000 + b) :: rest
    }
    .orElse(
      Rewrite.lift { case JapaneseNumber(a) :: '億' :: rest =>
        JapaneseNumber(a * 100000000) :: rest
      }
    )
    .orElse(
      Rewrite.lift { case Number(a) :: '億' :: Number(b) :: rest =>
        JapaneseNumber(a * 100000000 + b) :: rest
      }
    )
    .orElse(
      Rewrite.lift { case Number(a) :: '億' :: rest =>
        JapaneseNumber(a * 100000000) :: rest
      }
    )
    .orElse(
      Rewrite.lift { case '億' :: rest =>
        JapaneseNumber(100000000) :: rest
      }
    )

  val japaneseNumberCombination = Rewrite.lift[Entity] {
    case JapaneseNumber(a) :: JapaneseNumber(b) :: rest =>
      JapaneseNumber(a + b) :: rest
  }

  val mixedNumberCombination = Rewrite
    .lift[Entity] { case Number(a) :: JapaneseNumber(b) :: Number(c) :: rest =>
      JapaneseNumber(a * b + c) :: rest
    }
    .orElse(
      Rewrite
        .lift { case Number(a) :: JapaneseNumber(b) :: rest =>
          JapaneseNumber(a * b) :: rest
        }
    )
    .orElse(
      Rewrite.lift { case JapaneseNumber(a) :: Number(b) :: rest =>
        JapaneseNumber(a + b) :: rest
      }
    )

  val japaneseMonths: Rewrite[Entity] =
    Rewrite
      .lift[Entity] { case Number(a) :: '月' :: rest =>
        Month(a.toInt) :: rest
      } orElse (Rewrite.lift { case JapaneseNumber(a) :: '月' :: rest =>
      Month(a.toInt) :: rest
    })

  val japaneseYears: Rewrite[Entity] =
    Rewrite
      .lift[Entity] { case Number(a) :: '年' :: rest =>
        Year(a.toInt) :: rest
      }
      .orElse(
        Rewrite.lift { case JapaneseNumber(a) :: '年' :: rest =>
          Year(a.toInt) :: rest
        }
      )
      .orElse(
        Rewrite.lift { case '元' :: '年' :: rest => Year(1) :: rest }
      )
      .orElse(
        Rewrite.lift { case '明' :: '治' :: rest => Year(1868) :: rest }
      )
      .orElse(
        Rewrite.lift { case '大' :: '正' :: rest => Year(1912) :: rest }
      )
      .orElse(
        Rewrite.lift { case '昭' :: '和' :: rest => Year(1926) :: rest }
      )
      .orElse(
        Rewrite.lift { case '平' :: '成' :: rest => Year(1989) :: rest }
      )
      .orElse(
        Rewrite.lift { case '令' :: '和' :: rest => Year(2019) :: rest }
      )
      .orElse(
        Rewrite.lift { case Year(a) :: Year(b) :: rest =>
          Year(a + b - 1) :: rest
        }
      )

  private def log10(x: Int): Double = Math.log(x.toDouble) / Math.log(10)

  val multiDigit: Rewrite[Entity] =
    Rewrite.lift { case Number(a) :: Number(b) :: rest =>
      // Don't try to compute the log of 0; it's not defined!
      if (b < 10) Number(a * 10 + b) :: rest
      else
        Number(
          a * Math.pow(10, Math.floor(log10(b.toInt)) + 1).toInt + b
        ) :: rest
    }

}
