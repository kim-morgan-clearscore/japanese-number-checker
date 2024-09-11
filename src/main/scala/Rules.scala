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

  val singleWordDigit: Rewrite[Entity] =
    Rewrite
      .lift[Entity] { case 'o' :: 'n' :: 'e' :: rest =>
        WordDigit(1) :: rest
      }
      .orElse(
        Rewrite.lift { case 't' :: 'w' :: 'o' :: rest => WordDigit(2) :: rest }
      )
      .orElse(
        Rewrite.lift { case 't' :: 'h' :: 'r' :: 'e' :: 'e' :: rest =>
          WordDigit(3) :: rest
        }
      )
      .orElse(
        Rewrite.lift { case 'f' :: 'o' :: 'u' :: 'r' :: rest =>
          WordDigit(4) :: rest
        }
      )
      .orElse(
        Rewrite.lift { case 'f' :: 'i' :: 'v' :: 'e' :: rest =>
          WordDigit(5) :: rest
        }
      )
      .orElse(
        Rewrite.lift { case 's' :: 'i' :: 'x' :: rest => WordDigit(6) :: rest }
      )
      .orElse(
        Rewrite.lift { case 's' :: 'e' :: 'v' :: 'e' :: 'n' :: rest =>
          WordDigit(7) :: rest
        }
      )
      .orElse(
        Rewrite.lift { case 'e' :: 'i' :: 'g' :: 'h' :: 't' :: rest =>
          WordDigit(8) :: rest
        }
      )
      .orElse(
        Rewrite.lift { case 'n' :: 'i' :: 'n' :: 'e' :: rest =>
          WordDigit(9) :: rest
        }
      )

  val singleWordTens: Rewrite[Entity] =
    Rewrite
      .lift[Entity] { case 't' :: 'e' :: 'n' :: rest =>
        WordTen(10) :: rest
      }
      .orElse(
        Rewrite.lift { case 'e' :: 'l' :: 'e' :: 'v' :: 'e' :: 'n' :: rest =>
          WordTen(11) :: rest
        }
      )
      .orElse(Rewrite.lift {
        case 't' :: 'w' :: 'e' :: 'l' :: 'v' :: 'e' :: rest =>
          WordTen(12) :: rest
      })
      .orElse(
        Rewrite.lift {
          case 't' :: 'h' :: 'i' :: 'r' :: 't' :: 'e' :: 'e' :: 'n' :: rest =>
            WordTen(13) :: rest
        }
      )
      .orElse(
        Rewrite.lift {
          case 'f' :: 'i' :: 'f' :: 't' :: 'e' :: 'e' :: 'n' :: rest =>
            WordTen(15) :: rest
        }
      )
      .orElse(
        Rewrite.lift { case WordDigit(a) :: 't' :: 'e' :: 'e' :: 'n' :: rest =>
          WordTen(10 + a) :: rest
        }
      )
      .orElse(
        Rewrite.lift { case 't' :: 'w' :: 'e' :: 'n' :: 't' :: 'y' :: rest =>
          WordTen(20) :: rest
        }
      )
      .orElse(
        Rewrite.lift { case 't' :: 'h' :: 'i' :: 'r' :: 't' :: 'y' :: rest =>
          WordTen(30) :: rest
        }
      )
      .orElse(
        Rewrite.lift { case 'f' :: 'o' :: 'r' :: 't' :: 'y' :: rest =>
          WordTen(40) :: rest
        }
      )
      .orElse(
        Rewrite.lift { case 'f' :: 'i' :: 'f' :: 't' :: 'y' :: rest =>
          WordTen(50) :: rest
        }
      )
      .orElse(
        Rewrite.lift { case 's' :: 'i' :: 'x' :: 't' :: 'y' :: rest =>
          WordTen(60) :: rest
        }
      )
      .orElse(
        Rewrite.lift {
          case 's' :: 'e' :: 'v' :: 'e' :: 'n' :: 't' :: 'y' :: rest =>
            WordTen(70) :: rest
        }
      )
      .orElse(
        Rewrite.lift { case 'e' :: 'i' :: 'g' :: 'h' :: 't' :: 'y' :: rest =>
          WordTen(80) :: rest
        }
      )
      .orElse(
        Rewrite.lift { case 'n' :: 'i' :: 'n' :: 'e' :: 't' :: 'y' :: rest =>
          WordTen(90) :: rest
        }
      )

  val singleWordLargeUnits: Rewrite[Entity] = Rewrite
    .lift[Entity] {
      case 'h' :: 'u' :: 'n' :: 'd' :: 'r' :: 'e' :: 'd' :: rest =>
        WordHundred :: rest
    }
    .orElse(
      Rewrite.lift {
        case 't' :: 'h' :: 'o' :: 'u' :: 's' :: 'a' :: 'n' :: 'd' :: rest =>
          WordThousand :: rest
      }
    )
    .orElse(
      Rewrite.lift {
        case 'm' :: 'i' :: 'l' :: 'l' :: 'i' :: 'o' :: 'n' :: rest =>
          WordMillion :: rest
      }
    )
    .orElse(
      Rewrite.lift {
        case 'b' :: 'i' :: 'l' :: 'l' :: 'i' :: 'o' :: 'n' :: rest =>
          WordBillion :: rest
      }
    )

  val wordNumberPunctuationRemoval: Rewrite[Entity] = Rewrite
    .lift[Entity] { case (a: WordNumber) :: ' ' :: (b: WordNumber) :: rest =>
      a :: b :: rest
    }
    .orElse(
      Rewrite.lift[Entity] { case Number(a) :: ' ' :: (b: WordNumber) :: rest =>
        Number(a) :: b :: rest
      }
    )
    .orElse(Rewrite.lift[Entity] {
      case (a: WordNumber) :: ',' :: ' ' :: (b: WordNumber) :: rest =>
        a :: b :: rest
    })
    .orElse(
      Rewrite.lift[Entity] {
        case (a: WordNumber) :: ',' :: ' ' :: Number(b) :: rest =>
          a :: Number(b) :: rest
      }
    )
    .orElse(
      Rewrite.lift[Entity] {
        case (a: WordNumber) :: '-' :: (b: WordNumber) :: rest =>
          a :: b :: rest
      }
    )
    .orElse(
      Rewrite.lift[Entity] {
        case (a: WordNumber) :: ' ' :: 'a' :: 'n' :: 'd' :: ' ' :: (b: WordNumber) :: rest =>
          a :: b :: rest
      }
    )
    .orElse(
      Rewrite.lift[Entity] {
        case (a: WordNumber) :: ',' :: ' ' :: 'a' :: 'n' :: 'd' :: ' ' :: Number(
              b
            ) :: rest =>
          a :: Number(b) :: rest
      }
    )
    .orElse(
      Rewrite.lift[Entity] {
        case (a: WordNumber) :: ',' :: ' ' :: 'a' :: 'n' :: 'd' :: ' ' :: (b: WordNumber) :: rest =>
          a :: b :: rest
      }
    )
    .orElse(
      Rewrite.lift[Entity] {
        case (a: WordNumber) :: ' ' :: 'a' :: 'n' :: 'd' :: ' ' :: Number(
              b
            ) :: rest =>
          a :: Number(b) :: rest
      }
    )

  val wordDigitEvaluation: Rewrite[Entity] = Rewrite.lift[Entity] {
    case WordDigit(a) :: rest => Number(a) :: rest
  }

  val wordTenEvaluation: Rewrite[Entity] = Rewrite
    .lift[Entity] { case WordTen(a) :: Number(b) :: rest =>
      Number(a + b) :: rest
    }
    .orElse(
      Rewrite.lift[Entity] { case WordTen(a) :: rest =>
        Number(a) :: rest
      }
    )

  val wordHundredEvaluation: Rewrite[Entity] = Rewrite
    .lift[Entity] { case Number(a) :: WordHundred :: Number(c) :: rest =>
      Number(a * 100 + c) :: rest
    }
    .orElse(
      Rewrite.lift[Entity] { case Number(a) :: WordHundred :: rest =>
        Number(a * 100) :: rest
      }
    )
    .orElse(
      Rewrite.lift[Entity] { case WordHundred :: rest =>
        Number(100) :: rest
      }
    )

  val wordThousandEvaluation: Rewrite[Entity] = Rewrite
    .lift[Entity] { case Number(a) :: WordThousand :: Number(c) :: rest =>
      Number(a * 1000 + c) :: rest
    }
    .orElse(
      Rewrite.lift[Entity] { case Number(a) :: WordThousand :: rest =>
        Number(a * 1000) :: rest
      }
    )
    .orElse(
      Rewrite.lift[Entity] { case WordThousand :: rest =>
        Number(1000) :: rest
      }
    )

  val wordMillionEvaluation: Rewrite[Entity] = Rewrite
    .lift[Entity] { case Number(a) :: WordMillion :: Number(c) :: rest =>
      Number(a * 1000000 + c) :: rest
    }
    .orElse(
      Rewrite.lift[Entity] { case Number(a) :: WordMillion :: rest =>
        Number(a * 1000000) :: rest
      }
    )
    .orElse(
      Rewrite.lift[Entity] { case WordMillion :: rest =>
        Number(1000000) :: rest
      }
    )

  val wordBillionEvaluation: Rewrite[Entity] = Rewrite
    .lift[Entity] { case Number(a) :: WordBillion :: Number(c) :: rest =>
      Number(a * 1000000000 + c) :: rest
    }
    .orElse(
      Rewrite.lift[Entity] { case Number(a) :: WordBillion :: rest =>
        Number(a * 1000000000) :: rest
      }
    )
    .orElse(
      Rewrite.lift[Entity] { case WordBillion :: rest =>
        Number(1000000000) :: rest
      }
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
