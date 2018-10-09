package galaxytest

import com.typesafe.scalalogging.LazyLogging

import scala.util.matching.Regex

trait Converter extends LazyLogging

/**
  * Convertor for Roman to Numeral
  */
object RomanNumeralConvertor extends Converter{


  val romanNumeralMap: Map[Char, Int] = Map('I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000)

  //def convertToNumeral(romanNumeral: String): Either[String, Int] = {
  def convertToNumeral(romanNumeral: String) = {

    def performConversion(romanNumeral: String, f: (Char) => Int) = {
      val reversedCharArray: Array[Char] = romanNumeral.toCharArray.reverse
      val charValues: Array[Int] = reversedCharArray.map(f(_))
      var sum: Int = 0
      var maxVal: Int = 0

      def isNumberSmaller(n: Int) = n < maxVal

      charValues.foreach(n => {
        isNumberSmaller(n) match {
          case true => sum -= n
          case false => {
            sum += n
            maxVal = n
          }
        }
      })
      sum
    }

    if (isValidString(romanNumeral))
      Right(performConversion(romanNumeral, fetchRomanCharValue))
    else
      Left("Not a valid roman numeral string")
  }

  /**
    * validates the string against the business rule.
    * @param romanNumeral
    * @return
    */
  def isValidString(romanNumeral: String): Boolean = !hasInvalidChars(romanNumeral) && hasValidRepeatation(romanNumeral) && isValidSubtraction(romanNumeral)

  /**
    * validates if subtraction is possible based on the rules
    * @param romanNumeral
    * @return
    */
  def isValidSubtraction(romanNumeral: String): Boolean = {
    val charsArray = romanNumeral.toCharArray
    var continueLoop: Boolean = true
    var i: Int = 0
    var isValid: Boolean = true

    while (continueLoop) {
      val currentChar = charsArray(i)
      val nextChar = charsArray(i + 1)

      isValid = isSmallerNumeral(currentChar, nextChar) match {
        case true => isSubtractionPossible(currentChar, nextChar)
        case false => true
      }

      if (isValid && (i + 1 < charsArray.length - 1))
        i += 1
      else
        continueLoop = false
    }

    isValid
  }

  /**
    * compares value of numerals
    * @param currentChar
    * @param nextChar
    * @return
    */
  def isSmallerNumeral(currentChar: Char, nextChar: Char) = fetchRomanCharValue(currentChar) < fetchRomanCharValue(nextChar)

  def fetchRomanCharValue = (c: Char) => romanNumeralMap.getOrElse(c,-1)

  def isSubtractionPossible(currentChar: Char, nextChar: Char): Boolean = {
    val subtractionRule: Map[Char, List[Char]] = Map(
      'I' -> List('V', 'X'),
      'X' -> List('L', 'C'),
      'C' -> List('D', 'M')
    )

    val neverSubtractable = List('V', 'L', 'D')

    currentChar match {
      case k if neverSubtractable.contains(k) => {
        // println("checking never subtractable")
        false
      }
      case k if subtractionRule.contains(k)  => {
        //println(s"subtraction rule for $k states "+subtractionRule.get(k).getOrElse(List.empty).contains(nextChar))
        subtractionRule.getOrElse(k,List.empty).contains(nextChar)
      }
      case _ => false
    }
  }

  /**
    * Checks if any char outside of Roman char exist, return true if exist else false
    * @param romanNumeral
    * @return
    */
  def hasInvalidChars(romanNumeral: String) = {
    val nonRomanChar = for (
      c <- romanNumeral.toCharArray
      if (!romanNumeralMap.contains(c))
    ) yield c

    val isInvalid = nonRomanChar.length > 0
    //println("returning hasInvalidChars value as "+isInvalid)
    isInvalid
  }


  /**
    * return true if any of the given pattern exist else false
    * @param rNumeral
    * @param regExList
    * @return
    */
  def doesPatternExist(rNumeral:String)(regExList:List[String]) = {

    def patternMatched = (pattern: Regex, str: String) => pattern.findFirstMatchIn(str) match {
      case Some(_) => true
      case None => false
    }

    val result: Seq[Boolean] = for (
      regExStr <- regExList
      if patternMatched(regExStr.r, rNumeral)
    ) yield true

    //!result.contains(true)
    result.contains(true)
  }

  /**
    * return true if the patterns don't exist, indicating valid string
    *
    * @param romanNumeral
    * @return
    */
  def hasValidRepeatation(romanNumeral: String): Boolean = {
    val continiousRepeatationRegX: List[String] = List("(DD+)", "(LL+)", "VV+", "(IIII+)", "(XXXX+)", "(CCCC+)", "(MMMM+)")
    val nonContiniousRepeatationRegX: List[String] = List("(XXX[L,C,D,M]X+)")


    def continousPatternExist = doesPatternExist(romanNumeral)(continiousRepeatationRegX)

    def nonContinousRepeatPatternExist = doesPatternExist(romanNumeral)(nonContiniousRepeatationRegX)


    //hasValidRepeatation - negation here means 
    !(continousPatternExist || nonContinousRepeatPatternExist)

  }
}


