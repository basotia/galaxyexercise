package galaxytest

class RomanNumeralConvertorSpec extends BaseSpec {

  "V,L,D" should "never be subtracted" in {
    val vldCan_never_be_subtracted1 = "MIVLDM"
    println(s"checking $vldCan_never_be_subtracted1 is valid string value " + RomanNumeralConvertor.isValidString(vldCan_never_be_subtracted1))
    assert(false == RomanNumeralConvertor.isValidString(vldCan_never_be_subtracted1))
    assert(false == RomanNumeralConvertor.isSubtractionPossible(vldCan_never_be_subtracted1.charAt(2), vldCan_never_be_subtracted1.charAt(3)))
    assert(true == RomanNumeralConvertor.isSmallerNumeral(vldCan_never_be_subtracted1.charAt(2), vldCan_never_be_subtracted1.charAt(3)))

    val vldCan_never_be_subtracted2 = "MIVXM"
    println(s"checking $vldCan_never_be_subtracted2 is valid string value " + RomanNumeralConvertor.isValidString(vldCan_never_be_subtracted2))
    assert(false == RomanNumeralConvertor.isValidString(vldCan_never_be_subtracted2))
    assert(false == RomanNumeralConvertor.isSubtractionPossible(vldCan_never_be_subtracted2.charAt(2), vldCan_never_be_subtracted2.charAt(3)))
    assert(true == RomanNumeralConvertor.isSmallerNumeral(vldCan_never_be_subtracted2.charAt(2), vldCan_never_be_subtracted2.charAt(3)))

  }

  "Only certain characters" should "be allowed subtraction, invalid sequence is not allowed" in {
    val subtractionIandCisInvalid = "MICM"
    println(s"checking $subtractionIandCisInvalid is valid string value " + RomanNumeralConvertor.isValidString(subtractionIandCisInvalid))
    assert(false == RomanNumeralConvertor.isValidString(subtractionIandCisInvalid))
    assert(false == RomanNumeralConvertor.isSubtractionPossible(subtractionIandCisInvalid.charAt(1), subtractionIandCisInvalid.charAt(2)))
    assert(true == RomanNumeralConvertor.isSmallerNumeral(subtractionIandCisInvalid.charAt(1), subtractionIandCisInvalid.charAt(2)))

  }


  "Only valid sequence of certain characters" should "be allowed subtraction" in {
    val char_I_validSeqTest = "MCMXLIV"
    assert(true == RomanNumeralConvertor.isValidString(char_I_validSeqTest))
    println("checking MCMXLIV is valid string value " + RomanNumeralConvertor.isValidString(char_I_validSeqTest))

  }


  "3 times repeatation " should "be allowed only for I, X, C, M" in {

    val tripple_I_test_invalid = "MIIIIXM"
    println(s"checking 3 times repeatation $tripple_I_test_invalid is valid string value " + RomanNumeralConvertor.isValidString(tripple_I_test_invalid))
    assert(false == RomanNumeralConvertor.isValidString(tripple_I_test_invalid))

    val tripple_I_test_valid = "MIIIXM"
    println(s"checking 3 times repeatation $tripple_I_test_valid is invalid string value " + RomanNumeralConvertor.isValidString(tripple_I_test_valid))
    assert(false == RomanNumeralConvertor.isValidString(tripple_I_test_valid))

    val tripple_V_test_invalid = "MVVVXM"
    assert(false == RomanNumeralConvertor.isValidString(tripple_V_test_invalid))

  }

  it can "be repeated 4th time only if smaller number exist after 3rd char" in {
    assert(false == RomanNumeralConvertor.isValidString("XXXDX"))

    //X can be repeated only if smaller number exist after 3rd X
    assert(true == RomanNumeralConvertor.isValidString("XXXIX"))
  }

  "V L D" can "never be subtracted" in {
    assert(false == RomanNumeralConvertor.isValidString("XXXVX"))
  }

  "More than 4 occurrences " should "not be allowed" in {
    assert(false == RomanNumeralConvertor.isValidString("XXXVXX"))
  }


  "Valid roman numeral " should "be converted to numeric values " in {
    val test_valid = "MCMXLIV"
    assert(Right(1944) == RomanNumeralConvertor.convertToNumeral(test_valid))

    val test_invalid = "MZZCMXLIV"
    assert(Left("Not a valid roman numeral string") == RomanNumeralConvertor.convertToNumeral(test_invalid))
  }

}
