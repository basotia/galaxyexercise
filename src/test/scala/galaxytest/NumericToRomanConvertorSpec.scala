package galaxytest

class NumericToRomanConvertorSpec extends BaseSpec {

  "Given a number it " should "be able to convert to Roman " in {
    assert("MMVI"==NumericToRomanConvertor.convert(2006))
    assert("MCMIII"==NumericToRomanConvertor.convert(1903))
  }

}
