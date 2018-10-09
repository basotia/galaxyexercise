package galaxytest

import org.scalatest.{BeforeAndAfterAll, Matchers}

class InputProcessorSpec extends BaseSpec with BeforeAndAfterAll with Matchers{
  val str_for_CreditsProcessor ="glob glob Silver is 34 Credits"
  val str_for_HowMuchProcessor ="how much is pish tegj glob glob ?"
  val str_for_HowManyProcessor="how many Credits is glob prok Silver ?"
  val allocationProcessor = AllocationProcessor
  val creditProcessor = CreditsProcessor
  val howMuchProcessor = QuestionTypeHowMuchProcessor
  val howManyTypeQuestionProcessor = QuestionTypHowManyProcessor


  val allocation_str = "glob is I"


  override def beforeAll(): Unit = {
    InputProcessor.processInput("glob is I")
    InputProcessor.processInput("prok is V")
    InputProcessor.processInput("pish is X")
    InputProcessor.processInput("tegj is L")

    InputProcessor.processInput("glob glob Silver is 34 Credits")
    InputProcessor.processInput("glob prok Gold is 57800 Credits")
    InputProcessor.processInput("pish pish Iron is 3910 Credits")

  }




  "AllocationProcessor" should "be able to identify the expression" in{
    allocationProcessor.matches(allocation_str) should equal(true)
  }

  it should "be able to calculate assignment value" in{
    allocationProcessor.process(allocation_str) should equal(Right("I"))
  }

  it should "not match any of the other strings" in{
    allocationProcessor.matches(str_for_CreditsProcessor) should equal(false)
    allocationProcessor.matches(str_for_HowManyProcessor) should equal(false)
    allocationProcessor.matches(str_for_HowMuchProcessor) should equal(false)
  }


  "CreditsProcessor" should "be able to identify the expression" in{
    creditProcessor.matches(str_for_CreditsProcessor) should equal(true)
  }

  it should "be able to calculate assignment value" in{
    allocationProcessor.process(allocation_str) should equal(Right("I"))
  }

  it should "not match any of the other strings" in{
    allocationProcessor.matches(str_for_CreditsProcessor) should equal(false)
    allocationProcessor.matches(str_for_HowManyProcessor) should equal(false)
    allocationProcessor.matches(str_for_HowMuchProcessor) should equal(false)
  }



  "QuestionTypeHowMuchProcessor" should "be able to identify the expression" in{
    howMuchProcessor.matches(str_for_HowMuchProcessor) should equal(true)
  }

  it should "be able to calculate assignment value" in{
    howMuchProcessor.process(str_for_HowMuchProcessor) should equal(Right("pish tegj glob glob  is 42"))
  }

  it should "not match any of the other strings" in{
    howMuchProcessor.matches(str_for_CreditsProcessor) should equal(false)
    howMuchProcessor.matches(str_for_HowManyProcessor) should equal(false)
  }



  "QuestionTypHowManyProcessor" should "be able to identify the expression" in{
    howManyTypeQuestionProcessor.matches(str_for_HowManyProcessor) should equal(true)
  }
  it should "be able to calculate assignment value" in{
    howManyTypeQuestionProcessor.process(str_for_HowManyProcessor) should equal(Right("glob prok Silver is 68.0 Credits"))
  }

  it should "not match any of the other strings" in{
    howManyTypeQuestionProcessor.matches(str_for_CreditsProcessor) should equal(false)
    howManyTypeQuestionProcessor.matches(str_for_HowMuchProcessor) should equal(false)
  }
}
