package galaxytest

import com.typesafe.scalalogging.LazyLogging

/**
  * Small utility class
  */

object Util {
  def doesNumeralExist(numerals: List[String]): Boolean = {

    var exist = true
    for (str <- numerals) {
      if (!DataStore.hasNumber(str)) {
        exist = false
      }
    }
    exist
  }

  def getRomanNumeralString(numerals: List[String]) = {
    val rNumeral: StringBuilder = new StringBuilder
    for (s <- numerals) {
      rNumeral.append(DataStore.getValue(s).get)
    }

    rNumeral.toString()
  }
}

/**
  * Has different processor for different type of String.
  */
trait Processor extends LazyLogging{

  def process(line: String):Either[String,String]

  def matches(line: String): Boolean
}


case object CreditsProcessor extends Processor {
  val pattern = "((?:[a-z]+ )+)([A-Z]\\w+) is (\\d+) ([A-Z]\\w+)$".r

  override def matches(line: String): Boolean = pattern.findAllIn(line).nonEmpty

  override def process(line: String):Either[String,String] = {

    var numerals: List[String] = List.empty
    var metalName: String = ""
    var totalValue: Double = 0

    var result:Either[String,String]=Left("Unknown Numeral")
    pattern.findAllIn(line).matchData foreach {
      m => {
        numerals = m.group(1).split(" ").toList
        metalName = m.group(2)
        totalValue = m.group(3).toDouble
      }
        if (!Util.doesNumeralExist(numerals)) {
          println("Unknown Numeral")
          return Left("Unknown Numeral")
        }

         result = RomanNumeralConvertor.convertToNumeral(Util.getRomanNumeralString(numerals)) match {
          case Right(quantity) => {
            DataStore.addMetalPrice(metalName, (totalValue / quantity.toInt))
            DataStore.printMetalValues
            DataStore.getMetalValue(metalName) match {
              case Some(x) => Right(""+x)
              case None => Left("Not found")

            }
          }
          case Left(x) => {
            println(x) // prints the error
            //Breaks.break()
            Left(x)
          }
        }

    }
    result
  }
}

case object QuestionTypeHowMuchProcessor extends Processor {
  val pattern = "^how much is ((?:\\w+[^0-9] )+)\\?$".r

  override def matches(line: String): Boolean = pattern.findAllIn(line).nonEmpty

  override def process(line: String):Either[String,String] = {

    var numerals: List[String] = List.empty
    var numeralGroup = ""
    var result:Either[String,String]=Left("Unknown Numeral")

    pattern.findAllIn(line).matchData foreach {
      m => {
        numerals = m.group(1).split(" ").toList
        numeralGroup= m.group(1)
      }

        if (!Util.doesNumeralExist(numerals)) {
          println("Unknown Numeral")
          return Left("Unknown Numeral")
        }

        result= RomanNumeralConvertor.convertToNumeral(Util.getRomanNumeralString(numerals)) match {
          case Right(x) => {
            println(numeralGroup + " is " + x)
            Right(numeralGroup + " is " + x)
          }
          case Left(x) => {
            println(x)
           Left(x)
          }
        }
    }
    result
  }
}

case object QuestionTypHowManyProcessor extends Processor {
  val pattern = "^how many ([a-zA-Z]\\w+) is ((?:\\w+ )+)([A-Z]\\w+) \\?$".r

  override def matches(line: String): Boolean = pattern.findAllIn(line).nonEmpty

  override def process(line: String):Either[String,String] = {

    var numerals: List[String] = List.empty
    var metalName: String = ""
    var creditType: String = ""
    var numeralString: String = ""
    var result:Either[String,String]=Left("Unknown Numeral")

    pattern.findAllIn(line).matchData foreach {
      m => {
        numeralString = m.group(2)
        numerals = m.group(2).split(" ").toList
        metalName = m.group(3)
        creditType = m.group(1)
      }

        if (!Util.doesNumeralExist(numerals)) {
          println("Unknown Numeral")
          return Left("Unknown Numeral")
        }
        var resultStr = new StringBuilder

        result = RomanNumeralConvertor.convertToNumeral(Util.getRomanNumeralString(numerals)) match {
          case Right(quantity) => {
            val totalCredits = quantity * DataStore.getMetalValue(metalName).get
            resultStr.append(numeralString).append(metalName).append(" is ").append(totalCredits).append(" ").append(creditType)
            println( resultStr.toString())
            Right(resultStr.toString())
          }
          case Left(x) => //-1
            println(x)
            Left(x)
        }

    }
    result
  }
}

  case object AllocationProcessor extends Processor {


    override def matches(line: String): Boolean = line.split(" ").length == 3


    override def process(line: String):Either[String,String] = {
      val values = line.split(" ")
      val assignmentValue = values(2)
      val validAssignmentValues = List("I", "V", "X", "L", "C", "M")


      validAssignmentValues.contains(assignmentValue) match {
        case true => {
          DataStore.addValues(values(0), assignmentValue)
          println("Value stored map is")
          DataStore.printValues
          Right(assignmentValue)
        }
        case false => {
          println(s"I have no idea what you are talking about, assignment can be only $validAssignmentValues")
          Left("I have no idea what you are talking about")
        }
      }
    }
  }

object InputProcessor{
  /**
    * Main entry function to trigger and process all inputs
    * @param str
    * @return
    */
  def processInput(str: String)={
    val processors:List[Processor] = List(
      CreditsProcessor,
      QuestionTypeHowMuchProcessor,
      QuestionTypHowManyProcessor,
      AllocationProcessor
    )

    val matchingProcessor: List[Processor] = processors.filter(p => p.matches(str)==true)

    matchingProcessor.isEmpty match {
      case true => println("I have no idea what you are talking about.")
      case false => matchingProcessor.map(_.process(str))
    }
    //.map(_.process)
  }
}