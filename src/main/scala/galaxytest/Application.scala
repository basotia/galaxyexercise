package galaxytest

/**
  * Enter key is also considered as input with empty string
  * String validation are done based on the sample input.
  * Please follow the same pattern for application to work.
  *
  * Also have used println instead of log, as wanted to keep the dependency to minimal.
  */
object Application {



  def main(args: Array[String]): Unit = {
    while (true){
      InputProcessor.processInput(scala.io.StdIn.readLine())
    }
  }

}
