package galaxytest

import com.typesafe.scalalogging.LazyLogging

/**
  * Temporary datastore to capture user input
  *
  *
  */
object DataStore extends LazyLogging {

   var vulcanNumberMap = scala.collection.mutable.Map[String,String]()

   def addValues(key:String,value:String) = vulcanNumberMap.put(key,value)

   def getValue(key:String) = vulcanNumberMap.get(key)

   def hasNumber(key:String) = vulcanNumberMap.contains(key)

   def printValues = vulcanNumberMap.foreach(println)

   var metalValues = scala.collection.mutable.Map[String,Double]()

   def addMetalPrice(name:String,value:Double)=metalValues.put(name,value)

   def getMetalValue(name:String) = metalValues.get(name)

   def printMetalValues = metalValues.foreach(println)
}
