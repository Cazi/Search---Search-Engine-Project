package sol

import scala.util.matching.Regex

object Test {
  val words = "[[Category:Historiography Hello Vision]]"

  def main(args: Array[String]): Unit = {

    val words = "Example:Computer Science"
    val testWords = words.split(":")

    println(testWords.toList)

  }

}
