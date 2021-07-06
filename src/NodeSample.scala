package src

import scala.xml.{Node, NodeSeq}

/**
 * An example usage of the scala.xml library.
 */
object NodeSample {

  def main(args: Array[String]) {
    // this is a Node object, similar to what will be loaded into memory using
    // xml.XML.loadFile("path/to/file.xml")
    val root: Node =
      <xml>
        <course>
          <name>CS17</name>
          <description>Learn about functional programming</description>
        </course>
        <course>
          <name>CS18</name>
          <description>Study Data Structures and Algorithms</description>
        </course>
      </xml>

    val courseSeq: NodeSeq = root \ "course"
    val nameSeq: NodeSeq = courseSeq \ "name"

    // because nameSeq is a sequence, nameSeq.text will return the concatenation
    // of the text of all the nodes in the sequence
    println(nameSeq.text.trim)

    // you could loop through every node in the NodeSeq and access each node
    // and its children individually
    for (course <- courseSeq) {
      println("Course: " + (course \ "name").text.trim)
      println("Description: " + (course \ "description").text.trim)
    }
  }
}
