package sol

import src.{FileIO, PorterStemmer, StopWords}

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.reflect.internal.util.Collections
import scala.util.matching.Regex
import scala.xml.{Node, NodeSeq}

/**
 * Provides an XML indexer, produces files for a querier
 *
 * @param inputFile - the filename of the XML wiki to be indexed
 */
class Indexer(val inputFile: String) {
  //These variables are needed to compute term frequency
  //Number of pages in the corpus or n
  var numOfPages = 0.0
  //Number of words in the corpus
  var numOfWords = 0

  //These HashMaps are used to compute page rank
  //Unranked documents in the corpus
  var rMap = new HashMap[Int, Double]()
  //Corpus documents ranked with PageRank
  var rPrimeMap = new HashMap[Int, Double]()
  //All page ids mapped to the pages they link to
  var linksMap = new HashMap[Int, List[String]]()

  //Theses HashMaps link the titles and ids to each other in two HashMaps
  var idToTitle = new HashMap[Int, String]()
  var titleToId = new HashMap[String, Int]()

  // List of Ids and Titles
  var idList: List[Int] = idToTitle.keys.toList
  var titlesList: List[String] = titleToId.keys.toList
  //HashMap of words -> (page Id -> Term Frequency)
  var wordsMap = new HashMap[String, HashMap[Int, Double]]()

  //Accessing the xml file
  //Node containing entire input file
  val root: Node = xml.XML.loadFile(inputFile)
  //Sequence of pages extracted from the root node
  val pageSeq: NodeSeq = root \ "page"
  //Sequence of ids extracted from the root node
  val idSeq: NodeSeq = root \ "page" \ "id"
  //Sequence of titles extracted from the root node
  val titleSeq: NodeSeq = root \ "page" \ "title"

  //Begin indexing and page rank
  getTitlesIds()
  parse(inputFile)
  relevance()
  pageRank()

  def getTitlesIds(): Unit = {
    for (page <- pageSeq) {
      val pageTitle = (page \ "title").text.trim
      val pageId = (page \ "id").text.trim.toInt
      //Store ID to title
      idToTitle.put(pageId, pageTitle)
      //Store title to ID
      titleToId.put(pageTitle, pageId)
    }
  }


  /**
   *
   * @param link
   * @param pageId
   * @return
   */
  def getLinkText(link: String): List[String] = {
    if (link.contains("|")) {
      return link.substring(link.indexOf("|") + 1, link.length - 2).split(" ").toList
    } else if (link.contains(":")) {
      val noBrackets = link.substring(2, link.length - 2)
      return noBrackets.substring(0, link.indexOf(":") - 1).split(" ").toList :::
        noBrackets.substring(noBrackets.indexOf(":") + 1, noBrackets.length).split(" ").toList
    } else if (link.contains("[")) {
      val noBrackets = link.substring(2, link.length - 2).split(" ").toList
      return noBrackets
    }
    List()
  }

  /**
   *
   * @param link
   * @return
   */
  def getLinkTitle(link: String): String = {
    if (link.contains("|")) {
      return link.substring(2, link.indexOf("|") - 1)
    } else if (link.contains("[")) {
      return link.substring(2, link.length - 2)
    }
    link.trim
  }

  private def textLinkArrays(allWords: List[String]): (List[String], List[String]) = {
    var words: List[String] = List()
    var links: List[String] = List()
    for (word <- allWords) {
      //Don't change link titles when putting it in the links map, do lower case, stem, etc, on page link text afterwards
      if (word.matches("""\[\[[^\[]+?\]\]""")) {
        links = getLinkTitle(word) :: links
        val linkText = getLinkText(word.toLowerCase)
        words = words ::: PorterStemmer.stemArray(linkText.filterNot(x => StopWords.isStopWord(x)).toArray).toList
      } else {
        if (!StopWords.isStopWord(word)) {
          words = PorterStemmer.stem(word.toLowerCase) :: words
        }
      }
    }
    //println(links)
    (words, links)

  }


  /**
   * Parse method constructs [insert structures here]
   *
   * @param inputFile
   */
  def parse(inputFile: String): Unit = {
    for (page <- pageSeq) {
      numOfPages += 1
      //Grab the id, title, and text from the current page
      val pageId = (page \ "id").text.trim.toInt
      val pageTitle = (page \ "title").text.trim
      val pageText = (page \ "text").text.trim

      //Extract all the words and links from the pages
      val genRegex = new Regex("""\[\[[^\[]+?\]\]|[^\W_]+'[^\W_]+|[^\W_]+""")
      val matchesIterator = genRegex.findAllMatchIn(pageTitle + pageText)
      val matchesList = matchesIterator.toList.map(aMatch => aMatch.matched)
      val allText = textLinkArrays(matchesList)
      val wordList = allText._1
      val linksList = allText._2
      linksMap.put(pageId, linksList)

      for (word <- wordList) {
        if (wordsMap.keys.toList.contains(word)) {
          if(wordsMap(word).contains(pageId)) {
            wordsMap(word)(pageId) += 1
          }
          wordsMap(word).put(pageId, 1)
        }
        wordsMap.put(word, new HashMap(pageId, 1))
      }
    }
  }

  //Calculate relevance and store it in the outputWords HashMap
  //First, calculate term frequency
  def relevance(): Unit = {
    val occurrenceMap = new HashMap[Int, Double]()
    for ((_, map) <- wordsMap) {
      for (id <- map.keys) {
        if (occurrenceMap.keys.toList.contains(id)) {
          if (map(id) > occurrenceMap(id)) {
            occurrenceMap.update(id, map(id))
          }
          occurrenceMap.put(id, map(id))
        }
      }
    }
    //Using term frequency and inverse document frequency,
    // calculate relevance
    for ((word, map) <- wordsMap) {
      for (id <- map.keys) {
        if (map.keys.toList.contains(id) && occurrenceMap.keys.toList.contains(id)) {
          map.update(id,
            (map(id) / occurrenceMap(id)) * Math.log(numOfPages / wordsMap(word).size))
        }
      }
    }
  }

  /**
   * Calculates the weight that k (from) give to j (to)'s rank
   *
   * @param fromK - Page k
   * @param toJ   - Page j
   */
  def weight(fromK: Integer, toJ: Integer): Double = {

    if (fromK == toJ) { //k equals j
      println("k equals j " + fromK)
      return .15 / numOfPages
    } else if (linksMap(fromK).contains(idToTitle(toJ))) {
      //k links to j
      println("k links to j " + fromK)
      return .15 / numOfPages + (.85 / linksMap(fromK).distinct.size.toDouble)
    } else if (linksMap(fromK).isEmpty) {
      println("k links to nothing " + fromK)
      //k links to nothing
      return .15 / numOfPages + (.85 / (numOfPages - 1))
    } else {
      println("else case " + fromK)
      //all other cases
      .15 / numOfPages
    }
  }

  /**
   * Page rank helper that fills a given HashMap with a certain value
   *
   * @param value
   * @param from
   * @param to
   */
  def fillHashMap(value: Double, from: HashMap[Int, String],
                  to: HashMap[Int, Double]): Unit = {
    for (ids <- from.keys.toList) {
      to.put(ids, value)
    }
  }

  /**
   *
   * @param r
   * @param rprime
   *
   * To calculate distance we need to get the sum of the difference between r
   * and r' on every page of the hashMap
   */
  def rankDistance(r: HashMap[Int, Double],
                   rprime: HashMap[Int, Double]): Double = {
    var sum = 0.0
    for (i <- idToTitle.keys.toList) {
      sum += ((rprime(i) - r(i)) * (rprime(i) - r(i)))
    }
    Math.sqrt(sum)
  }

  def pageRank(): Unit = {
    fillHashMap(0.0, idToTitle, rMap)
    fillHashMap(1.0 / numOfPages, idToTitle, rPrimeMap)

    while (rankDistance(rMap, rPrimeMap) > .001) {
      //check rMap updating
      rMap = rPrimeMap.clone()
      for (j <- idToTitle.keys.toList) {
        rPrimeMap(j) = 0.0
        for (k <- idToTitle.keys.toList) {
          rPrimeMap(j) += weight(k, j) * rMap(k)
          //println(weight(k, j) + " " + k + " " + j)
        }
      }
    }
  }
}

object Indexer {
  /**
   * Processes a corpus and writes to index files.
   *
   * @param args args of the form [WikiFile, titlesIndex, docsIndex, wordsIndex]
   */
  def main(args: Array[String]): Unit = {
    if (!(args.length == 4)) {
      throw new IllegalArgumentException("Incorrect Args Config")
    }
    val indexer = new Indexer(args(0))
    FileIO.writeTitlesFile(args(1), indexer.idToTitle)
    FileIO.writeWordsFile(args(3), indexer.wordsMap)
    FileIO.writeDocsFile(args(2), indexer.rPrimeMap)
  }
}

