package sol

import src.{FileIO, PorterStemmer, StopWords}

import java.security.KeyException
import scala.collection.mutable
import scala.util.matching.Regex
import scala.xml.{Node, NodeSeq}

/**
 * Provides an XML indexer, produces files for a querier
 *
 * @param inputFile - the filename of the XML wiki to be indexed
 */
class Indexer(val inputFile: String) {
  //Number of pages in the corpus
  var numOfPages = 0.0
  //Number of words in the corpus
  var numOfWords = 0
  //Unranked documents in the corpus
  var prevIt = new mutable.HashMap[Int, Double]()
  //Corpus documents ranked with PageRank
  var currIt = new mutable.HashMap[Int, Double]()
  //Page ids -> titles
  var idToTitle = new mutable.HashMap[Int, String]()
  //titles to ids
  var titleToID = new mutable.HashMap[String, Int]()
  //List of ids
  var ids = idToTitle.keys.toList //TODO fix me
  //Stores all the pages a page links to
  var links = new mutable.HashMap[Int, List[String]]()
  //all titles for weights method
  var titles = titleToID.keys.toList //TODO fix me
  //Final HashMap of Words -> (DocId -> relevance)
  var outputWords = new mutable.HashMap[String,
    mutable.HashMap[Int, Double]]()
  //Node containing entire input file
  val root: Node = xml.XML.loadFile(inputFile)
  //Sequence of pages extracted from the root node
  val pageSeq: NodeSeq = root \ "page"
  //Sequence of ids extracted from the root node
  val idSeq: NodeSeq = root \ "page" \ "id"


  //Sequence of titles extracted from the root node
  val titleSeq: NodeSeq = root \ "page" \ "id"
  //Begin indexing and page rank
  getTitlesAndIds()
  parse(inputFile)
  pageRank()

  /**
   * TODO!!!
   */
  def getTitlesAndIds(): Unit = {
    for (page <- pageSeq) {
      val pageTitle = (page \ "title").text.trim
      val pageId = (page \ "id").text.trim.toInt
      //Store ID to title
      idToTitle.put(pageId, pageTitle)
      //Store title to ID
      titleToID.put(pageTitle, pageId)
    }

  }

  /**
   * Parse method constructs [insert structures here]
   *
   * @param inputFile
   */
  def parse(inputFile: String): Unit = {
    /*
    What do we need to accomplish in the parse stage:
    Store all the page ids, words
     */
    val maxOccurMap = new mutable.HashMap[Integer, Double]()

    for (page <- pageSeq) {
      val pageId = (page \ "id").text.trim.toInt
      val pageTitle = (page \ "title").text.trim
      val pageText = (page \ "text").text.trim
      println(pageId)
      //Extract all the words and links from the pages
      val genRegex = new Regex("""\[\[[^\[]+?\]\]|[^\W_]+'[^\W_]+|[^\W_]+""")
      val matchesIterator = genRegex.findAllMatchIn(pageTitle + pageText)
      val matchesList = matchesIterator.toList.map { aMatch => aMatch.matched }
      //val list = matchesList.toArray.map(x => x.replaceAll("|", ""))
      //val noBar = matchesList.toArray.map.replaceAll("\\|.+", "")
      //Parse the list of words and links
      //StartsWith([[)
      //Create our own wiki
      for (words <- matchesList) {
        if (words.matches("""\[\[\{|}*.*\{|}*.*\{|}*\]\]""")) {
          //Takes off the brackets on the Link
          val noBrackets = words.substring(2, words.length - 2)
          //Takes out the bar, and creates an array of the words
          val newWords = noBrackets.split("\\|")
          //linkTo is not counted in the numOf words, it's a title to be stored
          val linkTo = newWords(0)
          //Edge cases for processing links
          if (titles.contains(linkTo)) {
            if (!linkTo.equals(pageTitle)) {
              if (links.contains(pageId) && !links(pageId).contains(linkTo)) {
                val nList = linkTo :: links.default(pageId)
                links.put(pageId, nList)
              }
              links.put(pageId, List(linkTo))
            }
          }
          //countedText is counted in numOfWords
          val countedText = newWords(1)
          //Creates an array of
          val countTextArray = countedText.split("\\s+")
          for (i <- countTextArray) {
            //Can we do this faster??
            //Wrap in try catch to do faster??
            //Changes back to contains
            try {
              outputWords(i)(pageId) += 1
              numOfWords += 1
            } catch {
              case e: NoSuchElementException =>
                if (!StopWords.isStopWord(i)) {
                  outputWords.put(PorterStemmer.stem(i), new mutable.HashMap(pageId, 1))
                  numOfWords += 1
                }
            }
          }
        } else if (words.matches("""\[\[\{:}*.*\{:}*.*\{:}*\]\]""")) {
          //Takes off the brackets on the Link
          val noBrackets = words.substring(2, words.length - 2)
          if (!noBrackets.equals(pageTitle)) {
            if (links.contains(pageId)) {
              val nList = noBrackets :: links.default(pageId)
              links.put(pageId, nList)
            }
            links.put(pageId, List(noBrackets))
          }
          //Get the words before and after the colon
          val noColon = words.replaceAll(":", " ")
          val wordsArray = noColon.split(" ")

          for (i <- wordsArray) {
            try {
              outputWords(i)(pageId) += 1
              numOfWords += 1
            } catch {
              case e: NoSuchElementException =>
                if (!StopWords.isStopWord(i)) {
                  outputWords.put(PorterStemmer.stem(i), new mutable.HashMap(pageId, 1))
                  numOfWords += 1
                }
            }
          }
        } else if (words.matches("""\[\[.*\]\]""")) {
          //Takes off the brackets on the Link
          val noBrackets = words.substring(2, words.length - 2)
          if (titles.contains(noBrackets)) {
            if (!noBrackets.equals(pageTitle)) {
              if (links.contains(pageId)) {
                val nList = noBrackets :: links.default(pageId)
                links.put(pageId, nList)
              }
              links.put(pageId, List(noBrackets))
            }
          }
          //Creates an array of all words in the link
          val countTextArray = noBrackets.split("\\s+")
          for (i <- countTextArray) {
            try {
              outputWords(i)(pageId) += 1
              numOfWords += 1
            } catch {
              case e: NoSuchElementException =>
                if (!StopWords.isStopWord(i)) {
                  outputWords.put(PorterStemmer.stem(i), new mutable.HashMap(pageId, 1))
                  numOfWords += 1
                }
            }
          }
        } else {
          //Adding a non-link word to the HashMap
          val newWords =
            words.replaceAll("|", " ")
              .replaceAll(":", " ").split(" ")

          for (eachWord <- newWords) {
            try {
              outputWords(eachWord)(pageId) += 1
              numOfWords += 1
            } catch {
              case e: NoSuchElementException =>
                if (!StopWords.isStopWord(words)) {
                  outputWords.put(PorterStemmer.stem(words), new mutable.HashMap(pageId, 1))
                  numOfWords += 1
                }
            }
          }

        }
      }
      //Calculate relevance and store it in the outputWords HashMap
      //First, calculate term frequency
      for (page <- idToTitle.keys.toList) {
        var maxOccur = 0.0
        for (word <- outputWords.keys) {
          val wordOccur = word(page)
          if (wordOccur > maxOccur) {
            maxOccur = wordOccur
          }
        }
        maxOccurMap.put(page, maxOccur)
        //Using term frequency and inverse document frequency,
        // calculate relevance
        for (word <- outputWords.keys) {
          outputWords(word)(page) =
            (outputWords(word)(page) / maxOccurMap(page)) * Math.log(numOfPages / outputWords(word).size)
        }
      }
    }
  }



  /*
  TODO: Indexer:
  */

  /**
   * Calculates the weight that k (from) give to j (to)'s rank
   *
   * @param from - Page k
   * @param to   - Page j
   */
  def weight(from: Integer, to: Integer): Double = {
    if (!links.keys.toList.contains(from)) {
      .15 / numOfPages + (.85 / numOfPages - 1)
    } else if (links(from).contains(idToTitle(to))) {
      .15 / numOfPages + (.85 / links(to).toSet.size)
    } else {
      .15 / numOfPages
    }
  }

  //PageRank Helper, could be useful elsewhere
  def fillHashMap(value: Double, from: mutable.HashMap[Int, String],
                  to: mutable.HashMap[Int, Double]) = {
    for (ids <- from.keys) {
      to.put(ids, value)
    }
  }

  /**
   *
   * @param r1
   * @param r2
   *
   * To calculate distance we need to get the sum of the difference between r
   * and r' on every page of the hashMap
   */
  def rankDistance(r: mutable.HashMap[Int, Double],
                   rprime: mutable.HashMap[Int, Double]): Double = {
    var sum = 0.0
    for (i <- idToTitle.keys) {
      sum = sum + ((rprime(i) - r(i))  * (rprime(i) - r(i)))
      println(sum)
    }
    println(Math.sqrt(sum))
    Math.sqrt(sum)

  }

  def pageRank(): Unit = {
    fillHashMap(0.0, idToTitle, prevIt)
    fillHashMap(1 / numOfPages, idToTitle, currIt)
    println(idToTitle.keys)
    println(numOfPages)
    println("Before while")
    //prevIt is r and currIt is rprime
    var distance = 1.0
    while (distance > .001) {
      println("Got here!")
      for (page <- idToTitle.keys) {
        prevIt(page) = currIt(page)
      }
      for (j <- idToTitle.keys) {
        currIt.update(j, 0.0)

        for (k <- idToTitle.keys) {
          currIt.update(j, currIt(j) + (weight(k, j) * prevIt(k)))
          println(weight(k, j))
        }
      }
      distance = rankDistance(prevIt, currIt)
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
    //error check arg size, etc
    //FileIO.writeTitlesFile(args(1), indexer.idToTitle)
    //FileIO.writeWordsFile(args(3), indexer.outputWords)
    FileIO.writeDocsFile(args(2), indexer.prevIt)
  }
}
