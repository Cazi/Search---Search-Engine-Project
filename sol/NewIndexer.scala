package sol

import src.{FileIO, PorterStemmer, StopWords}

import scala.collection.mutable
import scala.util.matching.Regex
import scala.xml.{Node, NodeSeq}

/**
 * Provides an XML indexer, produces files for a querier
 *
 * @param inputFile - the filename of the XML wiki to be indexed
 */
class NewIndexer(val inputFile: String) {
  //These variables are needed to compute term frequency
  //Number of pages in the corpus or n
  var numOfPages = 0.0
  //Number of words in the corpus
  var numOfWords = 0

  //These HashMaps are used to compute page rank
  //Unranked documents in the corpus
  val rMap = new mutable.HashMap[Int, Double]()
  //Corpus documents ranked with PageRank
  val rPrimeMap = new mutable.HashMap[Int, Double]()
  //All page ids mapped to the pages they link to
  val linksMap = new mutable.HashMap[Int, List[Int]]()

  //Theses HashMaps link the titles and ids to each other in two HashMaps
  val idToTitle = new mutable.HashMap[Int, String]()
  val titleToId = new mutable.HashMap[String, Int]()

  // List of Ids and Titles
  val idList: List[Int] = idToTitle.keys.toList
  val titlesList: List[String] = titleToId.keys.toList

  //HashMap of words -> (page Id -> Term Frequency)
  val wordsMap = new mutable.HashMap[String,
    mutable.HashMap[Int, Double]]()

  //Accessing the xml file
  //Node containing entire input file
  val root: Node = xml.XML.loadFile(inputFile)
  //Sequence of pages extracted from the root node
  val pageSeq: NodeSeq = root \ "page"
  //Sequence of ids extracted from the root node
  val idSeq: NodeSeq = root \ "page" \ "id"
  //Sequence of titles extracted from the root node
  val titleSeq: NodeSeq = root \ "page" \ "title"

  //Begin Processing the file
  getTitlesIds()

  /**
   * getTitlesIds
   * Helper Method that constructs the titlesToId and idToTitle HashMaps
   */
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
   * The process method constructs the wordsMap HashMap by processing all the
   * words in the xml document, keeping track of the pages a word appears in,
   * and the term frequency of that word on that page
   *
   * @param inputFile
   */
  def process(inputFile: String): Unit = {
    for (page <- pageSeq) {
      numOfPages += 1
      //Grab the id, title, and text from the current page
      val pageId = (page \ "id").text.trim.toInt
      val pageTitle = (page \ "title").text.trim
      val pageText = (page \ "text").text.trim

      //Extract all the words and links from the pages
      val genRegex = new Regex("""\[\[[^\[]+?\]\]|[^\W_]+'[^\W_]+|[^\W_]+""")
      val matchesIterator = genRegex.findAllMatchIn(pageTitle + pageText)
      val matchesList = matchesIterator.toList.map { aMatch => aMatch.matched }
      println(matchesList)


      //Process the words
      val pageWords = matchesList.filter(x => !x.matches("""\[\[.*\]\]"""))
      println(pageWords)
      val noStop = pageWords.filter(x => !StopWords.isStopWord(x)).toArray
      println(noStop.toList)
      val filteredWords = PorterStemmer.stemArray(noStop.map(x => x.toLowerCase))
      for (word <- filteredWords) {
        if (wordsMap.keys.toList.contains(word)) {
          wordsMap(word)(pageId) += 1
        } else {
          wordsMap.put(word, new mutable.HashMap(pageId, 1))
        }
      }

      //Todo: Fix link processing
      /*
    [[Title we're linking to| Words on page]]
    Match on word before the bar, and word after the bar
    process word before bar as a title/link
    process word(s) after bar as words on page (copy paste)
     */
      val pageLinks = matchesList.filter(x => x.matches("""\[\[.*\]\]"""))

      val linkArray = pageLinks.toArray.map(x =>
        x.stripPrefix("[[").stripSuffix("]]"))
      val linkNames = linkArray.map(x => x.split("|"))
      val linkPageWordsArray =
        linkArray.map(x => x.substring(x.indexOf("|"), x.length))
        //if page is not in corpus, don't add it
      val namesInCorpus = linkNames.toSet.filterNot(_ == pageTitle)
      //Get rid of bar, add the words to the hashmap, add the links to hashmap
      val linkPageWords = linkPageWordsArray.map(x => x.split(" "))





      /*
      [[Category:Computer Science]] or [[commons:Category:Computer Science]]
      Strip off brackets, replace colons with white space, turn into array,
      stopWord and stemArray

       */
    }
    //Calculate relevance and store it in the outputWords HashMap
    //First, calculate term frequency
    val occurrenceMap = new mutable.HashMap[Int, Double]()
    fillHashMap(0.0, idToTitle, occurrenceMap)
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
        map(id) =
          (map(id) / occurrenceMap(id)) *
            Math.log(numOfPages / wordsMap(word).size)
      }
    }
  }
  // This section covers the computation of page rank

  /**
   * Page rank helper that fills a given HashMap with a certain value
   *
   * @param value - Value that each key will map to
   * @param from  -  HashMap the method will take the keys from
   * @param to    - HashMap that is being filled
   */
  def fillHashMap(value: Double, from: mutable.HashMap[Int, String],
                  to: mutable.HashMap[Int, Double]) = {
    for (ids <- from.keys) {
      to.put(ids, value)
    }
  }

  /**
   * Calculate the Euclidian Distance for HashMap
   *
   * @param r1 - r HashMap
   * @param r2 - rprime HashMap
   *
   *           To calculate distance we need to get the sum of the difference between r
   *           and r' on every page of the hashMap
   */
  def rankDistance(r: mutable.HashMap[Int, Double],
                   rprime: mutable.HashMap[Int, Double]): Double = {
    var sum = 0.0
    for (i <- idToTitle.keys) {
      sum += ((rprime(i) - r(i)) * (rprime(i) - r(i)))
    }
    Math.sqrt(sum)
  }

  /**
   * Calculates the weight that k (from) give to j (to)'s rank
   *
   * @param from - Page k
   * @param to   - Page j
   */
  def weight(fromK: Integer, toJ: Integer): Double = {
        if (linksMap.contains(fromK) && linksMap(fromK).contains(idToTitle(toJ))) {
          (.15 / numOfPages) + (.85 / linksMap(fromK).size.toDouble)
        } else if (linksMap.contains(fromK) && linksMap(fromK).isEmpty) {
          //(.15 / numOfPages) + (.85 / (numOfPages - 1))
          .15 / numOfPages
        } else {
          .15 / numOfPages
        }
  }

  /**
   * Method calculates the page rank for each page in the corpus
   */
  def pageRank(): Unit = {
    fillHashMap(0.0, idToTitle, rMap)
    fillHashMap(1 / numOfPages, idToTitle, rPrimeMap)
    //prevIt is r and currIt is rprime

    while (rankDistance(rMap, rPrimeMap) > .001) {
      for (page <- idToTitle.keys) {
        rMap.update(page, rPrimeMap(page))
      }
      for (j <- idToTitle.keys) {
        rPrimeMap.update(j, 0.0)

        for (k <- idToTitle.keys) {
          rPrimeMap(j) += weight(k, j) * rMap(k)
          println(weight(k, j))
        }
      }
    }
  }
}


object NewIndexer {
  /**
   * Processes a corpus and writes to index files.
   *
   * @param args args of the form [WikiFile, titlesIndex, docsIndex, wordsIndex]
   */
  def main(args: Array[String]): Unit = {
    if (!(args.length == 4)) {
      throw new IllegalArgumentException("Incorrect Args Config")
    }
    val indexer = new NewIndexer(args(0))
    //FileIO.writeTitlesFile(args(1), indexer.idToTitle)
    FileIO.writeWordsFile(args(3), indexer.wordsMap)
   // FileIO.writeDocsFile(args(2), indexer.rPrimeMap)
  }
}

