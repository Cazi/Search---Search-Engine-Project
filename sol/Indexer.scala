package sol

import src.{FileIO, PorterStemmer, StopWords}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.xml.{Node, NodeSeq}
import math.BigDecimal
/**
 * Provides an XML indexer, produces files for a querier
 *
 * @param inputFile - the filename of the XML wiki to be indexed
 */
class Indexer(val inputFile: String, val inputFile2: String, val inputFile3: String,
              val inputFile4: String) {
  val tempLinksMap = new mutable.HashMap[Int, mutable.HashSet[String]]()
  val linksMap = new mutable.HashMap[Int, mutable.HashSet[Int]]()
  val idToTitle = new mutable.HashMap[Int, String]()
  val titleToId = new mutable.HashMap[String, Int]()
  val rMap = new mutable.HashMap[Int, Double]()
  val rPrimeMap = new mutable.HashMap[Int, Double]()
  var wordsMap = new mutable.HashMap[String, mutable.HashMap[Int, Double]]()
  val pageSeq: NodeSeq = xml.XML.loadFile(inputFile) \ "page"
  val numOfPages = pageSeq.size

  parse(inputFile, inputFile2, inputFile3, inputFile4)

  /**
   * parse method processes the corpus
   *
   * @param inputFile  - xml file
   * @param inputFile2 - titles.txt
   * @param inputFile3 - docs.txt
   * @param inputFile4 - words.txt
   */
  def parse(inputFile: String, inputFile2: String, inputFile3: String,
            inputFile4: String): Unit = {
    for (page <- pageSeq) {
      processPage(page)
    }

    wordsMap = relevance(numOfPages)
    processLinks()
    pageRank()
    FileIO.writeTitlesFile(inputFile2, idToTitle)
    FileIO.writeDocsFile(inputFile3, rPrimeMap)
    FileIO.writeWordsFile(inputFile4, wordsMap)
  }

  /**
   *
   * @param page - Current page
   */
  def processPage(page: Node): Unit = {
    val pageId = (page \ "id").text.trim.toInt
    val pageTitle = (page \ "title").text.trim
    val pageText = (page \ "text").text.trim
    idToTitle.put(pageId, pageTitle)
    titleToId.put(pageTitle, pageId)
    linksMap.put(pageId, new mutable.HashSet[Int])

    val genRegex = new Regex("""\[\[[^\[]+?\]\]|[^\W_]+'[^\W_]+|[^\W_]+""")
    val matchesIterator = genRegex.findAllMatchIn(pageTitle + pageText)
    val matchesList = matchesIterator.toList.map(aMatch => aMatch.matched)
    val allText = textLinkArrays(matchesList, pageId)
    val wordList = allText

    for (word <- wordList) {
      if (wordsMap.keys.toList.contains(word)) {
        if (wordsMap(word).contains(pageId)) {
          wordsMap(word)(pageId) += 1
        } else {
          wordsMap(word).put(pageId, 1)
        }
      } else {
        wordsMap.put(word, new mutable.HashMap(pageId, 1))
      }
    }
    rMap.put(pageId, 0.0)
    rPrimeMap.put(pageId, 1.0 / numOfPages)
  }

  /**
   * getLinkText extracts the page text from a link
   *
   * @param link - The current link
   * @return The list of words in the link's text
   */
  def getLinkText(link: String): List[String] = {
    if (link.contains("|")) {
      link.substring(link.indexOf("|") + 1, link.length - 2).split(" ").toList
    } else if (link.contains(":")) {
      val noBrackets = link.substring(2, link.length - 2)
      noBrackets.substring(0, link.indexOf(":") - 1).split(" ").toList :::
        noBrackets.substring(noBrackets.indexOf(":") + 1, noBrackets.length).split(" ").toList
    } else {
      val noBrackets = link.substring(2, link.length - 2).split(" ").toList
      noBrackets
    }
  }

  /**
   * getLinkTitle extracts the page title from a link
   *
   * @param link - The current link
   * @return The title of page being linked to
   */
  def getLinkTitle(link: String): String = {
    if (link.contains("|")) {
      return link.substring(2, link.indexOf("|") - 1)
    } else if (link.contains("[")) {
      return link.substring(2, link.length - 2)
    }
    link.trim
  }

  /**
   * textLinkArrays processes the words and links in the corpus
   *
   * @param allWords - All the unprocessed words and links
   * @return A list of all processed words
   */
  def textLinkArrays(allWords: List[String], pageId: Int):
  List[String] = {
    val words: ListBuffer[String] = ListBuffer()
    val links: ListBuffer[String] = ListBuffer()
    for (word <- allWords) {
      if (word.matches("""\[\[[^\[]+?\]\]""")) {
        val linkTitle = getLinkTitle(word)
        links += linkTitle
        val linkText = getLinkText(word.toLowerCase)
        val newWords =
          PorterStemmer.stemArray(linkText.filterNot(x => StopWords.isStopWord(x)).toArray).toList
        words.concat(newWords)
      } else {
        if (!StopWords.isStopWord(word)) {
          words += PorterStemmer.stem(word.toLowerCase)
        }
      }
    }
    tempLinksMap.put(pageId, mutable.HashSet[String]())
    for (link <- links) {
      tempLinksMap(pageId).add(link)
    }
    words.toList
  }

  /**
   * Process links takes the temporary link structure and stores the correct
   * link structure now that titleToId has been filled
   */
  def processLinks(): Unit = {
    for ((page, lTitles) <- tempLinksMap) {
      for (link <- lTitles) {
        if (titleToId.contains(link)) {
          linksMap(page).add(titleToId(link))
        }
      }
    }
  }

  /**
   * Weight calculates the weight that k (from) give to j (to)'s rank
   *
   * @param fromK - Page k
   * @param toJ   - Page j
   */
  def weight(fromK: Integer, toJ: Integer): Double = {
    var weight = 0.0
    val epsilon = .15
    val default = epsilon / numOfPages
    if (linksMap(fromK).isEmpty) {
      weight = default + (.85 / (numOfPages.toDouble - 1.0))
      weight
    } else if (fromK == toJ) {
      weight = default
      weight
    } else if (linksMap(fromK).contains(toJ)) {
      weight = default + (.85 / linksMap(fromK).size.toDouble)
      weight
    } else {
      weight = default
      weight
    }
  }

  /**
   * To calculate distance we need to get the sum of the difference between r
   * and r' on every page of the hashMap
   *
   * @param r      - HashMap representing the previous iteration of page rank
   * @param rprime - HashMap representing the current iteration of page rank
   */
  def rankDistance(r: mutable.HashMap[Int, Double],
                   rprime: mutable.HashMap[Int, Double]): Double = {
    var sum = 0.0
    for (i <- idToTitle.keys) {
      sum += Math.pow(rprime(i) - r(i), 2)
    }
    Math.sqrt(sum)
  }

  /**
   * Calculate the relevance of every word in the corpus
   *
   * @param numPages - Number of pages in the corpus
   * @return Words to docs to frequency hashmap
   */
  def relevance(numPages: Double):
  mutable.HashMap[String, mutable.HashMap[Int, Double]] = {
    val maxWordOccurrence = new mutable.HashMap[Int, Double]()
    val wordFrequency = wordsMap.clone()

    for ((word, idoMap) <- wordsMap) {
      for ((id, occur) <- idoMap) {
        if (maxWordOccurrence.keys.toList.contains(id)) {
          if (occur > maxWordOccurrence(id)) {
            maxWordOccurrence.update(id, occur)
            wordFrequency(word)(id) =
              (wordsMap(word)(id) / maxWordOccurrence(id)) *
                Math.log(numPages / wordsMap(word).size)
          }
        }
        maxWordOccurrence.put(id, occur)
      }
    }
    wordFrequency
  }

  def pageRank(): Unit = {
    while (rankDistance(rMap, rPrimeMap) > .001) {
      for ((v, _) <- rPrimeMap) {
        rMap(v) = rPrimeMap(v)
      }
      for (j <- idToTitle.keys) {
        rPrimeMap(j) = 0.0
        for (k <- idToTitle.keys) {
          val weightCalc = weight(k, j) * rMap(k)
          rPrimeMap(j) =
            rPrimeMap(j) + weightCalc
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
    new Indexer(args(0), args(1), args(2), args(3))
  }


}

