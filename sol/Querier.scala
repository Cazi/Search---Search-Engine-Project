package sol

import src.{FileIO, IQuerier, PorterStemmer, StopWords}

import java.io.InputStreamReader
import java.io.BufferedReader
import scala.::
import scala.collection.mutable
import scala.collection.mutable.HashMap

/**
 * Class for a querier REPL that uses index files built from a corpus.
 *
 * @param titleIndex    - the filename of the title index
 * @param documentIndex - the filename of the document index
 * @param wordIndex     - the filename of the word index
 * @param usePageRank   - true if PageRank is to be used to rank results
 */
class Querier(titleIndex: String, documentIndex: String, wordIndex: String,
              usePageRank: Boolean) extends IQuerier {
  val idsToTitles = new mutable.HashMap[Int, String]()
  val idsToPageRanks = new mutable.HashMap[Int, Double]()
  val wordsToDocsToRelevance = new mutable.HashMap[String, HashMap[Int, Double]]()

  /**
   * method that takes in a string and gives a list of ids from greatest to least rankings
   * @param query a user query
   *  @return query results as a list of document ids
   */
  override def getResults(query: String): List[Int] = {
    // splits up the words
    val search = query.split(" ")
    // takes out any possible stop words
    val stopFilter = search.filter(x => !StopWords.isStopWord(x.toLowerCase))
    // stems out the words
    val stemFilter = PorterStemmer.stemArray(stopFilter.map(x => x.toLowerCase))
    // creates a Hashmap where the key is the Id and the value is the ranking
    val rankingList = new mutable.HashMap[Int, Double]

    // this is what happens when you are not using page rank
    if (!usePageRank) {
      // for every word from the query
      for (word <- stemFilter) {
        // check to see if the word is found in any document
        if (wordsToDocsToRelevance.contains(word)) {
          // if the word is found in documents, then go thru every doc
          for (doc <- wordsToDocsToRelevance(word).keys.toList) {
            //check to see if the hashmap has this doc already from other words in the query
            if (rankingList.contains(doc)) {
              // if so, add the values together
              rankingList(doc) = rankingList(doc) + wordsToDocsToRelevance(word)(doc)
            } else {
              // if not, create a new key and value pair in the hash map
              rankingList.put(doc, wordsToDocsToRelevance(word)(doc))
            }
          }
        }
      }
      // this is what happens when you are using page rank
    } else {
      // for every word from the query
      for (word <- stemFilter) {
        // check to see if the word is found in any document
        if (wordsToDocsToRelevance.contains(word)) {
          // if the word is found in documents, go thru each doc
          for (doc <- wordsToDocsToRelevance(word).keys.toList) {
            // check to see if the hashmap already carries a value for this key
            if (rankingList.contains(doc)) {
              // if so, add the product of the relevance score and page rank score
              rankingList(doc) = rankingList(doc) + (wordsToDocsToRelevance(word)(doc) * idsToPageRanks(doc))
            } else {
              // else, create a new key and value pair with the product of the relevance and page rank score
              rankingList.put(doc, wordsToDocsToRelevance(word)(doc) * idsToPageRanks(doc))
            }
          }
        }
      }
    }
    // take the hashmap of id and rank scores and transform it into a list sorted in descending order
    val sortedRankings = rankingList.toList.sortBy(_._2).reverse
    // create an empty list where we add the first 10 ids from the sorted list,
    var topTen: List[Int] = List()
    for ((id, score) <- sortedRankings.slice(0,10)) {
      topTen = id :: topTen
    }
    topTen
  }

  override def runRepl(): Unit = {
    // reads in the files
    FileIO.readTitles(titleIndex, idsToTitles)
    FileIO.readWordsFile(wordIndex, wordsToDocsToRelevance)
    FileIO.readDocsFile(documentIndex, idsToPageRanks)
    System.out.println("What would you like to SEARCH:")
    // sets up the reader
    val buffReader: BufferedReader = new BufferedReader(new InputStreamReader(System.in))
    var currLine = buffReader.readLine()
    while (currLine != ":quit" && currLine != null) {
      // rankings is the list of ids with top 10 rankings in descending order
      val rankings = getResults(currLine)
      if (rankings.isEmpty) {
        System.out.print("No results were found buddy, better luck next time \n")
      } else {
        var x = 0
        for (id <- rankings) {
          System.out.println((x + 1).toString ++ ". " ++ idsToTitles(id))
          x = x + 1
        }
      }
      System.out.println("What would you like to SEARCH next:")
      currLine = buffReader.readLine()
    }
    buffReader.close()
  }
}

object Querier {
  /**
   * Runs the querier REPL.
   * @param args args of the form [--pageRank (optional), titlesIndex, docsIndex,
   *             wordsIndex]
   */
  def main(args: Array[String]): Unit = {
    if (args.length > 4 || args.length < 3) {
      throw new IllegalArgumentException("Incorrect Args Config")
    } else if (args(0).equals("--pageRank")) {
      val query = new Querier(args(1), args(2), args(3), true)
      query.runRepl()
    } else {
      val query = new Querier(args(0), args(1), args(2), false)
      query.runRepl()
    }
  }
}

