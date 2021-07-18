//package sol
//
//import src.{FileIO, IQuerier, PorterStemmer, StopWords}
//
//import java.io.InputStreamReader
//import java.io.BufferedReader
//import scala.::
//import scala.collection.mutable
//import scala.collection.mutable.HashMap
//
///**
// * Class for a querier REPL that uses index files built from a corpus.
// *
// * @param titleIndex    - the filename of the title index
// * @param documentIndex - the filename of the document index
// * @param wordIndex     - the filename of the word index
// * @param usePageRank   - true if PageRank is to be used to rank results
// */
//class Querier(titleIndex: String, documentIndex: String, wordIndex: String,
//              usePageRank: Boolean) extends IQuerier {
//  val idsToTitles = new mutable.HashMap[Int, String]()
//  val idsToPageRanks = new mutable.HashMap[Int, Double]()
//  val wordsToDocsToRelevance = new mutable.HashMap[String, HashMap[Int, Double]]()
//
//  override def getResults(query: String): List[Int] = {
//    // TODO: implement getResults
//    val search = query.split(" ")
//    val stopFilter = search.filter(x => !StopWords.isStopWord(x.toLowerCase))
//    val stemFilter = PorterStemmer.stemArray(stopFilter.map(x => x.toLowerCase))
//    val rankingList = new mutable.HashMap[Int, Double]
//
//    if (!usePageRank) {
//      for (word <- stemFilter) {
//        if (wordsToDocsToRelevance.contains(word)) {
//          for (doc <- wordsToDocsToRelevance(word).keys.toList) {
//            if (rankingList.contains(doc)) {
//              rankingList(doc) = rankingList(doc) + wordsToDocsToRelevance(word)(doc)
//            } else {
//              rankingList.put(doc, wordsToDocsToRelevance(word)(doc))
//            }
//          }
//        }
//      }
//    } else {
//      for (word <- stemFilter) {
//        if (wordsToDocsToRelevance.contains(word)) {
//          for (doc <- wordsToDocsToRelevance(word).keys.toList) {
//            if (rankingList.contains(doc)) {
//              rankingList(doc) = rankingList(doc) + (wordsToDocsToRelevance(word)(doc) * idsToPageRanks(doc))
//            } else {
//              rankingList.put(doc, wordsToDocsToRelevance(word)(doc) * idsToPageRanks(doc))
//            }
//          }
//        }
//      }
//    }
//    val sortedRankings: Array[Int] = rankingList.keys.toArray.sortWith(rankingList(_) > rankingList(_))
//    var topTen = List()
//    for (x <- Array.copyAs(sortedRankings, Math.min(9, sortedRankings.length - 1))) {
//      topTen = x :: topTen
//    }
//    topTen
//  }
//
//  override def runRepl(): Unit = {
//    FileIO.readTitles(titleIndex, idsToTitles)
//    FileIO.readWordsFile(wordIndex, wordsToDocsToRelevance)
//    FileIO.readDocsFile(documentIndex, idsToPageRanks)
//    System.out.println("What would you like to SEARCH:")
//    val buffReader: BufferedReader = new BufferedReader(new InputStreamReader(System.in))
//    var currLine = buffReader.readLine()
//    // check for empty query, like spaces, regex maybe
//    while (currLine != ":quit" && currLine != null) {
//      val rankings = getResults(currLine)
//      if (rankings.isEmpty) {
//        System.out.print("No results were found buddy, better luck next time \n")
//      } else {
//        var x = 0
//        for (id <- rankings) {
//          System.out.println((x + 1).toString ++ ". " ++ idsToTitles(id))
//          x = x + 1
//        }
//      }
//      System.out.println("What would you like to SEARCH next:")
//      currLine = buffReader.readLine()
//    }
//    buffReader.close()
//  }
//}
//
//object Querier {
//  /**
//   * Runs the querier REPL.
//   * @param args args of the form [--pageRank (optional), titlesIndex, docsIndex,
//   *             wordsIndex]
//   */
//  def main(args: Array[String]): Unit = {
//    if (args.length > 4 || args.length < 3) {
//      throw new IllegalArgumentException("Incorrect Args Config")
//    } else if (args(0).equals("--pageRank")) {
//      val query = new Querier(args(1), args(2), args(3), true)
//      query.runRepl()
//    } else {
//      val query = new Querier(args(0), args(1), args(2), false)
//      query.runRepl()
//    }
//  }
//}
//
