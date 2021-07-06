package sol

import src.IQuerier

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

  override def getResults(query: String): List[Int] = {
    // TODO: implement getResults
    List()
  }

  override def runRepl(): Unit = {
    // TODO: implement runRepl
  }
}

object Querier {
  /**
   * Runs the querier REPL.
   * @param args args of the form [--pageRank (optional), titlesIndex, docsIndex,
   *             wordsIndex]
   */
  def main(args: Array[String]): Unit = {
    // TODO: implement main
  }
}
