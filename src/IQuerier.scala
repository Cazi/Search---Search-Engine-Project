package src

trait IQuerier {

  /**
   * Returns the results of a query to the corpus, returning the list of
   * document IDs.
   * @param query a user query
   * @return query results as a list of document ids
   */
  def getResults(query: String): List[Int]

  /**
   * Runs a REPL, allowing interactive user queries.
   */
  def runRepl(): Unit
}
