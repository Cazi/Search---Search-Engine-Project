package src

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}
import scala.collection.mutable.HashMap

/**
 * Object that contains methods for writing to and reading from index files
 * for Search.
 */
object FileIO {

  /**
   * Writes the titles index for the corpus to a file, storing the mapping
   * between document id and title, formatted as follows:
   *
   * docId1::title1
   * docId2::title2
   * ...
   *
   * @param titlesFile the file to write the titles index to
   * @param idsToTitles a HashMap from document id to document title
   */
  def writeTitlesFile(titlesFile: String, idsToTitles: HashMap[Int, String]) {
    val titleWriter = new BufferedWriter(new FileWriter(titlesFile))
    for ((id, title) <- idsToTitles) {
      titleWriter.write(id + "::" + title + "\n")
    }
    titleWriter.close()
  }

  /**
   * Writes the docs index for the corpus to a file, storing the mapping
   * between document id and PageRank, formatted as follows:
   *
   * docId1 pageRank1
   * docId2 pageRank2
   * ...
   *
   * @param docsFile the file to write the docs index to
   * @param idsToPageRanks a HashMap from document id to PageRank
   */
  def writeDocsFile(docsFile: String,
                    idsToPageRanks: HashMap[Int, Double]) {
    val docWriter = new BufferedWriter(new FileWriter(docsFile))
    for ((id, rank) <- idsToPageRanks) {
      docWriter.write(id + " " + rank + "\n")
    }
    docWriter.close()
  }

  /**
   * Writes the words index for the corpus to a file, storing the mapping
   * from word -> docId -> relevance/frequency, formatted as follows:
   *
   * word1 doc1 relevance1 doc2 relevance2 ...
   * word2 doc5 relevance5 doc9 relevance9 ...
   * ...
   *
   * Note: You can either compute and store relevance (which is tf * idf) in
   * this file, or you can store tf here and compute idf in Querier.
   */
  def writeWordsFile(wordsFile: String,
                     wordsToDocsToRelevance: HashMap[String, HashMap[Int, Double]]) {
    val wordWriter = new BufferedWriter(new FileWriter(wordsFile))
    for ((word, freqMap) <- wordsToDocsToRelevance) {
      wordWriter.write(word + " ")
      // print ids of documents followed by frequency of word in that document
      for ((id, frequency) <- freqMap) {
        wordWriter.write(id + " " + frequency + " ")
      }
      wordWriter.write("\n")
    }
    wordWriter.close()
  }


  /**
   * Reads in the titles index file and populates a HashMap from document id
   * to document title.
   *
   * @param titlesFile the file to read in the titles index from
   * @param idsToTitles a HashMap to fill with document id->title mappings
   */
  def readTitles(titlesFile: String, idsToTitles: HashMap[Int, String]) {
    val titlesReader = new BufferedReader(new FileReader(titlesFile))
    var line = titlesReader.readLine()

    while (line != null) {
      val tokens = line.split("::")
      // Create map of document ids to document titles
      idsToTitles(tokens(0).toInt) = tokens(1)
      line = titlesReader.readLine()
    }
    titlesReader.close()
  }

  /**
   * Reads in the docs index file and populates a HashMap from document id
   * to PageRank.
   *
   * @param docsFile the file to read in the docs index from
   * @param idsToPageRanks the HashMap to fill with document id->PageRank mappings
   */
  def readDocsFile(docsFile: String, idsToPageRanks: HashMap[Int, Double]) {
    val documentsReader = new BufferedReader(new FileReader(docsFile))
    var line = documentsReader.readLine()

    while (line != null) {
      val tokens = line.split(" ")

      // Save page rank for each document in map
      idsToPageRanks(tokens(0).toInt) = tokens(1).toDouble

      line = documentsReader.readLine()
    }
    documentsReader.close()
  }

  /**
   * Reads in the words index file and populates a HashMap from word -> doc -> relevance.
   * As discussed in {@link FileIO.writeWordsFile}, the words index can either
   * store full relevance score (tf*idf) or just tf, depending on whether you
   * will compute idf in Indexer or Querier.
   *
   * @param wordsFile the file to read in the words index from
   * @param wordsToDocsToRelevance the HashMap to fill with word -> docId -> relevance mappings
   */
  def readWordsFile(wordsFile: String,
                    wordsToDocsToRelevance: HashMap[String, HashMap[Int, Double]]) {
    val wordsReader = new BufferedReader(new FileReader(wordsFile))
    var line = wordsReader.readLine()

    while (line != null) {
      val tokens = line.split(" ")
      // create map of document ids to relevance of current word in that document
      wordsToDocsToRelevance(tokens(0)) = new HashMap[Int, Double]
      for (i <- 1 until (tokens.size - 1) by 2) {
        wordsToDocsToRelevance(tokens(0)) += (tokens(i).toInt -> tokens(i + 1).toDouble)
      }
      line = wordsReader.readLine()
    }
    wordsReader.close()
  }
}