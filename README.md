# Search-Engine-Project

Overview:
The search project involved creating a search engine that takes in a corpus of connected web pages, parses these pages, converting them into internally stored
data within various HashMaps, and allow the user to search this corpus for various words. The search engine contains an indexer class and a querier class.
The indexer takes in an xml file representing corpus of pages, parses them, calculates the relevance/ frequency of each word per page, and computes the 
Page Rank algorithm. The querier takes in various words or phrases, and returns the pages most relevant to those searches. 

How does it work?

The indexer constructor takes in an xml file for pre-processing. From there, the the user selects a text file called titles.txt to store the integer id and 
title of each page. Another text file, docs.txt stores the ids and their page ranks. The final text file, words.txt,
select a text file to store the words and the documents they appear on along with the relevance. This will be called the words index.
Furthermore, the user uses the querier to search for words and phrases, and including the Page Rank calculation within the word query is optional.
The querier prompts the user to type in a word, and 10 relevant titles are displayed. This process repeats until you type in :quit
