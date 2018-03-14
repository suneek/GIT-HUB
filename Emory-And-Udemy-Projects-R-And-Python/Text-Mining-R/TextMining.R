########################
# Set working directory#
########################
setwd("C:/Users/svemu/OneDrive/Suneetha_Docs/EMORY_Big_Data/WEEK_9_R")

#############################################
# Installing & Unpacking Required libraries #
#############################################
if("RCurl" %in% rownames(installed.packages()) == FALSE) {install.packages("RCurl")}
library(RCurl)
if("XML" %in% rownames(installed.packages()) == FALSE) {install.packages("XML")}
library(XML)
if("tm" %in% rownames(installed.packages()) == FALSE) {install.packages("tm")}
library(tm)
if("stringi" %in% rownames(installed.packages()) == FALSE) {install.packages("stringi")}
library(stringi)
if("proxy" %in% rownames(installed.packages()) == FALSE) {install.packages("proxy")}
library(proxy)
if("SnowballC" %in% rownames(installed.packages()) == FALSE) {install.packages("SnowballC")}
library(SnowballC)
if("wordcloud" %in% rownames(installed.packages()) == FALSE) {install.packages("wordcloud")}
library(wordcloud)
if("RColorBrewer" %in% rownames(installed.packages()) == FALSE) {install.packages("RColorBrewer")}
library(RColorBrewer)
if("rvest" %in% rownames(installed.packages()) == FALSE) {install.packages("rvest")}
library(rvest)
if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")}
library(stringr)
if("tidyr" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyr")}
library(tidyr)

# Read the text file from internet
filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text <- readLines(filePath)

# Load the data as a corpus
docs <- Corpus(VectorSource(text))

# Inspect the corpus
inspect(docs)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
docs <- tm_map(docs, stemDocument)

# Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Generate a Word Cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Explore frequent terms and their associations
findFreqTerms(dtm, lowfreq = 4)
findAssocs(dtm, terms = "freedom", corlimit = 0.3)

# Plot Word Frequencies
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

##################
# R Web Scraping #
##################

########
# IMDB #
########
lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")

rating <- lego_movie %>% 
  html_nodes("strong span") %>%
  html_text() %>%
  as.numeric()
rating

cast <- lego_movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()
cast

poster <- lego_movie %>%
  html_nodes(".poster img") %>%
  html_attr("src")
poster

########
# ESPN #
########
url <- 'http://espn.go.com/nfl/superbowl/history/winners'
webpage <- read_html(url)

# Extract the HTML table element and convert it to a data frame.
sb_table <- html_nodes(webpage, 'table')
sb <- html_table(sb_table)[[1]]
head(sb)

# Remove the first two rows, and set the column names.
sb <- sb[-(1:2), ]
sb <- sb[-(1:2), ]
names(sb) <- c("number", "date", "site", "result")
head(sb)

# Convert Roman numerals and date format
sb$number <- 1:51
sb$date <- as.Date(sb$date, "%B. %d, %Y")
head(sb)

# The result column can be split into four columns
sb <- separate(sb, result, c('winner', 'loser'), sep=', ', remove=TRUE)
head(sb)

# For this case pattern is a sequence of 1 or more digits at the end of a line
pattern <- " \\d+$"
sb$winnerScore <- as.numeric(str_extract(sb$winner, pattern))
sb$loserScore <- as.numeric(str_extract(sb$loser, pattern))
sb$winner <- gsub(pattern, "", sb$winner)
sb$loser <- gsub(pattern, "", sb$loser)
head(sb)

##############################################################
# HTML Scraping example from: http://uc-r.github.io/scraping #
##############################################################
scraping_wiki <- read_html("https://en.wikipedia.org/wiki/Emory_University")

scraping_wiki %>%
  html_nodes("h1")

scraping_wiki %>%
  html_nodes("h1") %>%
  html_text()

scraping_wiki %>%
  html_nodes("h2") %>%
  html_text()

p_nodes <- scraping_wiki %>% 
  html_nodes("p")

length(p_nodes)
p_nodes[1:6]

p_text <- scraping_wiki %>%
  html_nodes("p") %>%
  html_text()

p_text[1]
p_text[5]
p_text[6]

ul_text <- scraping_wiki %>%
  html_nodes("ul") %>%
  html_text()

length(ul_text)

ul_text[1]
# read the first 200 characters of the second list
substr(ul_text[2], start = 1, stop = 200)

li_text <- scraping_wiki %>%
  html_nodes("li") %>%
  html_text()

length(li_text)

li_text[1:8]

li_text[104:136]

all_text <- scraping_wiki %>%
  html_nodes("div") %>% 
  html_text()

body_text <- scraping_wiki %>%
  html_nodes("#mw-content-text") %>% 
  html_text()

# read the first 207 characters
substr(body_text, start = 1, stop = 207)
## [1] "Web scraping (web harvesting or web data extraction) is a computer software technique of extracting information from websites. Usually, such software programs simulate human exploration of the World Wide Web"

# read the last 73 characters
substr(body_text, start = nchar(body_text)-73, stop = nchar(body_text))
## [1] "See also[edit]\n\nData scraping\nData wrangling\nKnowledge extraction\n\n\n\n\n\n\n\n\n"

# Scraping a specific heading
scraping_wiki %>%
  html_nodes("#Techniques") %>% 
  html_text()
## [1] "Techniques"

# Scraping a specific paragraph
scraping_wiki %>%
  html_nodes("#mw-content-text > p:nth-child(20)") %>% 
  html_text()
## [1] "In Australia, the Spam Act 2003 outlaws some forms of web harvesting, although this only applies to email addresses.[20][21]"

# Scraping a specific list
scraping_wiki %>%
  html_nodes("#mw-content-text > div:nth-child(22)") %>% 
  html_text()
## [1] "\n\nApache Camel\nArchive.is\nAutomation Anywhere\nConvertigo\ncURL\nData Toolbar\nDiffbot\nFirebug\nGreasemonkey\nHeritrix\nHtmlUnit\nHTTrack\niMacros\nImport.io\nJaxer\nNode.js\nnokogiri\nPhantomJS\nScraperWiki\nScrapy\nSelenium\nSimpleTest\nwatir\nWget\nWireshark\nWSO2 Mashup Server\nYahoo! Query Language (YQL)\n\n"

# Scraping a specific reference list item
scraping_wiki %>%
  html_nodes("#cite_note-22") %>% 
  html_text()
## [1] "^ \"Web Scraping: Everything You Wanted to Know (but were afraid to ask)\". Distil Networks. 2015-07-22. Retrieved 2015-11-04. "

