# INVOCATION
library(pdftools)
library(tm)
library(ggplot2)
library(ggdendro)
library(dendextend)
library(circlize)

# IMPORTING CUSTOMIZED STOPWORDS LIST
setwd("wherever")
stopwords <- read.csv("stopwordsConstitutions.csv", header = FALSE)
stopwords <- as.character(stopwords$V1)
flibbertigibbet <- c(stopwords, stopwords())
indices <- read.csv("countryindices.csv")
dates <- read.csv("constitutionratification.csv")

theme_set(theme_classic())
Year <- dates$Year
g <- ggplot(dates, aes("Year")) + geom_histogram(binwidth = 1)

# ACCESSING FILES
setwd("wherever")
files <- list.files(pattern = "pdf$")
Rpdf <- readPDF(control = list(text = "-layout")) # The "layout" control here helps to ensure the formatting is kept in place for better word segmentation
docs <- Corpus(URISource(files), readerControl = list(reader = Rpdf))

# PREPROCESSING
docs <- tm_map(docs, content_transformer(tolower))
removeOddChars <- function(x) gsub("[^a-zA-Z ]", "", x)
docs <- tm_map(docs, content_transformer(removeOddChars))
docs <- tm_map(docs, content_transformer(removeNumbers))
docs <- tm_map(docs, content_transformer(removePunctuation))
docs <- tm_map(docs, content_transformer(stripWhitespace))
docs <- tm_map(docs, content_transformer(removeWords), flibbertigibbet)

# SETTING DOCUMENT TERM MATRIX
dtm <- DocumentTermMatrix(docs)
dtm <- removeSparseTerms(dtm, 0.98) # Removes terms absent in whatever percentage of documents; max allowed sparsity percentage is 0.998.
weighteddtm <- as.matrix(dtm)
weighteddtm <- weighteddtm/rowSums(weighteddtm)
dtm <- weightTfIdf(dtm, normalize = TRUE)

frequents <- findMostFreqTerms(dtm, n = 10L)

# HIERARCHICAL CLUSTERING
scaleddtm <- dist(scale(dtm), method = "euclidean")
hcluster <- hclust(scaleddtm, method = "ward.D2")
arbor <- as.dendrogram(hcluster)
arbor <- arbor %>% # Coloring for the dendrogram
  color_branches(k = 12) %>%
  color_labels
  par(mar = rep(0, 4))
  circlize_dendrogram(arbor, labels_track_height = .3, dend_track_height = .3) # Export at a maintained aspect ratio, with width of 3000
