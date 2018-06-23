# INVOCATION
library(pdftools)
library(tm)
library(subspace)
library(ggplot2)
library(ggdendro)
library(ggrepel)
library(dendextend)
library(circlize)
library(igraph)

# IMPORTING CUSTOMIZED STOPWORDS LIST
setwd("C:/Users/mstri/Documents")
stopwords <- read.csv("stopwordsConstitutions.csv", header = FALSE)
stopwords <- as.character(stopwords$V1)
flibbertigibbet <- c(stopwords, stopwords())
indices <- read.csv("countryindices.csv")
dates <- read.csv("constitutionratification.csv")

theme_set(theme_classic())
Year <- dates$Year
g <- ggplot(dates, aes("Year")) + geom_histogram(binwidth = 1)

# ACCESSING FILES
setwd("D:toku/Constitutions")
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

aba <- findAssocs(dtm, "right", 0.7)
write.csv(aba, "right.csv")
abb <- findAssocs(dtm, "freedom", 0.7)
write.csv(abb, "freedom.csv")
abc <- findAssocs(dtm, "religious", 0.7)
write.csv(abc, "religious.csv")
abd <- findAssocs(dtm, "tradition", 0.7)
write.csv(abd, "tradition.csv")

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

# SUBCLUSTERING
silhouette <- SubClu(weighteddtm, epsilon = 12, minSupport = 4)
silhouette
subcluster1 <- rbind(silhouette[[1]]$subspace, colnames(weighteddtm))
clustering_to_file(subcluster1, "subcluster1.csv", index_should_start_at = 0)

# CONVERTING TO COÖCCURRENCE MATRIX
cooccurrences <- Dtm2Tcm(dtm)
write.csv(as.matrix(cooccurrences), file = "AdjacencyMatrix.csv")

# BUILDING NETWORK
the_wordgraph <- graph_from_adjacency_matrix(cooccurrences, weighted = TRUE, mode = "lower")
net_clusters <- cluster_louvain(the_wordgraph)
cluster_results <- cbind(V(the_wordgraph)$name, net_clusters$membership)
write.csv(cluster_results, file = "ClusterResults.csv")

groups(net_clusters)

trimmed <- cluster_edge_betweenness(the_wordgraph, weights = E(the_wordgraph)$weight, directed = TRUE,
edge.betweenness = TRUE, merges = TRUE, bridges = TRUE, modularity = TRUE, membership = TRUE)

fluke <- neighbors(the_wordgraph, "freedom", mode = c("out", "in", "all", "total"))
neighs <- cbind(V(the_wordgraph)$name, fluke$membership)
write.csv(neighs, file = "Neighbors.csv")
