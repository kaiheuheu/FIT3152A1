# ============================================================
# FIT3152 Assignment 3 – Kai Williams (33739374)
# ============================================================

# ============================================================
# Setup
# ============================================================

# Clear workspace and load packages
rm(list = ls())                                     # reset environment
library(tm)                                         # corpus + DTM
library(SnowballC)                                  # stemming
library(slam)                                       # sparse matrices

# ============================================================
# Q2: Corpus creation
# ============================================================

# Folders for 3 genres
genre_paths <- c("cars", "golf", "crypto")

# Create corpus from all .txt files in these folders
docs = Corpus(DirSource(genre_paths))
summary(docs)

# ============================================================
# Q3: Text processing and DTM creation
# ============================================================

# Basic preprocessing
# Tokenisation
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, content_transformer(tolower))
# Filter words
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)
#Stem
docs <- tm_map(docs, stemDocument, language = "english")

# Create full DTM
dtm  <- DocumentTermMatrix(docs)

# Trial-and-error to get ~25 tokens after removing sparse terms
dtm_24 <- removeSparseTerms(dtm, 0.52)   # adjusted from 0.7 until ~ 25 (only managed to get 24 or 32)
dim(dtm_24)
inspect(dtm_24)

# Export DTM for appendix
dtm_24_mat <- as.matrix(dtm_24)
write.csv(dtm_24_mat, "DTM_24_tokens.csv", row.names = TRUE)

# ============================================================
# Q4: Hierarchical clustering with cosine distance
# ============================================================

library(proxy)   # for cosine distance

# Compute cosine distance between documents
dist_matrix <- proxy::dist(dtm_24_mat, method = "cosine")

# Hierarchical clustering and dendrogram plot
hc_fit <- hclust(dist_matrix, method = "ward.D")

plot(hc_fit,
     hang = -1,
     main = "Documents clustered using cosine distance",
     xlab = "Document",
     sub  = "")

# Cut tree to get 3 clusters (for 3 genres) and inspect membership
k <- 3
clusters <- cutree(hc_fit, k = k)
clusters

# Create genre vector in the same order as the corpus
genres <- c(
  rep("cars",   7),
  rep("crypto", 7),
  rep("golf",   7)
)

# Confusion table: true genre vs assigned cluster
table(Genre = genres, Cluster = clusters)

# Calculate clustering accuracy
correct <- sum(tapply(genres, clusters, function(g) {
  max(table(g))   # majority genre in each cluster
}))

accuracy <- correct / length(genres)
accuracy

# ============================================================
# Q5: Sentiment analysis by genre
# ============================================================

library(SentimentAnalysis)   

# Run sentiment analysis directly on the corpus
sent_results <- analyzeSentiment(docs)

head(sent_results)

# Extract one sentiment measure (SentimentLM which is better for financial texts) 
# and add genre labels
sent_df <- data.frame(
  Document      = rownames(sent_results),
  Genre         = genres,
  WordCount     = sent_results$WordCount,
  SentimentGI   = sent_results$SentimentGI,
  PositivityGI  = sent_results$PositivityGI,
  NegativityGI  = sent_results$NegativityGI,
  SentimentLM = sent_results$SentimentLM
)

write.csv(sent_df, "Sentiment_by_document.csv", row.names = FALSE)

# Compare average sentiment by genre (using SentimentGI)
aggregate(SentimentGI ~ Genre, data = sent_df, FUN = mean)
aggregate(SentimentGI ~ Genre, data = sent_df, FUN = sd)

# Compare average sentiment by genre (using SentimentLM)
aggregate(SentimentLM ~ Genre, data = sent_df, FUN = mean)
aggregate(SentimentLM ~ Genre, data = sent_df, FUN = sd)

# Simple boxplots of sentiment by genre 
boxplot(SentimentGI ~ Genre,
        data = sent_df,
        main = "SentimentGI by genre",
        ylab = "SentimentGI")

# Test differences in sentiment between genres
cars_sent  <- subset(sent_df, Genre == "cars")$SentimentGI
golf_sent  <- subset(sent_df, Genre == "golf")$SentimentGI
crypto_sent <- subset(sent_df, Genre == "crypto")$SentimentGI

t.test(cars_sent, golf_sent)       # cars vs golf
t.test(cars_sent, crypto_sent)     # cars vs crypto
t.test(crypto_sent, golf_sent)     # crypto vs golf

# SentimentLM for finance-dictionary
# Simple boxplots of sentiment by genre 
boxplot(SentimentLM ~ Genre,
        data = sent_df,
        main = "SentimentLM by genre",
        ylab = "SentimentLM")

# Test differences in sentiment between genres
cars_sent_lm  <- subset(sent_df, Genre == "cars")$SentimentLM
golf_sent_lm  <- subset(sent_df, Genre == "golf")$SentimentLM
crypto_sent_lm <- subset(sent_df, Genre == "crypto")$SentimentLM

t.test(cars_sent_lm, golf_sent_lm)       # cars vs golf
t.test(cars_sent_lm, crypto_sent_lm)     # cars vs crypto
t.test(crypto_sent_lm, golf_sent_lm)     # crypto vs golf

# ============================================================
# Q6: Single-mode document network
# ============================================================

library(igraph)   # for network analysis and plotting 

# Binary version: 1 if term appears in document, 0 otherwise
dtm_bin <- dtm_24_mat
dtm_bin[dtm_bin > 0] <- 1

# Document-by-document matrix: multiply binary matrix by its transpose
ByDocMatrix <- dtm_bin %*% t(dtm_bin)         
diag(ByDocMatrix) <- 0                      

# Create igraph object
g_docs <- graph_from_adjacency_matrix(
  ByDocMatrix,
  mode     = "undirected",
  weighted = TRUE,
  diag     = FALSE
)

# Basic Plot
plot(g_docs)

# Add genres vector from before
V(g_docs)$genre     <- genres

# Node colour by genre\
genre_cols <- c(
  cars   = rgb(70, 130, 180, maxColorValue = 255, alpha = 140),  
  crypto = rgb(255, 165,   0, maxColorValue = 255, alpha = 140), 
  golf   = rgb(34, 139,   34, maxColorValue = 255, alpha = 140)  
)
V(g_docs)$color <- genre_cols[V(g_docs)$genre]

# Node size by degree set to default
V(g_docs)$size   <- 15

# Edge width by weight (shared terms)
E(g_docs)$width <- 0.05*E(g_docs)$weight

set.seed(33739374)
layout_docs <- layout_with_fr(g_docs,
                              niter = 2000,
                              grid = "nogrid")

plot(
  g_docs,
  layout = layout_docs,
  vertex.label = V(g_docs)$label,
  vertex.label.cex = 0.7,
  vertex.label.color = "black",
  main = "Document network based on shared terms"
)

legend(
  "topleft",
  legend = c("Cars", "Crypto", "Golf"),
  col    = genre_cols[c("cars", "crypto", "golf")],
  pch    = 19,
  pt.cex = 1.5,
  bty    = "n",
  title  = "Genre"
)

# Degree centrality
sort(degree(g_docs), decreasing = TRUE)

# Betweenness centrality 
sort(betweenness(g_docs), decreasing = TRUE)

