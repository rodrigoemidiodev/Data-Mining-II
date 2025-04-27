# Load required packages
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)

# Create a corpus with the documents
docs <- c(
  doc1 = "Mining is important for finding gold",
  doc2 = "Classification and regression are data mining",
  doc3 = "Data mining deals with data"
)

corp <- corpus(docs)

# Tokenize and create a document-feature matrix
toks <- tokens(corp)
dfm <- dfm(toks)

# Get the information on the dfm
ndoc(dfm)
nfeat(dfm)
featnames(dfm)

# Inspect the dfm
dfm
as.matrix(dfm)

# Create a version of the dfm with a binary weighting
dfm_binary <- dfm_weight(dfm, scheme = "boolean")
as.matrix(dfm_binary)

# Create a version of the dfm with TF-IDF weighting
dfm_tfidf <- dfm_tfidf(dfm)
as.matrix(dfm_tfidf)

# Check terms with zero value in all documents for each weightings
all_zeros_binary <- which(colSums(as.matrix(dfm_binary)) == 0)
all_zeros_tfidf <- which(colSums(as.matrix(dfm_tfidf)) == 0)
all_zeros_binary
all_zeros_tfidf

# Analyze the cosine similarity between the three documents for each weighting scheme
sim_binary <- textstat_simil(dfm_binary, method = "cosine")
as.matrix(sim_binary)

sim_tfidf <- textstat_simil(dfm_tfidf, method = "cosine")
as.matrix(sim_tfidf)

# Rank the documents given the query "data mining" by the cosine similarity of the query to each document using the binary-weighted dfm
query <- "data mining"
query_corpus <- corpus(query)
query_tokens <- tokens(query_corpus)
query_dfm <- dfm(query_tokens)

query_dfm_matched <- dfm_match(query_dfm, featnames(dfm))

query_binary <- dfm_weight(query_dfm_matched, scheme = "boolean")
sim_query_binary <- textstat_simil(dfm_binary, query_binary, method = "cosine")
sim_query_binary

# Rank the documents given the query "data mining" by the cosine similarity of the query to each document using TF weighting
sim_query_tf <- textstat_simil(dfm, query_dfm_matched, method = "cosine")
sim_query_tf

# Rank the documents given the query "data mining" by the cosine similarity of the query to each document using TF-IDF weighting
query_tfidf <- dfm_tfidf(query_dfm_matched)
sim_query_tfidf <- textstat_simil(dfm_tfidf, query_tfidf, method = "cosine")
sim_query_tfidf

# Load the 'data_corpus_inaugural' dataset
inaug <- data_corpus_inaugural

# Inspect the first 5 documents of the loaded corpus
head(summary(inaug), 5)

# Obtain a graphical representation of the terms in the corpus
set.seed(100)
inaug_tokens <- tokens(inaug)
inaug_dfm <- dfm(inaug_tokens)
textplot_wordcloud(inaug_dfm, max_words = 100)

# Preprocess the tokens
inaug_tokens <- tokens(inaug, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_separators = TRUE)
inaug_tokens <- tokens_tolower(inaug_tokens)
inaug_tokens <- tokens_remove(inaug_tokens, stopwords("english"))
inaug_tokens <- tokens_wordstem(inaug_tokens)

inaug_dfm_processed <- dfm(inaug_tokens)

# Obtain a graphical representation of the frequencies of terms in the transformed corpus
set.seed(100)
textplot_wordcloud(inaug_dfm_processed, max_words = 100)

# Obtain the terms that occur more than 200 times
freq_terms <- topfeatures(inaug_dfm_processed, 100)
freq_terms_over200 <- freq_terms[freq_terms > 200]
freq_terms_over200

# Find the list of two words that frequently appear together
inaug_collocations <- textstat_collocations(inaug_tokens, size = 2, min_count =  10)
head(inaug_collocations, 20)

# Find correlated terms 
inaug_dfm_trim <- dfm_trim(inaug_dfm_processed, min_termfreq = 10)
term_cors <- textstat_simil(inaug_dfm_trim, inaug_dfm_trim, method = "correlation", margin = "features")
high_cors <- as.matrix(term_cors)
high_cors[high_cors < 0.8 ] <- 0
high_cors[high_cors == 1] <- 0
high_cor_pairs <- which(high_cors > 0, arr.ind = TRUE)

if(nrow(high_cor_pairs) > 0){
  for(i in 1:nrow(high_cor_pairs)) {
    row_idx <- high_cor_pairs[i, 1]
    col_idx <- high_cor_pairs[i, 2]
    term1 <- colnames(high_cors)[row_idx]
    term2 <- colnames(high_cors)[col_idx]
    cor_val <- high_cors[row_idx, col_idx]
    cat(term1, "-", term2, ":", cor_val, "\n")
  }
}
