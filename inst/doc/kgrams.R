## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(kgrams)

## -----------------------------------------------------------------------------
# Create an URL connection to Shakespeare's "Much Ado About Nothing" 
txt_con <- url("http://shakespeare.mit.edu/much_ado/full.html")

## -----------------------------------------------------------------------------
.preprocess <- function(x) {
        # Remove speaker name and locations (boldfaced in original html)
        x <- gsub("<b>[A-z]+</b>", "", x)
        # Remove other html tags
        x <- gsub("<[^>]+>||<[^>]+$||^[^>]+>$", "", x)
        # Apply standard preprocessing including lower-case
        x <- kgrams::preprocess(x)
        # Collapse to a single string to avoid splitting into more sentences at the end of lines
        x <- paste(x, collapse = " ")
        return(x)
}

.tknz_sent <- function(x) {
        # Tokenize sentences
        x <- kgrams::tknz_sent(x, keep_first = TRUE)
        # Remove empty sentences
        x <- x[x != ""]
        return(x)
}

## -----------------------------------------------------------------------------
freqs <- kgram_freqs(txt_con, # Read Shakespeare's text from connection
                     N = 5, # Store k-gram counts for k <= 5
                     .preprocess = .preprocess,  # preprocess text
                     .tknz_sent = .tknz_sent, # tokenize sentences
                     verbose = FALSE, # If TRUE, prints current progress info 
                     max_lines = Inf, # Read until the end-of-file
                     batch_size = 100 # Read text in batches of 100 lines
                     )
freqs

## -----------------------------------------------------------------------------
summary(freqs)

## -----------------------------------------------------------------------------
# Query some simple unigrams and bigrams
query(freqs, c("leonato", "enter leonato", "thy", "smartphones"))
# Query k-grams at the beginning or end of a sentence
query(freqs, c(BOS() %+% BOS() %+% "i", "love" %+% EOS()))
# Total number of words processed
query(freqs, "") 
# Total number of sentences processed
query(freqs, EOS()) 

## -----------------------------------------------------------------------------
smoothers()

## -----------------------------------------------------------------------------
info("kn")

## -----------------------------------------------------------------------------
kn <- language_model(freqs, "kn", D = 0.75)
kn

## -----------------------------------------------------------------------------
summary(kn)

## -----------------------------------------------------------------------------
parameters(kn)
param(kn, "D")
param(kn, "D") <- 0.6
param(kn, "D")
param(kn, "D") <- 0.75

## -----------------------------------------------------------------------------
param(kn, "N") <- 4 # 'kn' uses only 1:4-grams
param(kn, "N")
param(kn, "N") <- 5 # 'kn' uses also 5-grams

## -----------------------------------------------------------------------------
probability(c("Did he break out into tears?",
              "I see, lady, the gentleman is not in your books.",
              "We are predicting sentence probabilities."
              ),
            model = kn
            )

## -----------------------------------------------------------------------------
probability("tears" %|% "Did he break out into", model = kn)
probability("pieces" %|% "Did he break out into", model = kn)

## -----------------------------------------------------------------------------
set.seed(840)
sample_sentences(model = kn, 
                 n = 10,
                 max_length = 10
                 )

## -----------------------------------------------------------------------------
sample_sentences(model = kn, 
                 n = 10,
                 max_length = 10, 
                 t = 0.1 # low temperature
                 )
sample_sentences(model = kn, 
                 n = 10,
                 max_length = 10, 
                 t = 10 # high temperature
                 )

## -----------------------------------------------------------------------------
midsummer[840]

## -----------------------------------------------------------------------------
perplexity(midsummer, model = kn)

## ---- out.width="50%", fig.cap="Perplexity as a function of the discount parameter of Interpolated Kneser-Ney 2-gram (red), 3-gram (green), 4-gram (blue) and 5-gram (black) models."----
D_grid <- seq(from = 0.5, to = 0.99, by = 0.01)
FUN <- function(D, N) {
        param(kn, "N") <- N
        param(kn, "D") <- D
        perplexity(midsummer, model = kn)
}
P_grid <- lapply(2:5, function(N) sapply(D_grid, FUN, N = N))
oldpar <- par(mar = c(2, 2, 1, 1))
plot(D_grid, P_grid[[1]], type = "n", xlab = "D", ylab = "Perplexity", ylim = c(300, 500))
lines(D_grid, P_grid[[1]], col = "red")
lines(D_grid, P_grid[[2]], col = "chartreuse")
lines(D_grid, P_grid[[3]], col = "blue")
lines(D_grid, P_grid[[4]], col = "black")
par(oldpar)

## -----------------------------------------------------------------------------
sapply(c("2-gram" = 1, "3-gram" = 2, "4-gram" = 3, "5-gram" = 4),
       function(N) min(P_grid[[N]])
       )

