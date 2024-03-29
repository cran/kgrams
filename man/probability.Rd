% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/probability.R
\name{probability}
\alias{probability}
\alias{probability.kgrams_word_context}
\alias{probability.character}
\title{Language Model Probabilities}
\usage{
probability(object, model, .preprocess = attr(model, ".preprocess"), ...)

\method{probability}{kgrams_word_context}(object, model, .preprocess = attr(model, ".preprocess"), ...)

\method{probability}{character}(
  object,
  model,
  .preprocess = attr(model, ".preprocess"),
  .tknz_sent = attr(model, ".tknz_sent"),
  ...
)
}
\arguments{
\item{object}{a character vector for sentence probabilities,
a word-context conditional expression created with the
conditional operator \verb{\%|\%} (see \link[kgrams]{word_context}).
for word continuation probabilities.}

\item{model}{an object of class \code{language_model}.}

\item{.preprocess}{a function taking a character vector as input and
returning a character vector as output. Preprocessing transformation
applied to input before computing probabilities}

\item{...}{further arguments passed to or from other methods.}

\item{.tknz_sent}{a function taking a character vector as input and
returning a character vector as output. Optional sentence tokenization step
applied before computing sentence probabilities.}
}
\value{
a numeric vector. Probabilities of the sentences or word
continuations.
}
\description{
Compute sentence probabilities and word continuation conditional
probabilities from a language model
}
\details{
The generic function \code{probability()} is used to obtain both sentence
unconditional probabilities (such as Prob("I was starting to feel drunk"))
and word continuation conditional probabilities (such as
Prob("you" | "i love")). In plain words, these probabilities answer the
following related but conceptually different questions:
\itemize{
\item Sentence probability Prob(s): what is the probability that extracting a
single sentence (from a corpus of text, say) we will obtain exactly 's'?
\item Continuation probability Prob(w|c): what is the probability that a given
context 'c' will be followed exactly by the word 'w'?
}

In order to compute continuation probabilities (i.e. Prob(w|c)), one must
create conditional expressions with the infix operator \verb{\%|\%}, as shown in
the examples below. Both \code{probability} and \verb{\%|\%} are vectorized with
respect to words (left hand side of \verb{\%|\%}), but the context must be a length
one character (right hand side of \verb{\%|\%}).

The input is treated as in \link[kgrams]{query} for what concerns word
tokenization: anything delimited by (one or more) white space(s) is
tokenized as a word. For sentence probabilities, Begin-Of-Sentence and
End-Of-Sentence paddings are implicitly added to the input, but specifying
them explicitly does not produce wrong results as BOS and EOS tokens are
ignored by \code{probability()} (see the examples below). For continuation
probabilities, any context of more than \code{N - 1} words (where
\code{N} is the k-gram order the language model) is truncated to the last
\code{N - 1} words.

By default, the same \code{.preprocess()} and \code{.tknz_sent()}
functions used during model building are applied to the input, but this can
be overriden with arbitrary functions. Notice that the
\code{.tknz_sent} can be useful (for sentence probabilities) if
e.g. the input is a length one unprocessed character vector.
}
\examples{
# Usage of probability()

f <- kgram_freqs("a b b a b a b", 2)

m <- language_model(f, "add_k", k = 1)
probability(c("a", "b", EOS(), UNK()) \%|\% BOS(), m) # c(0.4, 0.2, 0.2, 0.2)
probability("a" \%|\% UNK(), m) # not NA

}
\author{
Valerio Gherardi
}
