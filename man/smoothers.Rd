% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smoothers.R
\name{smoothers}
\alias{smoothers}
\alias{info}
\title{k-gram Probability Smoothers}
\usage{
smoothers()

info(smoother)
}
\arguments{
\item{smoother}{a string. Code name of probability smoother.}
}
\value{
\code{smoothers()} returns a character vector, the list of code names
of probability smoothers available in \link[kgrams]{kgrams}.
\code{info(smoother)} returns \code{NULL} (invisibly) and prints some
information on the selected smoothing technique.
}
\description{
Information on available k-gram continuation probability smoothers.
\subsection{List of smoothers currently supported by \code{kgrams}}{
\itemize{
\item \code{"ml"}: Maximum Likelihood estimate
\insertCite{markov1913essai}{kgrams}.
\item \code{"add_k"}: Add-k smoothing
\insertCite{dale1995philosophical,lidstone1920note,johnson1932probability,jeffreys1998theory}{kgrams}.
\item \code{"abs"}: Absolute discounting \insertCite{ney1991smoothing}{kgrams}.
\item \code{"wb"}: Witten-Bell smoothing \insertCite{bell1990text,witten1991zero}{kgrams}
\item \code{"kn"}: Interpolated Kneser-Ney.
\insertCite{Kneser1995ImprovedBF,chen1999empirical}{kgrams}.
\item \code{"mkn"}: Interpolated modified Kneser-Ney.
\insertCite{chen1999empirical}{kgrams}.
\item \code{"sbo"}: Stupid Backoff \insertCite{brants-etal-2007-large}{kgrams}.
}
}
}
\examples{
# List available smoothers
smoothers()

# Get information on smoother "kn", i.e. Interpolated Kneser-Ney
info("kn")


}
\references{
\insertAllCited{}
}
\author{
Valerio Gherardi
}
