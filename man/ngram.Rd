% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/ngram.R
\name{ngram}
\alias{ngram}
\title{ngram tokenizes the text and returns a character vector with the ngram strings}
\usage{
ngram(text, gramLength)
}
\arguments{
\item{text}{string of text to load the ngrams.}

\item{gramLength}{Integer greater than zero}
}
\description{
Sentences are considerted to be the end of the processing. For example,
" this is the end. I will" will not contain an "end", "I", "will" trigram.
}

