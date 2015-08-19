#' ngram tokenizes the text and returns a character vector with the ngram strings
#'
#' Sentences are considerted to be the end of the processing. For example,
#' " this is the end. I will" will not contain an "end", "I", "will" trigram.
#'
#' @param text string of text to load the ngrams.
#' @param gramLength Integer greater than zero
#'
#' @import RWeka
#' @export
ngram <- function(text, gramLength){
  delim <- " \\t\\r\\n.!?,;\"()"

  NGramTokenizer(text, Weka_control(min=gramLength,max=gramLength, delimiters = delim))

}
