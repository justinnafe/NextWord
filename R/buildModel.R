#' Build a model based on N-gram probabilities
#'
#' Build a list of N-grams with probabilities. The range of N-grams go from 1 to 4 grams.
#'
#' @param ngramFreq List of data.frame objects that contain tokens, POS, and probabilities
#' @return List of N-grams
#' @export
buildModel <- function(ngramFreq){
  result <- list()
  for(i in ngramFreq){
    stopifnot(is.data.frame(i))

    # check the token column for what type of ngram it is
    if(length(unlist(strsplit(i[1,1], " "))) == 1){
      result$unigram <- i
    }
    if(length(unlist(strsplit(i[1,1], " "))) == 2){
      result$bigram <- i
    }
    if(length(unlist(strsplit(i[1,1], " "))) == 3){
      result$trigram <- i
    }
    if(length(unlist(strsplit(i[1,1], " "))) == 4){
      result$quadgram <- i
    }
  }
  result
}
