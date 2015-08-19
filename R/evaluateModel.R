#' Evaluate the N-gram model
#'
#' @description Evaluate the model with preferably unseen text and indicate to the
#' evaluation if the text has already been cleaned and tagged.
#'
#' @param model A list of N-gram data.frames sorted in descending order based on the probability of the token and POS sequence occuring.
#' @param input Character vector of words.
#' @param cleanedPosText Boolean indicating if the text has already been cleaned and tagged with POS.
#' @export
evaluateModel <- function(model, input, cleanedPosText = FALSE){

  # Split up the text into tokens
  tok <- unlist(strsplit(input, "\\s+"))
  accuracy <-rep(0, times = length(tok) - 1)
  prevEvaluatedStr <- ""
  # For each token, determine the next word and predict the word
  for(i in 2:length(tok) - 1){
    temp <- paste(prevEvaluatedStr, tok[i], sep = " ")
    if(!cleanedPosText){
      temp <- sub('.*?(\\w+\\W+\\w+\\W+\\w+\\W+\\w+)$', '\\1', temp)
      tempNextWord <- tok[i + 1]
    }
    else{
      temp <- sub('.*?(\\w+/\\w+\\s+\\w+/\\w+\\s+\\w+/\\w+\\s+\\w+/\\w+)$', '\\1', temp)
      tempNextWord <- gsub("([a-zA-Z])/[a-zA-Z]+", "\\1", tok[i + 1])
    }

    predNextWord <- NextWord::predict(model, temp, cleanedPosText)

    # If the predicted word is within the results, then the accuracy is 1, else 0
    if(sum(is.element(tempNextWord, predNextWord)) > 0){
      accuracy[i] <- 1
    }
    prevEvaluatedStr <- temp
  }
  if(length(accuracy) > 0){
    result <- sum(accuracy) / length(accuracy)
  }
  result
}
