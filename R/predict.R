
getQuadgramPrediction <- function(model, inputText){
  tags <- getPos(inputText)
  tokens <- getTokens(inputText)
  tokensPattern <- paste("^", sub('.*?(\\w+\\W+\\w+\\W+\\w+)$', '\\1', tokens), sep = "")
  posPattern <- paste("^", sub('.*?(\\w+\\W+\\w+\\W+\\w+)$', '\\1', tags), sep = "")
  modelResults <- model$quadgram[grep(tokensPattern, model$quadgram$token), ]
  modelResults[grep(posPattern, modelResults$pos), ]
}

getTrigramPrediction <- function(model, inputText){
  tags <- getPos(inputText)
  tokens <- getTokens(inputText)
  tokensPattern <- paste("^", sub('.*?(\\w+\\W+\\w+)$', '\\1', tokens), sep = "")
  posPattern <- paste("^", sub('.*?(\\w+\\W+\\w+)$', '\\1', tags), sep = "")
  modelResults <- model$trigram[grep(tokensPattern, model$trigram$token), ]
  modelResults[grep(posPattern, modelResults$pos), ]
}

getBigramPrediction <- function(model, inputText){
  tags <- getPos(inputText)
  tokens <- getTokens(inputText)
  tokensPattern <- paste("^", sub('.*?(\\w+)$', '\\1', tokens), sep = "")
  posPattern <- paste("^", sub('.*?(\\w+)$', '\\1', tags), sep = "")
  modelResults <- model$bigram[grep(tokensPattern, model$bigram$token), ]
  modelResults[grep(posPattern, modelResults$pos), ]
}

getUnigramPrediction <- function(model, inputText){
  model$unigram[1:3,]
}

#' Predict the next word
#'
#' @description Predict the next word using a given model. The prediction method uses a backoff
#' method if the token is not present in highest N-gram.
#' @inheritParams evaluateModel
#' @export
predict <- function(model, input, cleanedPosText = FALSE){
  if(input != ""){
    if(!cleanedPosText){
      input <- tagText(input)
    }
    currentTokens <- unlist(strsplit(input, " "))
    results <- head(model$unigram)
    tempResults <- head(model$unigram)
    # try quadgram
    if(length(currentTokens) >= 3){
      tempResults <- getQuadgramPrediction(model, input)
      results <- tempResults
      if(length(unique(results$token)) < 3){
        tempResults <- getTrigramPrediction(model, input)
        results <- rbind(results, tempResults)
        if(length(unique(results$token)) < 3){
          tempResults <- getBigramPrediction(model, input)
          results <- rbind(results, tempResults)
          if(length(unique(results$token)) < 3){
            tempResults <- getUnigramPrediction(model, input)
            results <- rbind(results, tempResults)
          }
        }
      }
    }
    if(length(currentTokens) == 2){
      tempResults <- getTrigramPrediction(model, input)
      results <- tempResults
      if(length(unique(results$token)) < 3){
        tempResults <- getBigramPrediction(model, input)
        results <- rbind(results, tempResults)
        if(length(unique(results$token)) < 3){
          tempResults <- getUnigramPrediction(model, input)
          results <- rbind(results, tempResults)
        }
      }
    }
    if(length(currentTokens) == 1){
      tempResults <- getBigramPrediction(model, input)
      results <- tempResults
      if(length(unique(results$token)) < 3){
        tempResults <- getUnigramPrediction(model, input)
        results <- rbind(results, tempResults)
      }
    }
  } else{
    results <- getUnigramPrediction(model, input)
  }
  unique(sub('.*?(\\w+)$', "\\1", results$token))[1:3]
  #results[1:3,]
}
