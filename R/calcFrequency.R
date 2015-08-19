#' Calculate the frequencies of the tokens
#'
#' @description Calculate the frequencies of the tokens.
#' @param input A character vector of tokens.
#' @return A data.frame with "token" and "count" columns, sorted in descending order by count.
#' @import dplyr
#' @export
calcFrequency <- function(input){
  inputFrame <- data.frame(input, rep(1, times=length(input)))
  colnames(inputFrame) <- c("token", "count")
  result <- inputFrame %>%
    group_by(token) %>%
    select(token, count) %>%
    summarize(count = sum(count)) %>%
    arrange(desc(count))
  rm(input)
  result
}

#' Get the POS
#'
#' @description Get the POS of a string where the format is "token/TAG".
#' @param tokens Character vector of tokens in the format of "token/TAG".
#' @export
getPos <- function(tokens){
  sapply(tokens, function(x){gsub("([a-z].*?)/([A-z].*?)", "\\2", x)})
}

#' Get the tokens
#'
#' @description Get the tokens of a string where the format is "token/TAG".
#' @inheritParams getPos
#' @export
getTokens <- function(tokens){
  sapply(tokens, function(x){gsub("([a-zA-Z])/[a-zA-Z]+", "\\1", x)})
}

#' Calculate the prbabilities of tokens with POS.
#'
#' @param input A character vector of tokens in the format of "token/TAG"
#' @return A data.frame with token, tokenprob, and pos columns.
#' @import dplyr
#' @export
calcProbsWithPos <- function(input){
  inputFrame <- data.frame(input, rep(1, times=length(input)))
  colnames(inputFrame) <- c("token", "tokenprob")

  frameLength <- length(inputFrame$token)
  result <- inputFrame %>%
    group_by(token) %>%
    select(token, tokenprob) %>%
    summarize(tokenprob = sum(tokenprob)/frameLength) %>%
    arrange(desc(tokenprob))
  tokens <- result$token
  result$pos <- getPos(tokens)
  result$token <- getTokens(result$token)

  rm(input)
  result
}

#' Get the POS probabilities.
#'
#' @param input A character vector in the format of "token/POS".
#' @return A data.frame with pos and posprob columns.
#' @import dplyr
#' @export
getPosProbs <- function(input){
  inputFrame <- data.frame(input, rep(1, times=length(input)))
  colnames(inputFrame) <- c("pos", "posprob")
  inputFrame$pos <- sapply(inputFrame$pos, function(x){gsub("([a-z].*?)/([A-z].*?)", "\\2", x)})
  frameLength <- length(inputFrame$pos)
  result <- inputFrame %>%
    group_by(pos) %>%
    select(pos, posprob) %>%
    summarize(posprob = sum(posprob)/frameLength) %>%
    arrange(desc(posprob))
  rm(input)
  as.data.frame(result)
}

#' Calculate the probabilities of the token occuring and the POS occuring.
#'
#' @param ngramFrame A data.frame with columns "token", and "tokenprob".
#' @param posFrame A data.frame with columns "pos" and "posprob".
#' @return A data.frame with columns "pos", "token", and "combprob", sorted in descending order by "combprob".
#' @import dplyr
#' @export
joinProbs <- function(ngramFrame, posFrame){
  result <- merge(ngramFrame, posFrame, all.x = TRUE, by = "pos")
  temp <- result %>%
    select(pos, token, tokenprob, posprob) %>%
    mutate(combprob = tokenprob * posprob) %>%
    arrange(desc(combprob))
  temp$tokenprob <- NULL
  temp$posprob <- NULL
  temp
}
