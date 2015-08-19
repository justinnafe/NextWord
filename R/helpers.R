#' Clean the text
#'
#' @description Clean the text by removing extra whitespace, normalize to lowercase, remove
#' special characters, and remove unwanted words.
#' @param input Character vector to clean.
#' @param wordsToRemove Character vector of words to remove. The default is an empty string.
#' @return Cleaned text
#' @examples
#' input <- c("this is an example", "what will it do!")
#' remove <- c("example", "it", "the")
#' cleanText(input, remove)
#' # returns "this is an"    "what will  do"
#' @import tm
#' @export
cleanText <- function(input, wordsToRemove = ""){

  # Make lowercase
  result <- tolower(input)

  # Remove special characters
  result <- gsub("[^a-z ]", "", result)

  result <- removeWords(result, wordsToRemove)

  # Trim white space.
  result <- gsub("(^\\s+|\\s+$)", "", result, perl = TRUE)

  result
}

#' Tag the text with POS
#'
#' @description Tag the text with POS in the "token/TAG" format. The text is also cleaned.
#' @inheritParams cleanText
#' @return Tagged text
#' @examples
#' input <- c("this is an example", "what will it do!")
#' remove <- c("example", "it", "the")
#' tagText(input, remove)
#' # returns "this/DT is/VBZ an/DT what/WP will/MD do/VB"
#' @import NLP
#' @import openNLP
#' @export
tagText <- function(input, wordsToRemove = ""){
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()

  result <- cleanText(input, wordsToRemove)

  result <- as.String(result)
  annResult <- annotate(result, list(sent_token_annotator, word_token_annotator))

  pos_tag_annotator <- Maxent_POS_Tag_Annotator()

  annTagResult <- annotate(result, pos_tag_annotator, annResult)
  annTagWordResult <- subset(annTagResult, type == "word")
  tags <- sapply(annTagWordResult$features, `[[`, "POS")

  ## Extract token/POS pairs (all of them): easy.
  result <- sprintf("%s/%s", result[annTagWordResult], tags)
  result <- paste(result, collapse = " ")
  rm(annResult)
  rm(annTagResult)
  rm(annTagWordResult)
  rm(tags)
  rm(sent_token_annotator)
  rm(word_token_annotator)
  rm(pos_tag_annotator)
  gc()
  result
}

#' Remove the tokens that have unknown POS.
#'
#' @param gram An N-gram data.frame
#' @param validPos Character vector of valid POS tags
#' @param n An integer specifying the N in N-gram
#' @return A data.frame of cleaned N-grams.
#' @export
removeUnknownPos <- function(gram, validPos, n){
  test <- sapply(gram$pos, function(x){
    temp <- unlist(strsplit(x, " "))
    result <- NA
    if(sum(is.element(temp, validPos)) == n){
      result <- x
    }
    unlist(result)
  })
  gram$temp <- test
  results <- gram[complete.cases(gram),]
  results$temp <- NULL
  results
}
