

wordPrediction <- function(input_text){

  bg<-readRDS("./bigram.RData")
  tg<-readRDS("./trigram.RData")
  qd<-readRDS("./quadgram.RData")

  names(bg)[names(bg) == 'word1'] <- 'w1'; names(bg)[names(bg) == 'word2'] <- 'w2';
  names(tg)[names(tg) == 'word1'] <- 'w1'; names(tg)[names(tg) == 'word2'] <- 'w2'; names(tg)[names(tg) == 'word3'] <- 'w3';
  names(qd)[names(qd) == 'word1'] <- 'w1'; names(qd)[names(qd) == 'word2'] <- 'w2'; names(qd)[names(qd) == 'word3'] <- 'w3';
  names(qd)[names(qd) == 'word4'] <- 'w4';
  #message <- ""
  
  # clean the world
  #input_text<-"hello"
  input_text<-as.character(input_text)
  input_text<-tolower(input_text)
  input_text<-removePunctuation(input_text,preserve_intra_word_dashes=T)
  input_text<-removeNumbers(input_text)
  input_text<-stripWhitespace(input_text)
  
  #create a list of words
  list_of_words <- as.character(strsplit(input_text, " ")[[1]])

  # create input for ngram functions
  input_ngram<-list_of_words
  length(input_ngram)
  
  # if one word check biagram, if two check triagram, if three or more check quadgram
  if (length(input_ngram)==1){
    if (identical(character(0),as.character(head(bg[bg$w1 == input_ngram[1], 2], 1)))){
      # testing print(bg$w1)
      #message<<-"If no word found the most used pronoun 'it' in English will be returned" 
      as.character(head("it",1))
    }else{
      #message <<- "Trying to Predict the Word using Bigram Freqeuncy Matrix  "
      as.character(head(bg[bg$w1 == input_ngram[1],2], 1))
      # testing print of bg$w1, the_word[1]
    }
  }else if(length(input_ngram)==2){
    # # testing print(the_word)
    if(identical(character(0),as.character(head(tg[tg$w1 == input_ngram[1] & tg$w2 == input_ngram[2], 3], 1)))){
      as.character(wordPrediction(input_ngram[2]))
      # testing print tg$w1, tg$w2, the_word[1], the_word[2]
    }else{
      # message<<- "Trying to Predict the Word using Trigram Fruequency Matrix "
      as.character(head(tg[tg$w1 == input_ngram[1] & tg$w2 == input_ngram[2], 3], 1))
      # testing print of tg$w1, tg$w2, the_word[1], the_word[2]
    }
  }else if(length(input_ngram)>=3){
    # testing print(the_word)
    if(identical(character(0),as.character(head(qd[qd$w1 == input_ngram[1] & qd$w2 == input_ngram[2] & qd$w3 == input_ngram[3], 4], 1)))){
      # testing print of qd$w1, qd$w2, qd#w3, the_word[1], the_word[2], the_word3
      as.character(wordPrediction(paste(input_ngram[2],input_ngram[3],sep=" ")))
    }else{
      #message <<- "Trying to Predict the Word using Quadgram Frequency Matrix"
      as.character(head(qd[qd$w1 == input_ngram[1] & qd$w2 == input_ngram[2] & qd$w3 == input_ngram[3], 4], 1))
      # testing print of qd$w1, qd$w2, qd#w3, the_word[1], the_word[2], the_word3
    }    
  }
  
} #end of function

