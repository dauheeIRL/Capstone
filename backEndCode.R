loadlib <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  suppressMessages(suppressPackageStartupMessages(library(p, character.only=TRUE)))
}

library(stringi)
library(shiny)
library(data.table)
library(quanteda)
library(doParallel)
library(foreach)
library(parallel)
library(DBI)
library(brotli)
library(LaF)


dtReturn<-NA


cleanText<-function(txt, userinput){
  
  #load in and get rid of non english characters
  txt<-char_tolower(
    iconv(txt, "latin1", "ASCII", "") 
  )
  
  #much of below code for cleaning taken from Julia Phelps as found in onine code snippit
  txt<- gsub("[[:cntrl:]]", "", txt) # remove tabs etc
  ## remove websites
  txt <- gsub(pattern="[[:alnum:]]+://([[:alnum:]]\\.)?(.+)\\.([[:alnum:]]+)/?([[:alnum:]]*[[:punct:]]*)*", replacement="", x=txt)
  ## remove emails
  txt <- gsub(pattern="[[:alnum:]]+[_|\\.]*[[:alnum:]]*@[[:alnum:]]+(_|-|\\.)*[[:alnum:]]*\\.[[:alnum:]]+", replacement="", x=txt)
  ## remove hashtags
  txt <- gsub(pattern="#[[:alpha:]]+(_*[[:alnum:]]*)*", replacement="", x=txt)
  ## remove twitter handles
  txt <- gsub(pattern=" @[[:alnum:]]+_*[[:alnum:]]*", replacement="", x=txt)
  ## remove remaining words joined by @ symbols
  txt <- gsub(pattern="[[:alnum:]]+(_|-|\\.)*[[:alnum:]]*@[[:alnum:]]+", replacement="", x=txt)
  ## replace hyphens and slashes with spaces
  txt <- gsub(pattern="[-|/|\\]", replacement=" ", x=txt)
  
  #may of below will be removed in tokenization anyhow but remove now to clear up some junk lines that will be identified below
  txt<-gsub("_", " ", txt)
  txt<-gsub("-", " ", txt)
  txt<-gsub("[(]", "", txt)
  txt<-gsub("[)])", "", txt)
  txt<-gsub("\"", "", txt)
  txt<-gsub(",", "", txt)
  txt<-gsub("'", "", txt)
  txt<-gsub("[.]", " ", txt)
  txt<-gsub("[;]", " ", txt)
  txt<-gsub("[:]", " ", txt)
  
  #now get rid of non alpha, and mask numbers as 0
  #txt<-stri_replace_all_regex (gsub("[^[:alnum:] ]", "", txt, perl=TRUE), "[[:digit:]]+", "0")
  txt<-stri_replace_all_regex (txt, "[[:digit:]]+", "0")
  
  #separate numbers from text as may mask their meaning and getting tokenized
  txt<-gsub("([0])([a-z])", "\\1 \\2", txt)
  txt<-gsub("([a-z])([0])", "\\1 \\2", txt)
  
  #get rid of multiple spaces
  txt<-gsub(' +',' ', txt)
  
  #get rid of multiple zeroes
  txt<-gsub('00+','0', txt)
  
  if(!userinput){
    #get rid of single lines where not multiple words or is very small - not usefull for prediction
    txt<-txt[(stri_length(txt) > 5) & (grepl(" ", txt))]
    
    #get rid of crlf that sample_lines leaves
    txt<-stri_sub(txt, 1, -2)
  }
  txt
}




searchNGRAM<-function(tok, ngrm_check, end_ignore){
  
  lentok<-length(unlist(tok))
  
  if((lentok>=(ngrm_check - 1))&((lentok-ngrm_check+2 - end_ignore)>0)){
    strFindformatted<-paste(unlist(tok)[(lentok-ngrm_check+2 - end_ignore):(lentok - end_ignore)], collapse = ' ')
    dtRet<-get(paste0("dtNG", ngrm_check))[strFindformatted, nomatch = 0]
  }else{
    dtRet<-data.table()
  }
  
  dtRet
}

performBackoffSearch<-function(searchText){
  
  #clean search text in same manner the corpus was
  searchText<-cleanText(searchText, TRUE)
  tok<-tokens(searchText, what="word", remove_numbers=FALSE, remove_punct=TRUE, remove_symbols=TRUE, remove_twitter=TRUE, remove_hyphens=TRUE, remove_url=TRUE, skip=0)
  tokstartlen<-4
  
  if(length(tok)< 4){
    tokstartlen<-length(tok)+1
  }
  
  #build up table of all ngrams and get highest likelyhood of MLE
  for(end_ignore in 0:3){
    dtRet<-rbindlist(
      lapply(4:2, function(x){
        dtRet<-searchNGRAM(tok, x, end_ignore)
        if (dim(dtRet)[1] > 0){
          dtRet$NGRAM<-x
          dtRet
          
        }
      })
    )
    if(nrow(dtRet)>0) break #we have found something so don't start ignoring (more) end words
  }
  
  if(nrow(dtRet)>0){
    #get the predict word, max MLE and list what ngrams it was found it
    dtRet<-dtRet[, list(MLE=sum(MLE), NGRAM=paste(NGRAM,collapse=",")), by = PREDICT]
    
    #add in count of predicted word in unigram as will be tiebreak on equal mle
    dtRet[,TIEBREAK:=unlist(sapply(PREDICT, function(x){dtNG1[.(x)]$N}))]
    dtRet[PREDICT=="0"]$PREDICT<-"[number]"
    dtRet$PREDICT<-as.character(dtRet$PREDICT)
    
    setnames(dtRet,"TIEBREAK","PREDICT_WORD_FREQ")
    setnames(dtRet,"MLE","RANKED_MLE")
    
    setorder(dtRet, -RANKED_MLE, -PREDICT_WORD_FREQ)
    
    dtReturn<<-dtRet
  
    return(list(nextwords=paste0("<b>Next words (in order of probability): </b>", paste(dtReturn[,PREDICT], collapse=' ')),
                cleaned=paste0("<b>Cleaned search text: </b>", paste(unlist(tok), collapse = ' ')),
                dt=dtReturn))
  }
}

dtNG1<-readRDS("./Data/ng1.rds")
dtNG2<-readRDS("./Data/ng2.rds")
dtNG3<-readRDS("./Data/ng3.rds")
dtNG4<-readRDS("./Data/ng4.rds")
