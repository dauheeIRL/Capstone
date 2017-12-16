loadlib <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  suppressMessages(suppressPackageStartupMessages(library(p, character.only=TRUE)))
}

loadlib("stringi")
loadlib("data.table")
loadlib("quanteda")
loadlib("doParallel")
loadlib("foreach")
loadlib("parallel")
loadlib("DBI")
loadlib("brotli")
loadlib("LaF")
#library(feather)

if(detectCores()<10){
  dirfiles<-"C:/Per/_Courseara_Data_Science/10-Capstone Project/Coursera-SwiftKey/en_US"
}else{
  dirfiles<-"C:/installs/en_US"
}
set.seed(34596459)


if(detectCores()<10){
  setwd("C:/Per/_Courseara_Data_Science/10-Capstone Project")
}else{
  setwd("C:/installs")
}

getProfanityWords <- function() {
  profanityFileName <- "profanity.txt"
  if (!file.exists(profanityFileName)) {
    profanity.url <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
    download.file(profanity.url, destfile = profanityFileName, method = "curl")
  }
  
  if (sum(ls() == "profanity") < 1) {
    profanity <- read.csv(profanityFileName, header = FALSE, stringsAsFactors = FALSE)
    profanity <- profanity$V1
    profanity <- profanity[1:length(profanity)-1]
  }
  
  profanity
}

multiThreadAction<-function(dtIn, ngs, action){
  print(Sys.time())
  no_cores <- detectCores() - 1
  registerDoParallel(no_cores)
  nr <- nrow(dtIn)
  
  if(action==1){
    #break out into equal groups per available cores - no real need, only as processing larger amounts of sample does it save time
    dtTemp = (foreach(dt=split(dtIn, rep(1:no_cores, each=nr/no_cores, length.out=nr)), .packages=c('data.table'), .final=rbindlist, .inorder=FALSE) %dopar% 
      {
        dt[, c("WORD") := gsub("\\s*\\w*$", "", V1)][, c("PREDICT") := tstrsplit(V1, " ", fixed=FALSE, keep=ngs)]
        dt[, c("MLE","TIEBREAK"):=NA]#create new column
        dt
      }
    )
  }else if(action==2){
    #compress values - this way we can squeeze in more corpus
    dtTemp = (foreach(dt=split(dtIn, rep(1:no_cores, each=nr/no_cores, length.out=nr)), .export=c('vcomp'), .packages=c('data.table', 'brotli'), .final=rbindlist, .inorder=FALSE) %dopar% 
    {
      if(dim(dt)[2]>2){
        dt[, c("WORD", "PREDICT") := list(vcomp(WORD),vcomp(PREDICT))]
      }else{
        dt[, c("V1") := vcomp(V1)]
      }
      dt
    }
    )
  }
  
  stopImplicitCluster()
  registerDoSEQ()
  
  dtTemp
}

saveNgram<-function(ngs, tok){
  
  print(paste0("creating DT:-------------", ngs))
  dtTemp<-as.data.table(
    unlist(tokens_ngrams(tok, n = ngs, skip = 0, concatenator = " "))
    , keep.rownames=FALSE)[,.N,by=V1]
  
  if(ngs>1){
    
    print(paste0("----------------open previous ngram"))
    prevdt<-readRDS(paste0("ng",ngs-1, ".rds"))
    unidt<-readRDS("ng1.rds")
    
    print(paste0("----------------now begin split word via multi-thread"))
    dtTemp<-multiThreadAction(dtTemp, ngs, 1)
    
    print(paste0("----------------calculate MLE"))
    # Create an ephemeral in-memory RSQLite database
    con <- dbConnect(RSQLite::SQLite(), ":memory:")
    #put tables into the DB
    dbWriteTable(con, "dtTemp", dtTemp, row.names = FALSE)
    dbWriteTable(con, "prevdt", prevdt, row.names = FALSE)
    dbWriteTable(con, "unidt", unidt, row.names = FALSE)
    #create relevant indexes for fast processing
    dbExecute(con, "CREATE INDEX prevdti ON prevdt (V1)")
    dbExecute(con, "CREATE INDEX dtTempi ON dtTemp (WORD)")
    dbExecute(con, "CREATE INDEX unidti ON unidt (V1)")
    #now perform the update for MLE - use a multiplier to give MLE greater weight for higher order ngrams
    dbExecute(con, paste0("UPDATE dtTemp set MLE= (select (cast(dtTemp.N as float)/cast(prevdt.N as float) * 1.", ngs, ") from prevdt where prevdt.V1=dtTemp.WORD), TIEBREAK = (select unidt.N from unidt where unidt.V1=dtTemp.PREDICT)"))
    #dbExecute(con, 'UPDATE dtTemp set MLE= (select cast(dtTemp.N as float)/cast(prevdt.N as float) from prevdt where prevdt.V1=dtTemp.WORD)')
    #dbGetQuery(con, 'select * from dtTemp as dt1 where rowid in (SELECT rowid from dtTemp as dt2 where dt2.WORD = dt1.WORD order by dt2.MLE, dt2.TIEBREAK limit 3)')
    dtTemp<-setDT(dbGetQuery(con, "SELECT * FROM dtTemp"))
    dbDisconnect(con)
    
    rm(unidt)
    rm(prevdt)
  }
  
  saveRDS(dtTemp, paste0("ng", ngs, ".rds"), compress=FALSE) #no compress for now for speed, but will later
  rm(dtTemp)
  
  invisible(gc())
}

tidyNgram<-function(ngs){
  
  print("sort and set key DT")
  
  dtTemp<-readRDS(paste0("ng",ngs, ".rds"))
  
  if(ngs>1){
    
    setorder(dtTemp, WORD, -MLE, -TIEBREAK)
    dtTemp[, INDEX := seq_len(.N), WORD] #frank(dtTemp, WORD, c(-MLE,TIEBREAK))
    dtTemp <- dtTemp[INDEX<= 2L][,c("INDEX", "V1", "TIEBREAK", "N"):=NULL][,PREDICT:=as.factor(PREDICT)]
    setorder(dtTemp, WORD, MLE, PREDICT)
  }else{
    setnames(dtTemp,"V1","WORD")
    setorder(dtTemp, WORD, N)
  }
  
  setkey(dtTemp, WORD)
  saveRDS(dtTemp, paste0( "ng", ngs, ".rds"))
  rm(dtTemp)
  invisible(gc())
}

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
  
  #remove words that have 3 or more repeated characters
  #sp <- strsplit(txt, ' ')[[1]]
  #s <- sp[!sapply(sp, function(y) any(rle(strsplit(y, '')[[1]])$lengths >= 3))]
  #txt<-paste(s, collapse = ' ')
  #rm(sp)
  #rm(s)

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


refreshFiles<-function(percent_sample, create_sample, create_ngram, ngstart, ngend){

  if (create_sample){
    file_list_original<-list.files(dirfiles, "*.txt", full.names = TRUE)
    file_len_original<- sapply(file_list_original, determine_nlines) #countLines
    
    #writing out the sample files as this may be usefull for quicker subsequent runs which may be required for the final solution
    for(i in seq_along(file_list_original)){
      file_name<-paste0(stri_sub(file_list_original[i], 1, -4), "log2")
      if (file.exists(file_name)) file.remove(file_name)
      
      #load in and get rid of non english characters
      txt<-cleanText(sample_lines(file_list_original[i], (file_len_original[i]/100)*percent_sample[i]), FALSE)
      
      fileConn<-file(file_name)
      write(txt, fileConn) #get sample
      close(fileConn)
    }
  }
  
  file_list<-list.files(dirfiles, "*.log2", full.names = TRUE)
  alltexts <- unlist(c(readLines(file_list[1], skipNul=TRUE) , readLines(file_list[2], skipNul=TRUE) , readLines(file_list[3], skipNul=TRUE) ))
  
  #not removing stop words as they can be used in prediction
  tok<-tokens(alltexts, what="word", remove_numbers=FALSE, remove_punct=TRUE, remove_symbols=TRUE, remove_twitter=TRUE, remove_hyphens=TRUE, remove_url=TRUE, skip=0)
  
  #remove tokens that have badly spelled words like aaaaaa rrrrrr (3 or more consecutive same characters)
  tok<-tok[lapply(tok,function(x) length(grep("(\\w)\\1{2, }",x,value=FALSE))) == 0]
  
  if(create_ngram){
    
    for (ng in ngstart:ngend){
      saveNgram(ng, tok)
    }
    
    for (ng in ngstart:ngend){
      tidyNgram(ng)
    }
  }
  
  rm(tok)
}


searchNGRAM<-function(tok, ngrm_check, end_ignore){
  
  lentok<-length(unlist(tok))
  
  if((lentok>=(ngrm_check - 1))&((lentok-ngrm_check+2 - end_ignore)>0)){
    strFindformatted<-paste(unlist(tok)[(lentok-ngrm_check+2 - end_ignore):(lentok - end_ignore)], collapse = ' ')
    #strFindformatted<-vcomp(strFindformatted)
    dtReturn<-get(paste0("dtNG", ngrm_check))[strFindformatted, nomatch = 0]
  }else{
    dtReturn<-data.table()
  }
  
  dtReturn
}

performBackoffSearch<-function(searchText){

  #clean search text in same manner the corpus was
  searchText<-cleanText(searchText, TRUE)
  tok<-tokens(searchText, what="word", remove_numbers=FALSE, remove_punct=TRUE, remove_symbols=TRUE, remove_twitter=TRUE, remove_hyphens=TRUE, remove_url=TRUE, skip=0)

  #build up table of all ngrams and get highest likelyhood of MLE
  for(end_ignore in 0:3){
    dtReturn<-rbindlist(
      lapply(4:2, function(x){
        dtReturn<-searchNGRAM(tok, x, end_ignore)
        if (dim(dtReturn)[1] > 0){
          dtReturn$NGRAM<-x
          #dtReturn[,c("PREDICT","MLE", "NGRAM"), with=FALSE]
          dtReturn
          
        }
      })
    )
    if(nrow(dtReturn)>0) break #we have found something so don't start ignoring (more) end words
  }
  
  #get the predict word, max MLE and list what ngrams it was found it
  dtReturn<-dtReturn[, list(MLE=sum(MLE), NGRAM=paste(NGRAM,collapse=",")), by = PREDICT]
  
  #add in count of predicted word in unigram as will be tiebreak on equal mle
  dtReturn[,TIEBREAK:=unlist(sapply(PREDICT, function(x){dtNG1[.(x)]$N}))]
  setorder(dtReturn, -MLE, -TIEBREAK)
  
  print(dtReturn) #[,WORD:=rawToChar(brotli_decompress(WORD))])
  return(dtReturn[1,PREDICT]) #return just the first predicted word
}

refreshFiles(c(17, 17, 3), TRUE, TRUE, 1,4)

dtNG1<-readRDS("ng1.rds")
dtNG2<-readRDS("ng2.rds")
dtNG3<-readRDS("ng3.rds")
dtNG4<-readRDS("ng4.rds")

if(TRUE){

  (performBackoffSearch("all of a sudden, it seemed like everyone was trying to write"))
  (performBackoffSearch("The guy in front of me just bought a pound of bacon, a bouquet, and a case of"))
  (performBackoffSearch("You're the reason why I smile everyday. Can you follow me please? It would mean the"))
  (performBackoffSearch("Hey sunshine, can you follow me and make me the"))
  (performBackoffSearch("Very early observations on the Bills game: Offense still struggling but the"))
  (performBackoffSearch("Go on a romantic date at the"))
  (performBackoffSearch("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"))
  (performBackoffSearch("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"))
  (performBackoffSearch("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"))
  (performBackoffSearch("Be grateful for the good times and keep the faith during the"))
  (performBackoffSearch("If this isn't the cutest thing you've ever seen, then you must be"))
  
  
  (performBackoffSearch("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"))
  
  (performBackoffSearch("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"))
  
  (performBackoffSearch("I'd give anything to see arctic monkeys this"))
  (performBackoffSearch("When you were in Holland you were like 1 inch away from me but you hadn't time to take a"))
  (performBackoffSearch("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"))
  (performBackoffSearch("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"))
  (performBackoffSearch("Every inch of you is perfect from the bottom to the"))
  (performBackoffSearch("I'm thankful my childhood was filled with imagination and bruises from playing"))
  (performBackoffSearch("I like how the same people are in almost all of Adam Sandler's"))
  
  #the space station will then return to its standard six person crew with the arrival of nasa astronaut joe acaba and russian colleagues gennady padalka and sergei revin  who will blast off from the baikonur cosmodrome in kazakhstan on may 0.
  
}

if(FALSE){
  #have loaded and ready to rock:
  dtNG1<-readRDS("ng1.rds")
  dtNG2<-readRDS("ng2.rds")
  dtNG3<-readRDS("ng3.rds")
  dtNG4<-readRDS("ng4.rds")
  
  
  loadlib("RSQLite")
  db = dbConnect(SQLite(), dbname="Test.sqlite")
  head(dbReadTable(db, "BASEBALL"))
  
  setorder(dtNG2, -MLE, -TIEBREAK, PREDICT)
  setorder(dtNG3, -MLE, -TIEBREAK, PREDICT)
  setorder(dtNG4, -MLE, -TIEBREAK, PREDICT)
  setkey(dtNG2, WORD)
  setkey(dtNG3, WORD)
  setkey(dtNG4, WORD)
  
  
  a<-memCompress(charToRaw("word"), "bzip2")
  b<-rawToChar(memDecompress(a))
  
  aa<-brotli_compress(charToRaw("word"))
  b<-rawToChar(brotli_decompress(aa))
  
  
  
  xx[, brotli_compress(charToRaw(a))]
  
  brotli_compress(charToRaw(xx$a))
  unlist(sapply(xx$a, charToRaw))
  
  brotli_compress(sapply(xx$a, charToRaw))
  (sapply(xx$a[2], charToRaw))
  
  charToRaw(xx$a[2])
  
  brotli_compress(sapply(xx$a[2], charToRaw))
  
}
#system.time(dtTemp<-readRDS("ng3.rds"))
#dtTemp["be on my", nomatch = 0]

#system.time(tbl<-readRDS(paste0("ng",6, ".rds")))

#write_feather(tbl, "ng6.feather")
#system.time(tbl <- read_feather("ng6.feather"))

#saveRDS(tbl,"ng6-non-compress.rds", compress = FALSE)
#system.time(tbl<-readRDS("ng6-non-compress.rds"))



