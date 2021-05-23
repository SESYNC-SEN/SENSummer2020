check_lib<- function(LibraryList) {
  # Expects list, e.g., LibraryList<-c("stringr","dplyr","rstudioapi")
  for (TheLibrary in LibraryList)
  {
    # check if libraries are installed, if not install them
    if(TheLibrary %in% rownames(installed.packages()) == FALSE) install.packages(TheLibrary)
  }
  # load libraries
  library(TheLibrary,character.only=TRUE)
}

install_or_load_pack <- function(pack){
  # https://nhsrcommunity.com/blog/a-simple-function-to-install-and-load-packages-in-r/
  # Swapped to this code, works better
  # install_or_load_pack(pack)
  # pack: expects list of libraries, e.g., pack<-c("tidyverse","tm","wordcloud","ggwordcloud","topicmodels")
  create_pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create_pkg))
    install.packages(create_pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}

load_pack <- function(pack){
  # load_pack(pack)
  # pack: expects list of libraries, e.g., pack<-c("tidyverse","tm","wordcloud","ggwordcloud","topicmodels")
  sapply(pack, require, character.only = TRUE)
}

strip_html <- function(s) {
  # https://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r
  load_pack(c("rvest"))
  html_text(read_html(s)) # strip html from text
}

custom_replace_symbol <- function(inputText){
  inputText <- gsub("&", " and ", inputText) # replace "&" with "and"
  inputText <- gsub("%", " percent ", inputText) # replace "%" with "percent"
  return(inputText)
}

create_ifnot_tokens <- function(outputFolderName, outputFileName, inputCorpus, custom_stop_words, overwrite=FALSE) {
  load_pack(c("stringi","tidyr","tidytext","data.table","dplyr","SnowballC"))
  outputFile = paste(outputFolderName,outputFileName,".RData",sep="")
  if (!file.exists(outputFile) | overwrite) {
    warning(paste("\nCreating new tokens:",outputFileName,"\n"))
    #create tokens
    text_cleaning_tokens <- inputCorpus %>% 
      tidytext::unnest_tokens(word, `Full text`) # convert individual words to rows
    text_cleaning_tokens<- text_cleaning_tokens %>% 
      mutate_at("word", funs(SnowballC::wordStem((.), language="english"))) # lemmatization & stemming
    text_cleaning_tokens$word <- gsub('[[:digit:]]+', '', text_cleaning_tokens$word) # remove digits
    text_cleaning_tokens$word <- gsub('[[:punct:]]+', '', text_cleaning_tokens$word) # remove punctuation
    text_cleaning_tokens <- text_cleaning_tokens %>% 
      filter(!(nchar(word) == 1)) %>% # remove single letters
      anti_join(custom_stop_words) # remove stop_words
    tokens <- text_cleaning_tokens %>% filter(!(word=="")) # remove empty rows
    # save output RData object to file
    save(tokens,file=outputFile) 
  } else {
    warning(paste("\nAlready exists, loading:",outputFileName,"\n"))
    # RData already exists & overwrite == FALSE; load saved RData object
    tokens_ld<-load(file=outputFile)
    tokens <- get(tokens_ld)
    rm(tokens_ld)
  }
  return(tokens)
}

create_ifnot_united_tokens <- function(outputFolderName, outputFileName, tokens, overwrite=FALSE) {
  load_pack(c("stringi","tidyr","tidytext","data.table","dplyr","tm.plugin.webmining"))
  outputFile = paste(outputFolderName,outputFileName,".RData",sep="")
  if (!file.exists(outputFile) | overwrite) {
    warning(paste("\nCreating new united tokens:",outputFileName,"\n"))
    #create tokens
    # convert accented text to normal text
    united_tokens <- tokens %>% 
      mutate(ind = row_number()) # create index from row number
    united_tokens <- united_tokens %>% 
      group_by(`ProQuest document ID`) %>% # re-group separated words by article ID
      mutate(ind = row_number()) %>% # re-do index based on row number
      tidyr::spread(key = ind, value = word) # add index key to each word
    united_tokens [is.na(united_tokens)] <- "" # replace NAs with blanks
    united_tokens <- tidyr::unite(united_tokens, `Full text`,-`ProQuest document ID`,sep =" " ) # unite data
    united_tokens$`Full text` <- trimws(united_tokens$`Full text`) # Trim whitespace
    # save output RData object to file
    save(united_tokens,file=outputFile) 
  } else {
    warning(paste("\nAlready exists, loading:",outputFileName,"\n"))
    # RData already exists & overwrite == FALSE; load saved RData object
    united_tokens_ld<-load(file=outputFile)
    united_tokens <- get(united_tokens_ld)
    rm(united_tokens_ld)
  }
  return(united_tokens)
}

create_ifnot_tokens_freq <- function(outputFolderName, outputFileName, tokens, overwrite=FALSE) {
  load_pack(c("ggplot2","broom","scales","tidyr","tidytext","data.table","dplyr"))
  outputFile = paste(outputFolderName,outputFileName,".RData",sep="")
  if (!file.exists(outputFile) | overwrite) {
    warning(paste("\nCreating new frequency tokens:",outputFileName,"\n"))
    #create tokens
    tokens_freq <- tokens %>%
      count(Year, word) %>% # count words per year
      complete(Year, word, fill = list(n = 0)) %>%
      group_by(Year) %>% # group words by year
      mutate(year_total = sum(n), # sum all words in a year
             percent = n / year_total) %>% # word occurrence/total all word occurrence
      ungroup()
    # save output RData object to file
    save(tokens_freq,file=outputFile)
  } else {
    warning(paste("\nAlready exists, loading:",outputFileName,"\n"))
    # RData already exists & overwrite == FALSE; load saved RData object
    tokens_freq_ld<-load(file=outputFile)
    tokens_freq <- get(tokens_freq_ld)
    rm(tokens_freq_ld)
  }
  return(tokens_freq)
}

create_ifnot_freqmodels <- function(outputFolderName, outputFileName, tokens_freq, overwrite=FALSE) {
  load_pack(c("ggplot2","broom","scales","tidyr","tidytext","data.table","dplyr"))
  outputFile = paste(outputFolderName,outputFileName,".RData",sep="")
  if (!file.exists(outputFile) | overwrite) {
    warning(paste("\nCreating new tokens:",outputFileName,"\n"))
    # Broom package to perform logistic regression on each word
    freq_models <- tokens_freq %>% 
      group_by(word) %>% # group by word
      filter(sum(n) > 50) %>% # sum all words greater than 50
      do(tidy(glm(cbind(n, year_total - n) ~ Year, ., # run glm on word total~year
                  family = "binomial"))) %>%
      ungroup() %>%
      filter(term == "Year")
    # save output RData object to file
    save(freq_models,file=outputFile)
  } else {
    warning(paste("\nAlready exists, loading:",outputFileName,"\n"))
    # RData already exists & overwrite == FALSE; load saved RData object
    freq_models_ld<-load(file=outputFile)
    freq_models <- get(freq_models_ld)
    rm(freq_models_ld)
  }
  return(freq_models)
  
}

tidy_words <- function(DTM, tidyTrim=FALSE) {
  # Predefined Cleaning Functions
  # https://cyberhelp.sesync.org/text-mining-lesson/course/
  # tidy_words(DTM, tidyTrim=FALSE)
  # Input: 
  # DTM - DocumentTermMatrix from tm library
  # tidyTrim - if FALSE, will not trim words in DTM. This step might not be necessary if 
  # DTM was already trimmed when created by specifying wordLengths in control_list_ngram
  load_pack(c("tm","tidytext","data.table","dplyr","tidyr"))
  # to be filled with outputs from tidy_words
  return_list <- list()
  # The tidytext package converts the (wide) Document Term Matrix into a longer form table with a row for every document and term combination.
  DTM_terms<- tidytext::tidy(DTM)
  return_list[['DTM_terms']]<-DTM_terms
  # The words data frame is more amenable to further inspection and cleaning, such as removing outliers.
  # frequency of word letter lengths -- if there are a lot of long words or short words, probably need to be dropped.
  DTM_summarise <- DTM_terms %>%
    group_by(term) %>%
    summarise(
      n = n(),
      total = sum(count)) %>%
    mutate(nchar = nchar(term))
  return_list[['DTM_summarise']]<-DTM_summarise
  if (tidyTrim) {
    # Words with too many characters are probably not actually words, and extremely uncommon words won’t help when searching for patterns.
    tidy_trim <- DTM_summarise %>%
      filter(
        nchar < 16,
        n > 1,
        total > 3) %>%
      select(term) %>%
      inner_join(DTM_terms)
    # Further steps in analyzing this “bag-of-words” require returning to the Document-Term-Matrix structure.
    DTM_trim <- tidy_trim %>%
      cast_dtm(document, term, count)
    return_list[['DTM_trim']]<-DTM_trim
  }
  return(return_list)
}

create_ifnot_dtm <- function(outputFolderName,outputFileName, control_list_ngram,inputCorpus, overwrite=FALSE) {
  load_pack(c("tm","tidytext","data.table","dplyr","tidyr"))
  # https://rstudio-pubs-static.s3.amazonaws.com/266040_d2920f956b9d4bd296e6464a5ccc92a1.html
  # If file does not exist, create. Otherwise, load. If overwrite is true, then re-write output.
  # Input:
  # control_list_ngram - inputs for DocumentTermMatrix
  # outputFolderName - folder path, e.g., "Data/02_Working/"
  # outputFileName - file name, e.g., "dtm_NGram-0"
  # inputCorpus - text in an r corpus data structure
  # overwrite - defaults to FALSE; if true, file will overwrite
  # output: 
  # DocumentTermMatrix and R.Data output of object
  outputFile = paste(outputFolderName,outputFileName,".RData",sep="")
  if (!file.exists(outputFile) | overwrite) {
    warning(paste("\nCreating new DTM:",outputFileName,"\n"))
    # create DTM with input control list & corpus
    DocTermMatrix <- tm::VectorSource(inputCorpus) %>%
      tm::VCorpus() %>%
      tm::DocumentTermMatrix(control = control_list_ngram)
    # save output RData object to file
    save(DocTermMatrix,file=outputFile)
  } else {
    warning(paste("\nAlready exists, loading:",outputFileName,"\n"))
    # RData already exists & overwrite == FALSE; load saved RData object
    DocTermMatrix_ld<-load(file=outputFile)
    DocTermMatrix <- get(DocTermMatrix_ld)
    rm(DocTermMatrix_ld)
  }
  return(DocTermMatrix)
}

create_ifnot_CreateDtm <- function(outputFolderName,outputFileName, ngrams=c(1,2),inputTokensText,inputTokensID, overwrite=FALSE) {
  load_pack(c("textmineR","tidytext","SnowballC"))
  # https://rstudio-pubs-static.s3.amazonaws.com/266040_d2920f956b9d4bd296e6464a5ccc92a1.html
  # If file does not exist, create. Otherwise, load. If overwrite is true, then re-write output.
  # Input:
  # ngrams - range of ngrams, c(1,2)
  # outputFolderName - folder path, e.g., "Data/02_Working/"
  # outputFileName - file name, e.g., "dtm_NGram-2"
  # inputTokensText - tokenized text from text cleaning; each article
  # inputTokensID - IDs of tokenized text; links to original publication
  # overwrite - defaults to FALSE; if true, file will overwrite
  # output: 
  # DocumentTermMatrix and R.Data output of object
  outputFile = paste(outputFolderName,outputFileName,".RData",sep="")
  if (!file.exists(outputFile) | overwrite) {
    warning(paste("\nCreating new DTM:",outputFileName,"\n"))
    #create DTM
    dtm <- textmineR::CreateDtm(doc_vec = inputTokensText, # character vector of documents
                                doc_names = inputTokensID, # document names, optional
                                ngram_window = ngrams, # minimum and maximum n-gram length
                                stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                                                 stopwords::stopwords(source = "smart")), # this is the default value
                                lower = TRUE, # lowercase - this is the default value
                                remove_punctuation = TRUE, # punctuation - this is the default
                                remove_numbers = TRUE, # numbers - this is the default
                                stem_lemma_function = function(x) SnowballC::wordStem(x, "english"))
    save(dtm,file=outputFile)
  } else {
    warning(paste("\nAlready exists, loading:",outputFileName,"\n"))
    # RData already exists & overwrite == FALSE; load saved RData object
    dtm_ld<-load(file=outputFile)
    dtm <- get(dtm_ld)
    rm(dtm_ld)
  }
  return(dtm)
}

create_ifnot_lda <- function(outputFolderName, outputFileName, numTopics=5, inputDTM, seed=12345, overwrite=FALSE) {
  load_pack(c("topicmodels"))
  # If file does not exist, create. Otherwise, load. If overwrite is true, then re-write output.
  # Input:
  # outputFolderName - folder path, e.g., "Data/02_Working/"
  # outputFileName - file name, e.g., "LDA_NGram-0"
  # numTopics - number of LDA output topics; default is 5
  # inputDTM - DocumentTermMatrix
  # overwrite - defaults to FALSE; if true, file will overwrite
  # seed - for setseed; default is 12345
  # output: 
  # LDA Model and R.Data output of object
  outputFile = paste(outputFolderName,outputFileName,".RData",sep="")
  if (!file.exists(outputFile) | overwrite) {
    warning(paste("\nCreating new LDA:",outputFileName,"\n"))
    lda_fit = LDA(full_DocTermMatrix, k = 5, control = list(seed=seed))
    # save output RData object to file
    save(lda_fit,file=outputFile)
  } else {
    warning(paste("\nAlready exists, loading:",outputFileName,"\n"))
    # RData already exists & overwrite == FALSE; load saved RData object
    lda_fit_ld<-load(file=outputFile)
    lda_fit <- get(lda_fit_ld)
    rm(lda_fit_ld)
  }
  return(lda_fit)
}

create_ifnot_plda <- function(outputFolderName, ldaFileNum, dtm, numTopicList, overwrite=FALSE) {
  # create outputFolderLDA
  outputFolderLDA <- paste0(outputFolderName,ldaFileNum,"_lda_models/")
  # if the dir doesn't exist, create it
  if (!dir.exists(outputFolderLDA)) dir.create(outputFolderLDA)
  fileNameLDAList <-  paste0(outputFolderLDA,ldaFileNum,"_lda_model_list.rda")
  if (!file.exists(fileNameLDAList) | overwrite) {
    load_pack(c("textmineR"))
    for (numTopics in numTopicList) {
      k_list <- seq(1, numTopics, by = 1)
      overwrite <- overwrite
      outputFolderLDA <- outputFolderLDA
      # save all model outputs to file & run all lda in parallel (i.e., at same time; runs faster)
      model_list <- textmineR::TmParallelApply(X = k_list, FUN = function(k){
        fileNameLDA = file.path(outputFolderLDA, paste0(k, "_lda_topics.rda"))
        if (!file.exists(fileNameLDA) | overwrite) {
          warning(paste("\nCreating new LDA:",fileNameLDA,"\n"))
          # find best model fit for each potential # of topics (1:numTopics)
          lda_model <- textmineR::FitLdaModel(dtm = dtm, k = k, iterations = 500)
          # k == number of topics 
          lda_model$k <- k
          lda_model$coherence <- textmineR::CalcProbCoherence(phi = lda_model$phi, dtm = dtm, M = 5)
          save(lda_model, file = fileNameLDA)
        } else {
          warning(paste("\nAlready exists, loading:",fileNameLDA,"\n"))
          # if does exists, load
          load(file=fileNameLDA)
        }
        lda_model
      }, export=c("dtm", "outputFolderLDA")) # export only needed for Windows machines
    }
    save(model_list, file = fileNameLDAList)
  } else {
    warning(paste("\nAlready exists, loading:",fileNameLDAList,"\n"))
    # RData already exists & overwrite == FALSE; load saved RData object
    load(file=fileNameLDAList)
  }
  return(model_list)
}

topic_subset_csv <- function(outputFolderName, rFileNum, outputFileName, inputCorpus, minPerc, maxPerc, theTopic, overwrite){
  ## e.g, minPerc = 0 & maxPerc = 0.4 is zero to 0.3999999999 ...
  outputFile = paste(outputFolderName,rFileNum,"_",outputFileName,".csv",sep="")
  if (!file.exists(outputFile) | overwrite) {
    warning(paste("\nCreating new topic csv:",outputFileName,"\n"))
    if (grepl("All", theTopic, ignore.case=TRUE)) {
      pq_topic <- pq_metajoin %>% filter(val >= minPerc, val < maxPerc)
    } else {
      pq_topic <- pq_metajoin %>% filter(val >= minPerc, val < maxPerc, topic == theTopic)
    }
    # Save cleaned corpus to new file 
    write.csv(pq_topic, outputFile, row.names=FALSE)
  } else {
    warning(paste("\nAlready exists, loading:",outputFileName,"\n"))
    # RData already exists & overwrite == FALSE; load saved RData object
    pq_topic <- data.table::fread(outputFile, colClasses = 'character')
  }
  return(pq_topic)
}