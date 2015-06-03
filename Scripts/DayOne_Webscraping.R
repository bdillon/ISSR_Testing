rm(list = ls())
library(stringr)
library(scrapeR)
library(ggplot2)

###### Fxns #######

ScrapePage <- function(cur.url) {
  cat(cur.url, "\n")
  cur.url <- tolower(cur.url)
  cur.page <- getURL(cur.url)
  cur.page <- str_split(cur.page, '\n')[[1]]
  start <- grep("112th CONGRESS", cur.page)
  end <- grep("&lt;all&gt;", cur.page)
  
  ### Three dodgy types:
  ### 1) Never found either start, or end: never found any text
  ### 2) Sometimes they were 'NA's... unclear why they were NA's
  ### 3) 
  
  if(length(end) > 0 & length(start) > 0){
    # Get just the text
    if(!is.na(start) & !is.na(end)){
      if(start < end & start > 0 & end > 0){
        bill.text <- cur.page[start:end]
      }else{
        print(start)
        print(end)
        bill.text <- ""
      }
    }else{
      print(page)
      bill.text <- ""
    }
  }else{
    bill.text <- ""
  }
  to_return <- list(page = cur.page, text = bill.text)
  Sys.sleep(5)      ## Important: so we don't cause a DOS attack
  return(to_return)
}

TokenizeString <- function(string){
  temp <- tolower(string)
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s:\\?\\!]", " ")
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  temp <- stringr::str_split(temp, " ")[[1]]
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  }
  return(temp)
}

ParseBill <- function(cur.bill) {
  cur.page <- cur.bill$page
  if (length(grep("dc.creator",cur.page)) > 0) {
    cur.creator <- str_split(cur.page[grep("dc.creator",cur.page)],"content=\"")[[1]][2]
    cur.bill$creator <- str_split(cur.creator,"\"")[[1]][1]
  } else {
    cur.bill$creator <- "None"
  }
  return(cur.bill)
}

TokenizeBill <- function(cur.bill) {
  cur.text <- cur.bill$text
  tokenized.text <- unlist(
    lapply(cur.text, TokenizeString)
  )
  cur.bill$tokenized <- tokenized.text
  cur.bill$N <- length(tokenized.text)
  cur.bill$types <- length(unique(tokenized.text))
  cur.bill$word.counts <- xtabs(~tokenized.text)
  return(cur.bill)
}

GetWordCount <- function(cur.word, all.bills) {
  all.counts <- {}
  for (bill in all.bills) {
    if (is.na(bill$word.counts[cur.word])) {
      cur.count <- 0 
    } else {
      cur.count <- bill$word.counts[cur.word]
    }
    all.counts <- c(all.counts,cur.count)
  }  
  return(sum(all.counts))
}

MakeDataFrame <- function(cur.bill) {
  if (cur.bill$text != ""){
    cur.summary <- data.frame(
                     N = cur.bill$word.counts,
                     Author = cur.bill$creator
                      )
    colnames(cur.summary) <- c("Word","Freq","Author")
  } else {
    cur.summary <- {}
  }
  return(cur.summary)
}

###### Body #######

###### Load data
load("./Data/Bill_URLs.Rdata")

###### Process URLs; update applied 6/1/2015

bill.urls <- sapply(Bill_URLs, 
  function(x) str_replace_all(x, 'http://beta.', 'https://www.'),
  simplify=TRUE)

bill.urls <- sapply(bill.urls, 
  function(x) paste(x, "/text?format=txt", sep=""),
  simplify=TRUE)

###### Process text

bills.raw <- lapply(bill.urls, ScrapePage)
bills.tokenized <- lapply(bills.raw, TokenizeBill)
bills.processed <- lapply(bills.tokenized, ParseBill)

###### Save processed text

save(bills.processed, file = "./Data/Processed_Bills.Rdata")
load("./Data/Processed_Bills.Rdata")

###### Unwrap into data.frame object

bills.summary <- {}
for (bill in bills.processed) {
  bills.summary <- rbind(bills.summary,MakeDataFrame(bill))  
}

###### Histogram plots of:
######    "defense" by author
######    "education" by author

ggplot(subset(bills.summary,Word == "defense" | Word == "education"),
              aes(y = Freq, x = Word)) + geom_bar(stat='identity') + facet_wrap(~Author)


