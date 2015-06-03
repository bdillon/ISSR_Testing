######## Init ######## 

library(doMC)
library(foreach)
library(statnet)

######## Params ######

congresses <- 11
bills.to.use <- 10
main.path <- '~/Documents/ISSR_Testing'
file.names <- dir(paste(main.path,"/Data",sep=""), pattern = "senmatrix.txt", full.names=TRUE, recursive=FALSE)

nCores <- 2
registerDoMC(nCores)

######## Fxns ########

######## Reads in all files in filenames list
######## Stores results in list, each element of which contains:
########        raw.data <- contents of csv
########        filename <- string of filename

ReadData <- function(filenames) {
  raw.data.list <- foreach(i=1:length(filenames)) %dopar% {
      list(
        raw.data = read.csv(filenames[[i]],
               stringsAsFactors = F, header = F),
        file.name = filenames[[i]]
      )
  }  
  return(raw.data.list)
}

######## Given a list index, the list output of ReadData, and a # of bills
######## Computes the sociomatrix for a given list index. 
######## Given N senators / rows in the data in raw.data.list, it returns:
######## N x N matrix which # of co-sponshorships (cols) for a given sponsor (row)

GenerateSociomatrix <- function(list.index, raw.data.list, num.bills){
  cat("Currently processing", raw.data.list[[list.index]]$file.name)
  cur.session <- raw.data.list[[list.index]]$raw.data
  num.bills <- min(num.bills,ncol(cur.session))
  cur.data <- cur.session[, 1:num.bills]
  
  num.senators <- length(cur.data[,1])
  cur.sociomatrix <- matrix(0, ncol = num.senators, nrow = num.senators)

  for (i in 1:num.bills) {
    spon <- which(cur.data[,i] == 1)
    co.spons <- which(cur.data[,i] == 2)
    cur.sociomatrix[spon,co.spons] <- cur.sociomatrix[spon,co.spons] + 1
  }
  return(cur.sociomatrix)
}  

######## Main processing loop:
######## Read in all data 
######## Make wrapper fxn
######## Parallelize wrapper fxn

PreprocessNetworkData <- function(filenames, num.bills){
  raw.sponsorship.data <- ReadData(filenames)
  MakeSociomat <- function (x) GenerateSociomatrix(x,raw.sponsorship.data,num.bills)
  all.sociomatrix <- foreach(i=1:length(filenames)) %dopar% {
      result <- MakeSociomat(i)
      }  
  return(all.sociomatrix)
}

######## Run it, time it

system.time({
  cosponsorship.matrix <- PreprocessNetworkData(file.names, bills.to.use)
})


######## Plot it

netplot <- function(year, color){
  net <- as.network(cosponsorship.matrix[[year]])
  plot(net, vertex.col = color)
  Sys.sleep(.2)
}

colors <- (1:11)
years <- c(1:length(file.names))
result <- mapply(netplot, years,colors)
