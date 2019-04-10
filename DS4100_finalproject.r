library(future)
library(dplyr)
library(jsonlite)
library(XML)
library(jsonlite)
library(gsubfn)
library(httr)


#### addresses of where we are scrapping from 
addressList <- list(pullup_address = "http://stats.nba.com/js/data/sportvu/pullUpShootData.js", 
                    drives_address = "http://stats.nba.com/js/data/sportvu/drivesData.js", defense_address = "http://stats.nba.com/js/data/sportvu/defenseData.js", 
                    passing_address = "http://stats.nba.com/js/data/sportvu/passingData.js", 
                    touches_address = "http://stats.nba.com/js/data/sportvu/touchesData.js", 
                    speed_address = "http://stats.nba.com/js/data/sportvu/speedData.js", rebounding_address = "http://stats.nba.com/js/data/sportvu/reboundingData.js", 
                    catchshoot_address = "http://stats.nba.com/js/data/sportvu/catchShootData.js", 
                    shooting_address = "http://stats.nba.com/js/data/sportvu/shootingData.js")


# function that grabs the data from the website and converts to R data frame
readIt <- function(address) {
  web_page <- readLines(address)
  
  ## regex to strip javascript bits and convert raw to csv format
  x1 <- gsub("[\\{\\}\\]]", "", web_page, perl = TRUE)
  x2 <- gsub("[\\[]", "\n", x1, perl = TRUE)
  x3 <- gsub("\"rowSet\":\n", "", x2, perl = TRUE)
  x4 <- gsub(";", ",", x3, perl = TRUE)
  
  # read the resulting csv with read.table()
  nba <- read.table(textConnection(x4), header = T, sep = ",", skip = 2, stringsAsFactors = FALSE)
  return(nba)
}
df_list <- lapply(addressList, readIt)
df_list

df_catchshoot <- lapply("http://stats.nba.com/js/data/sportvu/catchShootData.js",readIt)
df_drives <- lapply("http://stats.nba.com/js/data/sportvu/drivesData.js", readIt)

web_page <- readLines("http://stats.nba.com/js/data/sportvu/pullUpShootData.js")
x1 <- gsub("[\\{\\}\\]]", "", web_page, perl=TRUE)
x2 <- gsub("[\\[]", "\n", x1, perl=TRUE)
x3 <- gsub("\"rowSet\":\n", "", x2, perl=TRUE)
x4 <- gsub(";", ",",x3, perl=TRUE)
nba<-read.table(textConnection(x4), header=T, sep=",", skip=2, stringsAsFactors=FALSE)
nba

















































