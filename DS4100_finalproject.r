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









################################################### TEAMS DATA ##################################################################
## WEB SCRAPPING WITH RVEST TO PULL THE STANDINGS DATA
library(rvest)

page <- read_html("https://www.basketball-reference.com/leagues/NBA_2019_standings.html?fbclid=IwAR0xnccmjlJRX5qZBCeSbIF_ubY_PlMo9kMmzKwEvMnjoX4mq5UPA0Tvi_k#all_confs_standings_E")

cast <- page %>%
  html_nodes("#fs-sidewall-right-container , .center , .left , .right") %>%
  html_text()
cast


easternteams <- page %>%
  html_nodes("#fs-sidewall-right-container , #confs_standings_E .left") %>%
  html_text()
easternteams
easternteams

easternW <- page %>%
  html_nodes("#fs-sidewall-right-container , #confs_standings_E .left+ .right , #confs_standings_E .left+ .center") %>%
  html_text()
easternW
easternW

easternL <- page %>%
  html_nodes("#fs-sidewall-right-container , #confs_standings_E .right:nth-child(3) , #confs_standings_E .center+ .center") %>%
  html_text()
easternL
easternL


easternWL <- page %>% 
  html_nodes("#fs-sidewall-right-container , #confs_standings_E .right:nth-child(4)") %>%
  html_text()
easternWL 
easternWL
length(easternWL)


easternGB <- page %>%
  html_nodes("#fs-sidewall-right-container , #confs_standings_E .right:nth-child(5)") %>%
  html_text()
easternGB
easternGB
length(easternGB)

easternPSG<-page %>%
  html_nodes("#confs_standings_E .right:nth-child(6)") %>%
  html_text()
easternPSG
easternPSG


easternPAG <- page %>%
  html_nodes("#confs_standings_E .right:nth-child(7)") %>%
  html_text()
easternPAG
easternPAG

easternSRS <- page %>%
  html_nodes("#confs_standings_E .right:nth-child(7)") %>%
  html_text()
easternSRS
easternSRS

nba_info <- data.frame(easternteams, easternW, easternL, easternGB, easternPSG, easternPAG, easternSRS)
length_adress <- length(nba_info)
westernNames <- page %>%
  html_nodes("#confs_standings_W .full_table .left") %>%
  html_text()
westernNames


westernW <- page %>%
  html_nodes("#confs_standings_W .left+ .right") %>%
  html_text()
westernW

westernL <- page %>%
  html_nodes("#confs_standings_W .right:nth-child(3)") %>%
  html_text()
westernL

westernGB <- page %>%
  html_nodes("#confs_standings_W .full_table .right:nth-child(4)")%>%
  html_text()
westernGB

westernPSG <- page %>%
  html_nodes("#confs_standings_W .full_table .right:nth-child(6)")%>%
  html_text()
westernPAG

westernPAG <- page %>%
  html_nodes("#confs_standings_W .full_table .right:nth-child(7)")%>%
  html_text()
westernPAG


westernSRS <- page %>%
  html_nodes("#confs_standings_W .right:nth-child(8)")%>%
  html_text()
westernPAG

nba_info <- data.frame(westernNames, westernW, westernL, westernGB, westernPSG, westernPAG, westernSRS)







































