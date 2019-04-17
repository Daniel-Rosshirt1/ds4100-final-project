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
df_pullup <- lapply("http://stats.nba.com/js/data/sportvu/pullUpShootData.js", readIt)
str(df_drives)

web_page <- readLines("http://stats.nba.com/js/data/sportvu/pullUpShootData.js")
x1 <- gsub("[\\{\\}\\]]", "", web_page, perl=TRUE)
x2 <- gsub("[\\[]", "\n", x1, perl=TRUE)
x3 <- gsub("\"rowSet\":\n", "", x2, perl=TRUE)
x4 <- gsub(";", ",",x3, perl=TRUE)
nba<-read.table(textConnection(x4), header=T, sep=",", skip=2, stringsAsFactors=FALSE)
nba


group_by(df_pullup, TEAM_ABBREVIATION)


################################web scrapping##############################################
library(rvest)
url2015 = "https://www.basketball-reference.com/leagues/NBA_2015_standings.html"
url2014 = "https://www.basketball-reference.com/leagues/NBA_2014_standings.html"
url2016 = "https://www.basketball-reference.com/leagues/NBA_2016_standings.html"
url2017 = "https://www.basketball-reference.com/leagues/NBA_2017_standings.html"
url2018 = "https://www.basketball-reference.com/leagues/NBA_2018_standings.html"
url2019 = "https://www.basketball-reference.com/leagues/NBA_2019_standings.html"

scrapping_standings <- function(url) {
  page=read_html(url)
  easternteams <- page %>%
    html_nodes(".full_table .left") %>%
    html_text()
  easternteams

  easternW <- page %>%
    html_nodes(".right:nth-child(2)") %>%
    html_text()
  length(easternW)

  easternL <- page %>%
    html_nodes(".right:nth-child(3)") %>%
    html_text()
  length(easternL)

  
  easternWL <- page %>% 
    html_nodes(".full_table .right:nth-child(4)") %>%
    html_text()
  length(easternWL)
  
  easternGB <- page %>%
    html_nodes(".full_table .right:nth-child(5)") %>%
    html_text()
  
  length(easternGB)

  easternPSG<-page %>%
    html_nodes(".full_table .right:nth-child(6)") %>%
    html_text()
  length(easternPSG)
  
  easternPAG <- page %>%
    html_nodes(".full_table .right:nth-child(7)") %>%
    html_text()
  length(easternPAG)

  easternSRS <- page %>%
    html_nodes(".right:nth-child(8)") %>%
    html_text()
  length(easternSRS)

  nba_info_e <- data.frame(easternteams, easternW, easternL, easternWL, easternGB, easternPSG, easternPAG, easternSRS)
  return(nba_info_e)
}

df2014 <- scrapping_standings(url2014)
str(df2014)

# converting into numeric
df2014$easternW <- as.numeric(df2014$easternW)
df2014$easternPAG <- as.numeric(df2014$easternPAG)
df2014$easternPSG <- as.numeric(df2014$easternPSG)
# ranking by columns
rankedByW <- df2014 %>% arrange(desc(easternW))
rankedByPAG <- df2014 %>% arrange(easternPAG)

# Defense Team Analysis & dividing into tiers
rankedByPAGTier5 <- rankedByPAG[1:6,]
rankedByPAGTier4 <- rankedByPAG[7:12,]
rankedByPAGTier3 <- rankedByPAG[13:18,]
rankedByPAGTier2 <- rankedByPAG[19:24,]
rankedByPAGTier1 <- rankedByPAG[25:30,]
rankedByPAGTier4

# Offense Team Analysis & dividing into tiers
rankedByPSG <- df2014 %>% arrange(desc(easternPSG)) # points per game
rankedByPSGTier5 <- rankedByPSG[1:6,]
rankedByPSGTier4 <- rankedByPSG[7:12,]
rankedByPSGTier3 <- rankedByPSG[13:18,]
rankedByPSGTier2 <- rankedByPSG[19:24,]
rankedByPSGTier1 <- rankedByPSG[25:30,]
rankedByPSGTier5
str(df2014)




# ggplot2 plotting
library(ggplot2)


ggplot(nba_info_w, aes(x=westernNames, y=westernW, fill=westernNames)) +
  geom_bar(stat="identity")+theme_minimal()

ggplot(nba_info_e, aes(x=easternteams, y=easternW, fill=easternteams)) +
  geom_bar(stat="identity")+theme_minimal()


ggplot(nba_info_w, aes(x=westernNames, y=westernPAG, fill=westernNames)) +
  geom_bar(stat="identity")+theme_minimal()

ggplot(nba_info_e, aes(x=easternteams, y=easternPAG, fill=easternteams)) +
  geom_bar(stat="identity")+theme_minimal()


ggplot(nba_info_w, aes(x=westernNames, y=westernPSG, fill=westernNames)) +
  geom_bar(stat="identity")+theme_minimal()

ggplot(nba_info_e, aes(x=easternteams, y=easternPSG, fill=easternteams)) +
  geom_bar(stat="identity")+theme_minimal()

ggplot(nba_info_w, aes(x=westernNames, y=westernSRS, fill=westernNames)) +
  geom_bar(stat="identity")+theme_minimal()

ggplot(nba_info_e, aes(x=easternteams, y=easternSRS, fill=easternteams)) +
  geom_bar(stat="identity")+theme_minimal()


