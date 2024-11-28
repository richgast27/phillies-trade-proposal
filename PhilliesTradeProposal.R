##### #clear the working directory
rm(list=ls())

##### Install packages and libraries
library(tidyr)
library(dplyr)
library(readxl)





## Import arbitration data, contract data for Outfielders, and last 3 years of stats for Outfielders
mlb_arbitration <- read.csv("mlb-arbitration.csv")

mlb_contracts <- read.csv("mlb-contracts.csv")

outfielders_last3 <- read.csv("Outfielders_Last3.csv")



## Import 2024 stats for Phillies position players
PHI_player_stats <- read.csv("PHI_player_stats.csv")


## Import 2024 defensive stats for Phillies 
PHI_defensive_stats <- read.csv("PHI_defensive_stats.csv")



## Import positional WAR rankings for last 3 years 
positional_WAR2022 <- read_excel("Positional_WAR2022.xlsx")

positional_WAR2023 <- read_excel("Phillies/Positional_WAR2023.xlsx")

positional_WAR2024 <- read_excel("Phillies/Positional_WAR2024.xlsx")





############### Determine Team Need ###############
############### Determine Team Need ###############
############### Determine Team Need ###############






## Define the years and create an empty list to store the data frames
years <- c(2022, 2023, 2024)

rankings_list <- list()


## Set search parameters for Philadelphia, pattern needed due to data format
pattern <- "PHI[0-9]+"


## Loop through each of year to capture WAR rankings 
for (year in years) {
  
  
  ## Access the dataframe for each year
  positional_WAR <- get(paste0("positional_WAR", year))
  
  
  ## Search each column for the pattern and capture position (WAR rankings)
  positions <- lapply(positional_WAR, function(col) which(grepl(pattern, col)))
  
  
  ## Convert the list of positions to a data frame
  positional_rankings <- do.call(rbind, lapply(positions, function(x) {
    
    max_length <- max(sapply(positions, length))
    
    if (length(x) < max_length) {
      
      x <- c(x, rep(NA, max_length - length(x)))
      
    }
    
    return(x)
    
  }
  ))
  
  
  ## Transpose and convert to data frame
  positional_rankings <- as.data.frame(t(positional_rankings))
  
  ## Remove the first two columns and add a new one for the year
  positional_rankings <- positional_rankings[, -c(1:2)]
  
  positional_rankings <- cbind(Year = as.character(year), positional_rankings)
  
  ## Store the processed data frame in the list
  rankings_list[[as.character(year)]] <- positional_rankings
  
}

## Combine all years' data frames into one and prevent row names from being set
PHI_positional_rankings <- do.call(rbind, rankings_list)

row.names(PHI_positional_rankings) <- NULL 



## Calculate average ranking over last 3 years and add it as new row in dataframe
average_row <- round(colMeans(PHI_positional_rankings[, -1], na.rm = TRUE),1)


average_row_df <- data.frame(t(average_row))


average_row_df <- cbind(Player = "Average", average_row_df)


colnames(average_row_df) <- names(PHI_positional_rankings)


PHI_positional_rankings <- rbind(PHI_positional_rankings, average_row_df)





### Evaluate position players defensively 
### Evaluate position players defensively 
### Evaluate position players defensively 




## Remove pitchers and cleanup columns 
PHI_position_player_defense <- PHI_defensive_stats[!PHI_defensive_stats$Pos == "P", ]

PHI_position_player_defense <- PHI_position_player_defense[,-c(2,8:10)]



## Look for missing values and cleanup
sum(is.na(PHI_defensive_stats))

PHI_position_player_defense <- PHI_position_player_defense[complete.cases(PHI_position_player_defense), ]




## List of metrics for loop
metrics <- c("DRS", "UZR", "Def", "OAA")


## Loop over each metric
for (metric in metrics) {
  
  ## Sort the data by the current metric in ascending order
  PHI_position_player_defense <- PHI_position_player_defense[order(PHI_position_player_defense[[metric]]), ]
  
  ## Capture the bottom 10 players and create a new data frame
  bottom10_df <- head(PHI_position_player_defense, 10)
  
  assign(paste0("bottom10_", metric), bottom10_df)
  
  ## Count how many players in the bottom 10 are outfielders
  outfield_count <- sum(bottom10_df$Pos %in% c("LF", "CF", "RF"))
  
  
  ## Print the count for each metric 
  print(paste0(outfield_count, " out of bottom 10 in ", metric, " are Outfielders"))
  
}





############### Determine Potential Trade Options ###############
############### Determine Potential Trade Options ###############
############### Determine Potential Trade Options ###############




## Cleanup variable names 
names(mlb_arbitration)[1] <- "Player"

names(mlb_contracts)[4] <- "Team"

names(mlb_contracts)[5] <- "AgeAtSigning"

names(outfielders_last3)[2] <- "Player"



## Isolate players who are in their final two years of arbitration or going into a contract year
last2_arb_years <- subset(mlb_arbitration, mlb_arbitration$YOS >= "4")



walk_year <- subset(mlb_contracts, !(mlb_contracts$Start == "2025" & mlb_contracts$End == "2025"))

walk_year <- subset(walk_year, walk_year$End == "2025")




## Create subset of outfielders in their final two years of arbitration and cleanup unwanted columns
arbitration_outfielders <- last2_arb_years[which(last2_arb_years$Pos %in% c("LF","CF","RF")),]

arbitration_outfielders <- arbitration_outfielders[ , c(1:3)]



## Create subset of outfielders in their final two years of arbitration and cleanup columns
walkYear_outfielders <- walk_year[which(walk_year$Pos %in% c("LF","CF","RF")),]

walkYear_outfielders <- walkYear_outfielders[ , c(2:4)]

walkYear_outfielders$Team <- sub(" .*", "", walkYear_outfielders$Team)



## Combine arbitration and walk year players to create list potential OF trade options 
OF_trade_options <- rbind(arbitration_outfielders,walkYear_outfielders)



## Create dataframe of the last 3 years of stats for outfielder trade options 
trade_options_last3 <- outfielders_last3[outfielders_last3$Player %in% c(OF_trade_options$Player), ]




## Get average Def over the last 3 seasons for potential trade options and add column names
last3_AvgDef <- aggregate(x= trade_options_last3$Def, by = list(trade_options_last3$Player),      
                          FUN = mean)

names(last3_AvgDef)[1:2] <- c("Player", "AvgDef")


## Sort players from highest average Def to lowest over last 3 years
##
## Based on the results of the sort, we will further evaluate potential trades 
## for Tommmy Edman and Trent Grisham. 
##
## Daulton Varsho seems unlikely given he just won the gold glove and was second 
## on the Blue Jays in WAR
def_sort <- last3_AvgDef[order(-last3_AvgDef$AvgDef), ]








############### Determine Potential Trade Team Need ###############
############### Determine Potential Trade Team Need ###############
############### Determine Potential Trade Team Need ###############






### Evaluate Yankees Needs 
### Evaluate Yankees Needs 
### Evaluate Yankees Needs 


## Define the years and create an empty list to store the data frames
years <- c(2022, 2023, 2024)

rankings_list <- list()


## Set search parameters for Philadelphia, pattern needed due to data format
pattern <- "PHI[0-9]+"


## Loop through each of year to capture WAR rankings 
for (year in years) {
  
  
  ## Access the dataframe for each year
  positional_WAR <- get(paste0("positional_WAR", year))
  
  
  ## Search each column for the pattern and capture position (WAR rankings)
  positions <- lapply(positional_WAR, function(col) which(grepl(pattern, col)))
  
  
  ## Convert the list of positions to a data frame
  positional_rankings <- do.call(rbind, lapply(positions, function(x) {
    
    max_length <- max(sapply(positions, length))
    
    if (length(x) < max_length) {
      
      x <- c(x, rep(NA, max_length - length(x)))
      
    }
    
    return(x)
    
  }
  ))
  
  
  ## Transpose and convert to data frame
  positional_rankings <- as.data.frame(t(positional_rankings))
  
  ## Remove the first two columns and add a new one for the year
  positional_rankings <- positional_rankings[, -c(1:2)]
  
  positional_rankings <- cbind(Year = as.character(year), positional_rankings)
  
  ## Store the processed data frame in the list
  rankings_list[[as.character(year)]] <- positional_rankings
  
}

## Combine all years' data frames into one and prevent row names from being set
PHI_positional_rankings <- do.call(rbind, rankings_list)

row.names(PHI_positional_rankings) <- NULL 



## Calculate average ranking over last 3 years and add it as new row in dataframe
average_row <- round(colMeans(PHI_positional_rankings[, -1], na.rm = TRUE),1)


average_row_df <- data.frame(t(average_row))


average_row_df <- cbind(Player = "Average", average_row_df)


colnames(average_row_df) <- names(PHI_positional_rankings)


PHI_positional_rankings <- rbind(PHI_positional_rankings, average_row_df)


print(PHI_positional_rankings)




### Evaluate Yankees Needs 
### Evaluate Yankees Needs 
### Evaluate Yankees Needs 


## Define the years and create an empty list to store the data frames
years <- c(2022, 2023, 2024)

rankings_list <- list()


## Set search parameters for Philadelphia, pattern needed due to data format
pattern <- "NYY[0-9]+"


## Loop through each of year to capture WAR rankings 
for (year in years) {
  
  
  ## Access the dataframe for each year
  positional_WAR <- get(paste0("positional_WAR", year))
  
  
  ## Search each column for the pattern and capture position (WAR rankings)
  positions <- lapply(positional_WAR, function(col) which(grepl(pattern, col)))
  
  
  ## Convert the list of positions to a data frame
  positional_rankings <- do.call(rbind, lapply(positions, function(x) {
    
    max_length <- max(sapply(positions, length))
    
    if (length(x) < max_length) {
      
      x <- c(x, rep(NA, max_length - length(x)))
      
    }
    
    return(x)
    
  }
  ))
  
  
  ## Transpose and convert to data frame
  positional_rankings <- as.data.frame(t(positional_rankings))
  
  ## Remove the first two columns and add a new one for the year
  positional_rankings <- positional_rankings[, -c(1:2)]
  
  positional_rankings <- cbind(Year = as.character(year), positional_rankings)
  
  ## Store the processed data frame in the list
  rankings_list[[as.character(year)]] <- positional_rankings
  
}

## Combine all years' data frames into one and prevent row names from being set
NYY_positional_rankings <- do.call(rbind, rankings_list)

row.names(NYY_positional_rankings) <- NULL 



## Calculate average ranking over last 3 years and add it as new row in dataframe
average_row <- round(colMeans(NYY_positional_rankings[, -1], na.rm = TRUE),1)


average_row_df <- data.frame(t(average_row))


average_row_df <- cbind(Player = "Average", average_row_df)


colnames(average_row_df) <- names(NYY_positional_rankings)


NYY_positional_rankings <- rbind(NYY_positional_rankings, average_row_df)


print(NYY_positional_rankings)






### Evaluate Dodgers Needs 
### Evaluate Dodgers Needs 
### Evaluate Dodgers Needs 


## Define the years and create an empty list to store the data frames
years <- c(2022, 2023, 2024)

rankings_list <- list()


## Set search parameters for Philadelphia, pattern needed due to data format
pattern <- "LAD[0-9]+"


## Loop through each of year to capture WAR rankings 
for (year in years) {
  
  
  ## Access the dataframe for each year
  positional_WAR <- get(paste0("positional_WAR", year))
  
  
  ## Search each column for the pattern and capture position (WAR rankings)
  positions <- lapply(positional_WAR, function(col) which(grepl(pattern, col)))
  
  
  ## Convert the list of positions to a data frame
  positional_rankings <- do.call(rbind, lapply(positions, function(x) {
    
    max_length <- max(sapply(positions, length))
    
    if (length(x) < max_length) {
      
      x <- c(x, rep(NA, max_length - length(x)))
      
    }
    
    return(x)
    
  }
  ))
  
  
  ## Transpose and convert to data frame
  positional_rankings <- as.data.frame(t(positional_rankings))
  
  ## Remove the first two columns and add a new one for the year
  positional_rankings <- positional_rankings[, -c(1:2)]
  
  positional_rankings <- cbind(Year = as.character(year), positional_rankings)
  
  ## Store the processed data frame in the list
  rankings_list[[as.character(year)]] <- positional_rankings
  
}

## Combine all years' data frames into one and prevent row names from being set
LAD_positional_rankings <- do.call(rbind, rankings_list)

row.names(LAD_positional_rankings) <- NULL 



## Calculate average ranking over last 3 years and add it as new row in dataframe
average_row <- round(colMeans(LAD_positional_rankings[, -1], na.rm = TRUE),1)


average_row_df <- data.frame(t(average_row))


average_row_df <- cbind(Player = "Average", average_row_df)


colnames(average_row_df) <- names(LAD_positional_rankings)


LAD_positional_rankings <- rbind(LAD_positional_rankings, average_row_df)


print(LAD_positional_rankings)





