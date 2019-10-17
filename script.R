# Load libraries
library(readr)
library(dplyr)
library(rvest)

# Dataframe containing conferences as column names and schools as the data, used to add the conferenceerence to the player, 
# NA's added so all columns have equal rows
conferences_df <- data.frame("Big_Ten" = c("Iowa", "Ohio St.", "Wisconsin", "Penn St.", "Michigan", "Northwestern", "Michigan St.", "Maryland",
                                       "Purdue", "Minnesota", "Indiana", "Rutgers", "Illinois", "Nebraska"),
                         "SEC" = c("Alabama", "LSU", "Georgia", "Auburn", "Arkansas", "Mississippi St.", "Texas A&M", "Florida", "South Carolina",
                                   "Mississippi", "Missouri", "Kentucky", "Vanderbilt", "Tennessee"),
                         "Pac_12" = c("USC", "Oregon", "Stanford", "Washington", "Arizona", "Utah", "Colorado", "Washington St.", "California",
                                      "Arizona St.", "UCLA", "Oregon St.", "NA", "NA"),
                         "ACC"= c("Clemson", "Miami (FL)", "Virginia Tech", "Boston Col.", "Florida St.", "Duke", "Pittsburgh", "Wake Forest",
                                  "North Carolina St.", "Syracuse", "North Carolina", "Virginia", "Georgia Tech", "Louisville"),
                         "Big_12" = c("Texas", "Oklahoma", "West Virginia", "TCU", "Oklahoma St.", "Iowa St.", "Kansas St.", "Texas Tech", "Baylor",
                                      "Kansas", "NA", "NA", "NA", "NA"))

# Function to add the conferencerence the player went to school in to the draft results df
add_conferences <- function(c) { draft_results$Conference[draft_results$College %in% conferences_df[[c]]] <<- c }

# Empty list to hold each years data frame
draft_results <- list()

# Loop that creates a url for each year, gets the table and converts to a dataframe,
# then selects the columns of interest and adds the dataframe to the list
for(i in 2014:2018) {
  url <- paste("https://www.pro-football-reference.com/years/", i, "/draft.htm", sep = "")
  pg <- read_html(url)
  tb <- html_table(pg, fill = TRUE)
  df <- data.frame(tb[[1]])
  df$year <- i
  df <- df[c(1:6, 28, 30)]
  df <- filter(df, Var.1 != "Rnd")
  draft_results[[length(draft_results)+1]] <- df
}

# Convert the list of dataframes to a single dataframe
draft_results <- do.call(rbind, draft_results)

# Rename the columns
colnames(draft_results) <- c("Round", "Pick", "Team", "Player", "Position", "Age", "College", "Year")

# Add conferenceerence column to the draft results and set to other
draft_results$Conference <- "Other"

# A list of the conferenceerences to pass to the add_conference function
conferences <- list("SEC", "Big_Ten", "SEC", "Pac_12", "ACC", "Big_12")

# Call the add_conference function, using lapply to pass a list
lapply(conferences, add_conferences)

# Group and overwrite offensive and defensive line positions to "OL" and "DL"
draft_results$Position[draft_results$Position == "C" | draft_results$Position == "G" | draft_results$Position == "T"] <- "OL"
draft_results$Position[draft_results$Position == "DT" | draft_results$Position == "DE"] <- "DL"

# Group by player and postion, return df with total players drafted
df2 <- draft_results %>% group_by(Conference, Position) %>% summarize(Players_Drafted = n())

# Function to return the ranking in descending order of players draft at a postion by conference
most_drafted_at_position <- function(x) { df2 %>% filter(Position == x) %>% arrange(desc(Players_Drafted)) }

# Run most_drafted_at_postion function on the positions of interest
wrs_drafted <- most_drafted_at_position("WR")
rbs_drafted <- most_drafted_at_position("RB")
qbs_drafted <- most_drafted_at_position("QB")
ols_drafted <- most_drafted_at_position("OL")
dls_drafted <- most_drafted_at_position("DL")
lbs_drafted <- most_drafted_at_position("LB")
dbs_drafted <- most_drafted_at_position("DB")
