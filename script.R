
#Testing branching -- REMOVE


# Clear environment
rm(list = ls())

# Load libraries
library(readr)
library(dplyr)
library(rvest)
library(ggplot2)
library(scales)

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
print(draft_results$Position)


# Group and overwrite offensive and defensive line positions to "OL" and "DL"
draft_results$Position[draft_results$Position == "C" | draft_results$Position == "G" | draft_results$Position == "T"] <- "OL"
draft_results$Position[draft_results$Position == "DT" | draft_results$Position == "DE" | draft_results$Position == "NT"] <- "DL" 
draft_results$Position[draft_results$Position == "ILB" | draft_results$Position == "OLB"| draft_results$Position == "LB"]<- "LB"
draft_results$Position[draft_results$Position == "DB" | draft_results$Position == "CB"| draft_results$Position == "S"]<- "DB"

# Group by player and postion, return df with total players drafted
df2 <- draft_results %>% group_by(Conference, Position) %>% summarize(Players_Drafted = n())

# Group by specific conference (ACC) and position, return with total players drafted; sorts by players drafted
ACC <- draft_results %>% group_by(Conference ="ACC", Position) %>% summarize(Players_Drafted = n())
ACC <- ACC[order(-ACC$Players_Drafted),]

# Group by specific conference (ACC) and position, return with total players drafted by year
ACC_Year <- draft_results %>% group_by(Conference = "ACC", Position, Year) %>% summarize(Players_Drafted = n())
ACC_Year2 <- ACC_Year
ACC_Year2 <- ACC_Year2[order(-ACC_Year2$Players_Drafted),]

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

number_players_drafted <- rbind(wrs_drafted,rbs_drafted,qbs_drafted,ols_drafted,dls_drafted,lbs_drafted,dbs_drafted)

ggplot(data = number_players_drafted, aes_(x=number_players_drafted$Position, y=number_players_drafted$Players_Drafted, group=number_players_drafted$Conference)) +
  geom_line(aes(color=number_players_drafted$Conference)) +
  geom_point(aes(color=number_players_drafted$Conference))


# Aaron: Just messing with these, not finished graphs

ggplot(data = number_players_drafted, aes_(x=number_players_drafted$Conference, y=number_players_drafted$Players_Drafted)) +
  geom_line(aes(color=number_players_drafted$Players_Drafted))

ggplot(data = number_players_drafted, aes_(x = number_players_drafted$Conference[number_players_drafted$Conference == "Big_Ten"], y=number_players_drafted$Players_Drafted, group=number_players_drafted$Position))+geom_line(aes(color=number_players_drafted$Conference))




# Cameron: Working on the below
qplot(Year, data = draft_results, geom = "bar", facets = . ~ Conference)
qplot(Position, data = draft_results, geom = "bar", facets = . ~ Conference)

# Grouped Bar Plot of Position Most Drafted by Year
counts <- table(ACC_Year$Position, ACC_Year$Year)
barplot(counts, main="Players Drafted by Position",
        xlab="Year Drafted", legend = rownames(counts))

# Stacked Bar Plot with Colors and Legend
plot <- table(ACC_Year2$Players_Drafted, ACC_Year$Position)
barplot(plot, main="Car Distribution by Gears and VS",
        xlab="Position", ylab="Players Drafted", col=c("darkgreen", "green", "lightgreen", "white"),
        legend = rownames(ACC_Year2$Position))


#dot plot
qplot(Year, Players_Drafted, data = ACC_Year, geom = "jitter", log = "y",
      facets = ~ Position)

#1
qplot(Players_Drafted, data = ACC, geom = "bar",
      facets = ~ Position)

p <- qplot(Year, data = ACC, geom = "bar", facets = . ~ Conference, fill =
             Conference)
p <- p + scale_fill_brewer(name = "Conference of Draftpick")
p

#2
ggplot(data = df2_Year, aes_(y=df2_Year$Players_Drafted, x=df2_Year$Year, group=df2_Year$Position)) +
  geom_line(aes(color=df2_Year$Position))


#3
qplot(Players_Drafted, Position,data = ACC_Year, geom = "bar")




       