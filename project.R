# Clear environment
rm(list = ls())

# Load libraries
library(readr)
library(dplyr)
library(rvest)
library(ggplot2)
library(scales)
library(tidyverse)
library(forcats)
library(RColorBrewer)
library(colorRamps)

# Dataframe containing conferences as column names and schools as the data, used
# to add the conferenceerence to the player, NA's added so all columns have equal rows

conferences_df <- data.frame(
  "Big_Ten" = c(
    "Iowa", "Ohio St.", "Wisconsin", "Penn St.", "Michigan",
    "Northwestern", "Michigan St.", "Maryland", "Purdue",
    "Minnesota", "Indiana", "Rutgers", "Illinois", "Nebraska"),
  "SEC" = c(
    "Alabama", "LSU", "Georgia", "Auburn", "Arkansas",
    "Mississippi St.", "Texas A&M", "Florida", "South Carolina",
    "Mississippi", "Missouri", "Kentucky", "Vanderbilt",
    "Tennessee"),
  "Pac_12" = c(
    "USC", "Oregon", "Stanford", "Washington", "Arizona", 
    "Utah", "Colorado", "Washington St.", "California",
    "Arizona St.", "UCLA", "Oregon St.", "NA", "NA"),
  "ACC"= c(
    "Clemson", "Miami (FL)", "Virginia Tech", "Boston Col.",
    "Florida St.", "Duke", "Pittsburgh", "Wake Forest",
    "North Carolina St.", "Syracuse", "North Carolina",
    "Virginia", "Georgia Tech", "Louisville"),
  "Big_12" = c(
    "Texas", "Oklahoma", "West Virginia", "TCU", "Oklahoma St.",
    "Iowa St.", "Kansas St.", "Texas Tech", "Baylor","Kansas",
    "NA", "NA", "NA", "NA"))

# Function to add the conferencerence the player went to school in to the draft results df
add_conferences <- function(c) { 
  draft_results$Conference[draft_results$College %in% conferences_df[[c]]] <<- c 
}

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
colnames(draft_results) <- c("Round", "Pick", "Team", "Player", "Position", 
                             "Age", "College", "Year")

# Add conferenceerence column to the draft results and set to other
draft_results$Conference <- "Other"

# A list of the conferenceerences to pass to the add_conference function
conferences <- list("SEC", "Big_Ten", "SEC", "Pac_12", "ACC", "Big_12")

# Call the add_conference function, using lapply to pass a list
lapply(conferences, add_conferences)

# Group and overwrite offensive and defensive line positions to "OL" and "DL"
draft_results$Position[draft_results$Position == "C" |
                         draft_results$Position == "G" |
                         draft_results$Position == "T"] <- "OL"
draft_results$Position[draft_results$Position == "DT" |
                         draft_results$Position == "DE" |
                         draft_results$Position == "NT"] <- "DL" 
draft_results$Position[draft_results$Position == "ILB" |
                         draft_results$Position == "OLB" |
                         draft_results$Position == "LB"] <- "LB"
draft_results$Position[draft_results$Position == "DB" |
                         draft_results$Position == "CB" |
                         draft_results$Position == "S"] <- "DB"

# Group by player and postion, return df with total players drafted
df2 <- draft_results %>% 
  group_by(Conference, Position) %>%
  summarize(Players_Drafted = n())

# Group by specific conference (ACC) and position, return with total players drafted; sorts by players drafted
ACC <- draft_results %>% 
  group_by(Conference ="ACC", Position) %>%
  summarize(Players_Drafted = n())
ACC <- ACC[order(-ACC$Players_Drafted),]

# Group by specific conference (ACC) and position, return with total players drafted by year
ACC_Year <- draft_results %>%
  group_by(Conference = "ACC", Position, Year) %>%
  summarize(Players_Drafted = n())

# Group by specific conference (ACC) and position, return with total players drafted by year
Overall <- draft_results %>%
  group_by(Conference = "ACC", Position, Year) %>% 
  summarize(Players_Drafted = n())

# Function to return the ranking in descending order of players draft at a postion by conference
most_drafted_at_position <- function(x) {
  df2 %>% filter(Position == x) %>% arrange(desc(Players_Drafted)) 
}

# Run most_drafted_at_postion function on the positions of interest
wrs_drafted <- most_drafted_at_position("WR")
rbs_drafted <- most_drafted_at_position("RB")
qbs_drafted <- most_drafted_at_position("QB")
ols_drafted <- most_drafted_at_position("OL")
dls_drafted <- most_drafted_at_position("DL")
lbs_drafted <- most_drafted_at_position("LB")
dbs_drafted <- most_drafted_at_position("DB")

number_players_drafted <- rbind(wrs_drafted, rbs_drafted, qbs_drafted, 
                                ols_drafted,dls_drafted,lbs_drafted,dbs_drafted)

draft_results <- na.omit(draft_results)

#### Graphs


####### KEEP THIS IS YOU USED IT Creating a standard theme for the project to avoid reformating each graph
#theme_project <- function() {
#  theme_minimal() %+replace%
#    theme(
#      axis.text.x = element_text(size = 13, face="bold"), 
#      plot.title=element_text(size=18, face="bold", hjust = 0.5)
#    )
#}

# Group by specific conference and position, return with total players drafted by year
Big_12_Year <- draft_results %>% 
  filter(Conference == "Big_12") %>% 
  group_by(Conference, Position, Year) %>% 
  summarize(Players_Drafted = n())

SEC_Year <- draft_results %>% 
  filter(Conference == "SEC") %>% 
  group_by(Conference, Position, Year) %>% 
  summarize(Players_Drafted = n())

max_big12 <- max(Big_12_Year$Players_Drafted)
max_sec <- max(SEC_Year$Players_Drafted)

# Big 12 graphs
#1.
p <- ggplot(data = Big_12_Year, 
      aes_(x=Big_12_Year$Year, y=Big_12_Year$Players_Drafted, group=Big_12_Year$Position)) +
      geom_line(aes(color=Big_12_Year$Position)) + 
      geom_point(aes(color=Big_12_Year$Position))
p <- p + theme_minimal() + labs(color = "Positions")
p <- p + theme(axis.text.x = element_text(size = 11), 
               axis.text.y = element_text(size = 11, face="bold"), 
               title=element_text(size=12,face="bold"))
p <- p + labs(x = "", y = "", title = "Number of Players Drafted by Position in Big 12")
p <- p + theme(plot.title = element_text(hjust = 0.5))
p <- p + scale_y_continuous(limits=c(0, max_big12), breaks = (0:max_big12))
p
ggsave('Change_in_Position_by_Year_in_Big_12.png')

#2.
big_12 <- draft_results %>%
  filter(Conference == 'Big_12')

p <- big_12 %>% count(Position) %>%
  mutate(Position = fct_reorder(Position, n, .desc = FALSE)) %>%
  ggplot(aes(x = Position, y = n)) + 
  geom_bar(stat = 'identity', 
    aes(fill = Position), show.legend = FALSE)
p <- p + theme_minimal() + 
  theme(axis.text.x = element_text(size = 13, face="bold"), 
        axis.text.y = element_text(size = 13, face="bold"), 
        title=element_text(size=18,face="bold"))
p <- p + scale_fill_viridis_d(direction = -1) + coord_flip()
p <- p + labs(title = "Positions Most Drafted in Big 12", x = "", y = "")
p <- p + theme(plot.title = element_text(hjust = 0.5))
p
ggsave('big_12_players_by_position.png')

#3.
p <- big_12 %>% count(College) %>%
  mutate(College = fct_reorder(College, n, .desc = FALSE)) %>% top_n(5) %>% 
  ggplot(aes(x = College, y = n)) + 
  geom_bar(stat = 'identity', 
           aes(fill = College), show.legend = FALSE)
p <- p + theme_minimal() + 
  theme(axis.text.x = element_text(size = 13, face="bold"), 
        axis.text.y = element_text(size = 13, face="bold"), title=element_text(size=16,face="bold"))
p <- p + scale_fill_viridis_d(direction = -1) + coord_flip()
p <- p + labs(title = "Most Drafted Colleges in Big 12", x = "", y = "")
p <- p + theme(plot.title = element_text(hjust = 0.5))
p
ggsave('big_12_players_Drafted_by_College_Attended.png')

#4.
big_12 <- draft_results %>% 
  filter(Conference == 'Big_12')

p <- big_12 %>% count(Team) %>%
  mutate(Team = fct_reorder(Team, n, .desc = FALSE)) %>% top_n(5) %>% 
  ggplot(aes(x = Team, y = n)) + 
  geom_bar(stat = 'identity', aes(fill = Team), show.legend = FALSE)
p <- p + theme_minimal() + 
  theme(axis.text.x = element_text(size = 12, face="bold"),
        axis.text.y = element_text(size = 12, face="bold"), 
        title=element_text(size=14,face="bold"))
p <- p + scale_fill_viridis_d(direction = -1) + coord_flip()
p <- p + labs(title = "Teams Most Likely to Draft an Big 12 Player", x = "", y = "")
p <- p + theme(plot.title = element_text(hjust = 0.5))
p
ggsave('big_12_players_by_Teams.png')

# SEC graphs
#1.
p <- ggplot(data = SEC_Year, 
      aes_(x=SEC_Year$Year, y=SEC_Year$Players_Drafted, group=SEC_Year$Position)) +
      geom_line(aes(color=SEC_Year$Position)) + 
      geom_point(aes(color=SEC_Year$Position))
p <- p + theme_minimal() + labs(color = "Positions")
p <- p + theme(axis.text.x = element_text(size = 11), 
               axis.text.y = element_text(size = 11, face="bold"), 
               title=element_text(size=12,face="bold"))
p <- p + labs(x = "", y = "", title = "Number of Players Drafted by Position in SEC")
p <- p + theme(plot.title = element_text(hjust = 0.5))
p <- p + scale_y_continuous(limits=c(0, max_sec), breaks = (0:max_sec))
p
ggsave('Change_in_Position_by_Year_in_SEC.png')

#2. 
sec <- draft_results %>% 
  filter(Conference == 'SEC')

p <- sec %>% count(Position) %>%
  mutate(Position = fct_reorder(Position, n, .desc = FALSE)) %>%
  ggplot(aes(x = Position, y = n)) + 
  geom_bar(stat = 'identity', 
           aes(fill = Position), show.legend = FALSE)
p <- p + theme_minimal() + 
  theme(axis.text.x = element_text(size = 13, face="bold"), 
        axis.text.y = element_text(size = 13, face="bold"), 
        title=element_text(size=18,face="bold"))
p <- p + scale_fill_viridis_d(direction = -1) + coord_flip()
p <- p + labs(title = "Positions Most Drafted in SEC", x = "", y = "")
p <- p + theme(plot.title = element_text(hjust = 0.5))
p
ggsave('sec_players_by_position.png')


#3.
p <- sec %>% count(College) %>%
  mutate(College = fct_reorder(College, n, .desc = FALSE)) %>% top_n(5) %>% 
  ggplot(aes(x = College, y = n)) + 
  geom_bar(stat = 'identity', aes(fill = College), show.legend = FALSE)
p <- p + theme_minimal() + 
  theme(axis.text.x = element_text(size = 13, face="bold"), 
        axis.text.y = element_text(size = 13, face="bold"), 
        title=element_text(size=16,face="bold"))
p <- p + scale_fill_viridis_d(direction = -1) + coord_flip()
p <- p + labs(title = "Most Drafted Colleges in SEC", x = "", y = "")
p <- p + theme(plot.title = element_text(hjust = 0.5))
p
ggsave('sec_players_Drafted_by_College_Attended.png')

#4.
sec <- draft_results %>% 
  filter(Conference == 'SEC')
p <- sec %>% count(Team) %>%
  mutate(Team = fct_reorder(Team, n, .desc = FALSE)) %>% top_n(5) %>% 
  ggplot(aes(x = Team, y = n)) + 
  geom_bar(stat = 'identity', aes(fill = Team), show.legend = FALSE)
p <- p + theme_minimal() + 
  theme(axis.text.x = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12, face="bold"), 
        title=element_text(size=14,face="bold"))
p <- p + scale_fill_viridis_d(direction = -1) + coord_flip()
p <- p + labs(title = "Teams Most Likely to Draft an SEC Player", x = "", y = "")
p <- p + theme(plot.title = element_text(hjust = 0.5))
p
ggsave('sec_players_by_Teams.png')

draft_results <- subset(draft_results, Conference != "Other")

#1: Players Drafted by Year; showing all conferences.
c <- qplot(Year, data = draft_results, geom = "bar", facets = . ~ Conference, 
           color = Conference, fill = Conference)
c <- c + theme_minimal() + 
  theme(axis.text.x = element_text(size = 6), 
        axis.text.y = element_text(size = 10, face="bold"), 
        title=element_text(size=10,face="bold"))
c <- c + labs(x = "", title = "Number of Players Drafted by Year")
c <- c + theme(plot.title = element_text(hjust = 0.5))
c
ggsave('Players_Drafted_by_Year.png')

#2: Change in data over year by position. Will be on same slide as plot #1.
e <- ggplot(data = Overall, 
            aes_(x=Overall$Year, y=Overall$Players_Drafted, group=Overall$Position)) + 
     geom_line(aes(color=Overall$Position)) + geom_point(aes(color=Overall$Position))
e <- e + theme_minimal() + labs(color = "Positions")
e <- e + theme(axis.text.x = element_text(size = 11),
               axis.text.y = element_text(size = 11, face="bold"),
               title=element_text(size=12,face="bold"))
e <- e + labs(x = "", y = "", title = "Number of Players Drafted by Position")
e <- e + theme(plot.title = element_text(hjust = 0.5))
e <- e + scale_y_continuous(limits=c(0, 50))
e
ggsave('Change_in_Position_by_Year_Overall.png')

#3: Change in data over year by position. Will be on same slide as plot #1.
d <- ggplot(data = ACC_Year,
            aes_(x=ACC_Year$Year, y=ACC_Year$Players_Drafted, group=ACC_Year$Position)) +
     geom_line(aes(color=ACC_Year$Position)) +
     geom_point(aes(color=ACC_Year$Position))
d <- d + theme_minimal() + labs(color = "Positions")
d <- d + theme(axis.text.x = element_text(size = 11),
               axis.text.y = element_text(size = 11, face="bold"),
               title=element_text(size=12,face="bold"))
d <- d + labs(x = "", y = "", title = "Number of Players Drafted by Position in ACC")
d <- d + theme(plot.title = element_text(hjust = 0.5))
d <- d + scale_y_continuous(limits=c(0, 15))
d
ggsave('Change_in_Position_by_Year_in_ACC.png')

#4: Position Most Drafted in ACC
acc <- draft_results %>% 
  filter(Conference == 'ACC')

p <- acc %>% count(Position) %>%
  mutate(Position = fct_reorder(Position, n, .desc = FALSE)) %>%
  ggplot(aes(x = Position, y = n)) + 
  geom_bar(stat = 'identity', aes(fill = Position), show.legend = FALSE)
p <- p + theme_minimal() + 
  theme(axis.text.x = element_text(size = 13, face="bold"),
        axis.text.y = element_text(size = 13, face="bold"),
        title=element_text(size=18,face="bold"))
p <- p + scale_fill_viridis_d(direction = -1) + coord_flip()
p <- p + labs(title = "Positions Most Drafted in ACC", x = "", y = "")
p <- p + theme(plot.title = element_text(hjust = 0.5))
p
ggsave('ACC_players_by_position.png')

#5: College Most Drafted in ACC
p1 <- acc %>% count(College) %>%
  mutate(College = fct_reorder(College, n, .desc = FALSE)) %>% top_n(5) %>% 
  ggplot(aes(x = College, y = n)) + 
  geom_bar(stat = 'identity', aes(fill = College), show.legend = FALSE)
p1 <- p1 + theme_minimal() + 
  theme(axis.text.x = element_text(size = 13, face="bold"),
        axis.text.y = element_text(size = 13, face="bold"),
        title=element_text(size=16,face="bold"))
p1 <- p1 + scale_fill_viridis_d(direction = -1) + coord_flip()
p1 <- p1 + labs(title = "Most Drafted Colleges in ACC", x = "", y = "")
p1 <- p1 + theme(plot.title = element_text(hjust = 0.5))
p1
ggsave('ACC_players_Drafted_by_College_Attended.png')

#6: Teams to most likely draft Players out of ACC
acc <- draft_results %>% filter(Conference == 'ACC')

p2 <- acc %>% count(Team) %>%
  mutate(Team = fct_reorder(Team, n, .desc = FALSE)) %>% top_n(5) %>% 
  ggplot(aes(x = Team, y = n)) + 
  geom_bar(stat = 'identity', aes(fill = Team), show.legend = FALSE)
p2 <- p2 + theme_minimal() + 
  theme(axis.text.x = element_text(size = 12, face="bold"),
        axis.text.y = element_text(size = 12, face="bold"),
        title=element_text(size=14,face="bold"))
p2 <- p2 + scale_fill_viridis_d(direction = -1) + coord_flip()
p2 <- p2 + labs(title = "Teams Most Likely to Draft an ACC Player", x = "", y = "")
p2 <- p2 + theme(plot.title = element_text(hjust = 0.5))
p2
ggsave('ACC_players_by_Teams.png')

# Aaron: Just messing with these, not finished graphs
#
#ggplot(data = number_players_drafted, 
#       aes_(x=number_players_drafted$Conference,
#            y=number_players_drafted$Players_Drafted)) +
#  geom_line(aes(color=number_players_drafted$Players_Drafted))
#
#ggplot(data = number_players_drafted,
#      aes_(x = number_players_drafted$Conference
#            [number_players_drafted$Conference == "Big_Ten"],
#            y=number_players_drafted$Players_Drafted,
#            group=number_players_drafted$Position)) +
#  geom_line(aes(color=number_players_drafted$Conference))
#
#ggplot(data = number_players_drafted,
#       aes_(x = subset(number_players_drafted, Conference == "Big_Ten"), 
#            y = number_players_drafted$Players_Drafted,
#            group = number_players_drafted$Position))
#              
#              number_players_drafted$Conference
#            [number_players_drafted$Conference == "Big_Ten"],
#            y=number_players_drafted$Players_Drafted,
#            group=number_players_drafted$Position)) +
#  geom_line(aes(color=number_players_drafted$Conference))
