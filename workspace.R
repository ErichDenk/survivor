# Script for Iterative Building of the Shiny App
# Erich 05.01.2022

rm(list = ls())

# Packages, Themes, and Functions -------
library(shiny)
library(tidyverse)
library(tidyselect)
library(markdown)
library(forcats)

mytheme <- function(){theme(axis.text = element_text(size = 10),
      plot.title = element_text(face = "bold"),
      panel.background = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_line(linetype = "dotted",
                                        size = 0.5, color = "gray"),
      axis.line.x = element_line(linetype = "solid"))}

# Merge Cutoff and current week (These can be radio buttons to adjust tables)
mergeweek <- 6
currentweek <- 7

# Cast Members
castaways <- c("Jenny","Lindsay", "Drea","Johnathan", "Chanelle",
               "Hai","Maryanne","Mike","Omar","Swati","Tori", "Daniel",
               "Rocksroy", "Romeo","Lydia","Marya S")

# Our Selections
picks <- read_csv("picks.csv") %>%
  mutate(fullteam  = as.vector(paste(MVP, Pick2, Pick3, Pick4, Pick5, sep=",")))



# The Tribe Spoke: Table of eliminated choices
eliminated <- tribble(~cast, ~week,
                      "Marya S", 1,
                      "Jenny", 2,
                      "Swati", 3,
                      "Daniel", 4,
                      "Lydia", 5, 
                      "Chanelle", 6, # Jury Begins
                      "Rocksroy", 7,
                      "Tori", 7) 

# Create Main Scoring Table ----
picks <- picks %>%
  mutate(tot_remain = 5 - str_count(fullteam, 
                                    pattern = paste(eliminated$cast,collapse = "|")))

# Scoring
weekly_score <- function(x,y) {
  # Only the previously eliminated weeks considered
  nowelim <- eliminated %>%
    filter(week <= y) %>%
    select(cast) %>%
    .$cast
  
  # How many picks remain for the week?
   new <- x %>% 
    mutate(epi_remain = 5 - str_count(fullteam, 
                           pattern = paste(nowelim, collapse = "|"))) 
   
   if(y < mergeweek) {
     new$epi_score = new$epi_remain
   }
   else(new$epi_score = new$epi_remain*3)

   new %>%
     rename_with(~paste0(.,y), epi_remain:epi_score)

}


vect <- 1:currentweek
picks2 <-lapply(vect, weekly_score, x = picks)

# Reduce list down to just have necessary variables, with names and picks as key.
picks3 <- picks2 %>%
  reduce(left_join) %>%
  rowwise() %>%
  mutate(score =  sum(c_across(starts_with("epi_score"))))

goodtable <- picks3 %>%
  select(Name, score, MVP, starts_with("Pick"), tot_remain) %>%
  rename(`Remaining Survivors` = tot_remain) %>%
  arrange(desc(score))


# Fun Plots ----

popular_picks <- as_tibble(unname(unlist(sapply(picks$fullteam, 
                                             function(z) str_split(z, ","))))) %>%
  group_by(value) %>%
  rename(name = value) %>%
  summarise(picks = n()) %>% 
  arrange(picks) %>%
  mutate(name = as_factor(name))
  

# Most popular picks
popular <- ggplot(data = popular_picks, aes(fct_inorder(name),picks)) +
  geom_col(fill = "#358bbd") +
  scale_y_continuous(breaks = 1:15) +
  coord_flip() +
  ggthemes::theme_tufte() +
  mytheme() +
  theme(axis.title.y = element_blank())+
  labs(title = "Most Popular Picks, Season 42",
       y = "Count")


# 