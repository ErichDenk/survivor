# Script for Iterative Building of the Shiny App
# Erich 05.01.2022

rm(list = ls())

# Packages -------
library(shiny)
library(tidyverse)
library(tidyselect)
library(markdown)

# Cast Members
castaways <- c("Jenny","Lindsay", "Drea","Johnathan", "Chanelle",
               "Hai","Maryanne","Mike","Omar","Swati","Tori", "Daniel",
               "Rocksroy", "Romeo","Lydia","Marya S")

# Our Selections
picks <- read_csv("picks.csv") %>%
  mutate(fullteam  = as.vector(paste(MVP, Pick2, Pick3, Pick4, Pick5, sep=",")))

# Merge Cutoff
mergeweek <- 6
currentweek <- 7

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
   
   if(y<=mergeweek) {
     new$epi_score = new$epi_remain
   }
   else(new$epi_score = new$epi_remain*3)

   new %>%
     rename_with(~paste0(.,y), epi_remain:epi_score)

}


vect <- 1:currentweek
picks3 <- lapply(vect, weekly_score, x = picks) 

# Fun Plots ----

# Most popular picks



# 