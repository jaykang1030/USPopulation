library(shiny)
library(dplyr)
library(ggplot2)

# Load data
url = "https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/sc-est2017-alldata6.csv"
df <- read.csv(url)
df <- df[,-(10:11)]
df$SEX <- as.factor(df$SEX)
df$SEX <- ifelse(df$SEX == "0","TOTAL",ifelse(df$SEX == "1", "Male","Female"))

df <- df %>% mutate(.,AGE_GROUP = with(.,case_when(
    (AGE < 10) ~ "0 to 10",
    (AGE < 20) ~ "10 to 19",
    (AGE < 30) ~ "20 to 29",
    (AGE < 40) ~ "30 to 39",
    (AGE < 50) ~ "40 to 49",
    (AGE < 60) ~ "50 to 59",
    (AGE < 70) ~ "60 to 69",
    TRUE ~ "70 and above"
)))

shinyUI(fluidPage(
    titlePanel('US Population from 2010 to 2017'),
    
    sidebarPanel(
        selectInput('State', 'Select A State', unique(df$NAME)),
        
        sliderInput("Year", "Select Year", 2010, 2017, value = 2017)
    ),
    mainPanel(
        plotOutput('plot1')
    )
))
