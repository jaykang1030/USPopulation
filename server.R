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


shinyServer(function(input, output) {
   
    selectedState <- reactive({
        
        yearInput <- as.character(input$Year)
        stateInput <- as.character(input$State)
        
        df %>% 
            filter(NAME == stateInput) %>%
            
            group_by(AGE_GROUP,SEX) %>%
            
            summarise_at(c(grep(yearInput, colnames(df), value = TRUE)), 
                         function(x) round(sum(x)/100000,4)) %>%
            
            filter(SEX == "Male" | SEX == "Female") %>%
            
            rename(TOTAL = grep(yearInput, colnames(df), value = TRUE))
    })
    
    
    output$plot1 <- renderPlot({
        ggplot(data = selectedState(), aes(x = AGE_GROUP, y = TOTAL, fill = SEX)) +
            
            geom_bar(stat = "identity") +
            
            geom_text(aes(label = TOTAL), position = position_stack(vjust = 0.5),color = "white") +
            
            scale_fill_brewer(palette = "Paired") + 
            
            coord_flip() + 
            
            theme_minimal() + labs(x = "Age in Years", y = "Number of Residents in 100,000")
    })
})
