library(shiny)
library(ggplot2)
library(dplyr)

url = "https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/sc-est2017-alldata6.csv"
df <- read.csv(url)
df$SEX <- as.factor(df$SEX)
df$AGE_GROUP <- as.factor(ifelse(df$AGE >= 0 & df$AGE < 10, "0 to 10",
                                 ifelse(df$AGE >= 10 & df$AGE < 20, "10 to 19",
                                        ifelse(df$AGE >= 20 & df$AGE < 30, "20 to 29",
                                               ifelse(df$AGE >= 30 & df$AGE < 40, "30 to 39",
                                                      ifelse(df$AGE >= 40 & df$AGE < 50, "40 to 49",
                                                             ifelse(df$AGE >= 50 & df$AGE < 60, "50 to 59",
                                                                    ifelse(df$AGE >= 60 & df$AGE < 70, "60 to 69",
                                                                           ifelse(df$AGE >= 70 & df$AGE < 80, "70 to 79","80 and over")
                                                                    ))))))))
shinyServer(function(input, output) {

    selectedState <- reactive({
        df %>% filter(NAME == input$State) %>%
            group_by(AGE_GROUP,SEX) %>%
            summarise(TOTAL = round(sum(POPESTIMATE2017)/1000000,2)) %>%
            filter(SEX == "1" | SEX == "2")
    })
    
    output$plot1 <- renderPlot({
        ggplot(data = selectedState(), aes(x = AGE_GROUP, y = TOTAL, fill = SEX)) +
            geom_bar(stat = "identity")+
            geom_text(aes(label = TOTAL), 
                      position = position_stack(vjust = 0.5),color = "white")+
            scale_fill_brewer(palette = "Paired")+
            theme_minimal()
    })
})