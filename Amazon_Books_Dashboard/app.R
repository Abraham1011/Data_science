library(shiny)
library(plotly)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(shinydashboard)
library(DT)
library(tm)
library(tidytext)
library(wordcloud2)
library(syuzhet)

books <- read.csv('Top-100 Trending Books.csv')
#books <- books %>%
#  filter(!is.na(rating))
colnames(books) <- c("Rank",	"Title",
                     "Price",	"Rating",
                     "Author", "Year of publication",	
                     "Genre", "url")
books <- books %>%
  filter(!duplicated(Title))
books$Rank <- c(1:dim(books)[1])

clean <- function(txt){
  txt_new <- tolower(txt)
  txt_new <- gsub("[0-9]", "", txt_new)
  txt_new <- gsub("[[:punct:]]", "", txt_new)
  txt_new <- gsub("\\s+", " ", txt_new)
  return(txt_new)
}


df <- read.csv('customer reviews.csv')

df <- df %>% 
  inner_join(books, by = c("book.name" = "Title"))

df$text <- clean(df$review.description)



stop_words <- c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", 
                "you", "your", "yours", "yourself", "yourselves", "he", "him", "his", 
                "himself", "she", "her", "hers", "herself", "it", "its", "itself", 
                "they", "them", "their", "theirs", "themselves", "what", "which", "who", 
                "whom", "this", "that", "these", "those", "am", "is", "are", "was", 
                "were", "be", "been", "being", "have", "has", "had", "having", "do", 
                "does", "did", "doing", "a", "an", "the", "and", "but", "if", "or", 
                "because", "as", "until", "while", "of", "at", "by", "for", "with", 
                "about", "against", "between", "into", "through", "during", "before", 
                "after", "above", "below", "to", "from", "up", "down", "in", "out", 
                "on", "off", "over", "under", "again", "further", "then", "once", 
                "here", "there", "when", "where", "why", "how", "all", "any", "both", 
                "each", "few", "more", "most", "other", "some", "such", "no", "nor", 
                "not", "only", "own", "same", "so", "than", "too", "very", "s", "t", 
                "can", "will", "just", "don", "should", "now", "d", "ll", "m", "o", 
                "re", "ve", "y", "ain", "aren", "couldn", "didn", "doesn", "hadn", 
                "hasn", "haven", "isn", "ma", "mightn", "mustn", "needn", "shan", 
                "shouldn", "wasn", "weren", "won", "wouldn", "mightn",
                "book","books","read","reading")


stop_words <- data.frame(word = stop_words)



ui <- fluidPage(
  navbarPage(title = span("General information",style="color:#0469EE"),
             tabPanel(icon("home"),
                      fluidRow(align = "center",
                               column(2,
                                      h5(strong("This dashboard was created with
                                                the data set available in the following link:"),
                                         style="text-align:justify;color:#066AFF;"),
                                      HTML('<h5><font><center><a href="https://www.kaggle.com/datasets/anshtanwar/top-200-trending-books-with-reviews">
                               Top 100 Bestselling Book Reviews</a> </center></font></h5>'),
                                      h5("*Rank: The ranking of the book among the top 100 Bestselling books on Amazon.",
                                         style="text-align:justify"),
                                      h5("*Price: The price of the book in USD.",
                                         style="text-align:justify"),
                                      h5("*Rating: The overall rating of the book, 
                                         on a scale of 1 to 5.",
                                         style="text-align:justify"),
                                      h5("*Genre: The genre or category to which the book belongs.",
                                         style="text-align:justify"),
                                      br(),
                                      h5("Created by: Pedro Abraham Montoya Calzada",
                                         style="text-align:justify"),
                                      h5("Email: ",a("abraham.montoyaca11@gmail.com"))
                                      ),
                               column(5,plotlyOutput("p1"),
                                      br(),
                                      plotlyOutput("p2")
                                      ),
                               column(5,plotlyOutput("p3"),
                                      br(),
                                      plotlyOutput("p4")
                                      )
                               )
                      
                      ),
    tabPanel(title = span("Word cloud",style="color:#0469EE"),
             style = "text-align:justify;color:#0469EE;background: white",
             sidebarLayout(
               sidebarPanel(
                 style = "text-align:justify;color:#0469EE;background:#EBEBEB",
                 fluidRow(align = "center",
                 numericInput("n1","Number of words",
                              value = 20,min = 2,
                              max = 50,width = "110px"),
                 selectInput("s1",label = "Filter by genre",selected = "All",
                             choices = c("All",unique(df$Genre)))
                 )
                 
               ),
               mainPanel(
                 fluidRow(align = "center",
                          wordcloud2Output("w1"))
               )
             )
             
             ),
    
    tabPanel(title = span("Sentiment analysis",style="color:#0469EE"),
             style = "text-align:justify;color:#0469EE;background: white",
             
             fluidRow(align = "center",
                      column(6,h3("Sentiment by genre and author",
                                  style="text-align:center")),
                      column(6,h3('Relationship between average sentiment and rating',
                                  style="text-align:center")
                             
                             )
                      ),
             fluidRow(align = "center",
                      column(2,selectInput("s2",label = "Filter by genre",selected = "All",
                                           choices = c("All",unique(df$Genre))),
                             uiOutput(outputId = "ui1")
                            
                             ),
                      column(4,plotlyOutput("p5")),
                      column(1),
                      column(4,plotlyOutput("p6"),
                             h5("*Considering all reviews, author and genre
                                filters are not taken into account.")
                             ),
                      column(1)
                      )
             )
  )

)

server <- function(input, output) {
  
  output$p1 <- renderPlotly({
    p <- ggplot(data = books) + 
      geom_histogram(aes(x = Rating),bins = 10,
                     fill = "#0469EE",
                     color = "white") + 
      theme_bw() + 
      theme(panel.grid=element_blank(),
            plot.title=element_text(hjust=0.5,size=17)) +
      labs(title = "Book rating distribution",
           x = "Rating", y = "Frequency")
    ggplotly(p)
  })

  output$p2 <- renderPlotly({
    p <- ggplot(data = books) + 
      geom_point(aes( x = Price, y = Rating),
                 color = "#D20E1B", size = 2) + 
      theme_bw() + 
      theme(panel.grid=element_blank(),
            plot.title=element_text(hjust=0.5,size=17)) +
      labs(title = 'Relationship between price and rating',
           x = "Price", y = "Rating") + 
      geom_smooth(aes(x = Price, y = Rating),
                  method = lm, formula = y ~ splines::bs(x, 3))
    
    ggplotly(p)
  })
  
  output$p3 <- renderPlotly({
    p <- ggplot(data = books) + 
      geom_histogram(aes(x = Price),bins = 8,
                     fill = "#0469EE",
                     color = "white") + 
      theme_bw() + 
      theme(panel.grid=element_blank(),
            plot.title=element_text(hjust=0.5,size=17)) +
      labs(title = "Book price distribution",
           x = "Price", y = "Frequency")
    
    ggplotly(p)
  })
  
  output$p4 <- renderPlotly({
    Genre <- books %>% 
      group_by(Genre) %>%
      summarise(n = length(Genre)) %>%
      arrange(desc(n))
    
    
    Genre <- Genre[1:5,]

    p <- ggplot(data = Genre, 
                aes(x = reorder(Genre, -n), y = n)) +
      geom_bar(stat = "identity", fill = "#0469EE") + 
      geom_text(aes(label = n), vjust = -1)  +
      theme_bw() + 
      theme(panel.grid=element_blank(),
            plot.title=element_text(hjust=0.5,size=17)) +
      labs(title = "Best-selling genres (top 5)",
           x = "Genre", y = "Frequency")
    
    ggplotly(p)
  })
  
  df1 <- reactive({
    if(input$s1 == "All"){
      df1 <- df
    }else{
      df1 <- df %>%
        filter(Genre == input$s1)
    }
    return(df1)
  })
  
  
  output$w1 <- renderWordcloud2({
    token <- df1() %>% 
      unnest_tokens(word, text,token = "ngrams", n = 1) %>%
      anti_join(stop_words)
    
    word <- token %>% count(word, sort = TRUE)
    wordcloud2(word[1:input$n1,][-1,],
               minRotation = 0,maxRotation = 0,
               fontFamily ="Calibri",size = 0.7)
  })
  
  df2 <- reactive({
    if(input$s2 == "All"){
      df2 <- df
    }else{
      df2 <- df %>%
        filter(Genre == input$s2)
    }
    return(df2)
  })
  
  output$ui1 <- renderUI({
    selectInput("s3",label = "Filter by genre",selected = "All",
                choices = c("All",unique(df2()$Author)))
  })
  
  df3 <- reactive({
    if(input$s3 == "All"){
      df3 <- df2()
    }else{
      df3 <- df2() %>%
        filter(Author == input$s3)
    }
    return(df3)
  })
  
  output$p5 <- renderPlotly({
    sentiment <- get_sentiment(df3()$text,method = "bing")
    sentiment <- ifelse(sentiment < 0, "Negative",
                        ifelse(sentiment==0,"Neutral","Positive"))
    
    x <- table(sentiment)
    x <- as.data.frame(x)

    colores <- ifelse(x$sentiment == "Negative", "red",
                      ifelse(x$sentiment == "Neutral", "#0469EE", "#00FF22"))
    
    fig <- plot_ly(x = x$sentiment, 
                   y = x$Freq, 
                   type = 'bar',
                   text = x$Freq, 
                   textposition = 'auto',
                   marker = list(color = colores))
    
    fig <- fig %>% layout("",
                          yaxis = list(title = "Frequency"),
                          xaxis = list(title =  "Sentiment"))
    
    fig
    
  })
  
  output$p6 <- renderPlotly({
    sentiment <- get_sentiment(df$text,method = "bing")
    
    df$sentiment <- sentiment
    
    df4 <- df %>% 
      group_by(book.name) %>%
      summarise(Average = mean(sentiment)) %>%
      inner_join(books, by = c("book.name" = "Title"))
    
    p <- ggplot(data = df4) + 
      geom_point(aes(x = Rating, y = Average),
                 color = "#D20E1B") + 
      theme_bw() + 
      theme(panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 17)) +
      labs(title = "",
           x = "Sentiment", y = "Rating") 
    
    
    ggplotly(p)
  })
  
}

shinyApp(ui = ui, server = server)
