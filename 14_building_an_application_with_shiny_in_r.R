#---
#Title: "14 Building an Application with Shiny in R"
#author: "Gustavo R. Santos (original) | Antti Rask (modifications)"
#date: "2022-08-31"
#---

# Building an Application with Shiny in R ####

## Loading Libraries ####
library(plotly)       # Create Interactive Web Graphics via 'plotly.js'
library(randomForest) # Breiman and Cutler's Random Forests for Classification and Regression
library(shiny)        # Web Application Framework for R
library(shinythemes)  # Themes for Shiny
library(tidyverse)    # Easily Install and Load the 'Tidyverse'

## Loading the model ####
model <- readRDS("models/rf_model.rds")

## Function to prepare text for prediction ####

### Define spam words #####
prepare_input <- function(text) {
  
  spam_words <- c(
    "you",
    "your",
    "will",
    "free",
    "our",
    "all",
    "mail",
    "email",
    "business",
    "remove",
    "000",
    "font",
    "money",
    "internet",
    "credit",
    "over",
    "order",
    "3d",
    "address",
    "make",
    "people",
    "re",
    "receive"
  )
  
  words      <- unlist(strsplit(text, split = " "))
  spam_count <- sum(map_lgl(words, function(word) tolower(word) %in% spam_words))
  
  exclamation         <- str_count(text, pattern = "[!]")
  parenthesis         <- str_count(text, pattern = "[()]")
  dollar_sign         <- str_count(text, pattern = "[$]")
  total_uppercase     <- str_count(text, pattern = "[A-Z]")
  text_no_punctuation <- str_remove_all(text, pattern = "[:punct:]|[$]*")
  all_words           <- str_split(text_no_punctuation, pattern = " ")[[1]]
  
  char_counts <- map_dbl(
    all_words, function(word) {
      if (word == toupper(word)) {
        return(nchar(word))
      } else {
        return(0)
      }
    }
  )
  
  longest_upper <- max(char_counts)
  top_w         <- sum(
    map_lgl(
      all_words, function(word) {
        return(tolower(word) %in% spam_words)
      }
    )
  )
  
  input <- tibble(
    top_w_pct                  = 100 * top_w       / length(all_words),
    char_freq_exclam           = 100 * exclamation / length(all_words),
    char_freq_parenthesis      = 100 * parenthesis / length(all_words),
    char_freq_dollar           = 100 * dollar_sign / length(all_words),
    capital_run_length_total   = total_uppercase,
    capital_run_length_longest = longest_upper
  ) %>% 
    replace_na(
      list(
        top_w_pct                  = 0,
        char_freq_exclam           = 0,
        char_freq_parenthesis      = 0,
        char_freq_dollar           = 0,
        capital_run_length_total   = 0,
        capital_run_length_longest = 0
      )
    )
  
  return(input)
}

### User Interface ####
ui <- fluidPage(theme = shinytheme("united"),
                
                # This is the panel with all the tabs on top of the pages
                navbarPage(
                  theme = "united",
                  "Data Wrangling with R",
                  
                  # Tab About
                  tabPanel("About the Project",
                           mainPanel(
                             fluidRow(
                               h3("The Project"),
                               p("This project is part of the book Data Wrangling with R, published with", 
                                 strong(a("Packt Publishing.", href="https://www.packtpub.com/")), 
                                 "It consists in a classification model to predict what is the chance of a given e-mail to be
                               marked as spam or not, based on the key words from the training dataset. The user must go to the tab 
                               SPAM CLASSIFIER and input a text. The model will read it and classify as", em("SPAM"), "or", em("NOT SPAM"),
                                 ", together with the proability of each classification."),
                               h3("The Dataset"),
                               p("The dataset used in this project is the", em("Spambase"), "from the UCI Machine Learning Repository."),
                               h4("Dataset Credits"),
                               p(strong("Creators:"), "Mark Hopkins, Erik Reeber, George Forman, Jaap Suermondt", br(),
                                 strong("Donor:"), "George Forman"),
                               p(strong("URL Address:"), a("Spambase Dataset from UCI", href="https://archive.ics.uci.edu/ml/datasets/spambase")),
                               h3("The Model"),
                               p("The classifications are performed by a Random Forest model trained with the dataset previously mentioned.
                               The possible results are SPAM or NOT SPAM for a given text."),
                               h3("The Author"),
                               p(strong(a("Gustavo R Santos", href="https://www.linkedin.com/in/gurezende/")), 
                                 "is a Data Scientist in the retail industry, working daily with R Language, Python,
                             Databricks, and SQL.")
                             ) # fluidRow-About
                           ) # mainPanel-About
                  ), #tabPanel-About
                  
                  # Tab SPAM CLASSIFIER
                  tabPanel("SPAM Classifier",
                           mainPanel(
                             fluidRow(
                               h3("Instructions"),
                               p("This app receives a text as input. Write down or paste a text in the input box
                               and press the button submit. The model will read it and return a classification
                               as Spam or Not spam, together with the probability of each result."),
                               h4("Input your text here:"),
                               
                               # Text Input
                               textAreaInput(
                                 inputId = "text",
                                 NULL,
                                 " ",
                                 width   = "1000px",
                                 height  = "120px"
                               ),
                               submitButton(text = "Submit"),
                               column(
                                 5,
                                 h3("Prediction"),
                                 p("The probability (%) of this text being classified as spam or not are:"),
                                 h4(tableOutput("prediction")) 
                               ), # Column 1
                               column(
                                 7, 
                                 h3("Measurements"),
                                 p("These are the measurements of the variables that affect the classification. 
                                      The higher they are, more are the chances of your text being 
                                      classified as spam."),
                                 plotlyOutput(outputId = "measurements")
                               ) #column2
                             ) # fluidRow-spam_classifier
                           ) # mainPanel-spam_classifier
                  ) #tabPanel-spam_classifier
                ) # navbarPage
) #MainfluidPage-close

### Server ####
server <- function(input, output) {
  
  # Piece of code for the Prediction
  output$prediction <- renderTable({ 
    req(input$text)
    datatext <- NULL
    # If there is no text, show an empty table
    if (trimws(input$text) == "") {input$text} 
    else {
      
      # Prepare data for input in the model
      datatext <- prepare_input(input$text)
      
      # Predict
      prediction <- predict(
        model,
        datatext,
        type = "prob"
      )
      data.frame(prediction * 100)
    }#end if
    
  }) #output prediction
  
  # Piece of code for the measurements graphic
  output$measurements <- renderPlotly({
    req(input$text)
    datatext <- NULL
    # If there is no text, show a dummy graphic
    if (trimws(input$text) == "") {
      datatext <- tibble(variables = c("1", "2"), values = c(0, 0))
      g <- plot_ly(
        data   = datatext,
        x      = ~values,
        y      = ~variables,
        type   = "bar",
        name   = "Bar",
        alpha  = 0.6,
        width  = 500,
        height = 200
      )
      g <- g %>% layout()}
    else {
      
      # Prepare data as a dataset
      datatext           <- prepare_input(input$text)
      datatext           <- tibble(t(datatext))
      colnames(datatext) <- "values"
      datatext <- as_tibble(datatext)
      measurements <- c(
        "Spam words",
        "Presence of !!!",
        "Presence of ( )",
        "Presence of $$$",
        "Total UPPER",
        "Longest UPPER"
      )
      
      # Create graphic
      g <- plot_ly(
        data   = datatext,
        x      = ~values,
        y      = measurements,
        type   = "bar",
        name   = "Bar",
        alpha  = 0.85,
        color  = "darkorange",
        width  = 500,
        height = 200
      )
      g <- g %>% layout(xaxis = list(range = c(0, 100)))
      plotly_build(g)
      
    } #end if
    
  }) #output bar graphic
  
}# close server

### Shiny App ####
shinyApp(ui = ui, server = server)
