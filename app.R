library(shiny)
library(readxl)
library(tidyverse)
library(janitor)
library(plotly)
library(DT)
library(wordcloud2)
library(readr)


# Importing data


voc <- read_csv("Vocabulary Portfolio.csv")



create_wordcloud <- function(data, num_words = 15, background = "white") {
  
  # If text is provided, convert it to a dataframe of word frequencies
  if (is.character(data)) {
    corpus <- Corpus(VectorSource(data))
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    data <- sort(rowSums(tdm), decreasing = TRUE)
    data <- data.frame(word = names(data), freq = as.numeric(data))
  }
  
  # Make sure a proper num_words is provided
  if (!is.numeric(num_words) || num_words < 3) {
    num_words <- 3
  }  
  
  # Grab the top n most common words
  data <- head(data, n = num_words)
  if (nrow(data) == 0) {
    return(NULL)
  }
  
  wordcloud2(data, backgroundColor = background, minRotation = -pi/4, maxRotation = -pi/6,
             minSize = 2, size = 0.2)
}





# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    h1("Vocabulary Portfolio"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("section", "Select section",
                      #selected = NULL,
                      #multiple = FALSE,
                      choices = c("", as.vector(unique(voc$section)))),
          sliderInput(inputId = "num", label = "Maximum number of words (minimum 5)", value = 15, min = 5, max = 20),
          width = 2
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Word Cloud", wordcloud2Output(outputId = "cloud")),
            tabPanel("Vocabulary Portfolio", DT::DTOutput("table"))
          )        
        )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {

  
  # Define the reactive
  
  voc_all <- reactive({
    
    section_selection <- if(nchar(input$section) == 0) unique(voc$section) else input$section
    
    
    voc%>%
      filter(section %in% section_selection)%>%
      select(-true_false)%>%
      arrange(desc(rating))%>%
      rename(Word_Phrase = word_phrase, Explanation = explanation, Example = example, Synonym = synonym,
             Collocations = collocations, Link = link,
             Section = section, Rating = rating)
  })
  
  # Make the output for the table
  
  output$table <- DT::renderDT({
    
    voc_all()
    
    
  })
  
  
  # Make the output for the wordcloud
  
   output$cloud <- renderWordcloud2({

     
     create_wordcloud(voc_all()%>%
                        select(Word_Phrase, Rating), num_words = input$num)
     
     
   })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
