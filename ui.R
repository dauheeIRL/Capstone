library(shiny)
library(shinyjs)
library(shinycssloaders)




appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"
sidebartext<-"Sidebar text <b>xxx</b>"

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  useShinyjs(),
  inlineCSS(appCSS),
  titlePanel("Next Word Prediction for Data Science Capstone - by Dauhee"),
  div(
    id = "loading-content",
    h2("Please wait a couple of seconds to load dictionary ... it will be worth it :)"),
    img(src = "loading.gif",
        id = "loading-logo")
  ),
  
  sidebarLayout(
    sidebarPanel(
      tags$b("Please note:"),
      tags$p("You can type a sentence in the textbox on right and this shiny application will predict the next word using a custom algorithm"),
      tags$p("This app was developed as part of the capstone project for the Coursera Data Science course run by John Hopkins University"),
      tags$p("Please review the information tab to see an overview of how the application works"),
      tags$b("For fun, why not try clicking on suggested words in the data table to build up your search sentence")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Predict", 
                 hr(),
                 fluidRow(
                   column(12, textInput("textin", "Word(s) to use for prediction:", "You can type anything in here", width = '100%'))
                  ),

                 fluidRow(column(8, htmlOutput("nextwords"))       ),
                 p(),
                 fluidRow(column(8, htmlOutput("cleaned"))  ),
                 hr(),
                 withSpinner(DT::dataTableOutput('dt')) #
                 , icon=NULL),
        tabPanel("Information", 
                 hr(),
                 tags$p("At the core of this application is the cleansed data that has been sampled from the 3 sources provided by Coursera. Even with the best algorithim, without having suitable NGRAM input data, accurate predictions would not be possible. It is a balance of filtering and condensing for a compact engine versus having enough data that can generate resonable predictions for a larger spectrum of words that could be typed in."),
                 tags$p("Some tools and technologies used for are rsqlite, quanteda, stringr, data.table, doParalell - these enable rapid generation of NGRAMS, and a quick responding shiny app."),
                 tags$p("The algorithm uses a form of stupid backoff method to check 4-NGRAM, 3-NGRAM, 2-NGRAM with an associated Maximum Lightlyhood Estimation score for each prediction. The MLE is ranked so that higher NGRAMS have a multiplied higher MLE. All of the scores are aggregated up per predicted word to give the overall winners. Depending on aggregation, there could be between 2 to 6 words. The aggregation takes place when the same predicted word is seen in different NGRAMS."),
                 tags$p("Due to the memory limitations of shinyapps.io free version, the corpora had to be sampled. It was found that twitter data was not as good for prediction due to text-speak and wide ranges of improper spelling - because of this, a lower sample was taken compared to news and blogs. Some twitter was left included as it was found to aid some diverse/novel/trending words tha users could type in. This is helpful for trending superlatives and such like that don't have much bearing on the next word prediction."),
                 tags$p("For words outside the algorithim engine, the last word the user has typed in is ignored - this method enables predictions to be made for unknown words."),
                 tags$p(""),
                 tags$p("As a final note, this application is optimized purely from a 'next word prediction' perspective, with no stop word removal or stemming. If memory and processing power permitted, better results would be obtainable through use of more sophisticated techniques with sentiment analysis, semantics, parsing and classification etc.")
                 
                 
                 , icon=NULL)
      )
    )
  )
))
