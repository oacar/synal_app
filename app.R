#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(msaR)
library(Biostrings)

load('allaln.RData')
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   textInput(inputId="seqfile", label="Caption", value='YBR013C'),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
        column(2, wellPanel(selectInput("var", 
                    label = "Choose a variable to display",
                    choices = list("Amino Acid", 
                                   "Pairwise",
                                   "Syntenic Block", 
                                   "Subalignment"),
                    selected = "Amino Acid"))),
        column(2, wellPanel(
          # This outputs the dynamic UI component
          uiOutput("ui")
        ))
        #verbatimTextOutput("value")
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        msaROutput("msa", width="100%")
       # textOutput('msa')
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$ui <- renderUI({
    if (is.null(input$var))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    if(input$var== "Pairwise"){
      selectInput("dynamic", "Dynamic",
                  choices = c("Option 1" = "option1",
                              "Option 2" = "option2"),
                  selected = "option2"
      ) }
  })
  
  output$msa <- renderMsaR(
    {
      msaR(all_aln[[input$seqfile]]$aa,colorscheme = 'clustal')
    }
  )
  #output$msa <- renderText('hellos')
}

# Run the application 
shinyApp(ui = ui, server = server)
