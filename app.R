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

load('all_aln.RData')
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Synteny alignments"),
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
        column(3, wellPanel(
          # This outputs the dynamic UI component
          uiOutput("ui")
        )),
        column(3, wellPanel(
          # This outputs the dynamic UI component
          uiOutput("ui2")
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
                  choices = c(setdiff(names(all_aln[[input$seqfile]]),c('subalign','aa')))) }
  })
  output$ui2 <- renderUI({
    if (is.null(input$dynamic))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    if(input$var== "Pairwise"){switch(input$dynamic,
           "Spar" = selectInput("number", "Dynamic",
                                choices = c(names(all_aln[[input$seqfile]][["Spar"]]))),
           "Smik" =selectInput("number", "Dynamic",
                               choices = c(names(all_aln[[input$seqfile]][["Smik"]]))),
           "Skud" =  selectInput("number", "Dynamic",
                                 choices = c(names(all_aln[[input$seqfile]][["Skud"]]))),
           "Sbay" = selectInput("number", "Dynamic",
                                choices = c(names(all_aln[[input$seqfile]][["Sbay"]]))),
           "Sarb" = selectInput("number", "Dynamic",
                                choices = c(names(all_aln[[input$seqfile]][["Sarb"]]))))}
  })
  output$msa <- renderMsaR(
    {
      selection=switch (input$var,
              'Pairwise' = all_aln[[input$seqfile]][[input$dynamic]][[input$number]],
              "Amino Acid"=all_aln[[input$seqfile]][['aa']],
              "Syntenic Block"=all_aln[[input$seqfile]][['dna']],
              "Subalignment"=all_aln[[input$seqfile]][['subalign']]
      )
      
      msaR(selection,colorscheme = 'clustal')
    }
  )
  #output$msa <- renderText('hellos')
}

# Run the application 
shinyApp(ui = ui, server = server)
