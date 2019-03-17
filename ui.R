#CAUTION - Do not re-run lines 2-4
#library(doParallel)
# no_of_cores = (detectCores()/2)
# registerDoParallel(cores = no_of_cores)
library(shiny)
library(shinycssloaders)

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Personalised Healthcare Using Big Data"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      numericInput("PatientID","Patient ID",value = "") ,
      submitButton("OK"),
      hr(),
      helpText("Patient Data from Brazil"), width = 2
    ),
    # Create a spot for the barplot
    mainPanel(
      tabsetPanel(
        tabPanel("Patient Medical Record", tableOutput("patient")),
        tabPanel("Prediction", withSpinner({plotOutput("riskPlot")})))  
    )
  )
)
