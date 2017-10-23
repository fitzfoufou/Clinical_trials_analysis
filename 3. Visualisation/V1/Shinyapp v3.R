#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
setwd("C:/Users/ffitzpatrick/Desktop/Documents perso/Perso/Clinical trials - project/1. Data/1. Audit and cleaning/R database - phase 3")
load("./Phase3_simple_clean.Rda")

library(shiny)
r_state<-list()

# Define UI for application that draws a histogram
ui <- function(request) {
  fluidPage(
   
   # Application title
   titlePanel("Enrollment"),
   
   # Sidebar with a slider input for number bof bins 
   sidebarLayout(
      sidebarPanel(
        #Select Study type
        selectInput("Study_type", label = h3("Choose the study Type"), 
                    choices = list("Interventional" = "Interventional", "Observational" = "Observational [Patient Registry]", "Expanded Access" = "Expanded Access", "Other" = "NA"), 
                    selected = state_init("Study_type", init = "Interventional")),
        bookmarkButton("Intervention_type"),
        
        hr(),
        #Select condition
        #textInput("condition", label = h3("Choose condition"), value = "Enter text..."),
        
        
        #Select intervention type
         selectInput("Intervention_type", label = h3("Choose the intervention Type"), 
                     choices = list("Drug" = "Drug", "Device" = "Device", "Procedure" = "Procedure", "Behavioral" = "Behavioral", "Other" = "NA"), 
                     selected = "Procedure"),
         
         hr(),
         
         #Select age
         sliderInput("Age", label = h3("Select Age range"), min = 0, 
                     max = 100, value = c(0, 70)),
         hr(),
        
         downloadButton('state', 'Download state')
        ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("enrollPlot"),
         #numericInput("Break", label = h3("Break"), value = 5000),
         hr(),
         plotOutput("durationPlot")
      )
   )
)
}
# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  
  reactive_data<-reactive({subset(Phase3_simple, intervention.intervention_type==input$Intervention_type 
                                  & study_type.text==input$Study_type
                                  & Min.age>input$Age[1]
                                  & Max.age<input$Age[2])})
  
  #output$state<-reactive(input$study_init)
  #output$state="Observational"
  
  
  
  
  output$enrollPlot <- renderPlot({
    
     x <- reactive_data()$enrollment.text
     xlimit<- cat<- seq(0, 2000, length.out=5000)
     hist(x, freq= FALSE, breaks = 1000,xlim=c(0,2000), col = 'darkgray', border = 'white',
           xlab="Enrollment",main="Distribution of enrollment in Phase 3")
   })
  
  
  output$durationPlot <- renderPlot({
    
    y <- reactive_data()$Duration
    cat<- seq(0, 6000, by=200)
    
    hist(y, freq=FALSE, breaks=2000, xlim=c(0,10000), col = 'darkgray', border = 'white',
         xlab="Duration",main="Distribution of durations in Phase 3")
  })
}

enableBookmarking(store = "server")
# Run the application 
shinyApp(ui = ui, server = server)

