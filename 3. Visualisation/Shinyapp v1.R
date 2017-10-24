# This is a th R script of the R shiny app 

#This is the user interface part
#It's a dashboard made out of a header, a sidebar and a body
ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Clinical Trials Dashboard",titleWidth = 350),
  
  # This is the sidebar where I included all the filters I wanted to let the user filter the database as he wished
  dashboardSidebar(
    sidebarMenu(
      menuItem("Charts", tabName = "charts", icon = icon("line-chart")),
      menuItem("Data", tabName = "data", icon = icon("table"),badgeLabel = "new", badgeColor = "green")
      ),
    hr(),
    # Filter on the status of the clinical trial
    fluidRow(
      box(title = "Status", status = "primary", collapsible = TRUE,background = "black", width=11,
        checkboxGroupInput("status", label="",
                choices = list("Not yet recruiting" = "Not yet recruiting", 
                               "Recruiting" = "Recruiting", 
                               "Enrolling by invitation" = "Enrolling by invitation",
                               "Active, not recruiting"="Active, not recruiting",
                               "Others"=NA), 
                selected = "Recruiting"))),
    # Filter on the age of the patients included in the clinical trials
    fluidRow(
      box(title = "Eligibility criteria", status = "primary", collapsible = TRUE,background = "black",width=11,
        sliderInput("age", label = h4("Select Age range"), min = 0, 
                    max = 100, value = c(18, 80)),
        hr(),
        # Filter on the sex of the patient recruited
        checkboxGroupInput("sex",label = h4("Sex"), 
                           choices = list("All" = "All", "Male" = "Male", "Female" = "Female"), 
                           selected = "All"))),
        
    # Filter on the type of study                  
    fluidRow(
      box(title = "Study Type", status = "primary", collapsible = TRUE,background = "black",width=11,
        checkboxGroupInput("study_type",label="",
                choices = list("Interventional" = "Interventional",
                               "Observational" = "Observational","Expanded Access"="Expanded Access"),
                selected = "Interventional"))),
    # Filter on the study results
    fluidRow(
      box(title = "Study results", status = "primary", collapsible = TRUE,background = "black",width=11,
        selectInput("results",label="",
                    choices = list("All" = 1, "With results" = 2, "Without Results" = 3),
                    selected = 1))),
    # Filter on the phase of the clinical trial
    fluidRow(
      box(title = "Study phase", status = "primary", collapsible = TRUE,background = "black",width=11,
        checkboxGroupInput("phase",label="",
                    choices = list("Phase 1" = "Phase 1", "Phase 2" = "Phase 2",
                                        "Phase 3" = "Phase 3","Phase 4"="Phase 4"),
                    selected = "Phase 3")))
    
    ),
    
  # This is the body which includes the aggregated graph results and the database of the first results
  dashboardBody(
    tabItems(
      # First tab content: it will contain the graphs
      tabItem(tabName = "charts",
              fluidRow(
                #valueBox(10 * 2, "Enrollment Average", icon = icon("users")),
                #valueBox(10 * 2, "Duration Average", icon = icon("clock-o")),
                valueBoxOutput("Enrollment_average",width=6),
                valueBoxOutput("Duration_average", width=6),
                box(title = "Enrollment histogram", status = "warning", collapsible = TRUE, plotOutput("plot1", height = 250)),
                box(title = "Duration Histogram", status = "primary", collapsible = TRUE, plotOutput("plot2", height = 250))
              )
      ),
      
      # Second tab content: it will contain the database of the first results
      tabItem(tabName = "data",
              h2("Graphs tab content"),
              # fluidRow(
              #   box(title="Variables of interest", status="warning", width= 12,collapsible=TRUE, 
              #       checkboxGroupInput('show_vars', 'Columns in diamonds to show:',
              #                          names(CTsimple), selected = names(CTsimple)))),
              fluidRow(
                box(title= "Clinical Trials data", status="primary",width=12, DT::dataTableOutput("table"))
              )))
  )
)


#This is the server part
server <- function(input, output) {
  

  CTdyn<-reactive({
    CTsimple[ which(CTsimple$last_known_status.text==input$status
                         & CTsimple$Minimum_age >= input$age[1]
                          & CTsimple$Maximum_age <= input$age[2]
                          & CTsimple$eligibility.gender==input$sex
                          & CTsimple$study_type.text==input$study_type), ]
  })
  
  CTdyn2<-reactive({
    CTsimple[ which(CTsimple$last_known_status.text==input$status
                    & CTsimple$Minimum_age >= input$age[1]
                    & CTsimple$Maximum_age <= input$age[2]), ][sample(nrow(CTdyn()), 1000),
                                                               c("id_info.nct_id","condition.text","intervention.intervention_name","phase.text","study_type.text","enrollment","Duration")]
  })
  
  output$Enrollment_average <- renderValueBox({
    valueBox(
      paste0(round(mean(CTdyn()$enrollment, na.rm=TRUE)), " patients"), "Enrollment Average", icon = icon("users"),
      color = "orange"
    )
  })
  
  output$Duration_average <- renderValueBox({
    valueBox(
      paste0(round(as.numeric(mean(CTdyn()$Duration, na.rm=TRUE)))," days"), "Duration Average", icon = icon("clock-o"),
      color = "blue"
    )
  })
  
  output$plot1 <- renderPlot({
    ggplot(CTdyn(), aes(x=enrollment))+
      geom_histogram(binwidth=200,fill="white", color="black")+
      xlim(0,5000)+
      theme_classic() +
      geom_vline(aes(xintercept=mean(enrollment)), color="orange", linetype="dashed", size=1)

  })

  output$plot2 <- renderPlot({
    ggplot(CTdyn(), aes(x=Duration))+
      geom_histogram(binwidth=365,fill="white", color="black")+
      xlim(0,10000)+
      theme_classic() +
      geom_vline(xintercept=mean(CTsimple$Duration), color="orange", linetype="dashed", size=1)

  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(CTdyn2())
  })
  
}

#Finally this is the function to launch the app!
shinyApp(ui, server)

