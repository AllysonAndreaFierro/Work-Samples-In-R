# Allyson Fierro
# Shiny Dashboard-Project 1-Remake Learning 2018

#Loading in libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(readxl)
library(tidyr)
library(scales)
library(tidyverse)
library(rsconnect)

#Editing Benedum Grants data
Ben1 <- read_excel("Benedum.xlsx")
colnames(Ben1) <- c("Area", "Organization", "Amt")
Ben1$name <- as.factor(Ben1$Organization)
omit.Ben <- Ben1[240:1048451,]
na.omit(omit.Ben)
Ben <- drop_na(Ben1)

#Editing Spark Grants data 
Spark <- read_excel("Spark.xlsx")
colnames(Spark) <- c("Amt", "Name")
Spark$Name <- as.factor(Spark$Name)

#Editing APOST data 
APOST <-read_excel("APOST.xls")

mAPOST.1 <- melt(APOST, id.vars = "Organization")
mAPOST.1$variable <- NULL
mAPOST <- drop_na(mAPOST.1)

APOSTtable <- read_excel("APOSTtable.xls")

pdf(NULL)

#Building dashboard title and side bars/tabs/pages
header <- dashboardHeader(title = "Remake Learning APOST & Grant Info 2018",
                          titleWidth = 450)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("APOST Plot", tabName = "APOST", icon = icon("bar-chart")),
    menuItem("Spark Grants Plot", tabName = "SparkGrants", icon = icon("money")),
    menuItem("Benedum Grants Plot", tabName = "BenedumGrants", icon = icon("money")),
    menuItem("APOST Table", tabName = "APOSTPittsburgh", icon = icon("table")))

)

#Building the body of each tab/page
body <- dashboardBody(
  tabItems(
    tabItem("APOST",
            fluidRow(
              valueBox(length(APOST$Organization), "Number of Out-of-School Time Programs", icon = icon("clock-o"), color = "red")
              ,
              valueBox(length(unique(APOST$Organization)), "Number of Organizations", icon = icon("handshake-o"), color = "blue")
              ,
              valueBox(length(unique(APOST$PrimaryFocusArea)), "Number of Primary Focus Areas", icon = icon("book"), color = "yellow")
            ),
            fluidRow(
              box(
                selectInput("FocusSelect",
                          "Filter the APOST Program's Bar Chart by Focus Area:",
                          choices = sort(unique(mAPOST$value)),
                          multiple = TRUE,
                          selectize = TRUE,
                          selected = c("STEM", "Arts & Culture")),
                    width = 12,
                actionButton("reset", "Reset Filters", icon = icon("refresh"))
                )
          ),
          fluidRow(
            box(
              style='height:550px',
              title = "Allegheny Partners for Out-of-School-Time (APOST) provides after school learning opportunities in these focus areas",
                width = 12,
                plotlyOutput("APOSTPlot", height = "550px")
                ),
            box(title = "Note: Organizations have multiple program areas. Selecting a program area will count every instance in the data and additional program areas associated.",
                width = 12)
            )
          ),
    tabItem("SparkGrants",
            fluidRow(
              valueBox(length(Spark$Name), "Number of Grants Awarded", icon = icon("certificate"), color = "green")
              ,
              valueBox(format(sum(Spark$Amt), big.mark=",",scientific=FALSE), "Grand Total Awarded", icon = icon("dollar"), color = "purple")
              ,
              valueBox(length(unique(Spark$Name)), "Number of Grantees", icon = icon("users"), color = "orange")
              ),
            fluidRow(
              box(
                width = 12,
                sliderInput("SparkSelect",
                            "Grant Amount:",
                            min = min(Spark$Amt, na.rm = T),
                            max = max(Spark$Amt, na.rm = T),
                            value = c(min(Spark$Amt, na.rm = T), max(Spark$Amt, na.rm = T)),
                            step = 1,000)
           )
         ),
         fluidRow(
           box(
             style='height:900px',
             title = "Michelson Spark Grants fund impactful education initiatives",
                  width = 12,
                  (plotlyOutput("SparkPlot", height = "900px"))
               )
           )
         ),
    tabItem("BenedumGrants",
            fluidRow(
              valueBox(length(Ben$Organization), "Number of Grants Awarded", icon = icon("certificate"), color = "red")
              ,
              valueBox(format(sum(Ben$Amt), big.mark=",",scientific=FALSE), "Grant Total Awarded", icon = icon("dollar"), color = "blue")
              ,
              valueBox(length(unique(Ben$Organization)), "Number of Grantees", icon = icon("users"), color = "yellow")
              ),
            fluidRow(
              box(
                width = 12,
                sliderInput("BenedumSelect",
                            "Grant Amount:",
                            min = min(Ben$Amt, na.rm = T),
                            max = max(Ben$Amt, na.rm = T),
                            value = c(min(Ben$Amt, na.rm = T), max(Ben$Amt, na.rm = T)),
                            step = 10,000)
            )
          ),
          fluidRow(
            box(
              style='height:1800px',
              title = "The Benedum Foundation provides education grants for West Virgina and Southwestern Pennylvania",
                width = 12,
                plotlyOutput("BenedumPlot", height = "1800px")
                )
            )
          ),
    tabItem("APOSTPittsburgh",
            fluidRow(
              box(
                selectInput("OrgSelect",
                            "Filter the Data Table by Organization:",
                            choices = sort(unique(APOSTtable$Organization)),
                            multiple = TRUE,
                            selectize = TRUE,
                            selected = c("University of Pittsburgh", "Carnegie Science Center"))
                 ),
              box(
                downloadButton("downloadData","Download APOST Data Table")
              )
             ),
           fluidRow(
             box(width = 12,
               DT::dataTableOutput("table")
               )
             )
         )
    )
  )

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output, session) {
  APOSTInput <- reactive({
    APOSTreac <- APOST
      # ORG Filter
    if (length(input$FocusSelect) > 0 ) {
      APOSTreac <- subset(APOSTreac, `PrimaryFocusArea` %in% input$FocusSelect)
    }
    
    return(APOSTreac)
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "FocusSelect", selected = c("University of Pittsburgh", "Carnegie Science Center"))
  })
  # Reactive melted data
  mAPOSTInput <- reactive({
    APOSTInput() %>%
      melt(id = "Organization")
  })
  # APOST Plot
  output$APOSTPlot <- renderPlotly({
    dat <- mAPOSTInput()
      ggplot(data = remove_missing(dat, na.rm = TRUE, vars = "value"), aes(x = value, fill = "value", na.rm = TRUE)) + 
      geom_bar(stat = "count", na.rm = TRUE) + 
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
      labs(x = "Program Focus Areas", y = "Number of Programs", title = "APOST Program Focus Areas") +
      theme(legend.position="none")
  })
  
  SparkInput <- reactive({
    Sparkreac <- Spark %>%
    # Amt Filter
      filter(Amt >= input$SparkSelect[1] & Amt <= input$SparkSelect[2])

      return(Sparkreac)
  })
  # Spark Plot
  output$SparkPlot <- renderPlotly({
    dat <- SparkInput() %>%
      group_by(Name) %>% 
      summarise(Amt = sum(Amt))
    
    ggplot(data = dat, aes(x = Name, y = Amt)) +
      geom_bar(stat = "identity", fill = "#663096") +
      labs(x = "", y = "Total Amount Awarded", title = "Spark Grantees and Amount Awarded") +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      theme(legend.position="none")
  })
  
  BenInput <- reactive({
    DF <- Ben %>%
      # Amt Filter
      filter(Amt >= input$BenedumSelect[1] & Amt <= input$BenedumSelect[2])
    
    return(DF)
  })
  # Benedum Plot
  output$BenedumPlot <- renderPlotly({
    dat <- BenInput()
      ggplot(data = dat, aes(x = name, y = Amt, fill = "Amt")) +
      geom_bar(stat = "identity") +
      labs(x = "Grantee", y = "Total Amount Awarded", title = "Benedum Grantees and Amount Awarded") +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      theme(legend.position="none") +
      facet_wrap(~ Area)
  })
  #Table Filter
  APOSTTableInput <- reactive({
    DF <- APOSTtable
    # ORG Filter
    if (length(input$OrgSelect) > 0 ) {
      DF <- subset(DF, Organization %in% input$OrgSelect)
    }
    
    return(DF)
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(APOSTTableInput(), file)
    }
  ) 
  #APOST Table
  output$table <- DT::renderDataTable({
    APOSTTableInput()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)