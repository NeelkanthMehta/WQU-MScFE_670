library(shiny)
library(ggfortify)

# Execute once for loading the data then re-comment it.
#countries <- read.csv('countries.csv')
#rownames(countries) <- countries$CountryName
#countries <- countries[c(3:19)]
#countries_scalesd <- scale(countries)


# UI
ui <- fluidPage(
  tags$h1("Collaborative Review Assignment 01"),
  tags$h4("Neelkanth Mehta"),
  
  sidebarLayout(
    
    #Inputs
    sidebarPanel(
      numericInput(
        inputId = 'clusters', 
        label = 'Cluster count', 
        value = 3, 
        min = 2, max = 5),
      width = 2
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        # Tab 1: Plot
        tabPanel(
          title = "plot",
          tags$h3("Cluster Plot"),
          plotOutput(outputId = 'clusterplot'), # clusterplot
          br(),
          htmlOutput(outputId = "cluster") # cluster centers
        ),
        # Tab 2: output
        tabPanel(
          title = "output",
          tags$br(),
          verbatimTextOutput(outputId = "kmeanssummary") # cluster summary
        )
      ),
      tags$br(),
      tags$h4("Features Heatmap"),
      plotOutput(outputId = "heatmap"), # heatmap
      width = 9
    )
  )
)


# Server
server <- function(input, output){
  
  # reactive kmeans clusters
  clusters <- reactive({
    model <- kmeans(countries_scalesd, input$clusters)
  })
  
  # Create cluster plots
  output$clusterplot <- renderPlot({
    autoplot(clusters(), data = countries_scalesd, frame = TRUE, label = TRUE)
  })
  
  # display cluster size
  output$cluster <- renderUI({
    model_size <- clusters()$size
    HTML("cluster sizes: ", 
      paste(model_size),
      "<br/>"
      )
  })
  
  # display cluster summary
  output$kmeanssummary <- renderPrint({
    model <- clusters()
    print(summary(model), digits = 3)
  })
  
  # display heatmap
  output$heatmap <- renderPlot({
    autoplot(countries_dist)
  })
}

# Create a shiny app
shinyApp(ui = ui, server = server)