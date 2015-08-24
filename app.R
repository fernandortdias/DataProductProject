#This exercise is a 'crossing over and recombination' of two examples from R Studio Gallery:

#http://shiny.rstudio.com/gallery/kmeans-example.html and
#http://shiny.rstudio.com/gallery/widgets.html

#We also changed code in order to:
## Make it reactive, i.e., deleting the submitButton.
## Use a single app.R file and instead of ui.R and server.R files;
## Render chosen dataset column names to selectInput using the renderUI so called 'experimental feature' (neat...).

# First step: Loading librarires and defining pallette.
library(shiny)
library(datasets)
palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

# Setting ui
ui <- fluidPage(
  titlePanel("Exploring datasets"),
  sidebarLayout(

    #Showing widgets
    sidebarPanel(
      
      h5("This application allows you to explore 4 differents R datasets",
               "showing their summary data, a cluster chart on two chosen variables, and",
               "some observations."),
      
      selectInput('dataset', "Choose a dataset to explore:", 
                  choices = c("cars", "iris", "pressure", "rock")),
      
      # Showing selectInputs rendered after dataset was chosen
      uiOutput('xInput'),
      
      uiOutput('yInput'),
      
      numericInput('clusters', 'Choose a cluster count:', 3,
                   min = 1, max = 9),
      
      numericInput("obs", "Specify the number of observations to view:", 5, 
                   min = 1, max = 50),
      
      helpText("While the data view show only the specified",
               "number of observations, the clustering and summary are",
               "still based on the full dataset.")
    ),
    
    # Ploting results from server
    mainPanel(
      h4("Summary"), verbatimTextOutput("summary"),
      h4("Clusters"), plotOutput('plot1'),
      h4("Observations"), tableOutput("view")
) ) )

# Setting server
server <- function(input, output) {
  
  # Reacting to dataset chosen
  datasetInput <- reactive({
    switch(input$dataset, "cars" = cars, "iris" = iris, "pressure" = pressure, "rock" = rock)
  })
  
  # Rendering selectInput widget for X Variable from dataset column names 
  output$xInput <- renderUI({
    selectInput('xcol', 'Choose the X Variable for clustering:', names(datasetInput()))
  })

  # Rendering selectInput widget for Y Variable from dataset column names 
  output$yInput <- renderUI({
    selectInput('ycol', 'Choose the Y Variable for clustering:', names(datasetInput()),
               selected=names(datasetInput())[[2]])
  })
  
  # Reacting to selected data from the dataset chosen
  selectedData <- reactive({datasetInput()[, c(input$xcol, input$ycol)]})
  
  # Reacting to clusters calculated from selected data from the dataset chosen
  clusters <- reactive({kmeans(selectedData(), input$clusters)})
  
  # Rendering cluster plot from dataset and columns chosen
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })

  # Rendering summary from the dataset chosen
  output$summary <- renderPrint({summary(datasetInput())})
  
  # Rendering first n observations from the dataset chosen
  output$view <- renderTable({head(datasetInput(), n = input$obs)})
}
shinyApp(ui=ui, server=server)