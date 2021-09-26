histogramUI <- function(id){
  tagList(
    #selectInput(NS(id, "var"), "Variable", names(mtcars)),
    #numericInput(NS(id, "bins"), "bins", 10, min = 1),
    #plotOutput(NS(id, "hist")),
    fileInput(NS(id, "upload_ion"), "Upload Ion Result"),
    textOutput(NS(id, "text")),
  )
}

histogramServer <- function(id){
  moduleServer(id, function(input, output, session){
    data <- reactive(mtcars[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
    }, res = 96)
    # try load data
    load_file <- function(name, path){
      reactive({
        ext <- tools::file_ext(name)
        switch(ext,
               csv = read.csv(path, sep = ","),
               tsv = read.csv(path, sep = "\t"),
               validate("Invalid file; Please upload a .csv or .tsv file"))
      })
    }
    req(reactive(input$upload_ion))
    icpppm <- load_file(reactive(input$upload_ion)$name, reactive(input$upload_ion)$datapath)
    output$text <- renderText("test")
  })
}

histogramApp <- function(){
  ui <- fluidPage(
    histogramUI("hist1")
  )
  server <- function(input, output, session) {
    histogramServer("hist1")
  }
  shinyApp(ui, server)
}

histogramApp()
