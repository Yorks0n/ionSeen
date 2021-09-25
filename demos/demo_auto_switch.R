ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textOutput("panel"),
      tabsetPanel(
        id = "tabsetSide",
        tabPanel(title = "side 1", value = "panel 1", "side one content"),
        tabPanel(title = "side 2", value = "panel 2", "side two content"),
        tabPanel(title = "side 3", value = "panel 3", "side three content")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("panel 1", "main one content"),
        tabPanel("panel 2", "main two content"),
        tabPanel("panel 3", "main three content")
      )
    )
  )
)
server <- function(input, output, session) {
  output$panel <- renderText({
    paste("Current panel: ", input$tabset)
  })
  observeEvent(input$tabset, {
    updateTabsetPanel(session, "tabsetSide",
                      selected = input$tabset)
  })
}
shinyApp(ui, server)
