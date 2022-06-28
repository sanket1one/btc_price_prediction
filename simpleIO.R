library(shiny)

# Create app directory and file

ui<- fluidPage(
  selectInput("dataset",label = "Dataset",choices=ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table")
)

server <- function(input,out,session){
  
}

shinyApp(ui,server)
