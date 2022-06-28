

#Load R packages
library(shiny)
#library(shiythemes)


# Create app directory and file

ui<- fluidPage(
  "Hello world sanket patil!"
)

server <- function(input,out,session){
  
}

shinyApp(ui,server)