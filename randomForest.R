library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)
library(readxl)


#install.packages("randomForest")

# Read data
weather <- read_excel("weatherData.xlsx")
View(weather)

#ca and thal are correctly called factors, but one of the levels is"?" when we
# need it to be NA...
#weather[weather == "?"] <-NA
#weather[weather$outlook == "windy"]$outlool <- "Turbid"


# changing to factor
weather$outlook <- as.factor(weather$outlook)
weather$temperature <- as.factor(weather$temperature)
weather$humidity <- as.factor(weather$humidity)


# weather$windy <- ifelse(weather$windy == TRUE,1,0)
# weather$play <- ifelse(weather$play == "yes", 1,0)

weather$windy <- as.factor(weather$windy)
weather$play <- as.factor(weather$play)


# Arranging the data

# Since we are going to be random sampling things,
# set the seed for the random number generator so that we can
# reproduce our result


set.seed(45)

# Build model

model <- randomForest(play ~ ., data = weather, ntree = 20, mtry = 4, importance = TRUE)
typeof(model)
summary(model)
#save model in RDS file
#saveRDS(model,"model.rds")

# Read in the RF model
#model <- readRDS("model.rds")

# Save model to RDS file
# saveRDS(model, "model.rds")

# Read in the RF model
#model <- readRDS("model.rds")

####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("united"),
                
                # Page header
                headerPanel('Play Golf?'),
                
                # Input values
                sidebarPanel(
                  HTML("<h3>Input parameters</h3>"),
                  
                  selectInput("outlook", label = "Outlook:", 
                              choices = list("Sunny" = "sunny", "Overcast" = "overcast", "Rainy" = "rainy"), 
                              selected = "Rainy"),
                  sliderInput("count",label = "COUNT: ", 
                              min=-100,max=100, value = 0.1,step=0.1),
                  sliderInput("high",label = "High: ", 
                              min=-100,max=100, value = 0,step=0.01),
                  sliderInput("low","lowerValue:",
                              min=-20,max=20,value=0.1, step=0.01),
                  sliderInput("close","close: ",
                              min = -5,max=5,value =0, step=0.001),
                  sliderInput("volume","Volume:",
                              min=-5,max=5,value=0,step=0.001),
                  sliderInput("vwap","Vwap: ",
                              min=-10,max=10,value=0,step=0.001),
#                  sliderInput("target","target:",
#                              min=-10,max=10,value=0,step=0.001),
                  

                  actionButton("submitbutton", "Submit", class = "btn btn-primary")
                ),
                
                mainPanel(
                  tags$label(h3('Status/Output')), # Status/Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata') # Prediction results table
                  
                )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {

  # Input Data
  datasetInput <- reactive({  
    
    # outlook,temperature,humidity,windy,play
      df <- data.frame(
      Name = c("outlook",
               "temperature",
               "humidity",
               "windy"),
      Value = as.character(c(input$outlook,
                             input$temperature,
                             input$humidity,
                             input$windy)),
      stringsAsFactors = FALSE)
    
    play <- "play"
    df <- rbind(df, play)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    as.list(test)
    
    
     test$outlook <- factor(test$outlook, levels = levels(weather$outlook))
     test$windy <- factor(test$windy, levels= levels(weather$windy))
     test$temperature <-factor(test$temperature,levels = levels(weather$temperature))
     test$humidity <- factor(test$humidity,levels = levels(weather$humidity))
    #test <-data.frame("sunny",70,90,TRUE,"no")
    #colnames(test)<- c("outlook","temperature","humidity","windy","play") 
    Output <- data.frame(Prediction=predict(model,test), stringsAsFactors = TRUE)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

shinyApp(ui = ui, server = server)

