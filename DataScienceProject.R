library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)
library(readxl)


require("httr")
require("jsonlite")

base <- "https://api.wazirx.com/"
endpoint<- "sapi/v1/ticker/24hr"
crypto<- "btcinr"


call<- paste(base,endpoint,"?","symbol","=",crypto,sep="")

get_prices<-GET(call,type="basic")
get_prices

get_prices_text <- content(get_prices,"text")



get_prices_json <- fromJSON(get_prices_text,flatten = TRUE)

get_prices_df <- as.data.frame(get_prices_json)


#craeting the dataset
library(readxl)
library(dplyr)
crypto_data <- read_excel("D:/programs/dataScience/r/rassignment/Ds_lab3/btc_data.xlsx")
View(crypto_data)

btc_data<- subset(crypto_data,Asset_ID == "1")

btc_data<-data.frame(btc_data$Count,btc_data$High,btc_data$Low, btc_data$Close, btc_data$Volume, btc_data$Target)

head(btc_data)
colnames(btc_data)<-c('count','high','low', 'close', 'volume','target')


# normal scale
btc_data = scale(btc_data)
btc_data = as.data.frame(btc_data)

multi.fit = lm(formula=target~close+low + volume +high +count,data = btc_data)


library(ggplot2)
ggplot(multi.fit,aes(volume,target))+geom_point()+geom_smooth(method="lm",formula = y~x,color="Blue",se=TRUE)


# z-score outlier
z_scores <- as.data.frame(sapply(btc_data, function(btc_data) (abs(btc_data-mean(btc_data))/sd(btc_data)))) 

no_outliers <- z_scores[!rowSums(z_scores>3), ]
head(no_outliers)


# Adding outliers
Q1 <- quantile(btc_data$target, .25)
Q3 <- quantile(btc_data$target, .75)
IQR <- IQR(btc_data$target)

no_outliers <- subset(btc_data, btc_data$target > (Q1 - 1.5*IQR) & btc_data$target < (Q3 + 1.5*IQR))

#POLYNOMIAL REGRESSION
multi.fit = lm(formula=target~ count + poly(volume,3) + poly(high,2) + poly(low,2) ,data = no_outliers)

ggplot(no_outliers,aes(volume,target))+geom_point()+stat_smooth(method="lm",formula = y~poly(x,2),color="Blue",se=TRUE)


#hist(no_outliers$target, xlab = "target", col="blue",border="white");



#btc_data$target = btc_data$target*100
predictingValue <-data.frame(0.525,-0.14237,-0.11483266,-0.14614014,0.07553919, -9.8067592)
colnames(predictingValue)<- c("count","high","low","close","volume","target")


#predicting value
make<-predict(multi.fit, newdata = predictingValue)


ui <- fluidPage(theme = shinytheme("united"),
                
                # Page header
                headerPanel('btc prediction?'),
                
                # Input values
                sidebarPanel(
                  HTML("<h3>Input parameters</h3>"),
                  
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
#                  sliderInput("vwap","Vwap: ",
#                              min=-10,max=10,value=0,step=0.001),
#                  sliderInput("target","target:",
#                              min=-10,max=10,value=0,step=0.001),
                  
                  
                  actionButton("submitbutton", "Submit", class = "btn btn-primary")
                ),
                
                mainPanel(
                  tags$label(h3('Status/Output')), # Status/Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata'), # Prediction results table
                    # Output: ggPlot ----
                  plotOutput(outputId = "distPlot")
                    
                                    
                )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    predictingValue <-data.frame(
        
    )
    predictingValue <-data.frame(input$count,
                                 input$high,
                                 input$low,
                                 input$close,
                                 input$volume,
                                 no_outliers$target)
    colnames(predictingValue)<- c("count","high","low","close","volume","target")
    
    
    #predicting value
    Output <-predict(multi.fit, newdata = predictingValue,stringAsFactors= FALSE)    

    
    
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
      isolate(datasetInput()[1]*10000)
    } 
  })
  
  output$distPlot <- renderPlot({
    ggplot(no_outliers,aes(volume,target))+geom_point()+stat_smooth(method="lm",formula = y~poly(x,2),color="Blue",se=TRUE)
  })
  
}

shinyApp(ui = ui, server = server)

