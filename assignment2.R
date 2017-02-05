# install.packages("shiny")
# install.packages("dplyr")
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
data <- read.csv("./case.csv", sep = ";")
data$days <- as.Date(as.character(data$LastTrxn.Date), format="%d/%m/%Y") - as.Date(as.character(data$FirstTrxn.Date), format="%d/%m/%Y")

ui <- fluidPage(
  titlePanel("Analysis of Customer Behavior on various parameters"),
  h3("Input"),
  sidebarLayout(
    
    sidebarPanel(
    fileInput('datafile', "Choose file"),
    helpText("Max. file size should be 20 MB"),
    
    selectInput("var3", label = "# of Trxn should be > ", choices = unique(data$X..of.Trxn)),
    helpText("If we choose 2 then histogram curve of the number of transactions among customers who have performed more than 2 transactions"),
    
    actionButton("b1", "Show the Histogram and User_Id"),
    helpText("It shows us the histogram curve and info of those customers who performed more transactions than above selected value"),
    
    selectInput("var4", label = "Number of days ", choices = unique(data$days)),
    helpText("It shows the histogram of number of days b/w their first and last transaction"),
    
    selectInput("var1",label = "Variable 1", choices = colnames(data[c(1,2,3,4,5,6,7,10)]), selected = colnames(data[1])),
    selectInput("var2", label = "Variable 2", choices = colnames(data[c(1,2,3,4,5,6,7,10)]), selected = colnames(data[1])),
    helpText("Choose variable 1 and 2 to find the correlation between them")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Q.1(a)" , print("Customers showing Max. Transactions") , plotlyOutput('rel'),verbatimTextOutput('analysis1'), dataTableOutput('table1')),
        tabPanel("Q.1(b)", print("Customer showing signs of long term retention"), plotlyOutput('text3'), dataTableOutput('table2')),
        tabPanel("Q.2" ,verbatimTextOutput('text1'), tableOutput('cor'), verbatimTextOutput('text2'), tableOutput('about'))
      )
      ) 
    )
  )

server <- function(input, output, session){
  options(shiny.maxRequestSize=20*1024^2)
  
  filedata <- reactive({
    data <- input$datafile
    if(is.null(data)){
      
      return()
    }
  else read.csv(data$datapath, sep=";")
  })
  
  button <- eventReactive(input$b1,{
    if(is.null(filedata())){
      return()
    }
    else{
      return(input$var3)
      }
  })
  
  button2 <- eventReactive(input$b1,{
    if(is.null(filedata())){
      return()
    }
    else{
      return(input$b2)
    }
  })
   
output$about <- renderTable({ 
    if(is.null(filedata())){
      return()
    }
    else{
      mydata <- filedata()
      mydata$days <- as.Date(as.character(mydata$LastTrxn.Date), format="%d/%m/%Y") - as.Date(as.character(mydata$FirstTrxn.Date), format="%d/%m/%Y")
      mydata$days <- as.numeric(mydata$days)
      df <- mydata[,c(1,2,3,4,5,6,7,10,13)]
      cor(df,use="complete")
      
  }
})
  
output$cor <- renderTable(
  if(is.null(filedata())){
    return()
  }
  else{
    mydata <- filedata()
    
   df <- mydata %>% select_(input$var1, input$var2) 

   cor(df)
  }
)

output$text1 <- renderText(
  paste("Correlation between", input$var1, "&", input$var2, "is")
) 

output$text2 <- renderText(
  paste("Correlation Matrix")
)

output$text3 <- renderPlotly(
  if(is.null(filedata())){
    return()
  }
  else{
    mydata <- filedata()
    mydata$days <- as.Date(as.character(mydata$LastTrxn.Date), format="%d/%m/%Y") - as.Date(as.character(mydata$FirstTrxn.Date), format="%d/%m/%Y")
    mydata$days <- as.numeric(mydata$days)
    x <- input$var4
    x <- as.numeric(x)
     mydata2 <- filter(mydata, days>x) %>% select(days) %>% unlist 
     qplot(mydata2,
          geom="histogram",
          binwidth = 1,  
          main = "Frequency of # of Trxn", 
          xlab = "# of Trxn", 
          fill=I("blue"), 
          col=I("red"))
    }
)

output$rel <- renderPlotly({
    if(is.null(filedata())){
      return()
    }
    else{
      mydata <- filedata()
      p <- input$var3
      x <- button()
      x <- as.numeric(x)
      mydata2 <- filter(mydata, X..of.Trxn > x)  %>% select(X..of.Trxn) %>% unlist
      qplot(mydata2,
            geom="histogram",
            binwidth = 0.5,  
            main = "Frequency of # of Trxn", 
            xlab = "# of Trxn", 
            fill=I("blue"), 
            col=I("red"))
    }})
    
output$analysis1 <- renderText({
  if(is.null(filedata())){
    return()
  }
  else{
       mydata <- filedata()
      mydata2 <- filter(mydata, X..of.Trxn > 0) %>% select(X..of.Trxn) %>% unlist
      mydata3 <- filter(mydata, X..of.Trxn > round(mean(mydata2))) %>% select(X..of.Trxn) %>% unlist
      paste("If we exclude the customers who didn't use our app for any transactions then we are left with only ", length(mydata2), "customers. After this exclusion the average number of transactions per person are roughly" , round(mean(mydata2),2), "so if we calculate only those customers who are doing more than", round(mean(mydata2)), "transactions then it counts to only", length(mydata3))
    }
  })
    
output$table1 <- renderDataTable({
   x <- button()
   x <- as.numeric(x)
   mydata <- filedata()
   mydata2 <- filter(mydata, X..of.Trxn >= x) %>% select(user_id, X..Followers, FirstTrxn.Date, LastTrxn.Date, X..of.Trxn)
   print(mydata2)
   }
)

output$table2 <- renderDataTable({
  if(is.null(filedata())){
    return()
  }
  else{
   x <- input$var4
  x <- as.numeric(x)
  mydata <- filedata()
  mydata$days <- as.Date(as.character(mydata$LastTrxn.Date), format="%d/%m/%Y") - as.Date(as.character(mydata$FirstTrxn.Date), format="%d/%m/%Y")
  
  mydata2 <- filter(mydata, days >= x) %>% select(user_id, X..Followers, FirstTrxn.Date, LastTrxn.Date, X..of.Trxn)
  print(mydata2) }
}
)

output$test2 <- renderText({
  print("Mean")
})
}

shinyApp(ui=ui, server=server)
  
