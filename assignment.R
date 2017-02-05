library(dplyr)
library(ggplot2)
library(shiny)
#install.packages("plotly")
library(plotly)

data <- read.csv("./data.csv" , sep = ";", header = TRUE)

date <- unique(factor(data$SelfieDate))
date_sort <- sort.int(date, decreasing = FALSE)
 
n <- length(unique(factor(data$SelfieDate))) # number of days
m <- length(data$Bill.Size..Rs.)
trxnPerDay_All <- vector("numeric", n)
trxnPerDay_National <- vector("numeric", n)
trxnPerDay_Funded <- vector("numeric", n)
trxnPerDay_Local <- vector("numeric", n)
data$Bill.Size..Rs. <- as.numeric(data$Bill.Size..Rs.)

cashbackPerDay_All <- vector("numeric", n)
cashbackPerDay_National <- vector("numeric", n)
cashbackPerDay_funded <- vector("numeric", n)
cashbackPerDay_Local <- vector("numeric", n)
NewcashbackPerDay <- vector("numeric", n)
NewcashbackPerDayl <- vector("numeric", n)
NewcashbackPerDayf <- vector("numeric", n)
NewcashbackPerDayn <- vector("numeric", n)
for(i in 1:n){
  trxn_national <- filter(data, SelfieDate==date_sort[i], Status=="National") %>% select(Bill.Size..Rs.) %>% unlist
  p1 <- as.numeric(trxn_national)
  trxnPerDay_National[i] <- sum(p1)
}
for(i in 1:n){
  trxn_local <- filter(data, SelfieDate==date_sort[i], Status=="Local") %>% select(Bill.Size..Rs.) %>% unlist
  p2 <- as.numeric(trxn_local)
  trxnPerDay_Local[i] <- sum(p2)
}
for(i in 1:n){
  trxn_funded <- filter(data, SelfieDate==date_sort[i], Status=="funded") %>% select(Bill.Size..Rs.) %>% unlist
  p3 <- as.numeric(trxn_funded)
  trxnPerDay_Funded[i] <- sum(p3)
}
for(i in 1:n){
  trxn_All <- filter(data, SelfieDate==date_sort[i]) %>% select(Bill.Size..Rs.) %>% unlist
  p4 <- as.numeric(trxn_All)
  trxnPerDay_All[i] <- sum(p4)
}


for(i in 1:n){
  cashback_national <- filter(data, SelfieDate==date_sort[i], Status=="National") %>% select(Final.Cashback..Rs.) %>% unlist
  cashbackPerDay_National[i] <- sum(cashback_national)
}

for(i in 1:n){
  cashback_local <- filter(data, SelfieDate==date_sort[i], Status=="Local") %>% select(Final.Cashback..Rs.) %>% unlist
  cashbackPerDay_Local[i] <- sum(cashback_local)
}

for(i in 1:n){
  cashback_funded <- filter(data, SelfieDate==date_sort[i], Status=="funded") %>% select(Final.Cashback..Rs.) %>% unlist
  cashbackPerDay_funded[i] <- sum(cashback_funded)
}

for(i in 1:n){
  cashback_all <- filter(data, SelfieDate==date_sort[i]) %>% select(Final.Cashback..Rs.) %>% unlist
  cashbackPerDay_All[i] <- sum(cashback_all)
}

data["newSaving"] <- NA
 
cashback_x1 <- max(filter(data, Status=="National") %>% select(Final.Cashback..Rs.) %>% unlist)
c1  <- filter(data, Status=="National") %>% select(Bill.Size..Rs.) %>% unlist
f1 <- filter(data, Status=="National") %>% select(Final.Cashback..Rs.) %>% unlist
cashback_y1 <- round(median(f1/c1)*100, 2)


cashback_x2 <- max(filter(data, Status=="Local") %>% select(Final.Cashback..Rs.) %>% unlist)
c2  <- filter(data, Status=="Local") %>% select(Bill.Size..Rs.) %>% unlist
f2 <- filter(data, Status=="Local") %>% select(Final.Cashback..Rs.) %>% unlist
cashback_y2 <- round(median(f2/c2)*100, 2)

cashback_x3 <- max(filter(data, Status=="funded") %>% select(Final.Cashback..Rs.) %>% unlist)
c3  <- filter(data, Status=="funded") %>% select(Bill.Size..Rs.) %>% unlist
f3 <- filter(data, Status=="funded") %>% select(Final.Cashback..Rs.) %>% unlist
cashback_y3 <- round(median(f3/c3)*100, 2)

ui <- fluidPage(
  titlePanel("Analysis of CashBack across different outlets"),
  h3("Input"),
  sidebarLayout(
    
    sidebarPanel(
      selectInput("OutletInput", "Outlet", choices= c("National", "Local", "Funded", "Cummulative"), selected = "Cummulative"),
      numericInput("n1", "CashBack ( X %)", 10),
      numericInput("n2", "CashBack (UpperCap)", 100),
      actionButton("b1", "Submit")
    ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Info", verbatimTextOutput("summary"), verbatimTextOutput("summary2")),
      tabPanel("Graphical Analysis" , plotlyOutput("plot"), plotlyOutput("plot2")),
      tabPanel("Model" , plotlyOutput("plot3"))
      ) 
  )
  ))

server <- function(input, output, session){
  
dot_national <- reactive({
temp <- as.numeric(data$Bill.Size..Rs.)
  for(i in 1:m){
    if((temp[i]*input$n1/100) > input$n2)
      data$newSaving[i] <- input$n2
  else
    data$newSaving[i] <- (temp[i]*input$n1/100)
  }
  for(i in 1:n){
    Newcashback <- filter(data, SelfieDate==date_sort[i], Status == "National") %>% select(newSaving) %>% unlist
    NewcashbackPerDayn[i] <- sum(Newcashback)
  }
  return(NewcashbackPerDayn)

})

dot_local <- reactive({
  temp <- as.numeric(data$Bill.Size..Rs.)
  for(i in 1:m){
    if((temp[i]*input$n1/100) > input$n2)
      data$newSaving[i] <- input$n2
    else
      data$newSaving[i] <- (temp[i]*input$n1/100)
  }
  for(i in 1:n){
    Newcashback <- filter(data, SelfieDate==date_sort[i], Status == "Local") %>% select(newSaving) %>% unlist
    NewcashbackPerDayl[i] <- sum(Newcashback)
  }
  return(NewcashbackPerDayl)
  
})
  
dot_funded <- reactive({
  temp <- as.numeric(data$Bill.Size..Rs.)
  for(i in 1:m){
    if((temp[i]*input$n1/100) > input$n2)
      data$newSaving[i] <- input$n2
    else
      data$newSaving[i] <- (temp[i]*input$n1/100)
  }
  for(i in 1:n){
    Newcashback <- filter(data, SelfieDate==date_sort[i], Status == "funded") %>% select(newSaving) %>% unlist
    NewcashbackPerDayf[i] <- sum(Newcashback)
  }
  return(NewcashbackPerDayf)
})

dot <- reactive({
  temp <- as.numeric(data$Bill.Size..Rs.)
  for(i in 1:m){
    if((temp[i]*input$n1/100) > input$n2)
      data$newSaving[i] <- input$n2
    else
      data$newSaving[i] <- (temp[i]*input$n1/100)
  }
  for(i in 1:n){
    Newcashback <- filter(data, SelfieDate==date_sort[i]) %>% select(newSaving) %>% unlist
    NewcashbackPerDay[i] <- sum(Newcashback)
  }
  return(NewcashbackPerDay)
  
})
a <- eventReactive(input$b1,{
      
  if ("National" %in% input$OutletInput){
    temp1 <- dot_national()
    return(temp1)
  }
  else
    if("Local" %in% input$OutletInput){
    temp1 <- dot_local()
    return(temp1)
    }
  else
    if("funded" %in% input$OutletInput){
      temp1 <- dot_funded()
      return(temp1)
    }
  else
    if("Cummulative" %in% input$OutletInput){
      temp1 <- dot()
      return(temp1)
    }
  })
 
  barplottest <- reactive({
    if ( "National" %in% input$OutletInput) return(cashbackPerDay_National)
    if ( "Local" %in% input$OutletInput) return(cashbackPerDay_Local)
    if ( "Funded" %in% input$OutletInput) return(cashbackPerDay_funded)
    if ( "Cummulative" %in% input$OutletInput) return(cashbackPerDay_All)
    
  })
  
  barplottest2 <- reactive({
    if ( "National" %in% input$OutletInput) return((cashbackPerDay_National)/trxnPerDay_National)
    if ( "Local" %in% input$OutletInput) return((cashbackPerDay_Local)/trxnPerDay_Local)
    if ( "Funded" %in% input$OutletInput) return((cashbackPerDay_funded)/trxnPerDay_Funded)
    if ( "Cummulative" %in% input$OutletInput) return((cashbackPerDay_All)/trxnPerDay_All)
    
  })
  
  barplottest3 <- reactive({
    a()
    
  })
  
  output$plot <- renderPlotly({
    dataplots = barplottest()
    qplot(date_sort, dataplots, main = "CashBack Distribution", xlab="Date", ylab="CashBack")
  })
  
  output$plot2 <- renderPlotly({
    dataplots2 <-  barplottest2()
    qplot(date_sort, dataplots2, main = "CashBack per Expenditure Distribution", xlab="Date", ylab="CashBack/Expenditure")
  })
  
  output$plot3 <- renderPlotly({
    dataplots3 <- barplottest3()
    qplot(date_sort, dataplots3, main = "Variation in Cashback by changing X & Y", xlab="Date", ylab="CashBack")
  })

  datasetInput <- reactive({
    switch(input$OutletInput,
           "National" = filter(data, Status=="National") %>% select(Funding.Status ,Final.Cashback..Rs. ),
           "Local" = filter(data, Status=="Local") %>% select(Funding.Status ,Final.Cashback..Rs. ),
           "Funded" = filter(data, Status=="funded") %>% select(Funding.Status ,Final.Cashback..Rs. ),
           "Cummulative" = data %>% select(Funding.Status, Final.Cashback..Rs. ))
  })
  
  datasetInput2 <- reactive({
    switch(input$OutletInput,
           "National" = paste("Upper Cap of CashBack for National Outlet is",cashback_x1, "And Value of X is ", cashback_y1),
           "Local" =  paste("Upper Cap of CashbackBack for Local Outlet is", cashback_x2,"And Value of X is ", cashback_y2),
           "Funded" = paste("Upper Cap of CashbackBack for Funded Outlet is", cashback_x3,"And Value of X is ", cashback_y3),
           "Cummulative" = paste("NA"))
  })
  
  output$summary <- renderPrint({
    dat <- datasetInput()
    summary(dat)
  })
  
  output$summary2 <- renderPrint({
    dat2 <- datasetInput2()
    print(dat2)
  })
  
}

shinyApp(ui=ui, server=server)

