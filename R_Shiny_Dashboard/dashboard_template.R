library(shiny)
library(shinydashboard)
library(DT)
datasets <- read.csv(file = "german_credit_data.csv",header =TRUE)


ui <- dashboardPage(
  dashboardHeader(title="Assignment 2"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Problem 1",tabName = "Problem1"),
      menuItem("Problem 2",tabName = "Problem2")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("Problem1",h1("Problem 1: Build Simulator"),
              tabsetPanel(
                tabPanel("Gamma",
                         fluidRow(column(6,numericInput("simsize1","Simulation Size",value = 1000)),
                                  column(6,numericInput("samplesize1","Sample Size",value = 30)),
                         ),
                         fluidRow(column(4,numericInput("mean1","Population Mean",value = 20)),
                                  column(6,numericInput("sd1","Standard Deviation of Population",value = 5)),
                                  column(2,actionButton("go1","Action Button"))
                         ),
                         
                         fluidRow(plotOutput("plot1")),
                         fluidRow(plotOutput("plot2")),
                         fluidRow(textOutput("test1"))
                ),
                tabPanel("Uniform",fluidRow(column(6,numericInput("simsize2","Simulation Size",value = 1000)),
                                            column(6,numericInput("samplesize2","Sample Size",value = 30)),
                ),
                fluidRow(column(4,numericInput("mean2","Population Mean",value = 20)),
                         column(6,numericInput("sd2","Standard Deviation of Population",value = 5)),
                         column(2,actionButton("go2","Action Button"))
                ),
                
                fluidRow(plotOutput("plot3")),
                fluidRow(plotOutput("plot4")),
                fluidRow(textOutput("test2"))),)),
              
      tabItem("Problem2",h2("Problem 2: Data Visualisation"),
              tabsetPanel(
                tabPanel("Task1",
                         fluidRow(selectInput("variable", "Variable:",
                                              c("Sex" ,
                                                "Housing",
                                                "Saving.accounts",
                                                "Checking.account",
                                                "Purpose"
                                              )),
                         ),
                         fluidRow(actionButton("go3","Show")),
                         
                         fluidRow(plotOutput("plot5")),
                         fluidRow(plotOutput("plot6")),
                         
                ),
                tabPanel("Task2",
                         fluidRow(selectInput("variable1", "Variable:",
                                              c("Sex",
                                                "Housing" ,
                                                "Saving.accounts",
                                                "Checking.account",
                                                "Purpose"
                                              )),
                         ),
                         fluidRow(selectInput("variable2", "Variable:",
                                              c("Age" ,
                                                "Credit.amount",
                                                "Duration" = "Saving.accounts"
                                              )),
                         ),
                         fluidRow(actionButton("go4","Show")),
                         
                         fluidRow(plotOutput("plot7")),
                )
              )
              )
      )
              ),
      
              
        
    )


server <- function(input,output){
  observeEvent(input$go1,
               {output$plot1 <- renderPlot({
                 data1 = array(0)
                 for(i in 1:isolate(input$simsize1))
                 {
                   data1[i]= isolate({mean(rgamma((input$samplesize1), shape = (input$mean1)**2/(input$sd1)**2, scale = (input$sd1)**2/(input$mean1)))})
                 }
                 
                 hist( data1,main="Histogram of sample means")
               })
               output$plot2 <- renderPlot({
                 data1 = array(0)
                 for(i in 1:isolate(input$simsize1))
                 {
                   data1[i]= isolate({mean(rgamma((input$samplesize1), shape = (input$mean1)**2/(input$sd1)**2, scale = (input$sd1)**2/(input$mean1)))})
                 }
                 qqnorm(data1)
                 qqline(data1)
               })
               output$test1 <- renderPrint({
                 data1 = array(0)
                 for(i in 1:isolate(input$simsize1))
                 {
                   data1[i]= isolate({mean(rgamma((input$samplesize1), shape = (input$mean1)**2/(input$sd1)**2, scale = (input$sd1)**2/(input$mean1)))})
                 }
                 ks.test(data1,"pnorm",mean = mean(data1),sd= sd(data1))
               })
               }
               
  )
  observeEvent(input$go2,
               {output$plot3 <- renderPlot({
                 data2 =array(0)
                 for(i in 1:isolate(input$simsize2))
                 {
                   data2[i]= isolate({mean(runif((input$samplesize2), min = (input$mean2)-(input$sd2)*sqrt(3), max = (input$mean2)+ (input$sd2)*sqrt(3)))})
                 }
                 
                 
                 hist( data2,main="Histogram of sample means")
               })
               output$plot4 <- renderPlot({
                 data2 =array(0)
                 for(i in 1:isolate(input$simsize2))
                 {
                   data2[i]= isolate({mean(runif((input$samplesize2), min = (input$mean2)-(input$sd2)*sqrt(3), max = (input$mean2)+ (input$sd2)*sqrt(3)))})
                 }
                 
                 qqnorm(data2)
                 qqline(data2)
               })
               output$test2 <- renderPrint({
                 data2 =array(0)
                 for(i in 1:isolate(input$simsize2))
                 {
                   data2[i]= isolate({mean(runif((input$samplesize2), min = (input$mean2)-(input$sd2)*sqrt(3), max = (input$mean2)+ (input$sd2)*sqrt(3)))})
                 }
                 
                 ks.test(data2,"pnorm",mean = mean(data2),sd= sd(data2))
               })
               }
               
               
  )
  observeEvent(input$go3,
               {output$plot5 <- renderPlot({
                 v = isolate(input$variable)
                 if(v == "Sex")
                 {
                   pie(table(datasets$Sex))
                 }
                 else if(v == "Housing")
                 {
                   pie(table(datasets$Housing))
                 }
                 else if (v == "Saving.accounts")
                 {
                   pie(table(datasets$Saving.accounts))
                 }
                 else if (v == "Checking.account")
                 {
                   pie(table(datasets$Checking.account))
                 }
                 else if ( v == "Purpose")
                 {
                   pie(table(datasets$Purpose))
                 }
                 
               })
               output$plot6 <- renderPlot({
                 library(ggplot2)
                 ggplot(datasets, aes(x=get(input$variable)))+geom_bar()
               })
               
               }
               
  )
  observeEvent(input$go4,
               output$plot7<- renderPlot({
                 
                 boxplot(get(input$variable2) ~  get(input$variable1) ,data = datasets)
                 
               }))
  
  
  
  
}

shinyApp(ui,server)