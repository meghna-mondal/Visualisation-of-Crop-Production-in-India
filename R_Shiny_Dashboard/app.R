library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

crop_prod<-read.csv("datafile (2).csv")
crop_cost<-read.csv("datafile (1).csv")

ui <- dashboardPage(
  dashboardHeader(title="Crop Production India"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Description", tabName = "dd"),
      menuItem("Cost of Production",tabName = "Crop"),
      menuItem("Amount of Production",tabName = "Amount")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dd",h1("Data Description"),
              h4("India's agriculture is composed of many crops, with the foremost food staples being rice and wheat.
Indian farmers also grow pulses, potatoes, sugarcane, oilseeds, and such non-food items as cotton, tea, coffee, rubber, and jute.
Despite the overwhelming size of the agricultural sector, however, yields per hectare of crops in India are generally low compared to international standards.
So, here we wish to analyse more about the crop cultivation, cost invested in cultivation in different parts of our country.
Now we consider 2 datasets from the Kaggle dataset on Agricultural Crop Production in India (source:  https://www.kaggle.com/srinivas1/agricuture-crops-production-in-india/version/1?select=datafile+%282%29.csv ) namely datafile (1).csv and datafile (2).csv containing different variables described below.
"),tableOutput("Table1"),tableOutput("Table2")),
      tabItem("Crop",h1("Cost of Production"),
              tabsetPanel(
                tabPanel("Univariate",h2("Univariate Analysis of Cost of Production "),
                         fluidPage(
                           sidebarLayout(
                             sidebarPanel(
                               fluidRow(selectInput("y1","Select the Variable:",
                                                    c("Cost.of.Cultivation....Hectare..A2.FL",
                                                      "Cost.of.Cultivation....Hectare..C2",
                                                      "Cost.of.Production....Quintal..C2",
                                                      "Yield..Quintal..Hectare."))),
                               fluidRow(selectInput("x1","Select the Attribute:",
                                                    c("Crop",
                                                      "State"))),
                               
                               fluidRow(actionButton("go1","Show")),
                               width =5
                             ),
                             
                             mainPanel(plotOutput("plot1"),
                                       width = 7)
                           )
                         )
                ),
                tabPanel("Multivariate",h2("Multivariate Analysis of Cost of Production "),
                         fluidPage(
                           sidebarLayout(
                             sidebarPanel(
                               fluidRow(selectInput("y2","Select the Variable:",
                                                    c("Cost.of.Cultivation....Hectare..A2.FL",
                                                      "Cost.of.Cultivation....Hectare..C2",
                                                      "Cost.of.Production....Quintal..C2",
                                                      "Yield..Quintal..Hectare."))),
                               
                               fluidRow(actionButton("go2","Show")),
                               width =5
                             ),
                             
                             mainPanel(plotOutput("plot2"),
                                       plotOutput("plot5"),
                                       width = 7)
                           )
                         )
                )
              )
      ),
      
      
      tabItem("Amount",h1("Amount of Production"),
              tabsetPanel(
                tabPanel("Univariate",h2("Univariate Analysis of Amount of Production"),
                         fluidPage(
                           sidebarLayout(
                             sidebarPanel(
                               fluidRow(selectInput("y3", "Select a Variable:",
                                                    c("Production.2006.07" ,
                                                      "Production.2007.08",
                                                      "Production.2008.09",
                                                      "Production.2009.10",
                                                      "Production.2010.11",
                                                      "Area.2006.07" ,
                                                      "Area.2007.08",
                                                      "Area.2008.09",
                                                      "Area.2009.10",
                                                      "Area.2010.11",
                                                      "Yield.2006.07" ,
                                                      "Yield.2007.08",
                                                      "Yield.2008.09",
                                                      "Yield.2009.10",
                                                      "Yield.2010.11"
                                                    )),
                               ),
                               fluidRow(actionButton("go3","Show"))
                             ),                                
                             mainPanel(plotOutput("plot3"),
                             )
                           )
                         )                               
                ),
                tabPanel("Multivariate",h2("Multivariate Analysis of Amount of Production"),
                         fluidPage(
                           sidebarLayout(
                             sidebarPanel(
                               fluidRow(selectInput("y4", "Select the Variable:",
                                                    c("Area",
                                                      "Production" ,
                                                      "Yield"
                                                    )),
                               ),
                               
                               fluidRow(actionButton("go4","Show"))
                             ),
                             
                             mainPanel(plotOutput("plot4"))
                           )
                         )
                )
              )
      ))))

server <- function(input,output){
  output$Table1 <- renderTable({
    Variable_name = c("Crop", "State", "Cost.of.Cultivation....Hectare..A2.FL", "Cost.of.Cultivation....Hectare..C2", "Cost.of.Production....Quintal..C2", "Yield..Quintal..Hectare.")
    Variable_type = c("Nominal", "Nominal", "Continuous", "Continuous", "Continuous", "Continuous")
    Variable_content = c("Different types of crops", "States where the crop is cultivated", "Expected cost of cultivation of the crop per Hectare","Cost of cultivation of the crop per Hectare","Cost of Production per Quintal","Yield of crop in Quintal/Hectare")
    Table_1 = data.frame( Variable_name, Variable_type, Variable_content) 
    return(Table_1)})
  output$Table2 <- renderTable({
    Variable_Name = c("Crop", "Production.2006.07", "Production.2007.08", "Production.2008.09", "Production.2009.10", "Production.2010.11", "Area.2006.07", "Area.2007.08", "Area.2008.09", "Area.2009.10", "Area.2010.11", "Yield.2006.07", "Yield.2007.08", "Yield.2008.09", "Yield.2009.10", "Yield.2010.11")
    Variable_Type = c("Nominal", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous")  
    Variable_Content = c("Different types of crops cultivated", "Production of a particular crop in Year 2006-07", "Production of a particular crop in Year 2007-08","Production of a particular crop in Year 2008-09","Production of a particular crop in Year 2009-10","Production of a particular crop in Year 2010-11"
                         , "Area of cultivation of particular crop in Year 2006-07", "Area of cultivation of particular crop in Year 2007-08", "Area of cultivation of particular crop in Year 2008-09", "Area of cultivation of particular crop in Year 2009-10", "Area of cultivation of particular crop in Year 2010-11"
                         , "Yield of a particular crop in Year 2006-07", "Yield of a particular crop in Year 2007-08","Yield of a particular crop in Year 2008-09","Yield of a particular crop in Year 2009-10","Yield of a particular crop in Year 2010-11")
    
    
    Table_2 = data.frame( Variable_Name, Variable_Type, Variable_Content) 
    return(Table_2)})
  observeEvent(input$go1,
                                              {output$plot1 <- renderPlot({
                                                ggplot(crop_cost,aes(y=get(input$y1),x=get(input$x1)	,fill=get(input$x1)	))+geom_bar(stat="identity")+ylab("Y variable")+xlab("Attribute")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Barplot")
                                              })
                                              
                                              }
                                              
)
  observeEvent(input$go2,
               {output$plot2 <- renderPlot({
                 ggplot(crop_cost,aes(y=get(input$y2),x=Crop	,color=State))+geom_point(size=3)+ylab("Cost")+xlab("Crop")+theme(axis.text.x=element_text(angle=90))+ggtitle("Analysis wrt crop and state")
                 
               })
               output$plot5 <- renderPlot({
                 ggplot(crop_cost,aes(y=get(input$y2),x=State,color=State))+geom_point(size=2)+ylab("Cost")+xlab("Crop")+theme(axis.text.x=element_text(angle=90))+ggtitle("Analysis for each crop")+facet_wrap(~Crop	)
                 
               })
               
               }
               
               
  )
  
  observeEvent(input$go3,
               {output$plot3 <- renderPlot({
                 ggplot(crop_prod,aes(y=get(input$y3),x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Y variable")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Bar Plot")
                 
               })
               
               
               }
               
  )
  observeEvent(input$go4,
               output$plot4<- renderPlot({
                 v = isolate(input$y4)
                 if(v == "Production")
                 {
                   Production = data.frame(crop_prod$Production.2006.07,crop_prod$Production.2007.08,crop_prod$Production.2008.09,crop_prod$Production.2009.10,crop_prod$Production.2010.11)
                   P = as.matrix(Production)
                   par(mfrow=c(1,1))
                   barplot(t(P), main="Multiple Bar Diagram of Production in five year for different crop", ylab="y variable", beside=TRUE, 
                           col=terrain.colors(55))
                 }
                 else if(v == "Area")
                 {
                   Area = data.frame(crop_prod$Area.2006.07,crop_prod$Area.2007.08,crop_prod$Area.2008.09,crop_prod$Area.2009.10,crop_prod$Area.2010.11)
                   A = as.matrix(Area)
                   par(mfrow=c(1,1))
                   barplot(t(A), main="Multiple Bar Diagram of Area of cultivation in five year for different crop", ylab="y variable", beside=TRUE, 
                           col=terrain.colors(55))
                 }
                 else if (v == "Yield")
                 {
                   Yield = data.frame(crop_prod$Yield.2006.07,crop_prod$Yield.2007.08,crop_prod$Yield.2008.09,crop_prod$Yield.2009.10,crop_prod$Yield.2010.11)
                   Y = as.matrix(Yield)
                   par(mfrow=c(1,1))
                   barplot(t(Y), main="Multiple Bar Diagram of Yield in five year for different crop", ylab="y variable", beside=TRUE, 
                           col=terrain.colors(55))
                 }
               }))
  
  
}
shinyApp(ui,server)
