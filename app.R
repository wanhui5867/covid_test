#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse) 
library(lubridate)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Covid test - PHA group"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput("Name",
                     "Your full name: FirstName LastName",
                   value = 'Hui Wan'),
         
         dateInput("Date",
                   "Choose the sampling date",
                   value = Sys.Date() - 10 ,
                   format = 'mm/dd/yy'),
         
         submitButton(text = 'Search')
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         textOutput("exist"),
         tableOutput("result")
      )
   )
)

# Define server logic required to show the result
server <- function(input, output) {
  

  
  # read our result
  fileData <- read.table('data/file.xls', sep='\t',header = T, check.names = F)
 
   Text <- reactive({
     # check the Name and Date is right
     if (! input$Name %in%  fileData$Name) {
       return("Sorry, We did not find your name. If your input name is right, then it is our slow speed. Usually the test costs two weeks.")
     } else if (! input$Date %in% as.Date(fileData$Date[fileData$Name == input$Name ])) {
       #return(input$Date)
       return(paste("Sorry, your input date is wrong. You donated in: ", fileData$Date[fileData$Name == input$Name ], " Please choose the right date ", input$Date) )
     } else {
       return("Thanks for your donation. Here are your test result: ")
     } 
     
     
   })
  
   output$exist <- renderText({ Text()    })
   
   
   outData <- reactive({
     output = fileData %>% filter(Name == input$Name, Date == input$Date) %>% select(Date,IgG:IgM)
     return(output)
     #return(fileData)
   })
    
   output$result <- renderTable({
      outData()
     #fileData
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

runUrl( "https://test.covid.test.com")
