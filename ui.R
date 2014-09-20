library(shiny)
library(ggplot2)




# Define UI for application that plots random distributions 

shinyUI(pageWithSidebar(
  
  titlePanel("Welcome to the Energy VisualizeR!",              
              tags$head(
                tags$img(src="shiny_logo.png", height = 80, width = 80, align = "right")                      
            )          
  ),




  sidebarPanel(
 
    

           h3("This is a simple ShinyApp for the visualisation of Energy meters' data"),

           
           h5(strong("Usage:")),#, style = "color:red"),





h5("1) Enter the power of your PV production and your yearly electricity consumption (without heating neither hot water) to compute your rate of self-consumption!"),
fluidRow(
  
  column(6,
         numericInput("input_PV_kWp", label = h6("Enter the peak power of your photovoltaic [kWp] (e.g. 6000)"), value = 6000)
         #   verbatimTextOutput("PV_kWp")
  ),
  column(5,
         numericInput("input_kWh",    label = h6("Enter your yearly electricity consumption [kWh] (e.g. 4000)"), value = 4000)
         #    verbatimTextOutput("kWh")
  )  
),# end fluidrow


verbatimTextOutput("textEVO"),


   h5("2) Choose a day (data currently available from 2013-02-24 to 2014-06-03) or click on the play button: (warning: dates available are not the same for all energy meters!)"),
   


fluidRow(
  column(4,

      sliderInput("day_in_year",
                  #"Choose a day (data currently available from 2013-02-24 to 2014-06-03) or click on the play button:",
                  "",
                  min = 1,
                  max = 465,# limit data choice from from 2013-02-24 to 2014-06-03 (otherwise have no data...)
         #         max = max_nb_days,# limit data choice from from 2013-02-24 to 2014-06-03 (otherwise have no data...)
                  value = 1,
                  animate=TRUE
  #              animate=animationOptions(interval=10, loop=FALSE) #too fast for the server...
                  )
  )
),
  
h5("3) [Optional] Change the profiles by choosing different energy meters. Energy meters are classified by their loads. 
   DO NOT change energy meters while the function play is running through the days (confuses R Shiny), stop the video first!
   Because energy meters were installed at different times at clients' homes, data are not available for all dates for any energy meters (choose another energy meter if the graph show constantly zero for the selected load)"),

        
  fluidRow(
    column(6,
                 
      selectInput("select_Flag", label = "Select type of load:", 
            choices = list( "House"="House", "House heat pump"="House_WP", "Hot water heat pump"="House_WWWP"),        
            selected = "House"),
      
      selectInput("select_Flag2", label = "Select type of 2nd load:", 
             choices = list("Null"="Null", "House heat pump"="House_WP", "Hot water heat pump"="House_WWWP"),  
             selected = "House_WP"),   
      
      
      selectInput("select_Flag3", label = "Add a photovoltaic profile:", 
                  choices = list("Null"="Null","PV"="PV"),                                                                                    
                  selected = "PV")
      
  
    ),
    column(5,
       
        selectInput("select_MessPunkt", label = "Choose a measurement point:", 
                    choices = NULL, 
                    selected = NULL), # empty for now, will be updated in server.R
        #   http://stackoverflow.com/questions/25321462/how-to-use-shiny-conditional-panel/25321866#25321866
         
          
        selectInput("select_MessPunkt2", label = "Choose a 2nd measurement point:", 
                    choices = NULL, 
                    selected = NULL), # empty for now, will be updated in server.R
        #   http://stackoverflow.com/questions/25321462/how-to-use-shiny-conditional-panel/25321866#25321866
        
        
        selectInput("select_MessPunkt3", label = "Choose a 3rd measurement point:", 
                    choices = NULL, 
                    selected = NULL) # empty for now, will be updated in server.R
        #   http://stackoverflow.com/questions/25321462/how-to-use-shiny-conditional-panel/25321866#25321866       
    ))



    ),
  

  
  
 
 
 mainPanel( 
  plotOutput("Plot", height="auto") # automatic rescaling of the plot according to windows' size! (together with height=function() { session$clientData$output_Plot_width * 0.7 }) in server.R)
  )
 
 

))
