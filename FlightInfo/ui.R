library(shiny)

shinyUI(fluidPage(
  # Application title
  navbarPage(
    "Flight Information",
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(("Enter the departure city"),
                   selectInput(inputId = "departure_state","Select Depearture State", list("State"= c(as.vector(state.name)))),
                   textInput("dcity", label = h3("DepartureCity"), value = "New York City, NY"),
                   selectInput(inputId = "arrival_state","Select Arrival State", list("State"= c(as.vector(state.name)))),
                   textInput("acity", label = h3("ArrivalCity"), value = "San Francisco, CA"),
                   selectInput(inputId = "year","Select Year",list("Year"= c("2011","2012","2013","2014","2015"))),
                   selectInput(inputId = "quarter","Select Quarter",list("Quarter"= c("Q1","Q2","Q3","Q4")))
                  ),
      #Enter city name like Los Angelos
      
      #mainPanel(plotOutput("stat_depart_city_out"))))
      mainPanel(
        tabsetPanel(
        tabPanel("Trend of Fare Change",
        list(plotOutput("stat_depart_city_out1"),plotOutput("stat_depart_city_out2"),
                     plotOutput("stat_depart_city_out34"),plotOutput("city_out"),
                     plotOutput("stat_depart_city_out_fare"))),
        tabPanel("Help",h2(helpText('Tutorial (draft):')),
                 span(helpText("This FLIGHT shiny-app allows users to select the departure city and arrival city they are interested in, and to monitor the seasonal price fluctuation between places. Our goal is to assist users to find the possible lower average flight fare during different quarters in a year (or across different years) and to plan out their travel schedules ahead of the time.")),
                 h4(helpText('Data source:')),
                 span(helpText("Each year, the Department of Transportation releases a report provided information about average prices being paid by consumers in the 1,000 largest domestic city-pair markets within the 48 contiguous states. These markets account for approximately 75 percent of all 48 contiguous state passengers and 70 percent of total domestic passengers. This shiny app extracts the table 6 from this report that lists all city-pair markets that average at least ten passengers each day.")),
                 h4(helpText('Time Trend vs. Price Analysis:')),
                 span(helpText("Users could enter the departure and arrival city to monitor the flight fare. By choosing from the year of 2011 through 2015 and each quarter in a year, the app generates five descriptive graphs that specifies the price information that users might concern about.")),
                 h6(helpText('Graph description::')),
                 span(helpText("Graph 1: Graph one demonstrates the average fare for selected departure cities. ")),
                 span(helpText("Graph 2: Graph two shows the average passenger numbers for selected departure cities. ")),
                 span(helpText("Graph 3: Graph three specifies the relationship between travel miles and airline price as well as the relationship between numbers of travellers and the airline fare.")),
                 span(helpText("Graph 4: Graph four lays out the route(s) with various price range in different color(s) from departure cities to arrival cities. ")),
                 span(helpText("Graph 5: Graph five presents quarterly airline fare change in the given year."))
                 )
        )
       )
      )
    )
  )
)