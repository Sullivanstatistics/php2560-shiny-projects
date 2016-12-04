library(choroplethr)
library(choroplethrMaps)
library(mapproj)
library(ggplot2)
library(dplyr)
library(shiny)
library(shinythemes)

load("/home/jcochancela/food/working draft/ddata/foods.RDA")
load("/home/jcochancela/food/working draft/ddata/df_pop_state.RDA")
names(df_pop_state)=c("state", "pop")


# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  theme = shinytheme("slate"),
  # Application title
  titlePanel("Statewide"),
  
  tabsetPanel(              
    tabPanel(title = "Intro",
             navlistPanel(tabPanel(title = "Motivation",
                                   textOutput("Motivation")),
                          tabPanel(title = "How to Use",
                                   textOutput("HowTo")),
                          tabPanel(title = "About the Authors",
                                   textOutput("Authors")))),
    
    tabPanel(title = "Tutorial",
             tabsetPanel(
               tabPanel(title = "Sample Data", dataTableOutput("foodstb")), ### here we put foods data
               tabPanel(title = "Sample Analysis", sidebarLayout(
                 # Sidebar with a slider input for Year 
                 sidebarPanel(
                   sliderInput("Year",
                               min = min(foods$year),
                               max = max(foods$year),
                               #### Problem: in server, when using "Year" to subset 
                               #### foods, is it 2005 and 2008, instead of 2005 to 2008
                               value = c(min(foods$year),max(foods$year)), 
                               sep="",
                               label = "Choose a year"),
                   selectInput("Variable",
                               label = "Graphic Variable", 
                               choices = names(foods[,sapply(foods, is.numeric)]),
                               selected = "illnesses"),
                   selectInput("Statistic",
                               label = "Graphic Statistic", 
                               choices = c("sum", "mean"),
                               selected = "mean"),
                   selectInput("Table_Var",
                               label = "Numeric Table Variable",
                               choices = names(foods[,sapply(foods, is.numeric)]),
                               selected = "hospitalizations"),
                   selectInput("Table_Var2",
                               label = "Categorical Table Variable",
                               choices = names(foods[,sapply(foods, is.character)]),
                               selected = "state"),
                   selectInput("Table_Stat",
                               label = "Table Statistic",
                               choices = c("sum", "mean"),
                               selected = "sum")
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(fluidRow(plotOutput("map")),
                           #button to save plot and map
                           fluidRow(column(width = 5, downloadButton('downloadPlot', 'Download Plot'),
                                    column(width = 5, downloadButton('downloadMap', 'Download Map'), style="float:right")),
                          #outputplot and table side by side
                           fluidRow(column(width = 8, plotOutput("plot1")),
                                    column(width = 4, dataTableOutput("table"))
                        ))
                 ))))),
    
    #### http://shiny.rstudio.com/gallery/file-upload.html
    tabPanel(title = "Upload and View Data",
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 tags$hr(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                 radioButtons('quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"')
               ),
               mainPanel(dataTableOutput("Table")
               ))),
    
    tabPanel(title = "Analysis")
  )
)
)


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  #create reactive plot 
  
  plotInput <- reactive({
    food_year <- subset(foods, year >= input$Year[1] & year <= input$Year[2])
    
    illMeanByYearMonth <- food_year %>% group_by(month, year) %>% summarise_each(funs(mean), illnesses)    
    ##http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
    simpleCap <- function(x) {
      s <- strsplit(x, " ")[[1]]
      paste(toupper(substring(s, 1,1)), substring(s, 2),
            sep="", collapse=" ")
    }
    
    plotTitle <- simpleCap(paste(input$Statistic, input$Variable, "per month in selected years"))
    ylab <- simpleCap(paste(input$Statistic, input$Variable))
    
    hist <- ggplot(illMeanByYearMonth, aes(as.factor(month), illnesses, 
                                           color=year)) + geom_jitter() + geom_smooth(aes(group=year),se=FALSE) + 
      scale_x_discrete(labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", 
                                  "4" = "Apr", "5" = "May", "6" = "June", "7" = "July", "8" = "Aug", 
                                  "9" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec")) + xlab("Month") + 
      ylab(ylab) + ggtitle(plotTitle)
    
    
    hist
    
  })
  #call it plot1
  output$plot1 <- renderPlot({
    print(plotInput())
  })
  # code to download plot
  output$downloadPlot <- downloadHandler(
    filename = function() { paste('PlotOf ',input$Variable, input$Statistic,input$Year[1],'to',input$Year[2], '.png', sep='') },
    content = function(file) {
      ggsave(file,plotInput())
    }
  )
  
  output$table <-renderDataTable({
    food_year <- subset(foods, year >= input$Year[1] & year <= input$Year[2])
    formula<-paste(input$Table_Var, "~", input$Table_Var2)
    tab<-as.data.frame(aggregate(as.formula(formula), data = food_year, input$Table_Stat))
    tab
    }, options = list(pageLength = 10))
  
  ############################### FOR TUTORIAL###################################### 
  
  mapInput <- reactive({
    # generate map based on input$year from ui.R
    # http://stackoverflow.com/questions/38181744/r-shiny-input-slider-range-values
    food_year <- subset(foods, year >= input$Year[1] & year <= input$Year[2])
    #food_year <- gsub("DC", "district of columbia", food_year$state)
    formula<-paste(input$Variable, "~", "state")
    byState <- aggregate(as.formula(formula), data = food_year, input$Statistic)
    
    ## change all state names to lowercase
    byState$state<-tolower(byState$state)
    
    ## subsetting states that is coded in the mapping function
    sameStates<-byState$state %in% df_pop_state$state
    byStatewk<-byState[sameStates,]
    
    ## change col name from "state" to "region"
    names(byStatewk)<-c("region",input$Variable)
    
    byStatewk$value = byStatewk[,input$Variable]
    
    state_choropleth(byStatewk, title = "The Good Ol' US of A", num_colors = 8)
    
  })
  
  output$map <- renderPlot({
    print(mapInput())
  })
  # how to downlod images
  #http://stackoverflow.com/questions/14810409/save-plots-made-in-a-shiny-app
  output$downloadMap <- downloadHandler(
    filename = function() { paste('MapOf',input$Variable, input$Statistic,input$Year[1],'to',input$Year[2], '.png', sep='') },
    content = function(file) {
      ggsave(file,mapInput())
    }
  )
  
  
  
  ############################### W/ DEFAULT DATA "FOOD" ############################### 
  
  output$Motivation<-renderText("Statewide is more than a shiny app - it is a tool to analyze and 
                                visualize any dataset collected in the scope of the United States. That is, for any data that 
                                includes state as a variable, a user can choose their variables of interest and display neat 
                                and succinct data visualization as well as run an analysis of their choice on the response.
                                The idea for Statewide came to us as we worked with the CDC foodborne illnesses dataset1, which 
                                we refer to as foods. Foods includes occurrences of foodborne illnesses, year and month, state, 
                                number of illnesses, number of deaths, location of preparation, etc. Our goal is to inform policy 
                                makers of trends regarding foodborne illnesses. In this manner, the CDC can allocate funds 
                                accordingly to states and fund preventative measures limiting preventable such illnesses and 
                                deaths.")
  output$HowTo<-renderText("")
  output$Authors<-renderText("...we are really cool ...")
  
  #### http://shiny.rstudio.com/gallery/file-upload.html
  output$Table <- renderDataTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    inFile
  })
  #### http://shiny.rstudio.com/articles/datatables.html
  #### http://shiny.rstudio.com/articles/datatables.html
  output$foodstb = renderDataTable({
    foods
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)
