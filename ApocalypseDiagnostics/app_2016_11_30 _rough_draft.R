#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages
library(pacman)
pacman::p_load(shiny, shinythemes, colourpicker, ggplot2, ggthemes, ggmap, maps,
               mapproj, gmodels, stargazer, sjPlot, choroplethr, choroplethrMaps,
               xtable, leaflet, rgeos, rgdal, maptools, scales)

# Increase file upload size limit
options(shiny.maxRequestSize = 9*1024^2)


#Load base data
load("data/basefile.Rda")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cyborg"),
                
   # CSS
   tags$head(tags$style(HTML("pre { font-size: 12px !important; font-weight: bold !important; }", ".startables table td { padding: 0 15px; }",
                             ".startables table { background-color: #060606; }",
                             "div#intMap { height: 700px !important; }"))),
  

   # Application title
   titlePanel("Apocalypse Diagnostics"),
   
   navlistPanel(
     "Explore the Data",
     
     # DISTRIBUTIONS
     tabPanel("Distributions",
              fluidRow(
                #plot1 inputs
                column(3,
                       selectInput("plotType1",
                                   label = "Select plot 1",
                                   choices = c("Histogram", 
                                     "Bar plot")),
                       selectInput("varName1",
                                   label = "Select variable",
                                   choices = names(basefile[2:length(basefile)])),
                       numericInput("bins1",
                                    label = "Bins (for histograms)",
                                    value = 30)),
                #plot2 inputs
                column(3,
                       selectInput("plotType2",
                                   label = "Select plot 2",
                                   choices = c("Histogram",
                                     "Bar plot")),
                       selectInput("varName2",
                                   label = "Select variable",
                                   choices = names(basefile[2:length(basefile)])),
                       numericInput("bins2",
                                    label = "Bins (for histograms)",
                                    value = 30)),
                #plot3 inputs
                column(3,
                       selectInput("plotType3",
                                   label = "Select plot 3",
                                   choices = c("Histogram",
                                     "Bar plot")),
                       selectInput("varName3",
                                   label = "Select variable",
                                   choices = names(basefile[2:length(basefile)])),
                       numericInput("bins3",
                                    label = "Bins (for histograms)",
                                    value = 30)),
                #theme
                column(3,
                       selectInput("themeName",
                                   label = "Select theme",
                                   choices = c("Minimal" = "theme_minimal()",
                                     "Tufte" = "theme_tufte(ticks= F)",
                                     "Few" = "theme_few()",
                                     "FiveThirtyEight" = "theme_fivethirtyeight()",
                                     "Economist" = "theme_economist()",
                                     "Simple BW" = "theme_bw()")),
                       colourInput("lineColor",
                                   label = "Line color", "black"),
                       colourInput("fillColor",
                                   label = "Fill color", "#990000"))
              ), # End input row
              
              # Plot row
              fluidRow(
                column(4,
                       plotOutput("expPlot1")),
                column(4,
                       plotOutput("expPlot2")),
                column(4,
                       plotOutput("expPlot3"))
              ), # End plot row
              
              # Summary stats
              fluidRow(
                column(4,
                       htmlOutput("sumPlot1")),
                column(4,
                       htmlOutput("sumPlot2")),
                column(4,
                       htmlOutput("sumPlot3"))
              ) # End summary stats row

            ), # END DISTRIBUTION TAB PANEL
     
     # BIVARIATE STATS
     tabPanel("Bivariate Statistics",
              
              fluidRow(
                column(4,
                       selectInput("bivx",
                                   label = "X axis",
                                   choices = names(basefile[2:length(basefile)])),
                       selectInput("bivy",
                                   label = "Y axis",
                                   choices = names(basefile[2:length(basefile)])),
                       selectInput("themeName.biv",
                                   label = "Select theme",
                                   choices = c("Minimal" = "theme_minimal()",
                                               "Tufte" = "theme_tufte(ticks= F)",
                                               "Few" = "theme_few()",
                                               "FiveThirtyEight" = "theme_fivethirtyeight()",
                                               "Economist" = "theme_economist()",
                                               "Simple BW" = "theme_bw()")),
                       colourInput("loessLineColor", 
                                   label = "LOESS line color", "#070893"),
                       colourInput("regLineColor",
                                   label = "Regression line color", "#990000"),
                       colourInput("pointColor",
                                   label = "Point color", "#0b0b0b"),
                       sliderInput("pointAlpha", 
                                   label = "Point opacity",
                                   min = 0, max = 1, value = 0.3)),
                column(8,
                       plotOutput("scatterPlot"),
                       checkboxInput("loess",
                                     label = "LOESS curve", FALSE),
                       checkboxInput("regress",
                                     label = "Regression line", FALSE))
                )
             ),
 

     # DATA TABLE
     tabPanel("Dataset",
              dataTableOutput("baseData")), # END DATA TABLE
     
     # MAPPING
     tabPanel("Map",
              selectInput("mapVar",
                          label = "Select a variable to map:",
                          choices = names(basefile[2:length(basefile)])),
              #plotOutput("countyMap"),
              leafletOutput("intMap")), # END MAPPING
     
     # STATISTICAL ANALYSIS
     "Analyze the Data",
     tabPanel("Regression Models",
              column(2,
                     selectInput("outcome",
                                 label = "Outcome variable:",
                                 names(basefile[2:length(basefile)])),
                      selectInput("ex.numIn",
                                 label = "Explanatory variables (numeric)",
                                 names(basefile[2:length(basefile)]), multiple = TRUE),
                     selectInput("ex.catIn",
                                 label = "Explanatory variables (categorical)",
                                 names(basefile[2:length(basefile)]), multiple = TRUE)),
              column(8,
                     verbatimTextOutput("ex.type"))), # END STATISTICAL ANALYSIS
     
     "App Description",
     tabPanel("Purpose"),
     tabPanel("Credits and References"),
     
     # UPLOAD PANEL
     "Upload Your Own",
     tabPanel("Upload",
              
              fluidRow(
                column(2,
                  fileInput('file1', 'Choose file to upload',
                        accept = c(
                          'text/csv',
                          'text/comma-separated-values',
                          'text/tab-separated-values',
                          'text/plain',
                          '.csv',
                          '.tsv'
                        )
              ),
              tags$hr(),
              
              radioButtons('sep', 'Separator',
                           c(Comma=',',
                             Semicolon=';',
                             Tab='\t'),
                           ','),
              radioButtons('quote', 'Quote',
                           c(None='',
                             'Double Quote'='"',
                             'Single Quote'="'"),
                           '"'),
              tags$hr(),
              p('Please upload only CSV files with a header of your variable names. You must select a variable to be used as your
                FIPS number below:'
              ),
              uiOutput('fips')
              ),
              column(8,
                     h4('Your Data'),
                     dataTableOutput('uploadDT'),
                     h4('Pre Uploaded Data'),
                     dataTableOutput('baseDT'),
                     h4('Merged Data'),
                     dataTableOutput('mergeDT')
                     )
            ) #end fluidRow
          ), # END UPLOAD PANEL
     
     #theming of page
     widths = c(2,10),
     fluid = TRUE
   )

   
) #end page
   
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # Update plot type and variable name
  
  # plot reactives
  p1.type <- reactive({input$plotType1})
  p2.type <- reactive({input$plotType2})
  p3.type <- reactive({input$plotType3})
  v1.type <- reactive({input$varName1})
  v2.type <- reactive({input$varName2})
  v3.type <- reactive({input$varName3})
  bins1 <- reactive({input$bins1})
  bins2 <- reactive({input$bins2})
  bins3 <- reactive({input$bins3})
  
  # theme reactives
  theme.select <- reactive({input$themeName})
  line.color <- reactive({input$lineColor})
  fill.color <- reactive({input$fillColor})
  
  
  ##### DISTRIBUTION PLOTS
  #################################################
  
   # Plot1 output
   output$expPlot1 <- renderPlot({
     
      # Plot1 select
      if (p1.type() == "Histogram") {
        gp.layer <- geom_histogram(aes_string(x = v1.type()), col = line.color(), fill = fill.color(),
                                   bins = bins1())
      } else if (p1.type() == "Bar plot") {
        gp.layer <- geom_bar(aes(x = factor(eval(parse(text = v1.type())))), 
                             col = line.color(), fill = fill.color())
      } 
     
      # Theme select 
      skin <- eval(parse(text = theme.select()))
      # Generate plot
      ggplot(basefile) + gp.layer + skin
   })
   
   # Plot2 output
   output$expPlot2 <- renderPlot({
     
     # Plot2 select
     if (p2.type() == "Histogram") {
       gp.layer <- geom_histogram(aes_string(x = v2.type()), col = line.color(), fill = fill.color(),
                                  bins = bins2()) 
     } else if (p2.type() == "Bar plot") {
       gp.layer <- geom_bar(aes(x = factor(eval(parse(text = v2.type())))), col = line.color(), fill = fill.color())
     } 
     
     # Theme select 
     skin <- eval(parse(text = theme.select()))
     # Generate plot
     ggplot(basefile) + gp.layer + skin
   })
   
   # Plot3 output
   output$expPlot3 <- renderPlot({
     
     # Plot3 select
     if (p3.type() == "Histogram") {
       gp.layer <- geom_histogram(aes_string(x = v3.type()), col = line.color(), fill = fill.color(),
                                  bins = bins3()) 
     } else if (p3.type() == "Bar plot") {
       gp.layer <- geom_bar(aes(x = factor(eval(parse(text = v3.type())))), col = line.color(), fill = fill.color())
     } 
     
     # Theme select 
     skin <- eval(parse(text = theme.select()))
     # Generate plot
     ggplot(basefile, aes_string(x = v3.type())) + gp.layer + skin
   })
   
   ##### SUMMARY STATS
   #################################################
   
   
   # Plot1 Summary
   output$sumPlot1 <- renderUI({
     
     tags$div(class = "startables",
     if (p1.type() == "Histogram") {
      HTML(
        stargazer(data.frame(basefile[[v1.type()]]), type = "html", 
                     summary = T, flip = T, median = T, iqr = T,
                     covariate.labels = "", digits = 2)
        )
     } else if (p1.type() == "Bar plot") {
       xtable(table(basefile[[v1.type()]]))
     })
   })
   
   # Plot2 Summary
   output$sumPlot2 <- renderUI({

     tags$div(class = "startables",
     if (p2.type() == "Histogram") {
                HTML(
                  stargazer(data.frame(basefile[[v2.type()]]), type = "html", 
                            summary = T, flip = T, median = T, iqr = T,
                            covariate.labels = "", digits = 2)
                )
     } else if (p2.type() == "Bar plot") {
       HTML(
         sjt.frq(basefile[[v2.type()]])
       )
     })
   })
   
   # Plot3 Summary
   output$sumPlot3 <- renderUI({
     
     tags$div(class = "startables",
     if (p3.type() == "Histogram") {
       HTML(
         stargazer(data.frame(basefile[[v3.type()]]), type = "html", 
                   summary = T, flip = T, median = T, iqr = T,
                   covariate.labels = "", digits = 2)
       )
     } else if (p2.type() == "Bar plot") {
       prop.table(table(basefile[[v3.type()]]))*100
     })
    })
   
   
   ##### BIVARIATE
   #################################################
   
   # save line colors
   lcol <- reactive ({ input$loessLineColor })
   rcol <- reactive ({ input$regLineColor })
   pt.col <- reactive ({ input$pointColor })
   pt.alpha <- reactive ({ input$pointAlpha })

   output$scatterPlot <- renderPlot({
     
     if (input$loess == TRUE & input$regress == TRUE) {
       ggplot(basefile, aes_string(x = input$bivx, y = input$bivy)) + geom_point(col = pt.col(), alpha = pt.alpha()) + 
         eval(parse(text = input$themeName.biv)) + geom_smooth(method = "loess", col = lcol()) + geom_smooth(method = "lm", col = rcol()) 
     } else if (input$loess == TRUE) {
       ggplot(basefile, aes_string(x = input$bivx, y = input$bivy)) + geom_point(col = pt.col(), alpha = pt.alpha()) + 
         eval(parse(text = input$themeName.biv)) + geom_smooth(method = "loess", col = lcol())
     } else if (input$regress == TRUE) {
       ggplot(basefile, aes_string(x = input$bivx, y = input$bivy)) + geom_point(col = pt.col(), alpha = pt.alpha()) + 
         eval(parse(text = input$themeName.biv)) + geom_smooth(method = "lm", col = rcol())
     } else {
       ggplot(basefile, aes_string(x = input$bivx, y = input$bivy)) + geom_point(col = pt.col(), alpha = pt.alpha()) + 
         eval(parse(text = input$themeName.biv))
     }

     

   })
   
  
  ##### DATA TABLE
  ################################################
   
  output$baseData <- renderDataTable({
     basefile
   })
   
   
  #### COUNTY MAP
  ################################################
   
   # output$countyMap <- renderPlot ({
   # 
   #   # subset mapping data
   #   mapset <- data.frame("region" = as.numeric(basefile$FIPS),
   #                        "value" = basefile[[input$mapVar]])
   #   mapset <- na.omit(mapset)
   # 
   #   # plot map
   #   county_choropleth(mapset)
   # 
   # })
   
   output$intMap <- renderLeaflet ({
     
     # leaflet map guide:
     # https://www.datascienceriot.com/mapping-us-counties-in-r-with-fips/kris/
     
     # subset mapping data
     imapset <- data.frame("GEOID" = basefile$FIPS,
                           "value" = basefile[[input$mapVar]],
                           "winner_2012" = basefile$winner_2012,
                           "winner_2016" = basefile$winner_2016,
                           "pctwhite" = basefile$pctwhite,
                           "pctblack" = basefile$pctblack,
                           "pcthispanic" = basefile$pcthispanic,
                           "pctasian" = basefile$pctasian,
                           "pctunemploy" = basefile$pctunemploy,
                           "drugmort_rate" = basefile$drugmort_rate)
     
     # shapefile source:
     # https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
     us.map <- readOGR(dsn = "cb_2015_us_county_20m", 
                     layer = "cb_2015_us_county_20m", stringsAsFactors = F)
     
     # merge value data with map data
     leafmap <- merge(us.map, imapset, by = "GEOID")
     
     # popup tooltips
     if (is.numeric(leafmap$value)) {
       tip.val <- round(leafmap$value, digits = 2)
     } else {
       tip.val <- leafmap$value
     }
     
     popup_info <- paste0("<strong>County: </strong>",
                     leafmap$NAME,
                     "<br /><strong>Selected var: </strong>",
                     tip.val,
                     "<br /><br /><strong>2012 winner: </strong>",
                     leafmap$winner_2012,
                     "<br /><strong>2016 winner: </strong>",
                     leafmap$winner_2016,
                     "<br /><br /><strong>% White: </strong>",
                     round(leafmap$pctwhite, digits = 2),
                     "<br /><strong>% Black: </strong>",
                     round(leafmap$pctblack, digits = 2),
                     "<br /><strong>% Hispanic: </strong>",
                     round(leafmap$pcthispanic, digits = 2),
                     "<br /><strong>% Asian: </strong>",
                     round(leafmap$pctasian, digits = 2),
                     "<br /><br /><strong>% Unemployed: </strong>",
                     round(leafmap$pctunemploy, digits = 2),
                     "<br /><strong>Drug mortality rate: </strong>",
                     round(leafmap$drugmort_rate, digits = 2))
     
     palette <- colorQuantile("YlOrRd", NULL, n = 20)
     
     # plot map
     leaflet(data = leafmap) %>% 
       addTiles() %>%
       addPolygons(fillColor = ~palette(value),
                   fillOpacity = 0.8,
                   color = "#BDBDC3",
                   weight = 1,
                   popup = popup_info) %>%
       setView(lng = -93.85, lat = 37.45, zoom = 4.5) 
          
     
   })
   
   ##### STATISTICAL ANALYSIS
   ######################################################
   
   #reactives
   o.var <- reactive ({ input$outcome })
   ex.num <- reactive ({ input$ex.numIn })
   ex.cat <- reactive ({ input$ex.catIn })
   
   output$ex.type <- renderPrint({
     
     if(is.null(o.var()) | is.null(ex.num())) {
       return(print("Select your variables to get started!"))
     } else {
     
     # code source:
     ## https://github.com/Sullivanstatistics/php2560-shiny-projects/blob/master/LinearRegression/server.R 
       fmla<-as.formula(paste(o.var(),"~",paste(c(ex.num(), unlist(ex.num())),collapse = "+")))
       fit <- lm(formula=fmla,data=basefile)
       summary(fit)
     }
   })
   
   
   ##### UPLOAD DATA
   ######################################################
   
   inFile <- reactive({
     if (is.null(input$file1)) {
       return(NULL)
     } else {
       input$file1
     }
   })
   
   myData <- reactive({
     inFile <- input$file1
     if (is.null(inFile)) return(NULL)
     data <- read.csv(inFile$datapath, sep = input$sep, quote = input$quote, header = TRUE)
     data
   })
   
   output$baseDT <- renderDataTable({
     basefile
   })
   
   output$uploadDT <- renderDataTable({
     myData()
   })
   
   
   
   output$fips <- renderUI({
     myData<-myData()
     if (is.null(inFile())) {
       return(NULL)
     } else {
       selectInput("fipsInput", "FIPS",
                   names(myData))
       
     }
   })
   
   
   fips.var <- reactive({ input$fipsInput })
   
   
   output$mergeDT <- renderDataTable({
     if (is.null(inFile())) {
       return(NULL)
     } else {

       staticUpload <- myData()
       staticUpload$FIPS_MERGE <- str_pad(staticUpload[[fips.var()]], 5, "left", pad = "0")
       newbase <<- merge(basefile, staticUpload, by.x = "FIPS", by.y = "FIPS_MERGE")
       newbase
       #staticUpload
     }
   })
   

   
}

# Run the application 
shinyApp(ui = ui, server = server)

