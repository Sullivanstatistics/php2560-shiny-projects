
library(shiny)
library(leaflet)
library(geosphere)
library(maps)
#library(maptools)
#library(rgeos)
library(leaflet)
library(rgdal)
library(zipcode)

data(zipcode)
load("counties.rda")
load("realtime_usa.rda")

ui <- fluidPage(tags$head(
  tags$style(HTML(".navbar .navbar-nav {float: right}
                  "))
  ),
  navbarPage("Air Pollution Monitor",inverse=TRUE,
             tabPanel("Home",
                      absolutePanel(top = 0,right ="0%",left="0%",
                                    column(img(src='https://cdn.theatlantic.com/assets/media/img/photo/2013/01/chinas-toxic-sky/c01_59565822/main_1200.jpg',heigh=941.5,width=1548,align="top",style = 'opacity: 0.60'),width=3)
                                    ),
                      absolutePanel(top = 70,width = "95%",
                                    helpText(HTML("<style>h1{color: black;text-align:center;}p{color: #4C4A4A;text-align:center;font-size: 20px}b{color: black;text-align:center;font-size:22px}</style>
                                                   <body>
                                                   <h1 >Welcome!</h1>
                                                   <p>There is invisible threat around you!</p>
                                                   <p>I am talking about air pollution!</p>
                                                   <p>I am going to help you monitor it.</p>
                                                   <p>You can find out real time pollution level in the <b>Overview</b> page.</p> 
                                                   <p>You can explore real time and future pollution at certain location by enter zipcode in the <b>Details</b> page.</p>
                                                   <p>You can get advice on how to avoid heavy air pollution in the <b>Advice</b> page.</p>
                                                   <p>To start, please clik the button below.</p>
                                                   </body>")
                                             )
                                    ),
                      absolutePanel(id = "control0",top = 450,right ="46%",left = "auto",
                           tags$head(tags$script('
                                                 Shiny.addCustomMessageHandler("myCallbackHandler0",
                                                 function(typeMessage) {console.log(typeMessage)
                                                 if(typeMessage == 2){
                                                 console.log("got here");
                                                 $("a:contains(Overview)").click();
                                                 }
                                                 });
                                                 ')
                                     ),
                           actionButton("submit",h4("Let's Start"))
                           )
             ),
             tabPanel("Overview",
                      br(),
                      br(),
                      absolutePanel(id = "wait",right = "30%",
                            img(src = 'http://myteachsource.ascd.org/images/loading.gif',style = 'align:middle')),
                      leafletOutput("map",height = 750),
                      absolutePanel(id = "control1",top = 55,right = 30,fixed = TRUE,
                                    tags$head(tags$script('
                                          Shiny.addCustomMessageHandler("myCallbackHandler",
                                                           function(typeMessage) {console.log(typeMessage)
                                                           if(typeMessage == 1){
                                                           console.log("got here");
                                                           $("a:contains(Details)").click();
                                                           }
                                                           if(typeMessage == 2){
                                                           $("a:contains(Select Data range)").click();
                                                           }
                                                           });
                                                           ')),
                                     actionButton("submit0",h5("Explore AQI at Your Location"))
                      ),
                      br(),
                      br(),
                      h3("Nationwide Statistic",align = "center"),
                      hr(),
                      fluidRow(
                              column(4,
                                     plotOutput("pie_pm25")),
                              column(4,
                                     plotOutput("pie_so2")),
                              column(4,
                                     plotOutput("pie_o3"))
                      )
             ),
             tabPanel("Details",
                      
                      h3(textOutput("text0")),
                      
                      fluidRow(column(4,
                                      br(),
                                      br(),
                                      tableOutput("aqitable")),
                               column(8,
                                      br(),
                                      leafletOutput("map2",height = 500))
                               
                      ),
                      div(class="outer",
                          
                          
                          absolutePanel(id = "controls", top = 50, right = 20,
                                        width = 270, height = 120,
                                        h4("Zipcode:"),
                                        fluidRow(
                                          column(8,
                                                 textInput("zipcode",label = NULL)),
                                          column(4,
                                                 actionButton("submit1","GO"))
                                        ),
                                        HTML("<style>p1{color: #4C4A4A;text-align:left;font-size: 15px}</style>
                                                <body><p1>Please enter zipcode to get detailed information</p1></body>")
                          ))
                      ),
             tabPanel("Advice",
                      titlePanel(h2("Get Advice Exclusive for You",style="color:black;text-align:left")),
                      sidebarLayout(
                        sidebarPanel(h3("Please Input Your Health Data"),
                                     numericInput("age",label="Please Enter Your Age:",0,0,100,1),
                                     selectInput("gender",label="Please Select Your Gender:",choices=c("","Male","Female")),
                                     selectInput("disease",label="Please Indicate Whether You Have the Following Condition:",choices=c("None","Asthma","Lung Cancer")),
                                     numericInput("height",label="Please Enter Your Height(cm):",160,0,250,1),
                                     numericInput("weight",label="Please Enter Your Weight(kg):",50,0,250,1),
                                     numericInput("vitalcap",label="Please Enter Your Vital Capacity:",3000,0,10000,100)),
                        mainPanel())
             ),
             tabPanel("Help",
                      h3("What Is Air Quality Index(AQI)?"),
                      br(),
                      p("An air quality index (AQI) is a number used by government agencies to communicate to the public how polluted the air currently is or how polluted it is forecast to become. As the AQI increases, an increasingly large percentage of the population is likely to experience increasingly severe adverse health effects.",style="color: black;text-align:left;font-size: 18px"),
                      br(),
                      br(),
                      h3("How Do We Measure Air Quality and Pollution?",style="text-align:left"),
                      HTML("<head>
                           <style>
                           table {font-family: arial, sans-serif;border-collapse: collapse;width: 100%;}
                           td, th {border: 1px solid #dddddd;text-align: left;padding: 8px;}
                           tr:nth-child(even) {background-color: #dddddd;}
                           </style>
                           </head>
                           <body>
                           <table>
                             <tr>
                               <th>Air Quality Index(AQI) Values</th>
                               <th>Levels of Health Concern</th>
                             </tr>
                             <tr>
                               <td>0 to 50</td>
                               <td>Good</td>
                             </tr>
                             <tr>
                               <td>51 to 100</td>
                               <td>Moderate</td>
                             </tr>
                             <tr>
                               <td>101 to 150</td>
                               <td>Unhealthy for Sensitive Groups</td>
                             </tr>
                             <tr>
                               <td>151 to 200</td>
                               <td>Unhealthy</td>
                             </tr>
                             <tr>
                               <td>201 or above</td>
                               <td>Very Unhealty</td>
                             </tr>
                           </table>
                           </body>
                           "),
                      br(),
                      br(),
                      h3("How Do We Predict Air Pollution?"))
  )
)


  

server <- function(input, output, session) {
  
  observe({
    if(input$submit > 0){
      print('2')
      session$sendCustomMessage("myCallbackHandler0", "2")
    }
  })
  
  
  
  #Get the county data from http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_csa_5m.zip
  #counties <- readOGR("D:/BROWN/2016 courses/PHP 2560/cb_2015_us_county_500k/cb_2015_us_county_500k.shp",
                      #layer = "cb_2015_us_county_500k", verbose = FALSE)
  
  county_lonlat <- counties@polygons[[1]]@labpt
  for (i in 2:length(counties@polygons)) {
    county_lonlat <- rbind(county_lonlat,counties@polygons[[i]]@labpt)
  }
  county_lonlat <- data.frame(matrix(county_lonlat,ncol = 2,byrow = FALSE))
  names(county_lonlat) <- c("longtitude","latitude")
  countyname <- data.frame(counties$NAME,stringsAsFactors = FALSE)
  county_lonlat <- data.frame(countyname,county_lonlat,stringsAsFactors = FALSE)
  
  #compute the pollution for every county
  
  county_pollute <- function(x,y,type){
    sites_lonlat <- as.matrix(realtime_usa[which(!is.na(realtime_usa[type])),2:3])
    pollute <- realtime_usa[which(!is.na(realtime_usa[type])),]
    pollute <- cbind(pollute[,1],pollute[type])
    lonlat <- matrix(rep(c(x,y),dim(sites_lonlat)[1]),ncol = 2,byrow = TRUE)
    dist <- distGeo(sites_lonlat,lonlat)
    dist <- cbind(pollute,dist)
    dist <- dist[order(dist$dist),]
    return(dist[1,2])
  }
  
  #PM2.5,SO2,O3
  county_pm25 <- mapply(county_pollute,x = county_lonlat[,2],y = county_lonlat[,3],type = "PM2.5")
  county_SO2 <- mapply(county_pollute,x = county_lonlat[,2],y = county_lonlat[,3],type = "SO2")
  county_O3 <- mapply(county_pollute,x = county_lonlat[,2],y = county_lonlat[,3],type = "O3")
  
  #draw map
  pal1 <- colorNumeric(
    palette = c("#FFEDA0","tomato2"),
    domain = county_pm25
  )
  pal2 <- colorNumeric(
    palette = c("darkolivegreen1","gold4"),
    domain = county_SO2
  )
  pal3 <- colorNumeric(
    palette = c("skyblue1","midnightblue"),
    domain = county_O3
  )
  
  siteIcon <- makeIcon(
    iconUrl = "http://image.flaticon.com/icons/png/512/283/283386.png",
    iconWidth = 20, iconHeight = 20,
    iconAnchorX = 0, iconAnchorY = 50
  )
  pop <- function(x){
    return(paste(x["sites"],"\n","PM2.5:",x["PM2.5"],"\n","SO2:",x["SO2"],"\n","O3:",x["O3"]))
  }
  popups <- realtime_usa[,1]
  for (i in 1:dim(realtime_usa)[1]) {
    popups[i] <- pop(realtime_usa[i,])
  }
  
  output$map <- renderLeaflet({
    leaflet(counties) %>%
      addTiles(group = "OSM (default)") %>%
      setView(-95,39,zoom = 5) %>%
      addMarkers(lng = realtime_usa[,2],lat = realtime_usa[,3],icon = siteIcon,popup = popups,group = "monitor sites") %>%
      addPolygons(
        stroke = FALSE, fillOpacity = 0.9, smoothFactor = 0, group = "PM2.5",
        color = ~pal1(county_pm25)
      ) %>% 
      addPolygons(
        stroke = FALSE, fillOpacity = 0.9, smoothFactor = 0, group = "SO2",
        color = ~pal2(county_SO2)
      ) %>%
      addPolygons(
        stroke = FALSE, fillOpacity = 0.9, smoothFactor = 0, group = "O3",
        color = ~pal3(county_O3)
      ) %>%
      #  addLegend("bottomright", pal = pal1, values = county_pm25,
      #              opacity = 1,title = "PM2.5 AQI",group = "PM2.5"
      #  ) %>%
      addLayersControl(
        overlayGroups = c("PM2.5", "SO2","O3","monitor sites"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("SO2","O3","monitor sites"))
    })
  
  library(XML)
  library(rvest)
  url <- read_html("https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code")
  statecode <- html_table(html_nodes(url,"table")[1],fill = TRUE)
  statecode <- data.frame(statecode)[,2:3]
  statecode <- statecode[-which(statecode[,1] == ""),]
  statecode <- statecode[order(statecode[,2]),]
  countypollute <- cbind(as.character(countyname),data.frame(as.numeric(counties$STATEFP)),county_pm25,county_SO2,county_O3)
  names(countypollute) <- c("county","state","PM2.5","SO2","O3")
  library(ggplot2)
  library(dplyr)
  
  
  #plot
  statepollute <- countypollute %>% select(state,PM2.5,SO2,O3) %>% group_by(state) %>% 
          summarise(stateplt1 = mean(PM2.5,na.rm=TRUE),
                    stateplt2 = mean(SO2,na.rm=TRUE),
                    stateplt3 = mean(O3,na.rm=TRUE))
  statepollute <- statepollute[-c(3,7,14,43),]
  statepollute$name <- statecode[1:52,1]
  evaluation <- function(m){
          if (is.na(m)) {return(NA)} 
          else {
                  if (m<= 50) {return("Good")}
                  else {if (m <= 100) {return("Moderate")}
                          else {if (m <= 150) {return ("Unhealthy for Sensitive Group")}
                                  else {if (m <= 200) {return("Unhealthy")}
                                          else {return("Very Unhealty")}}}}
          }
  }
  statepollute$eva1 <- mapply(evaluation,data.frame(unlist(statepollute[,2]))[,1]) 
  #pm2.5 plot 
  output$pie_pm25 <- renderPlot({
          ggplot(statepollute, aes(x = factor(1),fill = factor(eva1))) +
          geom_bar(width = 1)+coord_polar(theta = "y") +
          scale_fill_manual(values = c("darkolivegreen1", "gold1","salmon","indianred2","indianred4")) +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                legend.position="none",
                plot.title = element_text(hjust = 0.5,size = 18)) + ggtitle("PM2.5")
  })
  
  
  #SO2 plot
  statepollute$eva2 <- mapply(evaluation,data.frame(unlist(statepollute[,3]))[,1]) 
  output$pie_so2 <- renderPlot({
          ggplot(statepollute, aes(x = factor(1),fill = factor(eva2))) +
          geom_bar(width = 1)+coord_polar(theta = "y") +
          scale_fill_manual(values = c("darkolivegreen1", "gold1","salmon","indianred2","indianred4")) +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                legend.position="none",
                plot.title = element_text(hjust = 0.5,size = 18)) + ggtitle("SO2")
  })
  
  #O3 plot
  statepollute$eva3 <- mapply(evaluation,data.frame(unlist(statepollute[,4]))[,1]) 
  output$pie_o3 <- renderPlot({
          ggplot(statepollute, aes(x = factor(1),fill = factor(eva3))) +
          geom_bar(width = 1)+coord_polar(theta = "y") +
          scale_fill_manual(values = c("darkolivegreen1", "gold1","salmon","indianred2","indianred4")) +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                legend.position="none",
                plot.title = element_text(hjust = 0.5,size = 18)) + ggtitle("O3")
  })
  
  
  observe({
    if(input$submit0 > 0){
      print('1')
      session$sendCustomMessage("myCallbackHandler", "1")
    }
  })


 observe({
   if(input$submit1 >0){
     zip_info <- subset(zipcode,zipcode == input$zipcode)
     
     ##match the zipcode with monitor sites
     
     
     sites_lonlat <- as.matrix(realtime_usa[,2:3])
     zip_lonlat <- matrix(rep(c(zip_info$longitude,zip_info$latitude),dim(sites_lonlat)[1]),ncol = 2,byrow = TRUE)
     dist <- distGeo(sites_lonlat,zip_lonlat)
     dist <- cbind(realtime_usa,dist)
     dist <- dist[order(dist$dist),]
     
     
     ##call the matched monitor sites dataset "local" 
     local <- dist[1:5,-10]
     ##calculate the pollution for input zipcode, use mean() for now
     local <- colMeans(local[,c(4,6,7)],na.rm = TRUE)
     
     for (i in 1:3) {
       if(is.nan(local[i])) {local[i] <- NA}
     }
     
     evaluation <- function(x){
       if (is.na(x)) {return(NA)} 
       else {
         if (x <= 50) {return("Good")}
         else {if (x <= 100) {return("Moderate")}
           else {if (x <= 150) {return ("Unhealthy for Sensitive Group")}
             else {if (x <= 200) {return("Unhealthy")}
               else {return("Very Unhealty")}}}}
       }
     }
     
     eva <- mapply(evaluation,local)
     aqitable <- data.frame(c("PM2.5","SO2","O3"),local,eva)
     names(aqitable) <- c("Pollution", "AQI","Evaluation")
     output$text0 <-renderText("Current Air Quality Index:")
     output$aqitable <- renderTable(aqitable,width = "100%",align = 'c',hover = TRUE,striped = FALSE,bordered = FALSE)
     
     output$map2 <- renderLeaflet({
       leaflet(counties) %>%
         addTiles(group = "OSM (default)") %>%
         setView(zip_info$longitude,zip_info$latitude,zoom = 10)%>%
         addPolygons(
           stroke = FALSE, fillOpacity = 0.9, smoothFactor = 0, group = "PM2.5",
           color = ~pal1(county_pm25)
         ) %>%
           addPopups(lng = zip_info$longitude,lat = zip_info$latitude, paste0(zip_info$city,",",zip_info$state),options = popupOptions(closeButton = FALSE))%>%
           addLegend(position = "bottomright", pal = pal1, value = county_pm25, title = "AQI Level" )
     })
   }
 })
}
shinyApp(ui, server)