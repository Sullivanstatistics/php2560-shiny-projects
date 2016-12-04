#This R script will contain background codes (functions, calculations, etc.) of our final project app.
#May merge with ui.R if possible.
library(shiny)

shinyServer(function(input, output) {
  
  #Test codes that reads user-entered rankings and generate a string vector that lists parameters accordingly
  priority_rank_test = reactive({
    priority_rank = rep(0,5)
    priority_rank[input$indicator_input1] = "|Weather|"
    priority_rank[input$indicator_input2] = "|City Size|"
    priority_rank[input$indicator_input3] = "|Work Opportunities|"
    priority_rank[input$indicator_input4] = "|Political Preferences|"
    #priority_rank[input$indicator_inputN] = "|...Input parameter n|"
    print(priority_rank)
  })
  #output$priority_rank_test = renderText({priority_rank_test()})
  
  output$myplot = renderPlot({
    
    # ggplot with proper reference to reactive function <<data.r()>>
    library(ggplot2)
    library(maps)
    #load us map data
    all_states <- map_data("state")
    #plot all states with ggplot
    p <- ggplot()
    p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
      
    
    print(p)
    
  })
  
  
  #We need codes that could:
  #Scrape a list of US cities and their info from potentical data sources, and store them in a list;
  #Filter the list of cities based on different input parameters (creating functions likely necessary);
  #Perhaps also rank the list of cities to select the final top 5 choices, then list them as part of the output;
  #Also list the city info and/or reasons for the final choices, if possible;
  #(What if after all the filters, less than 5 cities remain?)
  #Scrape the top jobs available based on preferred job type if indicated, then list them as part of the output
  
  
})