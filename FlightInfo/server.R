library(shiny)

source("Code_essential.R")
shinyServer(function(input, output){
  
  #input for statistical analysis departure part for p1
  formula_stat_depart_city1<-reactive({
    stat_depart_city_output1<-statistical_departp1(input$dcity
                                              )
  plot(stat_depart_city_output1)
  })
  #input for statistical analysis departure part for p2
  formula_stat_depart_city2<-reactive({
    stat_depart_city_output2<-statistical_departp2(input$dcity
    )
    plot(stat_depart_city_output2)
  })
  
  #input for statistical analysis departure part for p3p4
  formula_stat_depart_city34<-reactive({
    stat_depart_city_output34<-statistical_departp3p4(input$dcity
    )
    plot(stat_depart_city_output34)
  })
  #input for statistical analysis departure part for airline average fare change
  formula_stat_depart_city_fare<-reactive({
    stat_depart_city_output_fare<-statistical_depart_infor(input$dcity,input$acity
    )
    plot(stat_depart_city_output_fare)
  })
  #input for city_map_visulization
  formula_city<-reactive({
    city_output<-city_information(input$acity
                                  )
    plot(city_output)
    })
  
  #link output for statistical analysis departure part p1
  output$stat_depart_city_out1 <- renderPlot({
    formula_stat_depart_city1()
  })
  #link output for statistical analysis departure part p2
  output$stat_depart_city_out2 <- renderPlot({
    formula_stat_depart_city2()
  })
  #link output for statistical analysis departure part p3p4
  output$stat_depart_city_out34 <- renderPlot({
    formula_stat_depart_city34()
  })
  
  #link output for statistical analysis departure part for airline average fare change
  output$stat_depart_city_out_fare <- renderPlot({
    formula_stat_depart_city_fare()
  })
  
  #link output for city_map_visulization  
  output$city_out <- renderPlot({
    formula_city()
  })

}

)

