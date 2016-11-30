#This file creates the back-end server funtionality for the Emulator Shiny app.

library(shiny)

#This enables the server to use the underlying R code for the actual emulations.
source("helpers.R")
source("my_GP_plot.R")

shinyServer(function(input, output) {

  #FOR TUTORIAL SECTION ONLY
  data_tutorial <- eventReactive(input$go_tutorial,{
    
    #If the user selects the Test 1 model in the drop-down box the following code will run the appropriate emulation.
    if(input$userinput_tutorial=='t1') { 
      res1 <- run.optimal.dp(micro.sim.model=mock.model1, n.init.dp=input$N.init.dp_tutorial, max.num.dp=(input$max.N.dp_tutorial+1))$res.GP.model
      res1
      
      #If the user selects the Test 2 model in the drop-down box the following code will run the appropriate emulation.
    } else if(input$userinput_tutorial=='t2') { 
      res1 <- run.optimal.dp(micro.sim.model=mock.model2, n.init.dp=input$N.init.dp_tutorial, max.num.dp=(input$max.N.dp_tutorial+1))$res.GP.model
      res1
      
      #If the user selects the Test 3 model in the drop-down box the following code will run the appropriate emulation.
    } else {
      res1 <- run.optimal.dp(micro.sim.model=mock.model3, n.init.dp=input$N.init.dp_tutorial, max.num.dp=(input$max.N.dp_tutorial+1))$res.GP.model
      res1
    }
  })
  
  #This creates the design point table output for the tutorial after the "Calculate" button is pressed.
  dp_tutorial <- eventReactive(input$go_tutorial,{
    t(sort(data_tutorial()[[(input$max.N.dp_tutorial+1)]]$X))     
  })
  
  #This sends the created tutorial table to the user interface for outputting.
  output$x_tutorial <- renderTable({
    round(dp_tutorial(),2)
  })
  
  
  #This creates the plot output for the tutorial after the "Calculate" button is pressed.
  plot_tutorial <- eventReactive(input$go_tutorial,{
    data_tutorial()[[(input$max.N.dp_tutorial+1)]]
  })
  
  plot_base_tutorial <- eventReactive(input$go_tutorial,{
    
    #This creates Test plot 1.
    if(input$userinput_tutorial=='t1') { 
      mock.model1(plot.x)
      
      #This creates Test plot 2.
    } else if(input$userinput_tutorial=='t2') {
      mock.model2(plot.x)
      
      #This creates Test plot 3.
    } else {
      mock.model3(plot.x)
    }
  })
  
  #This sends the created tutorial plot to the user interface for outputting.
  output$testplot_tutorial <- renderPlot({
    my.plot.GP(plot_tutorial(), set.ylim=c(min(plot_base_tutorial()), max(plot_base_tutorial())))
    lines(x=plot.x, y=plot_base_tutorial(), col="black")

  })

  #THE USER INPUT SECTION
  
  #If the user selects the option to upload a model file from the drop-down menu, then this code obtains the model to later be used by the emulator.
  sim <- reactive({
    if(input$userinput=='m'){ 
      inMod <- input$modfile
      if (is.null(inMod))
        return(NULL)
      
      source(inMod$datapath)
    } else{
      inMod <- input$modfile2
      if (is.null(inMod))
        return(NULL)
      
      source(inMod$datapath)
      
    }
    
  })
  
  #This code only runs when the user hits the "calculate" button
  data <- eventReactive(input$go,{
    
    #This invokes the emulator to run for the user-specified model and design points.
    if(input$userinput=='m') { 
      res1 <- run.optimal.dp(micro.sim.model=sim()$value, n.init.dp=input$N.init.dp, max.num.dp=(input$max.N.dp1+1))$res.GP.model
      res1
      
      #If the user decides to upload design points from a csv file, this code enables the upload process, transforms it into the appropriate
      #and invokes the emulator to run
    } else if(input$userinput=='d') { 
      inFile <- input$dpfile
      
      if (is.null(inFile))
        return(NULL)
      
      #Reads in the design points.
      file.upload <- read.csv(inFile$datapath, header=TRUE)
      
      #Invokes the emulator.
      res1 <- run.optimal.dp(n.init.dp=dim(file.upload)[1], max.num.dp=(input$max.N.dp2+1),
                                have.dp=TRUE, user.init.dp=file.upload)$res.GP.model
      res1
    
      #If the user decides to upload both a model and design points, this code enables the upload process for both elements, transforms the design
      #points into the appropriate format and invokes the emulator to run.
    } else {

      inFile2 <- input$dpfile2
      
      if (is.null(inFile2))
        return(NULL)
      
      #Reads in the design points.
      file.upload2 <- read.csv(inFile2$datapath, header=TRUE)
     
      #Invokes the emulator.
      res1 <- run.optimal.dp(micro.sim.model=sim()$value,n.init.dp=dim(file.upload2)[1], 
              max.num.dp=(input$max.N.dp3+1),have.dp=TRUE, user.init.dp=file.upload2)$res.GP.model
      res1
    }
  })
  
 

  #This creates the x-axis of the plot
  plot.x <- seq(from=0, to=1, length.out=100)

  #After the user hits the "Calculate" button, this enables the table of design points to be created.
  dp <- eventReactive(input$go,{
    
    #Table of design points for the upload function option.
    if(input$userinput=='m') { 
        t(sort(data()[[(input$max.N.dp1+1)]]$X))
    
      #Table of design points for the upload design points option.  
    } else if(input$userinput=='d') { 
        t(sort(data()[[(input$max.N.dp2+1)]]$X))
    
      #Table of design points for the option for both uploads.  
    } else {
        t(sort(data()[[(input$max.N.dp3+1)]]$X))
    }  
  })
  
  #This sends the created table to the user interface for outputting.
  output$x <- renderTable({
    round(dp(),2)
  })  
  
  #After the user hits the "calculate" button, this enables the emulation plot.
    plot_real <- eventReactive(input$go,{
      
      #Emulation plot if the user uploads a function.
    if(input$userinput=='m') { 
      data()[[(input$max.N.dp1+1)]]
      
      #Emulation plot if the user uploads design points.
    } else if(input$userinput=='d') {
      data()[[(input$max.N.dp2+1)]]
      
      #Emulation plot if the user uploads design points.
    } else {
      data()[[(input$max.N.dp3+1)]]
    }
  })

  plot_base <- eventReactive(input$go,{
    
    #This creates Test plot 1.
    if(input$userinput=='m') { 
      sim()$value(plot.x)
      
      #This creates Test plot 2.
    } else if(input$userinput=='b') {
      sim()$value(plot.x)
    } 
  })
  
  
  #This sends the created plot to the user interface for outputting.
  output$testplot <- renderPlot({
    my.plot.GP(plot_real(), set.ylim=c(min(plot_base()), max(plot_base())))
    if(input$userinput!='d') {
      lines(x=plot.x, y=plot_base(), col="black")
    }
  })
  
})