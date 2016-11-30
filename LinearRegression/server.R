library(shiny)
library(MASS)
library(car)
library(ggplot2)

shinyServer(function(input, output) {
  ##Argument names:
  #Using the header names from the data 
  ArgNames <- reactive({
    Names <- names(formals("read.csv")[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  
  ### Data import:
  Dataset <- reactive({
    
    # User has not uploaded a file yet
    if (is.null(input$file) && input$myLoader==0) {
      return(data.frame())
    }
    
    #loading test dataset
    if (input$myLoader && is.null(input$file)){
      return(birthwt)
    }
    
    #loading csv. when data has been uploaded
    
    args <- grep(paste0("^","read.csv","__"), names(input), value = TRUE)
    
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text=input[[args[i]]]))
    }
    names(argList) <- gsub(paste0("^","read.csv","__"),"",args)
    
    argList <- argList[names(argList) %in% ArgNames()]
    
    Dataset <- as.data.frame(do.call("read.csv",c(list(input$file$datapath),argList)))
    return(Dataset)
  })
  
  
  # Select variables part 1:
  output$varselect_num <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    # Independent Numeric Variable selection:    
    selectInput("varnum", "Explantory Variables (Numeric):",
                names(Dataset()), multiple =TRUE)
  })
  
  # Select variables part 2:
  output$varselect_cat <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    # Independent Categorical Variable selection:    
    selectInput("varcat", "Explantory Variables (Categorical):",
                names(Dataset()), multiple =TRUE)
  })
  
  # Select variables part 3:
  output$outcomeselect <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    # Dependent Variable selection:
    selectInput("outcome","Outcome Variable:",
                names(Dataset()), names(Dataset()))
  })
  
  # Dataset Name:
  output$datasetnameout <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    if (input$myLoader && is.null(input$file)){
      textInput("datasetame", "Name of dataset", value = "birthwt")
    }
    else if (is.null(input$file)==FALSE){
      textInput("datasetame", "Name of dataset", value = "Enter text...")
    }
  })
  
  ## Building MLR model 
  
  
  model <- reactive({
    if(is.null(input$varnum) && is.null(input$varcat) || is.null(input$outcome)) return(NULL)
    
    else if (is.null(input$varnum)==FALSE && is.null(input$varcat) && is.null(input$outcome)==FALSE){
      fmla <- as.formula(paste(input$outcome,"~",paste(input$varnum,collapse="+")))
      return(lm(formula=fmla,data=Dataset()))
    }
    else if (is.null(input$varnum) && is.null(input$varcat)==FALSE && is.null(input$outcome)==FALSE){
      fmla <- as.formula(paste(input$outcome,"~",paste(paste("as.factor(",input$varcat,")",sep=""),collapse="+")))
      return(lm(formula=fmla,data=Dataset()))
    }
    else if (is.null(input$varnum)==FALSE && is.null(input$varcat)==FALSE && is.null(input$outcome)==FALSE){
      fmla <- as.formula(paste(input$outcome,"~",paste(c(input$varnum,paste("as.factor(",input$varcat,")",sep="")),collapse="+")))
      return(lm(formula=fmla,data=Dataset()))
    }
    
  })
  
  ### Descrpitive Statistics and Plots
  # Scatterplot Matrix
  output$ScatterMatrix <- renderPlot({
    if (is.null(model())||is.null(input$varnum)) return(NULL)
    else if (length(input$varnum)==1){
      plot(as.formula(paste(input$outcome,"~",input$varnum)),data=Dataset(),xlab=input$varnum,ylab=input$outcome,main=paste("Scatterplot for",input$outcome,"versus",input$varnum))
    }
    else if (length(input$varnum)>1){
      pairs(as.formula(paste("~",paste(c(input$varnum,input$outcome),collapse="+"))),data=Dataset())
    }
  })
  
  
  
  ## Boxplot
  output$BoxPlot <- renderPlot({
    if (is.null(model())||is.null(input$varcat)) return(NULL)
    if (length(input$varcat)==1){
      par(mfrow=c(1,1))
      boxplot(as.formula(paste(input$outcome,"~",input$varcat)),xlab=input$varcat,ylab=input$outcome,data=Dataset(),main=paste("Boxplot of",input$outcome,"versus",input$varcat))
    }
    else if (length(input$varcat)==2){
      par(mfrow=c(1,2))
      for (i in 1:length(input$varcat)){
        boxplot(as.formula(paste(input$outcome,"~",(input$varcat)[i])),xlab=(input$varcat)[i],ylab=input$outcome,data=Dataset(),main=paste("Boxplot of",input$outcome,"versus",(input$varcat)[i]))
      }
    }
    else if (length(input$varcat)==3 || length(input$varcat)==4){
      par(mfrow=c(2,2))
      for (i in 1:length(input$varcat)){
        boxplot(as.formula(paste(input$outcome,"~",(input$varcat)[i])),xlab=(input$varcat)[i],ylab=input$outcome,data=Dataset(),main=paste("Boxplot of",input$outcome,"versus",(input$varcat)[i]))
      }
    }
    else if (length(input$varcat)==5 ||length(input$varcat)==6){
      par(mfrow=c(2,3))
      for (i in 1:length(input$varcat)){
        boxplot(as.formula(paste(input$outcome,"~",(input$varcat)[i])),xlab=(input$varcat)[i],ylab=input$outcome,data=Dataset(),main=paste("Boxplot of",input$outcome,"versus",(input$varcat)[i]))
      }
    }
    else if (length(input$varcat)==7 ||length(input$varcat)==8 ||length(input$varcat)==9){
      par(mfrow=c(3,3))
      for (i in 1:length(input$varcat)){
        boxplot(as.formula(paste(input$outcome,"~",(input$varcat)[i])),xlab=(input$varcat)[i],ylab=input$outcome,data=Dataset(),main=paste("Boxplot of",input$outcome,"versus",(input$varcat)[i]))
      }
    }
  })
  
  ## Summary Statistics
  
  output$lmResults <- renderPrint({
    if (is.null(model())) return(NULL)
    summary(model())
  })
  
  # diagonstic plots
  output$diagnostics <- renderPlot({
    if (is.null(model())) return(NULL)
    par(mfrow=c(2,2))
    plot(model())
  })  
  
  # Added Variables Plots
  output$added <- renderPlot({
    if (is.null(model())) return(NULL)
    else if ((dim(summary(model())$coef)[1]-1)<=9){
      avPlots(model())
    }
    else avPlots(model(),ask=FALSE,layout=c(5,3))
  })  
  
  # MMP
  output$MMPlot <- renderPlot({
    if (is.null(model())) return(NULL)
    else if (is.null(input$varnum)&&is.null(input$varcat)==FALSE) return(NULL)
    mmps(model(),terms=as.formula(paste("~",paste(input$varnum,collapse="+"))))
  })  
  
  ## download report
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('myreport.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'myreport.Rmd')
      
      library(rmarkdown)
      out <- render('myreport.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
  
  ## Linking to Help documentation
  getPage<-function() {
    return(includeHTML("help_document.html"))
  }
  output$inc<-renderUI({getPage()})
})



