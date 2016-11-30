library(shiny)
library(shinydashboard)

header <- dashboardHeader(title = "Blood Alcohol Concentration Estimator",
                          titleWidth = 400)

### SideBar:
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Personal Information", tabName = "pi", icon = icon("fa fa-circle")),
    menuItem("Drink Information", tabName = "di", icon = icon("fa fa-circle")),
    menuItem("Graphs", tabName = "graph", icon = icon("fa fa-circle")),
    menuItem("Help", tabName = "help", icon = icon("fa fa-info-circle"))
  )
)

### Dashboard:
body <- dashboardBody(
  
  
  
  tabItems(
    
    ### TAB 1 = dashboard:
    tabItem(tabName = "pi",
            
            fluidRow(
              
              ## personal information
              mainPanel(selectInput("Sex", "Sex", c("Female" = "Female", "Male" = "Male")),
                  textInput("Age","Age", value=21),
                  textInput(inputId="Height",label="Enter Your  Height (cm)",value=170),
                  textInput(inputId="Weight",label="Enter Your Weight (kg)",value=50))
                  )
            ),
    
    tabItem(tabName = "di",
            fluidRow(
              box(width = 4, solidHeader = TRUE, status = "primary",
                  title = "Alcohol Volume for Your Drinks (ml)",
                  
                  sliderInput("Volume_1","Drink 1", min=1,max=800,step=1,value=0),
                  sliderInput("Volume_2","Drink 2", min=1,max=800,step=1,value=0),
                  sliderInput("Volume_3","Drink 3", min=1,max=800,step=1,value=0),
                  sliderInput("Volume_4","Drink 4", min=1,max=800,step=1,value=0)
                  
                  ),
              
              box(width = 4, solidHeader = TRUE, status = "primary",
                  title = "Percent alcohol (%) of Your Drinks",
                    
                  sliderInput("alc_percent_1","Drink 1",min=0,max=60,step=1,value=0),
                  sliderInput("alc_percent_2","Drink 2",min=0,max=60,step=1,value=0),
                  sliderInput("alc_percent_3","Drink 3",min=0,max=60,step=1,value=0),
                  sliderInput("alc_percent_4","Drink 4",min=0,max=60,step=1,value=0)
                ),
              
              box(width = 4, solidHeader = TRUE, status = "primary",
                  title = "Time Since Your Drinks (min)",
                  
                  sliderInput("drink_time_1","Drink 1", min=0,max=180,step=1,value=0),
                  sliderInput("drink_time_2","Drink 2", min=0,max=180,step=1,value=0),
                  sliderInput("drink_time_3","Drink 3", min=0,max=180,step=1,value=0),
                  sliderInput("drink_time_4","Drink 4", min=0,max=180,step=1,value=0)
              )
            )  
          ),
    
    tabItem(tabName = "graph",
    ## hide error message when personal information is blank
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"),
            
            fluidRow(

            mainPanel(style="width: 100%",
                
                box(width =12,
                    title = "Mixed Effect",
                    solidHeader = TRUE, status = "primary",
                    plotOutput("Curve_Mixed", width = "100%"),
                    textOutput("text1"),
                    textOutput("text2")
                )
              ),
             
             mainPanel(style="width: 100%",
               
               box(width = 12,
                 title = "One-Drink Effect",
                   solidHeader = TRUE, status = "primary",
                   plotOutput("Curve_1", width = "100%"),
                   plotOutput("Curve_2", width = "100%"),
                   plotOutput("Curve_3", width = "100%"),
                   plotOutput("Curve_4", width = "100%")
               )
             )
            )
    ),
    
    tabItem(tabName = "help",
            
            fluidRow(
              
              mainPanel(
                p(strong('Reference Only')),
                helpText('This app is intended for entertainment purposes. Please never use it for any serious purpose!'),
                p(strong('Background:')),
                helpText('Excessive alcohol consumption can lead to potential harm which can keep us from practicing daily activities. From this perspective, our app is intended to help users better understand physiological effects of alcohol use. Users can get a rough idea about when their blood alcohol concentration can return to normal level after drinking'),
                p(strong('Theory behind our app')),
                helpText('Widmark formula:'),
                withMathJax(),
                helpText('$$C=\\frac{A*(1-e^{-k*t})}{r*W}-(\\beta*t)$$'),
                helpText('Notation:'),
                helpText('C:blood alcohol concentration'),
                helpText('A:mass of alcohol consumed'),
                helpText('r:Widmark factor'),
                helpText('W:body weight'),
                helpText('k:absorption rate constant'),
                withMathJax(),
                helpText('β:elimination rate'),
                helpText('t:elapsed time since the start of drinking'),
                p(strong('Parameters:')),
                helpText('r(Widmark factor):'),
                helpText('$$r(male) =  0.62544 + 0.13664H-W(0.00189+ 0.002425/H^2)+1/W(0.57986+2.545H-0.02255G)$$'),
                helpText('$$r(female) =  0.50766 + 0.11165H-W(0.001612+ 0.0031/H^2)-1/W(0.62115-3.1665H)$$'),
                withMathJax(),
                helpText('β:average alcohol elimination rate = 0.018%/h'),
                p(strong('References')),
                helpText('[1]Posey, D., & Mozayani, A. (2007). The estimation of blood alcohol concentration. Forensic Science, Medicine, and Pathology, 3(1), 33-39.'),
                helpText('[2]Rockerbie, D. W., & Rockerbie, R. A. (1995). Computer simulation analysis of blood alcohol. Journal of clinical forensic medicine, 2(3), 137-141.'),
                helpText('[3]Seidl, S., Jensen, U., & Alt, A. (2000). The calculation of blood ethanol concentrations in males and females. International journal of legal medicine, 114(1-2), 71-77.')
                
 )
)
)
)
)


ui <- dashboardPage(header, sidebar, body)

######### server
server<-function(input,output){
  ############# Curve 1
  output$Curve_1<-renderPlot({
    ## parameters
    A1<-as.numeric(input$Volume_1)/1000 #convert ml to liter
    P1<-as.numeric(input$alc_percent_1)/100
    W<-as.numeric(input$Weight)
    H<-as.numeric(input$Height)/100
    G<-as.numeric(input$Age)
    K<- 0.2/60
    r_F=0.50766+0.11165*H-W*(0.001612+0.0031/H^2)-(1/W)*(0.62115-3.1665*H)
    r_M=0.62544+0.13664*H-W*(0.00189+0.002425/H^2)+(1/W)*(0.57986+2.545*H-0.02255*G)
    ifelse(input$Sex=="Female",r<-r_F,r<-r_M)
    
    ## Formula used to calculate Blood Alcohol Concentration
    t<-seq(0,1440,length=1000)
    y1<-A1*P1*0.789*(1- exp(-K*(t*60)))/(r*W)-(0.018/100/3600)*(t*60)
    y1[y1<0]=0
    y1 <- y1*100
    ## plot Curve 1
    plot(t, y1, type="l", xlab="Time Since the Start of Drinking", ylab="Blood Alcohol Concentration (%)",main="Alcohol Metabolism Curve 1")
    
    
  })
  
  ############ Curve 2
  output$Curve_2<-renderPlot({
    ## parameters
    A2<-as.numeric(input$Volume_2)/1000 
    P2<-as.numeric(input$alc_percent_2)/100
    W<-as.numeric(input$Weight)
    H<-as.numeric(input$Height)/100
    G<-as.numeric(input$Age)
    K<- 0.2/60
    r_F=0.50766+0.11165*H-W*(0.001612+0.0031/H^2)-(1/W)*(0.62115-3.1665*H)
    r_M=0.62544+0.13664*H-W*(0.00189+0.002425/H^2)+(1/W)*(0.57986+2.545*H-0.02255*G)
    ifelse(input$Sex=="Female",r<-r_F,r<-r_M)
    time_interval_12<-as.numeric(input$drink_time_1)-as.numeric(input$drink_time_2)
    
    ## Formula to calculate Blood Alcohol Concentration
    t<-seq(0,1440,length=1000)
    y2<-A2*P2*0.789*(1- exp(-K*(t*60)))/(r*W)-(0.018/100/3600)*(t*60)
    y2[y2<0]=0
    y2 <- y2*100
    ## plot Curve 2
    plot(t+time_interval_12, y2, xlim=c(0,1440), type="l", xlab="Time Since the Start of Drinking", ylab="Blood Alcohol Concentration (%)",main="Alcohol Metabolism Curve 2")
  })
  
  ############ Curve 3
  output$Curve_3<-renderPlot({
    ## parameters
    A3<-as.numeric(input$Volume_3)/1000
    P3<-as.numeric(input$alc_percent_3)/100
    W<-as.numeric(input$Weight)
    H<-as.numeric(input$Height)/100
    G<-as.numeric(input$Age)
    K<- 0.2/60
    r_F=0.50766+0.11165*H-W*(0.001612+0.0031/H^2)-(1/W)*(0.62115-3.1665*H)
    r_M=0.62544+0.13664*H-W*(0.00189+0.002425/H^2)+(1/W)*(0.57986+2.545*H-0.02255*G)
    ifelse(input$Sex=="Female",r<-r_F,r<-r_M)
    time_interval_13<-as.numeric(input$drink_time_1)-as.numeric(input$drink_time_3)
    
    ## Formula to calculate Blood Alcohol Concentration
    t<-seq(0,1440,length=1000)
    y3<-A3*P3*0.789*(1- exp(-K*(t*60)))/(r*W)-(0.018/100/3600)*(t*60)
    y3[y3<0]=0
    y3 <- y3*100
    ## plot Curve 3
    plot(t+time_interval_13, y3, xlim=c(0,1440), type="l", xlab="Time Since the Start of Drinking", ylab="Blood Alcohol Concentration (%)", main="Alcohol Metabolism Curve 3")
    
  })
  
  ############ Curve 4
  output$Curve_4<-renderPlot({
    ## parameters
    A4<-as.numeric(input$Volume_4)/1000
    P4<-as.numeric(input$alc_percent_4)/100
    W<-as.numeric(input$Weight)
    H<-as.numeric(input$Height)/100
    G<-as.numeric(input$Age)
    K<- 0.2/60
    r_F=0.50766+0.11165*H-W*(0.001612+0.0031/H^2)-(1/W)*(0.62115-3.1665*H)
    r_M=0.62544+0.13664*H-W*(0.00189+0.002425/H^2)+(1/W)*(0.57986+2.545*H-0.02255*G)
    ifelse(input$Sex=="Female",r<-r_F,r<-r_M)
    time_interval_14<-as.numeric(input$drink_time_1)-as.numeric(input$drink_time_4)
    
    ## Formula to calculate Blood Alcohol Concentration
    t<-seq(0,1440,length=1000)
    y4<-A4*P4*0.789*(1- exp(-K*(t*60)))/(r*W)-(0.018/100/3600)*(t*60)
    y4[y4<0]=0
    y4 <- y4*100
    ## plot Curve 4
    plot(t+time_interval_14, y4, xlim=c(0,1440), type="l", xlab="Time Since the Start of Drinking", ylab="Blood Alcohol Concentration (%)", main="Alcohol Metabolism Curve 4")
    
  })
  
  ############  Mixed effect
  ## parameters
  output$Curve_Mixed<-renderPlot({
    A1<-as.numeric(input$Volume_1)/1000
    P1<-as.numeric(input$alc_percent_1)/100
    A2<-as.numeric(input$Volume_2)/1000
    P2<-as.numeric(input$alc_percent_2)/100
    A3<-as.numeric(input$Volume_3)/1000
    P3<-as.numeric(input$alc_percent_3)/100   
    A4<-as.numeric(input$Volume_4)/1000
    P4<-as.numeric(input$alc_percent_4)/100   
    W<-as.numeric(input$Weight)
    H<-as.numeric(input$Height)/100
    G<-as.numeric(input$Age)
    K<- 0.2/60
    r_F=0.50766+0.11165*H-W*(0.001612+0.0031/H^2)-(1/W)*(0.62115-3.1665*H)
    r_M=0.62544+0.13664*H-W*(0.00189+0.002425/H^2)+(1/W)*(0.57986+2.545*H-0.02255*G)
    ifelse(input$Sex=="Female",r<-r_F,r<-r_M)
    
    ## Calculate the mixed Blood Alcohol Concentration   
    t<-seq(0,1439,length=1440)
    y<-rep(0,1440)
    time_interval_12<-as.numeric(input$drink_time_1)-as.numeric(input$drink_time_2)
    time_interval_13<-as.numeric(input$drink_time_1)-as.numeric(input$drink_time_3)
    time_interval_14<-as.numeric(input$drink_time_1)-as.numeric(input$drink_time_4)
    for(i in 1:1440) {
      j<- i-time_interval_12
      k<- i-time_interval_13
      m<- i-time_interval_14
      j[j<1] <-1
      k[k<1] <-1
      m[m<1] <-1
      y[i]<-max(0,A1*P1*0.789*(1- exp(-K*t[i]*60))/(r*W)-(0.018/100/3600)*t[i]*60)
      y[i]<-y[i]+max(0,A2*P2*0.789*(1-exp(-K*t[j]*60))/(r*W)-(0.018/100/3600)*t[j]*60)
      y[i]<-y[i]+max(0,A3*P3*0.789*(1-exp(-K*t[k]*60))/(r*W)-(0.018/100/3600)*t[k]*60)
      y[i]<-y[i]+max(0,A4*P4*0.789*(1-exp(-K*t[m]*60))/(r*W)-(0.018/100/3600)*t[m]*60)
    } 
      y <- y*100
    ## plot Curve_Mixed
    plot(t, y, type="l", xlab="Time Since the Start of Drinking", ylab="Blood Alcohol Concentration (%)",main="Alcohol Metabolism Curve-Mixed Effect",lwd=2)
    ## 0.02 line
    i <- which(y>0.02)
    i.n<-i[length(i)]
    abline(v=t[i.n],col="green",lwd=2)
    ## 0.08 line
    j<-which(y>0.08)
    j.n<-j[length(j)]
    abline(v=t[j.n],col="blue",lwd=2)
    ## current line
    abline(v=as.numeric(input$drink_time_1),col="red",lwd=2)
    legend('topright', c("Legally Impaired Cutoff (Age < 21)","Legally Impaired Cutoff (Age >= 21)","You Are Here"), lty=1, col=c('green','blue','red'), bty='n', cex=1.5)
    ## text
    output$text1 <- reactive({
      paste("Waiting time before you can drive (Age >= 21):", t[j.n]-as.numeric(input$drink_time_1),"min")
    })
    output$text2 <- reactive({
      paste("Waiting time before you can drive (Age < 21):", t[i.n]-as.numeric(input$drink_time_1),"min")
    })
  })
  
}

shinyApp(ui=ui,server = server)

