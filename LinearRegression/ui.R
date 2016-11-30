library(shiny)
shinyUI(
  pageWithSidebar(
    
    
    # Header:
    headerPanel("Linear Regression"),
    
    
    # Input in sidepanel:
    sidebarPanel(
      tags$head(
        tags$style(type="text/css", "select { max-width: 200px; }"),
        tags$style(type="text/css", ".span4 { max-width:200px; }"),
        tags$style(type="text/css", ".well { max-width: 300px; }")
      ),
      
      # Upload data:
      fileInput("file", "Upload csv data-file:"),
      
      #Using action buttoms to load sample dataset
      #change the color of the buttom to contrast with previous blank
      actionButton("myLoader", "Load test dataset",  
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      
      #add block between each part
      hr(),
      
      # Variable selection:
      #Independent Numeric variable selection:
      htmlOutput("varselect_num"),
      
      #Independent Categorical variable selection:
      htmlOutput("varselect_cat"),
      
      #Dependent variable selection:
      htmlOutput("outcomeselect"),
      
      #Because next part is the download file part, so we add a line to block between variable selection and 
      #file download
      
      hr(),
      
      #Name of dataset
      
      htmlOutput("datasetnameout"),
      
      
      #Name on report
      textInput("name", "Author name", value = "Name"),
      
      #Radio buttons for choosing format
      radioButtons('format', "Document format", c('PDF', 'HTML', 'Word'), inline = TRUE),
      
      #Download button
      downloadButton('downloadReport')
      , width=3),
    
    # Main:
    mainPanel(
      tags$head(
        tags$style(type='text/css', 
                   ".nav-tabs {font-size: 14px} ")), 
      tabsetPanel(type = "tabs", 
                  tabPanel("Scatterplots", plotOutput("ScatterMatrix", width = "100%", height = "580px"),
                           textInput("text_scatt", label = "Interpretation", value = "Enter text...")), 
                  tabPanel("Boxplots", plotOutput("BoxPlot", height = "580px"),
                           textInput("text_box", label = "Interpretation", value = "Enter text...")),
                  tabPanel("Summary statistics", br(),verbatimTextOutput("lmResults"),
                           textInput("text_summary", label = "Interpretation", value = "Enter text...")), 
                  tabPanel("Diagnostic plots",  plotOutput("diagnostics", height = "580px"),
                           textInput("text_diagno", label = "Interpretation", value = "Enter text...")),
                  tabPanel("Added variable plots",  plotOutput("added", height = "580px"),
                           textInput("text_added", label = "Interpretation", value = "Enter text...")),
                  tabPanel("Marginal model plots",  plotOutput("MMPlot", height = "580px"),
                           textInput("text_mmp", label = "Interpretation", value = "Enter text...")),
                  tabPanel("Help",  htmlOutput("inc"))
                  
      )
    )
  ))