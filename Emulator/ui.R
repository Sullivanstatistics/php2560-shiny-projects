#This file creates the user-interface funtionality for the Emulator Shiny app.

library(shiny)
library(shinydashboard)

#This file creates the user interface for the Emulator Shiny app.

dashboardPage(skin="black",
  dashboardHeader(title = "Emulator App"),
  dashboardSidebar(
    sidebarMenu(
      
      #The following lines of code refer to each tab of the app.
      menuItem("Background", tabName = "background", icon = icon("info-circle")),
      menuItem("Tutorial", tabName = "tutorial", icon = icon("arrow-right")),
      menuItem("Input your own data", tabName = "input", icon = icon("file"))
    )
  ),
  dashboardBody(
    tabItems(
      
      #This creates the contents of the "Background" tab.
      tabItem(tabName = "background",
              fluidRow(
                box(title = "What is an emulator?", status = "warning", solidHeader = TRUE, width = 12,
                    helpText("Emulators or metamodels are easy-to-evaluate (statistical) approximations of 
                             mathematical functions, and can be used to approximate detailed models (e.g.,  
                             complex simulation models), thereby helping to mitigate challenges related to the 
                             high computational cost of the latter. An emulator is built from a dataset 
                             of inputs and the corresponding output (the design points) from a series of 
                             evaluations of the detailed model (the simulator). Because it is not practical 
                             to evaluate all possible combination of inputs due to the computational burden of  
                             the simulator, the design points are a subset of the design space, i.e., the region of 
                             possible values."),
                    
                    helpText("A good emulator has 3 properties. 
                             (1) The emulator should be quicker to evaluate than the simulator
                             (2) The emulator should aggree with the simulator at the design points (e.g., 
                             the model passes through all design points and there is no uncertainty) 
                             (3) The emulator should interpolate the simulator output at other points, with 
                             decreasing uncertainty closer to the design points"),
                    
                    helpText("Figure 1 illustrates that ordinary least squares regression models are not 
                             preferred as emulators of detailed models, because they do not satisfy the 
                             last two properties: the mean prediction from the regression generally does 
                             not pass through the design points and the uncertainty around the regression 
                             line is not zero at the observed (design) points. Thus, while regressions have 
                             been used to develop model emulators, we will favor Gaussian Processes instead.
                             See Figure 2 for an illustration of a Gaussian Process emulator."),
                    
                    helpText("The choice of the design points (e.g., the number of points and their location) 
                             influences how well the emulator approximates the detailed model. This tool will
                             determine the optimal design points for a Gaussian Process emulator. Refer to the
                             Tutorial tab for more information on how this is done.")
                ),
                box(title="Figure 1: Regression", status="success", width=12,
                    img(src="plot5_reg.png", width="100%")
                ),
                box(title="Figure 2: GP emulator", status="success", width=12,
                    img(src="plot5_1.png", width="100%")
                ),
                box(title = "How do you upload files?", status = "warning", solidHeader = TRUE, width = 12,
                    
                    helpText("Figure 3 shows an example R function that can be uploaded into the Emulator app.
                             Figure 4 displays how the user should format the csv file in order to upload design 
                             points. The data must be formatted with x-vlaues in the first column and corresponding
                             y-values in the second column.")
                ),    
                box(title="Figure 3: Function Upload", status="success", width=12,
                    img(src="function_ex.png", width="100%")
                ),
                box(title="Figure 4: Function Upload", status="success", width=12,
                    img(src="dp_ex.png", width="100%")
                )    
              )
      ),
      
      #This creates the contents of the "Tutorial" tab.
      tabItem(tabName = "tutorial",
    
        # Boxes need to be put in a row (or column)
        fluidRow(
          box(title = "Tutorial overview", status = "warning", solidHeader = TRUE, width = 12,
              helpText("This tutorial will allow you to see how the choice of design points influences
                       the emulator and the fit of the emulator")
          ),
          box(title = "Inputs: Simulator", status = "primary", width = 12,
              
              #This creates a drop-down box which allows the user to choose 1 of 3 test models to simulate.
              selectInput("userinput_tutorial", label = h4("Choose an example model"), 
                          choices = c("",
                                      "Test 1" ="t1", 
                                      "Test 2" ="t2",  
                                      "Test 3" ="t3")),
              #This creates a user-entry box to define the number of initial design points  
              numericInput("N.init.dp_tutorial", label="Enter number of initial design points", value=5),
              
              #This creates a user-entry box to define the maximum number of additional design points  
              numericInput("max.N.dp_tutorial", label="Enter maximum number of additional design points", value=3),
              h6("Note: See how changing these inputs affects the output and fit of the emulator"),

              ##The following line of code enables the "calculate" button.
              actionButton(inputId = "go_tutorial", 
                             label = "Calculate")
          
          ),
          
          #This creates the plot of the tutorial simulator.
          box(title = "Output: Emulator", status = "success", width = 12,
            plotOutput("testplot_tutorial")
          ),
          
          #This creates a table of the chosen design points for the tutorial simulator.
          box(title = "Output: Design Points", status = "success",width = 12,
            tableOutput("x_tutorial"))
        )
      ),
      
      #This creates the contents of the "Input" tab.
      tabItem(tabName = "input",
        fluidRow(
          box(title = "Use your own data", status = "warning", solidHeader = TRUE, width = 12,
              helpText("Refer to tutorial for help")
          ),
          
          #This enables the user to choose the input method using a drop-down box. 
          #The choices are to upload a function of their model, to upload a list of 
          #initial design points given the built-in model and to do both.
          box(title = "Inputs: Simulator", status = "primary", width = 12,
              selectInput("userinput", label = h4("Which of the following can you upload?"), 
                          choices = c("",
                                      "A function of my model"="m", 
                                      "A set of initial design points" ="d",  
                                      "Both"="b")),
              h6("If none, you can still use the tutorial"),
              
              #The following panel appears only if the user select the "upload model" 
              #option immediately above. This allows them to choose the file and enter 
              #initial and maximum design points. 
              conditionalPanel(
                condition = "input.userinput=='m'",
                fileInput("modfile", label="Upload model"),
                h6("File must be in .R formating, starting with 'function(...)'"),
                
                numericInput("N.init.dp", label="Enter number of initial design points", value=5),
                numericInput("max.N.dp1", label="Enter maximum number of additional design points", value=3)
                
              ),
              
              #The following panel appears only if the user select the "upload design points" 
              #option immediately above. This allows them to choose the file and enter 
              #maximum design points. 
              conditionalPanel(
                condition = "input.userinput=='d'",
                fileInput("dpfile", label="Upload initial design points"),
                h6("File must be in .csv format with columns Y and X"),
                
                numericInput("max.N.dp2", label="Enter maximum number of additional design points", value=3)
              ),
              
              #The following panel appears only if the user select the "both" 
              #option immediately above. This allows them to choose both files and enter 
              #maximum design points. 
              conditionalPanel(
                condition = ("input.userinput=='b'"),
                fileInput("modfile2", label="Upload model"),
                h6("File must be in .R formating, starting with 'function(...)'"),
                fileInput("dpfile2", label="Upload initial design points"),
                h6("File must be in .csv format with columns Y and X"),
                
                numericInput("max.N.dp3", label="Enter maximum number of additional design points", value=3)
              )
              
              ##The following line of code enables the "calculate" button
              , actionButton(inputId = "go", 
                             label = "Calculate")
          ),
          
          #This creates the plot of the  simulated output.
          box(title = "Output: Emulator", status = "success", width = 12,
              plotOutput("testplot")
          ),
          
          #This creates a table of the chosen design points for the simulated output.
          box(title = "Output: Design Points", status = "success",width = 12,
              tableOutput("x"))
        )
      )
    )
  )
)