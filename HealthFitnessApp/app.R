
# ui.R

library(shiny)
options(shiny.deprecation.messages=FALSE)

ui<-shinyUI(navbarPage(
  id = "page-nav",
  tags$head(tags$style(HTML(
    "#page-nav > li:first-child { display: none; }"
  ))),

#Tags used to import Google fonts for customization

  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin|Bangers:400,700');
                    "))
    ),uiOutput('ui'),

#Three main headings
  
  h1("Health and Fitness App", 
     style = "font-family: 'Lobster', cursive;
     font-weight: 500; line-height: 1.1; 
     color:magenta;font-size: 70px"),
  
  h2("Julia Callaway, Yanbing Wang, Iman Saeed", 
     style = "font-family: 'Lobster', cursive;
     font-weight: 500; line-height: 1.1; 
     color:orange;"),
  
  h3("PHP 2560: Statistical Computing", 
     style = "font-family: 'Lobster', cursive;
     font-weight: 500; line-height: 1.1; 
     color:red;"),
  
#First tab panel created
  tabsetPanel( id="tabs",
  tabPanel( title="Enter Information",
            width=10,
            tags$style(type="text/css", '#wellPanel { width:200px; float:left;}'),
            br(),
            h1("Let's begin!", style = "font-family: 'Lobster', cursive;
               font-weight: 500; line-height: 1.1;color:yellow"),
            helpText(h3("Start by scrolling down and entering all of your information to receive a graphical and textual summary of your health and fitness. ",style="font-family: 'Lobster', cursive;color:black")),
            br(),
            fluidRow(
            column(img(src='http://media.bestofmicro.com/M/P/519217/original/fitness_tracker_guide_cover_2.jpg', height=200,width=320,align="middle"),width=3),
            column(img(src='https://solutionstopollution.files.wordpress.com/2015/04/good-food-guide-editor-calls-for-more-veggie-options-on-pub-menus.jpg', height=200,width=320,align="middle"),width=3),
            column(img(src='http://magazine-fitness.com/wp-content/uploads/2014/12/324104_1359589543_active-health-and-fitness-services-1.jpg', height=200,width=320,align="middle"),width=3),
            column(img(src='http://www.nautilusplus.com/content/uploads/2015/01/pieges_karineL.jpg', height=200,width=310,align="middle"),width=3)
            ),
            h1("About You", style = "font-family: 'Bangers';
               font-weight: 500; line-height: 1.1; color: green;font-size: 40px"),

#All of the widgets are stored in different rows according to their category
            
            fluidRow(
              column(numericInput("num", 
                                    label = h3("Height in inches",style="font-family: 'Bangers';color:hotpink"), 
                                    value = 1),width=3),
              column(numericInput("num2", 
                                    label = h3("Weight in pounds",style="font-family: 'Bangers';color:hotpink"), 
                                    value = 1),width=3),
              column(numericInput("num3", 
                                    label = h3("Age",style="font-family: 'Bangers';color:hotpink"), 
                                    value = 1),width=3),
              column(radioButtons("checkGroup", 
                                    label = h3("Check Gender",style="font-family: 'Bangers';color:hotpink"), 
                                    choices = list("Female" = 1, 
                                                   "Male" = 2),selected = 1),width=3)),
            fluidRow(column(4,img(src='http://dietpics.com/wp-content/uploads/2015/11/Calculate-target-heart-rate-for-weight-loss.gif', height=300,width=650),offset=2)),
            h1("Your Daily Activities", style = "font-family: 'Bangers';
               font-weight: 500; line-height: 1.1;color:green;font-size: 40px"),
            fluidRow(column(2,radioButtons("checkGroup2", 
                                    label = h3("Check Activity Level",style="font-family: 'Bangers';color:hotpink"), 
                                    choices = list("Sedentary" = 1, 
                                                   "Lightly Active" = 2, "Moderately Active"=3,"Very Active"=4,"Extra 
                                                   Active"=5),selected = 1)),
                     column(4,img(src='http://www.bikejames.com/wp-content/uploads/2015/04/Health-and-Fitness.jpg', height=300,width=450),offset=1)),
            helpText(h3("Enter Time (Hours) Spent in Each Physical Activity Selected",style="font-family: 'Bangers';color:hotpink"),
                     fluidRow(
                       column(numericInput("num4", 
                                             label = h5("Walking"),
                                             value = 0),width=4),
                       column(numericInput("num5",
                                             label = h5("Running"), 
                                             value = 0),width=4),
                       column(numericInput("num6", 
                                             label = h5("Swimming"), 
                                             value = 0),width=4)),
                     fluidRow(
                       column(numericInput("num7", 
                                             label = h5("Sitting"), 
                                             value = 0),width=4),
                       column(numericInput("num8", 
                                             label = h5("Standing"), 
                                             value = 0),width=4),
                       column(numericInput("num9", 
                                             label = h5("Weight Training"), 
                                             value = 0),width=4)),
                     numericInput("num10", 
                                  label = h5("Sleeping"), 
                                  value = 0),
                     fluidRow(
                     column(img(src='http://www.realfitmdphd.com/wp-content/uploads/2016/01/grapes-690230_1280-1170x550.jpg', height=300,width=550),width=6),
                     column(img(src='http://www.outsideonline.com/sites/default/files/styles/full-page/public/migrated-images/organic-food-fad_h.jpg?itok=uFTeli5i',height=300,width=450),width=6)
            ),
              h1("Your Eating Habits", style = "font-family: 'Bangers';
               font-weight: 500; line-height: 1.1;color:green"),
                     fluidRow(
                       column(radioButtons("radio", label = h3("Select Servings of Vegetables",style="font-family: 'Bangers';color:hotpink"), 
                                             choices = list("0-1" = 1, "2-4" = 2,
                                                            "5-7" = 3, "8-10"=4), selected = 1),width=4),
                       column(radioButtons("radio2", label = h3("Select Servings of Fruits",style="font-family: 'Bangers';color:hotpink"), 
                                             choices = list("0-1" = 1, "2-4" = 2,
                                                            "5-7" = 3, "8-10"=4), selected = 1),width=4),
                       column(radioButtons("radio3", label = h3("Select Servings of Grains",style="font-family: 'Bangers';color:hotpink"), 
                                             choices = list("0-1" = 1, "2-4" = 2,
                                                            "5-7" = 3, "8-10"=4), selected = 1),width=4)),
                     fluidRow(
                       column(radioButtons("radio4", label = h3("Select Servings of Protein",style="font-family: 'Bangers';color:hotpink"), 
                                             choices = list("0-1" = 1, "2-4" = 2,
                                                            "5-7" = 3, "8-10"=4), selected = 1),width=4),
                       column(radioButtons("radio5", label = h3("Select Servings of Dairy",style="font-family: 'Bangers';color:hotpink"), 
                                             choices = list("0-1" = 1, "2-4" = 2,
                                                            "5-7" = 3, "8-10"=4), selected = 1),width=4),
                       column(radioButtons("radio6", label = h3("Select Glasses of Water",style="font-family: 'Bangers';color:hotpink"), 
                                             choices = list("0-1" = 1, "2-4" = 2,
                                                            "5-7" = 3, "8-10"=4), selected = 1),width=4)),
                     helpText(h1(a("To calculate the number of calories consumed for each meal, click here.",target="_blank",href="https://www.caloriecount.com/")),style = "font-family: 'Lobster', cursive;
                              font-weight: 500; line-height: 1.1;color:red"),
                     fluidRow(
                       column(sliderInput("slider1", label = h3("Calories Consumed for Breakfast",style="font-family: 'Bangers';color:black"),
                                            min = 0, max = 1000, value = 50), width = 6),
                       column(sliderInput("slider2", label = h3("Calories Consumed for Lunch",style="font-family: 'Bangers';color:black"),
                                            min = 0, max = 1000, value = 50), width = 6)),
                     fluidRow(
                       column(sliderInput("slider4", label = h3("Calories Consumed for Snack",style="font-family: 'Bangers';color:black"),
                                             min = 0, max = 1000, value = 50), width = 6),
                       column(sliderInput("slider3", label = h3("Calories Consumed for Dinner",style="font-family: 'Bangers';color:black"),
                                            min = 0, max = 1000, value = 50), width = 6)),
              fluidRow(column(4,img(src='http://bmedia.fooducate.com/wp-content/uploads/2015/10/serving-size-key.jpg',height=500,width=650,align='center'),offset=1)),
              h1("What You Can Do to Manage Your Portions**", style = "font-family: 'Bangers';
               font-weight: 500; line-height: 1.1;color:green;font-size: 40px;color:hotpink"),
            h3('1.	Not sure what a portion size should be? Make sense of portion sizes by using hand symbols for portions.'),
            h3('2.	Learn to read food labels.'),
            h3('3.	Compare marketplace portions to recommended serving sizes.' ),
            h3('4.	 Repackage supersize bags.'),
            h3('5.	Share a meal.'), 
            h3('6.	 Eat half or less.') ,
            h3('7.	 Use a smaller plate.'), 
            h3('8.	Slow down and skip second helpings.') ,
            
              br(),  
              h1("Your Mental Health", style = "font-family: 'Bangers';
               font-weight: 500; line-height: 1.1;color:green;font-size: 40px"),
                     fluidRow(
                       column(div(checkboxGroupInput("select5", label = h3("Select Item(s) that Apply to You to Calculate Stress*",style="font-family: 'Bangers';color:hotpink"), 
                                                       choices = list("I find myself less eager to go back to work or to resume my chores after a weekend." =1,  
                                                                      "I feel less and less patient and/or sympathetic listening to other peoples' problems."=2,
                                                                      "I ask more closed-ended questions to discourage dialogue with friends and co-workers than open-ended ones to encourage it."=3,
                                                                      "I try to get away from people as soon as I can."=4,
                                                                      "My dedication to work, exercise, diet, and friendships is waning."=5,
                                                                      "I am falling further behind in many of the responsibilities in my life."=6,
                                                                      "I am losing my sense of humor."=7,
                                                                      "I find it more and more difficult to see people socially."=8,
                                                                      "I feel tired most of the time."=9,
                                                                      "I don't seem to have much fun anymore."=10,
                                                                      "I feel trapped."=11,
                                                                      "I know what will make me feel better, but I just can't push myself to do it and I'll say Yes, but ignore any suggestions that people make."=12
                                                       ), 
                                                       selected = 1
                       )), width = 4),
                       column(2,img(src='http://athrucoaching.com/wp-content/uploads/2014/03/Stress2.jpg', height=300,width=450),offset=1, align="right")),
                     
                     fluidRow(column(img(src='https://s-media-cache-ak0.pinimg.com/236x/00/c0/91/00c09179a235205330ce29b9bb599a80.jpg', height=200,width=200,align="middle"),width=3),
                              column(img(src='https://pbs.twimg.com/media/A9vIZjLCUAAYqst.jpg', height=200,width=200,align="middle"),width=3),
                              column(img(src='http://thebornworthy.com/wp-content/uploads/2014/06/photo51-e1404101549578.jpg', height=200,width=200,align="middle"),width=3),
                              column(img(src='http://cdn2.stylecraze.com/wp-content/uploads/2012/12/Health-Is-Wealth-10-Simple-Tips-To-Stay-Healthy.jpg',height=200,width=250),width=3)),
                    br(),
               h1("Get Healthy Inspiration", style = "font-family: 'Bangers';
               font-weight: 500; line-height: 1.1;color:green;font-size: 40px"),
                     fluidRow(
                       column(selectInput("select2", label = h3("Select a Meal Category for Healthy Recipes",style="font-family: 'Bangers';color:hotpink"), 
                                            choices = list("Breakfast" = 1, "Lunch" = 2,
                                                           "Dinner" = 3, "Snack"=4), selected = 1),width=4),
                       column(selectInput("select3", label = h3("Select an Exercise Category for Work-Out Ideas",style="font-family: 'Bangers';color:hotpink"), 
                                            choices = list("Endurance" = 1, "Strength" = 2,
                                                           "Balance" = 3, "Flexibility"=4), selected = 1),width=4),
                       column(selectInput("select4", label = h3("Select an Exercise Category for a Playlist",style="font-family: 'Bangers';color:hotpink"), 
                                            choices = list("Strength Training" = 1, "Walking" = 2,
                                                           "Running" = 3, "Biking"=4,"Yoga"=5), selected = 1),width=4)
                     ),
                     br(),
                    br(),
                    fluidRow(
                     column(img(src='https://45.media.tumblr.com/62134276a08d66134ee40b8ea56fee50/tumblr_nt18109fDi1tq4of6o1_500.gif',height=400,width=550),width=12,align='center')
                    ),
                     br(),
                     br(),
                     fluidRow(
                    column(h1("Done! Proceed to the 'Results' tab to see your health and fitness summary!", style="font-family: 'Bangers';color:red;font-size: 50px",align='center'),width=12)
                     ), 
            
#Tab link
            h1(actionLink("link_to_tabpanel_b", "Link to 'Results' tab"),align='center'),
                    br(),
                     helpText("*Stress Quiz procured from: http://journalofofficeworkers.com/find-out-how-stressed-you-are/."),
                      helpText('**Tips procured from http://www.healthyeating.org/Healthy-Eating/Healthy-Living/Weight-Management/Article-Viewer/Article/348/correct-portion-sizes-how-to-keep-portion-distortion-in-check.aspx'))),
  
#Second tab panel created

tabPanel(title="Results", value="Results",br(),h2("Below are the results from your entered information!",style = "font-family: 'Lobster', cursive;
                                  font-weight: 500; line-height: 1.1;color:yellow"),
           list(tags$head(tags$style("body {background-color: skyblue; }"))),
         
    #Within the output tab panel, there are 4 additional tabs
    #Tab 1
    
           tabsetPanel(
             tabPanel(h1("Plot",style = "font-family: 'Lobster', cursive;
                         font-weight: 500; line-height: 1.1;"), plotOutput("distPlot",width="100%"),
                      plotOutput("distPlot2",width="100%"),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      plotOutput("distPlot3",width="100%"),
                      br(),
                      br(),
                      br(),
                      br(),
                      plotOutput("distPlot4"),
                      br(),
                      br(),
                      br(),
                      h1("Having trouble interepreting the plots? Head over to the 'About Me' tab!",style="font-family: 'Bangers';color:darkviolet",align='center')),
  #Tab 2 
  
             tabPanel(h1("Summary",style = "font-family: 'Lobster', cursive;
                         font-weight: 500; line-height: 1.1;color:"), 
                      h2("About You",style="font-family: 'Bangers';color:darkviolet;font-size: 50px"),
                      h1(uiOutput("text1")),img(src='https://www.fitneass.com/wp-content/uploads/2014/07/Body-mass-index-BMI.jpg',height=400,width=450), 
                      h2("Your Eating Habits",style="font-family: 'Bangers';color:darkviolet;font-size: 50px"),
                      uiOutput("text2"),
                      uiOutput("text3"), uiOutput("text4"),
                     uiOutput("text5"),uiOutput("text6"),
                     uiOutput("text13"),uiOutput("text14"),img(src='http://www.shareifitness.com/wp-content/uploads/2015/11/how-much-water-should-i-drink-to-lose-weight-360x255.gif', height=300,width=450,align="middle"),
                      uiOutput("text7"),
                      uiOutput("text8"),
                     uiOutput("text9"),uiOutput("text10"),
                      uiOutput("text12"),
                      img(src='http://www.shapesense.com/images/calories.jpg',height=300,width=450),
                      h2("Your Daily Activities",style="font-family: 'Bangers';color:darkviolet;font-size: 50px"),
                      uiOutput("text11"), uiOutput("text15"),img(src='http://az616578.vo.msecnd.net/files/2016/03/13/635934252318645778-1369130504_exercise.jpg',height=300,width=450),
                      h2("Your Mental Health",style="font-family: 'Bangers';color:darkviolet;font-size: 50px"),
                     uiOutput("dynamic"),
                      img(src='https://mackinstitute.wharton.upenn.edu/wp-content/uploads/2015/08/mental-health.jpg',height=300,width=550)),
  #Tab 3

        tabPanel(h1("Healthy Inspiration",style = "font-family: 'Lobster', cursive;
                         font-weight: 500; line-height: 1.1;"), h2("Recipe for Your Meal of Choice",style="font-family: 'Bangers';color:red"),
                      uiOutput("iframe"),h2("Work-Out for Your Category of Choice",style="font-family: 'Bangers';color:red"), uiOutput("iframe2"),
                      h2("Playlist for Your Work-Out of Choice",style="font-family: 'Bangers';color:red"),uiOutput("iframe3")),
 #Tab 4
 
       tabPanel(h1("References",style = "font-family: 'Lobster', cursive;
                         font-weight: 500; line-height: 1.1;"),
                      h4("http://media.bestofmicro.com/M/P/519217/original/fitness_tracker_guide_cover_2.jpg"),
                      br(),
                      h4("https://solutionstopollution.files.wordpress.com/2015/04/good-food-guide-editor-calls-for-more-veggie-options-on-pub-menus.jpg"),
                      br(),
                      h4("http://magazine-fitness.com/wp-content/uploads/2014/12/324104_1359589543_active-health-and-fitness-services-1.jpg"),
                      br(),
                      h4("http://dietpics.com/wp-content/uploads/2015/11/Calculate-target-heart-rate-for-weight-loss.gif"),
                      br(),
                      h4("https://s-media-cache-ak0.pinimg.com/236x/00/c0/91/00c09179a235205330ce29b9bb599a80.jpg"),
                      br(),
                      h4("https://pbs.twimg.com/media/A9vIZjLCUAAYqst.jpg"),
                      br(),
                      h4("http://thebornworthy.com/wp-content/uploads/2014/06/photo51-e1404101549578.jpg"),
                      br(),
                      h4("https://s-media-cache-ak0.pinimg.com/736x/74/5d/3e/745d3edf8569d2eb4ba17adb35393782.jpg"),
                      br(),
                      h4("http://www.shareifitness.com/wp-content/uploads/2015/11/how-much-water-should-i-drink-to-lose-weight-360x255.gif"),
                      br(),
                      h4("http://www.realfitmdphd.com/wp-content/uploads/2016/01/grapes-690230_1280-1170x550.jpg"),
                      br(),
                      h4("http://athrucoaching.com/wp-content/uploads/2014/03/Stress2.jpg"),
                      br(),
                      h4("http://cdn2.stylecraze.com/wp-content/uploads/2012/12/Health-Is-Wealth-10-Simple-Tips-To-Stay-Healthy.jpg"),
                      br(),
                      h4("http://ourgrandlife.com/wp-content/uploads/2016/03/fitness-hub_357x171_108225350.jpg"),
                      br(),
                      h4("http://www.shapesense.com/images/calories.jpg"),
                      br(),
                      h4("http://az616578.vo.msecnd.net/files/2016/03/13/635934252318645778-1369130504_exercise.jpg"),
                      br(),
                      h4("http://images.agoramedia.com/EHBlogImages/johannah-sakimura/2015/03/healthier-smoothie-722x406.jpg?width=726"),
                      br(),
                      h4("http://www.capitalotc.com/wp-content/uploads/2015/04/Smiley1.jpg"),
                      br(),
                      h4("http://www.outsideonline.com/sites/default/files/styles/full-page/public/migrated-images/organic-food-fad_h.jpg?itok=uFTeli5i"),
                      br(),
                      h4("http://www.angrycrapper.com/wp-content/uploads/2013/11/wl_bmtn_finish_line_020313.jpg"),
                      br(),
                      h4('https://45.media.tumblr.com/62134276a08d66134ee40b8ea56fee50/tumblr_nt18109fDi1tq4of6o1_500.gif'),
                      br(),
                      h4('https://www.fitneass.com/wp-content/uploads/2014/07/Body-mass-index-BMI.jpg'),
                      br(),
                      h4('https://mackinstitute.wharton.upenn.edu/wp-content/uploads/2015/08/mental-health.jpg'),
                      br(),
                      h4('http://www.bikejames.com/wp-content/uploads/2015/04/Health-and-Fitness.jpg'),
                      br(),
                      h4('http://www.nautilusplus.com/content/uploads/2015/01/pieges_karineL.jpg')
                      )
             )),

#Third (final) tab panel created

  tabPanel(title="About Me",
           h1("Welcome!",style = "font-family: 'Lobster', cursive;
              font-weight: 500; line-height: 1.1;color:yellow"),
           h2("We see this app being used as a tool to evaluate your health and fitness by keeping track of your diet and exercise; as well as an aid to weight loss and maintenance. Reflecting on what you eat helps you become conscious of your inclinations, and hopefully modifies your behavior. ",style = "font-family: 'Lobster', cursive;
              font-weight: 500; line-height: 1.1;"),
           br(),
           fluidRow(
           column(img(src='http://images.agoramedia.com/EHBlogImages/johannah-sakimura/2015/03/healthier-smoothie-722x406.jpg?width=726',height=300,width=550),width=6),
           column(img(src='http://www.capitalotc.com/wp-content/uploads/2015/04/Smiley1.jpg',height=300,width=500),width=6)
           ),
           br(),
           h3("To begin, click on the 'Enter Information' tab at the top of the screen. Here, you will be asked to enter information pertinent to your health and fitness. If you are not sure about the number of calories you have consumed, a link is provided that details the calorie content of all foods. There are five sections to complete -- 'About You', 'Your Eating Habits', 'Your Daily Activities', 'Your Mental Health', and 'Get Health Inspirations'."),
           br(),
           h3("Once you have finished entering all the information required in the 'Enter Information' tab, you will progress to the 'Results' tab at the top of the page. Here, you will be provided with three different sources of information. The first tab, 'Plot', offers four different graphs to help you evaluate your health and fitness. The first graph is a histogram of your recommended calorie distribution, with a red line indicating your position on it. See how close you are to the mean of the distribution. The second graph is a pie chart of the number of calories you have consumed from each meal. Do you notice a particular meal with a large number of calories? The third graph is a pie chart of the hours spent in daily activities including exercise and sleep. What activity seems to take up the most time? The last graph is a scatter plot of the number of calories you will require as you age if your weight stays constant."),
           br(),
           h3("The second tab, 'Summary', offers feedback on your entered health information in four different categories: 'About You', 'Your Eating Habits', 'Your Daily Activities', and 'Your Mental Health'."),
           br(),
           h3("The third tab, 'Healthy Inspiration', offers three different Youtube videos to motivate you towards greater health and fitness, according to the categories you chose. The first video is a healthy recipe based on the meal of your choosing. The second video is a work-out routine based on the exercise category of your choosing. The third video is a music playlist based on the exercise category of your choosing. Music distracts people from pain and fatigue, elevates mood, increases endurance, reduces perceived effort and may even promote metabolic efficiency. When listening to music, people run farther, bike longer and swim faster than usual, often without realizing it."),
           br(),
           h2("We hope you find this app informative and enjoyable! Good luck on your health and fitness journey.",style = "font-family: 'Lobster', cursive;
              font-weight: 500; line-height: 1.1;color:hotpink",align='center'),
           fluidRow(column(img(src='http://ourgrandlife.com/wp-content/uploads/2016/03/fitness-hub_357x171_108225350.jpg', height=300,width=550),width=12,align='center')))
  
           )
  ))


# server.R

#A multitude of functions were created to keep the actual server section succinct

suppressWarnings(library(ggplot2))

#BMI function

bmi.f<-function(height,weight){
  bmi<-weight/(height^2) * 703
  if (bmi<18.5){
    f<-h3("Your bmi is:", bmi, "you are underweight.",style="color:red")
  }
  if (bmi>=18.5&bmi<25){
    f<-h3("Your bmi is:", bmi, "you are healthy.",style="color:green")
  }
  if (bmi>=25&bmi<30){
    f<-h3("Your bmi is:", bmi, "you are overweight.",style="color:red")
  }
  if (bmi>=30){
    f<-h3("Your bmi is:", bmi, "you are obese.",style="color:red")
  }
  return(f)
}

#Food servings functions

veg<-function(num){
  if(num>=3){
    f<-h3("You are meeting the minimum daily recommended amount of vegetables.",style="color:green")
  }
  else{
    f<-h3("You are NOT meeting the minimum daily recommended amount of vegetables.",style="color:red")
  }
  return(f)
}
fruit<-function(num){
  if(num>=2){
    f<-h3("You are meeting the minimum daily recommended amount of fruits.",style="color:green")
  }
  else{
    f<-h3("You are NOT meeting the minimum daily recommended amount of fruits.",style="color:red")
  }
  return(f)
}
grains<-function(num){
  if(num>=3){
    f<-h3("You are meeting the minimum daily recommended amount of grains.",style="color:green")
  }
  else{
    f<-h3("You are NOT meeting the minimum daily recommended amount of grains.",style="color:red")
  }
  return(f)
}

meat<-function(num){
  if(num>=2){
    f<-h3("You are meeting the minimum daily recommended amount of meat/poultry.",style="color:green")
  }
  else{
    f<-h3("You are NOT meeting the minimum daily recommended amount of meat/poultry.",style="color:red")
  }
  return(f)
}

dairy<-function(num){
  if(num>=2){
    f<-h3("You are meeting the minimum daily recommended amount of dairy.",style="color:green")
  }
  else{
    f<-h3("You are NOT meeting the minimum daily recommended amount of dairy.",style="color:red")
  }
  return(f)
}

water<-function(num){
  if(num==4){
    f<-h3("You are drinking the daily minimum recommended amount of water.",style="color:green")
  }
  else{
    f<-h3("You are NOT drinking the daily minimum recommended amount of water.",style="color:red")
  }
  return(f)
}
sleep<-function(num){
  if(num>=7){
    f<-h3("You are sleeping the daily minimum recommended amount of hours.",style="color:green")
  }
  else{
    f<-h3("You are NOT sleeping the daily minimum recommended amount of hours.",style="color:red")
  }
  return(f)
}

#Water function

water2<-function(weight,one,two,three,four){
  activity<-sum(one,two,three,four)*60
  hydrate<-(weight*0.5)+((activity/30)*12)
  h3("You require",hydrate,"ounces of water everyday based on weight and activity.",style="color:black")
}

#Basal metabolic rate function

bmr<-function(height,weight,age,gender){
  if(gender==1){
    bmr<- 655 + ( 4.35 * weight) + ( 4.7 * height) - ( 4.7 * age)
  }
  else{
    bmr<-66 + ( 6.23 * weight) + ( 12.7 * height) - ( 6.8 * age )
  }
  return(bmr)
}

#Calorie function that uses BMR

bmr2<-function(activity,height,weight,age,gender){
  if(activity==1){
    bmr2<-bmr(height,weight,age,gender)*1.2
    f<-h3("You require",bmr2,"calories to maintain your weight if your activity level stays the same.",style="color:black")
  }
  if(activity==2){
    bmr2<-bmr(height,weight,age,gender)*1.375
    f<-h3("You require",bmr2,"calories to maintain your weight if your activity level stays the same.",style="color:black")
  }
  if(activity==3){
    bmr2<-bmr(height,weight,age,gender)*1.55
    f<-h3("You require",bmr2,"calories to maintain your weight if your activity level stays the same.",style="color:black")
  }
  if(activity==4){
    bmr2<-bmr(height,weight,age,gender)*1.725
    f<-h3("You require",bmr2,"calories to maintain your weight if your activity level stays the same.",style="color:black")
  }
  if(activity==5){
    bmr2<-bmr(height,weight,age,gender)*1.9
    f<-h3("You require",bmr2,"calories to maintain your weight if your activity level stays the same.",style="color:black")
  }
  return(f)
}

#Calorie function that uses BMR without any text ouput

bmr3<-function(activity,height,weight,age,gender){
  if(activity==1){
    bmr2<-bmr(height,weight,age,gender)*1.2
  }
  if(activity==2){
    bmr2<-bmr(height,weight,age,gender)*1.375
  }
  if(activity==3){
    bmr2<-bmr(height,weight,age,gender)*1.55
  }
  if(activity==4){
    bmr2<-bmr(height,weight,age,gender)*1.725
  }
  if(activity==5){
    bmr2<-bmr(height,weight,age,gender)*1.9
  }
  return(bmr2)
}

#Calories over time function that uses BMR

bmr4<-function(activity,height,weight,age,gender){
  if(activity==1){
    bmr2<-bmr(height,weight,age+10,gender)*1.2
    f<-h3("You will require",bmr2,"calories in 10 years if your weight stays the same.You need fewer calories every decade. We move around less, we have less muscle, and our metabolic rate goes down.The challenge while eating less overall is to eat more nutrient-rich foods, such as fruits, vegetables, whole grains, nuts, beans, fish, low-fat dairy products, and lean cuts of meat.",style="color:black")
  }
  if(activity==2){
    bmr2<-bmr(height,weight,age+10,gender)*1.375
    f<-h3("You will require",bmr2,"calories in 10 years if your weight stays the same.You need fewer calories every decade. We move around less, we have less muscle, and our metabolic rate goes down.The challenge while eating less overall is to eat more nutrient-rich foods, such as fruits, vegetables, whole grains, nuts, beans, fish, low-fat dairy products, and lean cuts of meat.",style="color:black")
  }
  if(activity==3){
    bmr2<-bmr(height,weight,age+10,gender)*1.55
    f<-h3("You will require",bmr2,"calories in 10 years if your weight stays the same.You need fewer calories every decade. We move around less, we have less muscle, and our metabolic rate goes down.The challenge while eating less overall is to eat more nutrient-rich foods, such as fruits, vegetables, whole grains, nuts, beans, fish, low-fat dairy products, and lean cuts of meat.",style="color:black")
  }
  if(activity==4){
    bmr2<-bmr(height,weight,age+10,gender)*1.725
    f<-h3("You will require",bmr2,"calories in 10 years if your weight stays the same.You need fewer calories every decade. We move around less, we have less muscle, and our metabolic rate goes down.The challenge while eating less overall is to eat more nutrient-rich foods, such as fruits, vegetables, whole grains, nuts, beans, fish, low-fat dairy products, and lean cuts of meat.",style="color:black")
  }
  if(activity==5){
    bmr2<-bmr(height,weight,age+10,gender)*1.9
    f<-h3("You will require",bmr2,"calories in 10 years if your weight stays the same.You need fewer calories every decade. We move around less, we have less muscle, and our metabolic rate goes down.The challenge while eating less overall is to eat more nutrient-rich foods, such as fruits, vegetables, whole grains, nuts, beans, fish, low-fat dairy products, and lean cuts of meat.",style="color:black")
  }
  return(f)
}

#Calories burned function using MET values

burned<-function(weight,walk,run,swim,sit,stand,weightt,sleep){
  burn<-(weight/2.2) * ((3.8*walk)+(7.5*run)+(6*swim)+(0.92*sit)+(2.3*stand)+(3*weightt)+(0.92*sleep))
  f<-h3("You have burned",burn,"calories.",style="color:black")
  return(f)
}

#Total calories function

total2<-function(breakfast,lunch,dinner,snack,activity,height,weight,age,gender){
  t<-sum(breakfast,lunch,dinner,snack)
  b<-bmr3(activity,height,weight,age,gender)
  if (t <= (0.8*b)){
    f<-h3("You have consumed",t,"calories. You are eating at least 20% UNDER your recommended calorie consumption.",style="color:red")
  }
  else if (t >= (1.2*b)){
    f<-h3("You have consumed",t,"calories. You are eating at least 20% OVER your recommended calorie consumption.",style="color:red")
  }
  else{
    f<-h3("You have consumed",t,"calories. You are eating within a healthy range of your recommended calorie consumption.",style="color:green")
  }
  return(f)
}

#Function that determines where you are eating the most/least calories

meal<-function(breakfast,lunch,dinner,snack,activity,height,weight,age,gender){
  t<-c(breakfast,lunch,dinner,snack)
  m<-which.max(t)
  m2<-which.min(t)
  b<-bmr3(activity,height,weight,age,gender)
  t2<-sum(breakfast,lunch,dinner,snack)
  if (t2 <= (0.8*b)){
    if (m==m2){
      f<-h3("You are consuming an equal number of calories for all meals.",style="color:green")
    }
    else if(m2==1){
      f<-h3("You are consuming the fewest number of calories for breakfast. Try eating 1 small whole-wheat bagel, 1 ounce reduced-fat cheese, and 1 cup fresh strawberries for a healthier breakfast.",style="color:red")
    }
    else if(m2==2){
      f<-h3("You are consuming the fewest number of calories for lunch. Try eating  a chicken sandwich on whole grain bread (with light mayonnaise, lettuce, and tomato), and 1 cup non-fat yogurt with 1/2 cup of granola for a healthier lunch.",style="color:red")
    }
    else if(m2==3){
      f<-h3("You are consuming the fewest number of calories for dinner. Try eating a veggie burger on a whole-wheat bun with roasted red bell peppers and steamed asparagus, broccoli, and/or cauliflower, with lemon juice and sauteed garlic for a healthier dinner.",style="color:red")
    }
    else {
      f<-h3("You are consuming the fewest number of calories for your snack(s). Try eating a pear with 1 cup of popcorn for a healthier snack.",style="color:red")
    }}
  else if (t2 >= (1.2*b)){
    if (m==m2){
      f<-h3("You are consuming an equal number of calories for all meals.",style="color:green")
    }
    else if(m==1){
      f<-h3("You are consuming the highest number of calories for breakfast. Try eating 1 small whole-wheat bagel, 1 ounce reduced-fat cheese, and 1 cup fresh strawberries for a healthier breakfast.",style="color:red")
    }
    else if(m==2){
      f<-h3("You are consuming the highest number of calories for lunch. Try eating  a chicken sandwich on whole grain bread (with light mayonnaise, lettuce, and tomato), and 1 cup non-fat yogurt with 1/2 cup of granola for a healthier lunch.",style="color:red")
    }
    else if(m==3){
      f<-h3("You are consuming the highest number of calories for dinner. Try eating a veggie burger on a whole-wheat bun with roasted red bell peppers and steamed asparagus, broccoli, and/or cauliflower, with lemon juice and sauteed garlic for a healthier dinner.",style="color:red")
    }
    else {
      f<-h3("You are consuming the highest number of calories for your snack(s). Try eating a pear with 1 cup of popcorn for a healthier snack.",style="color:red")
    }}
  else{
    f<-h3("Nice job with controlling your caloric intake. Make sure to include portions from all five food groups.",style="color:green")
  }
  return(f)
  
}

#Exercise function

exercise<-function(one,two,three,four){
  t<-sum(one,two,three,four)
  if(t<1/3){
    f<-h3("You are NOT meeting your daily minimum amount of recommended exercise.",style="color:red")
  }
  else{
    f<-h3("You are meeting your daily minimum amount of recommended exercise.",style="color:green")
  }
  return(f)
}

#Function that creates pie chart of calories

pie2<-function(breakfast,lunch,dinner,snack){
  slices <- c(breakfast, lunch,dinner, snack)
  lbls <- c("Breakfast", "Lunch", "Dinner", "Snack")
  pie(slices, labels = lbls, main="Pie Chart of Calories from Meals",col=c("red","blue","green","purple"))
}

#Function that creates pie chart of activities

pie3<-function(num4,num5,num6,num7,num8,num9,num10){
  if (sum(num4,num5,num6,num7,num8,num9,num10)>0){
    slices <- c(num4,num5,num6,num7,num8,num9,num10)
    lbls <- c("Walking", "Running", "Swimming", "Sitting","Standing","Weight Training","Sleeping")
    pie(slices, labels = lbls, main="Pie Chart of Activities",col=c("red","blue","green","purple","pink","black","orange","yellow"))
  }
  else if (num4==0&num5==0&num6==0&num7==0&num8==0&num9==0&num10==0){
    print("You have entered 0 hours for all activities.")
  }
}

#Function that creates scatterplot of calories and age

old<-function(activity,height,weight,age,gender){
  age.s<-seq(age,age+30,5)
  calories<-sapply(age.s,bmr3,activity=1,height=67,weight=130,gender=1)
  plot(age.s,calories,main="Calories Required Over Time",col="blue",pch=21,bg="red",cex=2)
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
         "green")
  points(age.s,calories,main="Calories Required Over Time",col="blue",pch=21,bg="red",cex=2)
}

#Stress function

stress<-function(num){
  p<-unlist(num)
  p<-as.vector(p)
  if (length(p)<=3){
    f<-h3("Your stress level is VERY LOW, within a healthy range.",style="color:green")
  }
  else if (length(p)>3 & length(p)<7){
    f<-h3("Your stress level is LOW and within a healthy range, but may be rising.",style="color:green")
  }
  else if (length(p)>6 & length(p)<10){
    f<-h3("Your stress level is MEDIUM. To effectively combat stress, we need to activate the body's natural relaxation response. You can do this by practicing relaxation techniques such as deep breathing, meditation, rhythmic exercise, and yoga.",style="color:red")
  }
  else{
    f<-h3("Your stress level is HIGH. To effectively combat stress, we need to activate the body's natural relaxation response. You can do this by practicing relaxation techniques such as deep breathing, meditation, rhythmic exercise, and yoga.",style="color:red")
  }
  return(f)
}

#Actual server section

server<-shinyServer(function(input, output,session) {
  updateTabsetPanel(session, inputId="page-nav", selected = "Enter Information")
  
  output$text1 <- renderUI({ 
    bmi.f(height=input$num,weight=input$num2)
  })
  
  output$text2 <- renderUI({ 
    veg(input$radio)
  })
  
  output$text3 <- renderUI({ 
    fruit(input$radio2)
  })
  output$text4 <- renderUI({ 
    grains(input$radio3)
  })
  output$text5 <- renderUI({ 
    meat(input$radio4)
  })
  output$text6 <- renderUI({ 
    dairy(input$radio5)
  })
  output$text13 <- renderUI({ 
    water(input$radio6)
  })
  output$text15 <- renderUI({ 
    sleep(input$num10)
  })
  output$text14 <- renderUI({ 
    water2(weight=input$num2,one=input$num4,two=input$num5,three=input$num6,
           four=input$num9)
  })
  output$text7 <- renderUI({ 
    bmr2(height=input$num,weight=input$num2,age=input$num3,gender=input$checkGroup,
         activity=input$checkGroup2)
  })
  output$text8 <- renderUI({ 
    burned(weight=input$num2,walk=input$num4,run=input$num5,swim=input$num6,
           sit=input$num7,stand=input$num8,weightt=input$num9,sleep=input$num10)
  })
  output$text9 <- renderUI({ 
    total2(height=input$num,weight=input$num2,age=input$num3,gender=input$checkGroup,
           activity=input$checkGroup2, breakfast=input$slider1, lunch=input$slider2, 
           dinner=input$slider3,snack=input$slider4)
  })
  output$text10 <- renderUI({ 
    meal(height=input$num,weight=input$num2,age=input$num3,gender=input$checkGroup,
         activity=input$checkGroup2, breakfast=input$slider1, lunch=input$slider2, 
         dinner=input$slider3,snack=input$slider4)
  })
  output$text11 <- renderUI({ 
    exercise(one=input$num4,two=input$num5,three=input$num6, four=input$num9)
  })
  output$text12 <- renderUI({ 
    bmr4(height=input$num,weight=input$num2,age=input$num3,gender=input$checkGroup,
         activity=input$checkGroup2)
  })
  
  #Creates histogram of distriubtion of recommended calories
  
  output$distPlot <-reactivePlot(function(){
    rec=bmr3(height=input$num,weight=input$num2,age=input$num3,gender=input$checkGroup,
             activity=input$checkGroup2) 
    actual=sum(breakfast=input$slider1, 
                 lunch=input$slider2,dinner=input$slider3,snack=input$slider4)
    vector <- rnorm(100000,rec,300)
    breaks <- quantile(vector,seq(0,1,by=0.1))
    labels = 1:(length(breaks)-1)
    seg <- cut(vector,breaks,
               labels=labels,
               include.lowest = TRUE, right = TRUE)
    df = data.frame(vector=vector,seg=seg)
    g<-ggplot(df, aes(x=vector)) +   
      geom_histogram(col="yellow",binwidth=20)+
      geom_vline(xintercept =actual ,col="red",lwd=2)+
      labs(title="Your Position on the Recommended Calorie Distribution")+ 
      labs(x="Calories", y="Count") +	
      theme(plot.background=element_blank())
    return(g)
  },bg = "transparent")
  
  output$distPlot2 <-reactivePlot(function(){
    pie2(breakfast=input$slider1, lunch=input$slider2, 
         dinner=input$slider3,snack=input$slider4)
  },bg = "transparent",height = 600, width = 600)
  
  output$distPlot3 <-reactivePlot(function(){
    pie3(input$num4,input$num5,input$num6,input$num7,input$num8,input$num9,input$num10)
  },bg = "transparent",height = 600, width = 600)
  
  output$distPlot4 <-reactivePlot(function(){
    old(height=input$num,weight=input$num2,age=input$num3,gender=input$checkGroup,
        activity=input$checkGroup2)
  },bg = "transparent")
  
  #Returns a Youtube video based on the category chosen
  
  output$iframe <-renderUI({
    if (input$select2==1){
      a='<iframe width="560" height="315" src="https://www.youtube.com/embed/CJN1n3fId_A" frameborder="0" allowfullscreen></iframe>'
    }
    else if (input$select2==2){
      a<-'<iframe width="560" height="315" src="https://www.youtube.com/embed/TiNtS6wrgsk" frameborder="0" allowfullscreen></iframe>'
    }
    else if (input$select2==3){
      a<-'<iframe width="560" height="315" src="https://www.youtube.com/embed/v4XZAFH9gsw" frameborder="0" allowfullscreen></iframe>'
    }
    else if (input$select2==4){
      a<-'<iframe width="560" height="315" src="https://www.youtube.com/embed/1RRx2Xl9xJQ" frameborder="0" allowfullscreen></iframe>'
    }
    HTML(a)
    
  })
  
  #Returns a Youtube video based on the category chosen
  
  output$iframe2 <-renderUI({
    if (input$select3==1){
      a='<iframe width="560" height="315" src="https://www.youtube.com/embed/wmF2m6cEgEw" frameborder="0" allowfullscreen></iframe>'
    }
    if (input$select3==2){
      a<-'<iframe width="560" height="315" src="https://www.youtube.com/embed/o3dCzUT0oIY" frameborder="0" allowfullscreen></iframe>'
    }
    if (input$select3==3){
      a<-'<iframe width="560" height="315" src="https://www.youtube.com/embed/6PIVgUe6z3E" frameborder="0" allowfullscreen></iframe>'
    }
    if (input$select3==4){
      a<-'<iframe width="560" height="315" src="https://www.youtube.com/embed/smUbGxenfYw" frameborder="0" allowfullscreen></iframe>'
    }
    HTML(a)
    
  })
  
  #Returns a Youtube video based on the category chosen
  
  output$iframe3 <-renderUI({
    if (input$select4==1){
      a='<iframe width="560" height="315" src="https://www.youtube.com/embed/RYnFIRc0k6E?list=PLNMjG7KJEiduWL1ECQRMWoOQ6x6q874OS" frameborder="0" allowfullscreen></iframe>'
    }
    if (input$select4==2){
      a<-'<iframe width="560" height="315" src="https://www.youtube.com/embed/LDZX4ooRsWs?list=PLEw_zAd4YTpAtczbk3rxbbETmSeJEm2dT" frameborder="0" allowfullscreen></iframe>'
    }
    if (input$select4==3){
      a<-'<iframe width="560" height="315" src="https://www.youtube.com/embed/0KSOMA3QBU0?list=PLQV6-CrRDV28HhlYv_sudVjf7KaOxoZoz" frameborder="0" allowfullscreen></iframe>'
    }
    if (input$select4==4){
      a<-'<iframe width="560" height="315" src="https://www.youtube.com/embed/ZfNR98ajB1U?list=PLcFX0ajmWtbA0ywgxXyCDhITpf7rhNjQt" frameborder="0" allowfullscreen></iframe>'
    }
    if (input$select4==5){
      a<-'<iframe width="560" height="315" src="https://www.youtube.com/embed/gyiKO8obZ9s?list=PLcFG89LolLWxXuBjiPJvyTFYY_Nj9Cnge" frameborder="0" allowfullscreen></iframe>'
    }
    HTML(a)
    
  })
  
  output$dynamic <- renderUI({
    stress(input$select5)
  })
  
  #Creates link to 'Results' tab
  
  observeEvent(input$link_to_tabpanel_b, {
    newvalue <- "Results"
    updateTabsetPanel(session, "tabs", newvalue)
  })
  
})

shinyApp(ui, server)



