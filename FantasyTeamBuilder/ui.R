library(shiny)
library(scatterD3)

shinyUI(fluidPage(
  titlePanel("Optimize Your DraftKings Lineup!"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sport", label="Choose your Sport", 
                  choices = list("NHL"="NHL", "NBA"="NBA","PGA"="PGA","Soccer"="SOC","MMA"="MMA","NASCAR"="NASCAR"),
                  selected = 1),
      
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
                )
      ),
      
      p("We recommend that you either use the csv file from the DraftKings website,
        or use that csv file as your starting template. Do not change any of the header names."
      ),
      
      htmlOutput("exclusion1"),
      htmlOutput("exclusion2"),
      htmlOutput("exclusion3"),
      htmlOutput("exclusion4"),
      htmlOutput("exclusion5")
    ),
    
    mainPanel(
       tabsetPanel(
         tabPanel("Instructions",
                  h3("How to Retrieve Data:"),
                  p("Go to the DraftKings Lobby (http://bit.ly/1MitceV)."),
                  p("Pick a sport on the top banner and click on a contest that will occur soon."),
                  p("Hit the Draft Team button that appears at the bottom right of the pop-up."),
                  p("Note: DraftKings will only let you draft a lineup for a contest that is starting
                    in the next couple of days, so if there is no contest occuring soon in a sport you
                    are interested in, DraftKings will not allow you to draft a team. This means you
                    will not be able to use this application for that sport, as you need to complete this step.
                    Sometimes a sport will have multiple dates - one that is within a couple days 
                    and one that is far out. In this case, choose the date that is close to make sure that you will 
                    be able to enter the contest."),
                  p("At the bottom of the player selection table, click Export to CSV."),
                  p("This csv file, or an edited version of it, should be the input to the application."),
                  p("Note: Do not edit any of the column names or remove any of the columns. If you do so,
                    then the app will not run."),
                  hr(),
                  h3("Data Format:"),
                  p("Make sure your data is formatted in the default format as given by a csv downloaded from the DraftKings website."),
                  p("Keep headers in the file, and make sure your 6 columns are in the following order (regardless of sport): 
                    \"Position\", \"Name\", \"Salary\", \"GameInfo\", \"AvgPointsPerGame\", \"teamAbbrev\". You are welcome to 
                    adjust the \"AvgPointsPerGame\" column with your own predicted points for the given players."),
                  p("Keep in mind that if you do not edit the file you downloaded off of the DraftKings
                    website, then your data is already in the right format! This only applies to users
                    who wish to edit the data within the downloaded csv file before uploading it to 
                    our application. Reasons you may wish to edit include wanting to adjust the expected number of points
                    you expect a player to get and deleting a bunch of players before starting the app. However, keep in 
                    mind that we will already allow you to exclude up to 5 players per lineup within the app. "),
                  p("Hence, when you begin to use the app, we recommend that you start by using the default csv that you
                    downloaded off of the appropriate DraftKing contest's webpage. Once you have a better feel for the app
                    and the data, an edit that can be made is changing the numbers inside the average points per game column within
                    the csv. Additionally, if you wish to remove more than 5 players from the dataset, then this is the only
                    other edit that you can make. To reiterate, only advanced users should consider making edits to the data 
                    before uploading it to our application."),
                  hr(),
                  h3("How to Use Our App:"),
                  p("On the left, choose your sport in the dropdown menu."),
                  p("Upload your properly formatted csv below the menu."),
                  p("In the \"Lineup\" tab, you will find the lineup that our algorithm suggests. The lineup's total salary, 
                    total expected number of points, and visual breakdowns of salaries and player values are included as well."),
                  p("In the \"Explore\" tab, you will find an interactive scatter plot (which plots Projected Points vs. Salary) 
                    for all players in the uploaded dataset, broken down by position. You can zoom into the plot using your mouse scroll, 
                    mouseover players for player-specific information, drag around name labels to rearrange them for clarity, and mouseover 
                    the colors in the legend to highlight players of a specific position. Additionally, toggle the sliders below the plot 
                    to adjust Label Size, Point Opacity (useful for dense clusters), Point Size, and Transitions.")),
         
         tabPanel("Lineup",
                   p("Below, you will find the lineup we suggest:"),
                   tableOutput('matrix'),
                   p("The lineup's total salary:"),
                   verbatimTextOutput('salary'),
                   plotOutput('salaryviz'),
                   p("The lineup's total expected number of points:"),
                   verbatimTextOutput('points'),
                   p("And breakdown of the value for each player:"),
                   plotOutput('valueviz')
                   ),
         
         tabPanel("Explore",
                  p("Explore the available players below:"),
                  p("Hints: Scroll over the legend to look at a specific position, scroll on the plot to zoom, 
                    mouseover a point to see player-specific information."),
                  scatterD3Output('scatter', width = "110%"),
                  sliderInput("labelsize", "Label Size", min = 0, max = 15, value = 9),
                  sliderInput("opacity", "Point Opacity", min = 0, max = 1, value = 0.5),
                  sliderInput("pointsize", "Point Size", min = 5, max = 100, value = 35),
                  checkboxInput("transition", "Transitions", value = TRUE)),
         
         tabPanel("About",
                  h3("How the Algorithm Works:"),
                  p("The algorithm we developed optimizes a DraftKings daily fantasy sports lineup for a 
                    variety of different sports that 
                    can be selected by the user. The algorithm works in the following way: 
                    First, we define a value for each player. We do this by finding the total number of points a 
                    player can provide to your team per dollar spent on the player. Second, we initalize a matrix 
                    to hold the lineup. Third, we fill the lineup by selecting the players with the highest 
                    expected point totals for each position. Fourth, we calculate the budget of the lineup that 
                    we generated in the third step. While the budget of the created lineup exceeds the 
                    DraftKings salary cap of 50,000 virtual dollars, we remove the player in the lineup 
                    with the lowest value and replace him with the player from his position with the next highest
                    point total. The budget is recalculated, and this process of iterative
                    removal of the lowest value player with the player with the next
                    highest expected point total is repeated until the lineup has a budget
                    below 50,000. This is the lineup we output as optimal."),
                  hr(),
                  h3("Excluding Players:"),
                  p("For every sport, we allow for the user to exclude up to 5 specific
                    players for the generated lineup. Reasons for excluding a player can
                    include fandom, injury, or personal judgment about how the player will perform.
                    We include a dropdown menu of all possible players (which also gives the option to type in an exclusion); however, changes only will
                    when you exclude a player who is currently in the lineup. Once a player is excluded
                    the player is removed from the loaded data and then the lineup generation procedure
                    is run again. However, when a player is excluded the majority of the lineup does not change.
                    This is because our lineup generator algorithm 'prefers' certain types of players that
                    have high expected point outputs for low salary inputs - high value players. Excluding one player does not
                    change the preference for the sort of player our model prefers. It can be confirmed
                    that our algorithm is re-run without the excluded player, because in rare instances
                    multiple lineup changes will occur as a result of 1 exclusion. If an optimized lineup
                    has one less player than it should be expected to have, we encourage you to try excluding
                    a player. You will likely see that when you do so, sometimes the missing player is replaced
                    with an actual player, and the excluded player is replaced by another unique player.
                    If our algorithm did not exclude players selected before running the lineup generating
                    procedure, then there could not be 2 lineup changes as a result of 1 exclusion."),
                  hr(),
                  h3("Algorithm Convergence:"),
                  p("In most cases, our algorithm works well. However, in some specific
                    edge cases, the algorithm we have developed does not converge.
                    Typically, our algorithm does not work if a sport has a small number
                    of potential player options at a particular position. For example, take
                    goalies in soccer; there are very few goalies relative to other positions.
                    If goalies are low value players relative to other players, then they will be
                    iteratively removed from the lineup. If it reaches the point that every goalie
                    has been inserted and removed from the lineup, then the algorithm would crash.
                    We could solve this problem via inserting a goalie into the lineup and not
                    letting the goalie be removed, but this would not necessarily lead to an optimal
                    lineup. The question would become which goalie is optimal given the setup of the
                    other selected players; this is not a question that can be answered with our lineup
                    selection algorithm. If we had taken another approach - say a brute force method -
                    then the algorithm would always converge, but at the cost of runtime. Hence, we are
                    happy to have a fast algorithm that generally converges in comparison to a much slower
                    algorithm that always converges. While we do allow you to exclude up to 5 players from
                    the lineup, keep in mind if you selectively exclude all five players from a position
                    that does not contain many players to begin with, the algorithm may crash."),
                  hr(),
                  p("A second case where our algorithm will not converge occurs if the algorithm loops
                    through all possible full lineups in accordance with the rules we set out, and is
                    unable to find a lineup that meets the 50,000 dollar budget constraint. In this case
                    the algorithm will merely remove the lowest value player in the lineup; this will
                    likely solve the budget constraint issue, and the lineup will be outputted with a
                    missing player. In these cases, we encourage the user to use the data exploration tab
                    to see if there are any players he can still afford; the user can then include any player
                    that he can still afford. Keep in mind that in rare instances, a n-1 player lineup (filled with more
                    expensive players) can be better than having a lineup with n more mediocre players.
                    Hence, in the event the lineup generated has an empty space, we encourage the user to see
                    if there are any players he can still afford; if there are, the algorithm did not converge
                    properly, but if not, then the algorithm did converge properly."),
                  hr(),
                  h3("Developed by Nikhil Patel and Victor Li, May 2016, Brown University PHP2560: Statistical Computing"),
                  hr(),
                  h3("Future Work:"),
                  p("After developing the algorithm for this application, we have several areas of improvement
                    that we would like to implement in the future. These can also be used as suggestions for 
                    how to build a better lineup optimization algorithm for future statisticians that are interested
                    in this sort of work. First, the algorithm we have built is linear in nature. Player value is 
                    determined by the points the player is expected to provide per dollar spent on the player. We 
                    believe treating player value as linear was a good place to start, but would be interested in 
                    tweaking the lineup to take account for the fact that players with larger salaries are typically
                    lower in value holding all else equal - player with high prices are rarer commodities in the game
                    and the value proposition for a highly salaried player is different from that of a low value player.
                    Our current setup makes it easy to find bargain buys, but likely penalizes strong players for the 
                    strong salary they command - a high salary player probably has a reduced variance in expected 
                    result than a low salary player, and adjusted the way we calculate player value could lead to a 
                    potentially better algorithm. Second, we would suggest a top-down rebuild of the algorithm we have
                    designed in order to allow for easier implementation of an include players feature. This was something
                    that we wanted to include, but we did not think about this possibility while coding up our initial 
                    algorithm. After various attempts to implement this, we determined that this would be extremely hard
                    without a thorough redesign of the algorithm and as a result would suggest that future researchers
                    begin by trying to implement a cleaner version of our lineup optimization functions to allow for 
                    easier implementation of complex features, such as player inclusion."),
                  h3("Ackowledgements:"),
                  p("Thanks to Professor Adam Sullivan for providing us an 
                    environment to work on a project that we are very passionate about,
                    as well as for all the help he has provided to us along the way!"),
                  p("scatterD3 Credit: Julien Barnier (http://bit.ly/1WAjDgV)"))
       )
    )
  )
))