shinyServer(function(input, output) {
  library(scatterD3)
  
  findLineupNHL <- function(df=DKSalaries,exclude1="",exclude2="",exclude3="",exclude4="",exclude5=""){

    # Adds value variable to our dataframe. Multiply by 10,000 just to make the numbers larger (doesn't actually matter)
    df$Value <- df$AvgPointsPerGame/df$Salary *10000

    # Exclude user-selected players
    # This code was written prior to the change of exclude player to a dropdown menu, which explains its structure
    if (exclude1 != ""){
      if (length(which(df$Name==exclude1))==1){
        remove <- which(df$Name==exclude1)
        df <- df[-c(remove),]
      }
    }
    if (exclude2 != ""){
      if (length(which(df$Name==exclude2))==1){
        remove <- which(df$Name==exclude2)
        df <- df[-c(remove),]
      }
    }
    if (exclude3 != ""){
      if (length(which(df$Name==exclude3))==1){
        remove <- which(df$Name==exclude3)
        df <- df[-c(remove),]
      }
    }
    if (exclude4 != ""){
      if (length(which(df$Name==exclude4))==1){
        remove <- which(df$Name==exclude4)
        df <- df[-c(remove),]
      }
    }
    if (exclude5 != ""){
      if (length(which(df$Name==exclude5))==1){
        remove <- which(df$Name==exclude5)
        df <- df[-c(remove),]
      }
    }

    # Initalizes lineup matrix to be 9 rows and 4 columns. Sets first column to the positions we are looking to fill. 
    lineup <- matrix(nrow=9,ncol=5)
    lineup[,1] <- c("C","C","W","W","W","D","D","G","UTIL")
    
    # Creates dataframe of only the centers, and orders based on average points per game. 
    centers <- subset(df, df$Position=="C")
    centers <- centers[order(centers$AvgPointsPerGame,decreasing=T),]
    rownames(centers)=NULL
    
    # Creates dataframe of only the wings, and orders based on average points per game. 
    wings <- subset(df, df$Position=="LW"|df$Position=="RW")
    wings <- wings[order(wings$AvgPointsPerGame,decreasing=T),]
    rownames(wings)=NULL
    
    # Creates dataframe of only the defensemen, and orders based on average points per game.
    defense <- subset(df, df$Position=="D")
    defense <- defense[order(defense$AvgPointsPerGame,decreasing=T),]
    rownames(defense)=NULL
    
    # Creates dataframe of all non-goalies (for the Utility wildcard slot in the lineup), and orders based on average points per game. 
    util <- subset(df, df$Position != "G")
    util <- util[order(util$AvgPointsPerGame,decreasing=T),]
    rownames(util)=NULL
    
    # Creates dataframe of all goalies, and orders based on average points per game. 
    goalies <- subset(df, df$Position == "G")
    goalies <- goalies[order(goalies$AvgPointsPerGame,decreasing=T),]
    rownames(goalies)=NULL
    
    # Loop creates our initial lineup 
    for (i in 1:9){
      # Chooses the top two centers in average points per game, assigns name to column 2, salary to column 3 and value to column 4. 
      if (i==1 | i==2){
        lineup[i,2] = as.character(centers$Name[1])
        lineup[i,3] = as.numeric(centers$Salary[1])
        lineup[i,4] = as.numeric(centers$Value[1])
        lineup[i,5] = as.character(centers$teamAbbrev[1])
        #Eliminates player from dataframe once they are assigned to our lineup.
        centers <- centers[-c(1),]
        rownames(centers)=NULL
      }
      # Chooses the top three wings in average points per game, assigns name to column 2, salary to column 3 and value to column 4. 
      if (i==3 | i==4 | i==5){
        lineup[i,2] = as.character(wings$Name[1])
        lineup[i,3] = as.numeric(wings$Salary[1])
        lineup[i,4] = as.numeric(wings$Value[1])
        lineup[i,5] = as.character(wings$teamAbbrev[1])
        #Eliminates player from dataframe once they are assigned to our lineup.
        wings <- wings[-c(1),]
        rownames(wings)=NULL
      }
      # Chooses the top two defensemen in average points per game, assigns name to column 2, salary to column 3 and value to column 4. 
      if (i==6 | i==7){
        lineup[i,2] = as.character(defense$Name[1])
        lineup[i,3] = as.numeric(defense$Salary[1])
        lineup[i,4] = as.numeric(defense$Value[1])
        lineup[i,5] = as.character(defense$teamAbbrev[1])
        #Eliminates player from dataframe once they are assigned to our lineup.
        defense <- defense[-c(1),]
        rownames(defense)=NULL
      }
      # Chooses the top utility player in average points per game, assigns name to column 2, salary to column 3 and value to column 4. 
      if (i==8){
        lineup[i,2] = as.character(goalies$Name[1])
        lineup[i,3] = as.numeric(goalies$Salary[1])
        lineup[i,4] = as.numeric(goalies$Value[1])
        lineup[i,5] = as.character(goalies$teamAbbrev[1])
        #Eliminates player from dataframe once they are assigned to our lineup.
        goalies <- goalies[-c(1),]
        rownames(goalies)=NULL
      }
      # Chooses the top goalie in average points per game, assigns name to column 2, salary to column 3 and value to column 4. 
      if (i==9){
        lineup[i,2] = as.character(util$Name[1])
        lineup[i,3] = as.numeric(util$Salary[1])
        lineup[i,4] = as.numeric(util$Value[1])
        lineup[i,5] = as.character(util$teamAbbrev[1])
        #Eliminates player from dataframe once they are assigned to our lineup.
        util <- util[-c(1),]
        rownames(util)=NULL
      }
    }
    # Makes sure the utility player is different from the other skaters 
    while (length(unique(lineup[,2])) != 9){
      lineup[9,2] <- as.character(util$Name[1])
      lineup[9,3] <- as.numeric(util$Salary[1])
      lineup[9,4] <- as.numeric(util$Value[1])
      lineup[9,5] <- as.character(util$teamAbbrev[1])
      util <- util[-c(1),]
      rownames(util)=NULL
    }
    
    # We need to find budget. 
    budget = sum(as.integer(lineup[,3]),na.rm=TRUE)

    # We remove the player of lowest value, and add the next best player. 
    while (budget > 50000){
      # Removing the player of lowest value. 
      lowestValue = min(as.numeric(lineup[,4]))
      removeThis = which(as.numeric(lineup[,4])==lowestValue)
      lineup[removeThis,2] = NA
      lineup[removeThis,3] = NA
      lineup[removeThis,4] = NA
      lineup[removeThis,5] = NA
      
      # Replacing player, if we removed a center.
      if (lineup[removeThis,1]=="C"){
        if (nrow(centers)==0) return(NULL)
        lineup[removeThis,2] = as.character(centers$Name[1])
        lineup[removeThis,3] = as.numeric(centers$Salary[1])
        lineup[removeThis,4] = as.numeric(centers$Value[1])
        lineup[removeThis,5] = as.character(centers$teamAbbrev[1])
        centers <- centers[-c(1),]
        rownames(centers)=NULL
        while (length(unique(lineup[,2])) != 9){
          lineup[removeThis,2] <- as.character(centers$Name[1])
          lineup[removeThis,3] <- as.numeric(centers$Salary[1])
          lineup[removeThis,4] <- as.numeric(centers$Value[1])
          lineup[removeThis,5] <- as.character(centers$teamAbbrev[1])
          centers <- centers[-c(1),]
          rownames(centers)=NULL
        }
      }
      
      # Replacing player, if we removed a wing.
      if (lineup[removeThis,1]=="W"){
        if (nrow(wings)==0) return(NULL)
        lineup[removeThis,2] = as.character(wings$Name[1])
        lineup[removeThis,3] = as.numeric(wings$Salary[1])
        lineup[removeThis,4] = as.numeric(wings$Value[1])
        lineup[removeThis,5] = as.character(wings$teamAbbrev[1])
        wings <- wings[-c(1),]
        rownames(wings)=NULL
        while (length(unique(lineup[,2])) != 9){
          lineup[removeThis,2] <- as.character(wings$Name[1])
          lineup[removeThis,3] <- as.numeric(wings$Salary[1])
          lineup[removeThis,4] <- as.numeric(wings$Value[1])
          lineup[removeThis,5] <- as.character(wings$teamAbbrev[1])
          wings <- wings[-c(1),]
          rownames(wings)=NULL
        }
      }
      
      # Replacing player, if we removed a defenseman.
      if (lineup[removeThis,1]=="D"){
        if (nrow(defense)==0) return(NULL)
        lineup[removeThis,2] = as.character(defense$Name[1])
        lineup[removeThis,3] = as.numeric(defense$Salary[1])
        lineup[removeThis,4] = as.numeric(defense$Value[1])
        lineup[removeThis,5] = as.character(defense$teamAbbrev[1])
        defense <- defense[-c(1),]
        rownames(defense)=NULL
        while (length(unique(lineup[,2])) != 9){
          lineup[removeThis,2] <- as.character(defense$Name[1])
          lineup[removeThis,3] <- as.numeric(defense$Salary[1])
          lineup[removeThis,4] <- as.numeric(defense$Value[1])
          lineup[removeThis,5] <- as.character(defense$teamAbbrev[1])
          defense <- defense[-c(1),]
          rownames(defense)=NULL
        }
      }
      
      # Replacing player, if we removed a goalie.
      if (lineup[removeThis,1] == "G"){
        if (nrow(goalies)==0) return(NULL)
        lineup[removeThis,2] = as.character(goalies$Name[1])
        lineup[removeThis,3] = as.numeric(goalies$Salary[1])
        lineup[removeThis,4] = as.numeric(goalies$Value[1])
        lineup[removeThis,5] = as.character(goalies$teamAbbrev[1])
        goalies <- goalies[-c(1),]
        rownames(goalies)=NULL
        while (length(unique(lineup[,2])) != 9){
          lineup[removeThis,2] <- as.character(goalies$Name[1])
          lineup[removeThis,3] <- as.numeric(goalies$Salary[1])
          lineup[removeThis,4] <- as.numeric(goalies$Value[1])
          lineup[removeThis,5] <- as.character(goalies$teamAbbrev[1])
          goalies <- goalies[-c(1),]
          rownames(goalies)=NULL
        }
      }
      
      # Replacing player, if we removed a utility.
      if (lineup[removeThis,1] == "UTIL"){
        if (nrow(util)==0) return(NULL)
        lineup[removeThis,2] = as.character(util$Name[1])
        lineup[removeThis,3] = as.numeric(util$Salary[1])
        lineup[removeThis,4] = as.numeric(util$Value[1])
        lineup[removeThis,5] = as.character(util$teamAbbrev[1])
        util <- util[-c(1),]
        rownames(util)=NULL
        while (length(unique(lineup[,2])) != 9){
          lineup[removeThis,2] <- as.character(util$Name[1])
          lineup[removeThis,3] <- as.numeric(util$Salary[1])
          lineup[removeThis,4] <- as.numeric(util$Value[1])
          lineup[removeThis,5] <- as.character(util$teamAbbrev[1])
          util <- util[-c(1),]
          rownames(util)=NULL
        }
      }
      budget = sum(as.integer(lineup[,3]),na.rm=TRUE)
    }
    
    #  Output Team Salary and Points
    colnames(lineup) = c("Position","Player","Salary","Value", "Team")
    salary <- sum(as.numeric(lineup[,3]),na.rm = TRUE)
    points <- sum(as.numeric(lineup[,4])/10000 * as.numeric(lineup[,3]),na.rm = TRUE)
    return(list(lineup,salary,points))
  }
  
  findLineupPGA <- function(df=DKSalaries,exclude1="",exclude2="",exclude3="",exclude4="",exclude5=""){

    # Adds value variable to our dataframe. Multiply by 1000 just to make the numbers larger (doesn't actually matter)
    df$Value <- df$AvgPointsPerGame/df$Salary *1000

    # Exclude user-selected players
    # This code was written prior to the change of exclude player to a dropdown menu, which explains its structure
    if (exclude1 != ""){
      if (length(which(df$Name==exclude1))==1){
        remove <- which(df$Name==exclude1)
        df <- df[-c(remove),]
      }
    }
    if (exclude2 != ""){
      if (length(which(df$Name==exclude2))==1){
        remove <- which(df$Name==exclude2)
        df <- df[-c(remove),]
      }
    }
    if (exclude3 != ""){
      if (length(which(df$Name==exclude3))==1){
        remove <- which(df$Name==exclude3)
        df <- df[-c(remove),]
      }
    }
    if (exclude4 != ""){
      if (length(which(df$Name==exclude4))==1){
        remove <- which(df$Name==exclude4)
        df <- df[-c(remove),]
      }
    }
    if (exclude5 != ""){
      if (length(which(df$Name==exclude5))==1){
        remove <- which(df$Name==exclude5)
        df <- df[-c(remove),]
      }
    }
    
    # Initalizes lineup matrix to be 6 rows and 3 columns.  
    lineup <- matrix(nrow=6,ncol=3)
    
    # Creates dataframe of all golfers, and ordered by average points per game. 
    golfers <- subset(df, df$Position=="G")
    golfers <- golfers[order(golfers$AvgPointsPerGame,decreasing=T),]
    rownames(golfers)=NULL
    
    # Loop creates our initial lineup 
    for (i in 1:6){
      # Chooses the top six golfers in average points per game, assigns name to column 1, salary to column 2 and value to column 3. 
      lineup[i,1] = as.character(golfers$Name[1])
      lineup[i,2] = as.numeric(golfers$Salary[1])
      lineup[i,3] = as.numeric(golfers$Value[1])

      #Eliminates player from dataframe once they are assigned to our lineup.
      golfers <- golfers[-c(1),]
      rownames(golfers)=NULL
    }
    
    # We need to find budget. 
    budget = sum(as.integer(lineup[,2]),na.rm=TRUE)

    # We remove the player of lowest value, and add the next best player. 
    while (budget > 50000){
      # Removing the player of lowest value. 
      lowestValue = min(as.numeric(lineup[,3]))
      removeThis = which(as.numeric(lineup[,3])==lowestValue)
      lineup[removeThis,1] = NA
      lineup[removeThis,2] = NA
      lineup[removeThis,3] = NA
      
      # Replacing player.
      if (nrow(golfers)==0) return(NULL)
      lineup[removeThis,1] = as.character(golfers$Name[1])
      lineup[removeThis,2] = as.numeric(golfers$Salary[1])
      lineup[removeThis,3] = as.numeric(golfers$Value[1])
      golfers <- golfers[-c(1),]
      rownames(golfers)=NULL
      
      # Recalculating budget.
      budget = sum(as.integer(lineup[,2]),na.rm=TRUE)
    } 
        
    # Output Team Salary and Points
    colnames(lineup) = c("Player","Salary","Value")
    salary <- sum(as.numeric(lineup[,2]),na.rm=TRUE)
    points <- sum(as.numeric(lineup[,3])/1000 * as.numeric(lineup[,2]),na.rm=TRUE)
    return(list(lineup,salary,points))
  }
  
  findLineupSOC <- function(df=DKSalaries,exclude1="",exclude2="",exclude3="",exclude4="",exclude5=""){

    # Adds value variable to our dataframe. Multiply by 10,000 just to make the numbers larger (doesn't actually matter)
    df$Value <- df$AvgPointsPerGame/df$Salary *10000

    # Exclude user-selected players   
    # This code was written prior to the change of exclude player to a dropdown menu, which explains its structure
    if (exclude1 != ""){
      if (length(which(df$Name==exclude1))==1){
        remove <- which(df$Name==exclude1)
        df <- df[-c(remove),]
      }
    }
    if (exclude2 != ""){
      if (length(which(df$Name==exclude2))==1){
        remove <- which(df$Name==exclude2)
        df <- df[-c(remove),]
      }
    }
    if (exclude3 != ""){
      if (length(which(df$Name==exclude3))==1){
        remove <- which(df$Name==exclude3)
        df <- df[-c(remove),]
      }
    }
    if (exclude4 != ""){
      if (length(which(df$Name==exclude4))==1){
        remove <- which(df$Name==exclude4)
        df <- df[-c(remove),]
      }
    }
    if (exclude5 != ""){
      if (length(which(df$Name==exclude5))==1){
        remove <- which(df$Name==exclude5)
        df <- df[-c(remove),]
      }
    }

    # Initalizes lineup matrix to be 11 rows and 5 columns. Sets first column to the positions we are looking to fill. 
    lineup <- matrix(nrow=11,ncol=5)
    lineup[,1] <- c("GK","D","D","D","M","M","M","F","F", "UTIL", "UTIL")
    
    # Creates dataframe of only the goalies, and orders based on average points per game. 
    goalies <- subset(df, df$Position=="GK")
    goalies <- goalies[order(goalies$AvgPointsPerGame,decreasing=T),]
    rownames(goalies)=NULL
    
    # Creates dataframe of only the defenders, and orders based on average points per game.
    defense <- subset(df, df$Position=="D")
    defense <- defense[order(defense$AvgPointsPerGame,decreasing=T),]
    rownames(defense)=NULL
    
    # Creates dataframe of only the midfielders, and orders based on average points per game. 
    midfield <- subset(df, df$Position=="M")
    midfield <- midfield[order(midfield$AvgPointsPerGame,decreasing=T),]
    rownames(midfield)=NULL
    
    # Creates dataframe of only the forwards, and orders based on average points per game.
    forward <- subset(df, df$Position=="F")
    forward <- forward[order(forward$AvgPointsPerGame,decreasing=T),]
    rownames(forward)=NULL
    
    # Creates dataframe of all non-goalies, and orders based on average points per game. 
    util <- subset(df, df$Position != "GK")
    util <- util[order(util$AvgPointsPerGame,decreasing=T),]
    rownames(util)=NULL
        
    # Loop creates our initial lineup 
    for (i in 1:11){
      # Chooses the top goalie in average points per game, assigns name to column 2, salary to column 3 and value to column 4. 
      if (i==1){
        lineup[i,2] = as.character(goalies$Name[1])
        lineup[i,3] = as.numeric(goalies$Salary[1])
        lineup[i,4] = as.numeric(goalies$Value[1])
        lineup[i,5] = as.character(goalies$teamAbbrev[1])
        #Eliminates player from dataframe once they are assigned to our lineup.
        goalies <- goalies[-c(1),]
        rownames(goalies)=NULL
      }
      # Chooses the top three defenders in average points per game, assigns name to column 2, salary to column 3 and value to column 4. 
      if (i==2 | i==3 | i==4){
        lineup[i,2] = as.character(defense$Name[1])
        lineup[i,3] = as.numeric(defense$Salary[1])
        lineup[i,4] = as.numeric(defense$Value[1])
        lineup[i,5] = as.character(defense$teamAbbrev[1])
        #Eliminates player from dataframe once they are assigned to our lineup.
        removeThisDefenseman <- which(util$Name==defense$Name[1])
        defense <- defense[-c(1),]
        rownames(defense)=NULL
        util <- util[-c(removeThisDefenseman),]
        rownames(util)=NULL
      }
      # Chooses the top three midfielders in average points per game, assigns name to column 2, salary to column 3 and value to column 4. 
      if (i==5 | i==6 | i==7){
        lineup[i,2] = as.character(midfield$Name[1])
        lineup[i,3] = as.numeric(midfield$Salary[1])
        lineup[i,4] = as.numeric(midfield$Value[1])
        lineup[i,5] = as.character(midfield$teamAbbrev[1])
        #Eliminates player from dataframe once they are assigned to our lineup.
        removeThisMidfield <- which(util$Name==midfield$Name[1])
        midfield <- midfield[-c(1),]
        rownames(midfield)=NULL
        util <- util[-c(removeThisMidfield),]
        rownames(util)=NULL
      }
      # Chooses the top two forwards in average points per game, assigns name to column 2, salary to column 3 and value to column 4. 
      if (i==8 | i==9){
        lineup[i,2] = as.character(forward$Name[1])
        lineup[i,3] = as.numeric(forward$Salary[1])
        lineup[i,4] = as.numeric(forward$Value[1])
        lineup[i,5] = as.character(forward$teamAbbrev[1])
        #Eliminates player from dataframe once they are assigned to our lineup.
        removeThisForward <- which(util$Name == forward$Name[1])
        forward <- forward[-c(1),]
        rownames(forward)=NULL
        util <- util[-c(removeThisForward),]
        rownames(util)=NULL
      }
      # Chooses the top two utility players in average points per game, assigns name to column 2, salary to column 3 and value to column 4. 
      if (i==10 | i==11){
        lineup[i,2] = as.character(util$Name[1])
        lineup[i,3] = as.numeric(util$Salary[1])
        lineup[i,4] = as.numeric(util$Value[1])
        lineup[i,5] = as.character(util$teamAbbrev[1])
        #Eliminates player from dataframe once they are assigned to our lineup.
        util <- util[-c(1),]
        rownames(util)=NULL
      }
    }
    
    # We need to find budget. 
    budget = sum(as.integer(lineup[,3]),na.rm=TRUE)

    # We remove the player of lowest value, and add the next best player. 
    while (budget > 50000){
      # Removing the player of lowest value. 
      lowestValue = min(as.numeric(lineup[,4]))
      removeThis = which(as.numeric(lineup[,4])==lowestValue)
      lineup[removeThis,2] = NA
      lineup[removeThis,3] = NA
      lineup[removeThis,4] = NA
      lineup[removeThis,5] = NA
      
      # Replacing player, if we removed a goalie.
      if (lineup[removeThis,1]=="GK"){
        if (nrow(goalies)==0) return(NULL)
        lineup[removeThis,2] = as.character(goalies$Name[1])
        lineup[removeThis,3] = as.numeric(goalies$Salary[1])
        lineup[removeThis,4] = as.numeric(goalies$Value[1])
        lineup[removeThis,5] = as.character(goalies$teamAbbrev[1])
        goalies <- goalies[-c(1),]
        rownames(goalies)=NULL
        while (length(unique(lineup[,2])) != 11){
          lineup[removeThis,2] <- as.character(goalies$Name[1])
          lineup[removeThis,3] <- as.numeric(goalies$Salary[1])
          lineup[removeThis,4] <- as.numeric(goalies$Value[1])
          lineup[removeThis,5] <- as.character(goalies$teamAbbrev[1])
          goalies <- goalies[-c(1),]
          rownames(goalies)=NULL
        }
      }
      
      # Replacing player, if we removed a defender.
      if (lineup[removeThis,1]=="D"){
        if (nrow(defense)==0) return(NULL)
        lineup[removeThis,2] = as.character(defense$Name[1])
        lineup[removeThis,3] = as.numeric(defense$Salary[1])
        lineup[removeThis,4] = as.numeric(defense$Value[1])
        lineup[removeThis,5] = as.character(defense$teamAbbrev[1])
        removeThisDefenseman <- which(util$Name==defense$Name[1])
        defense <- defense[-c(1),]
        rownames(defense)=NULL
        util <- util[-c(removeThisDefenseman),]
        rownames(util)=NULL
        while (length(unique(lineup[,2])) != 11){
          lineup[removeThis,2] <- as.character(defense$Name[1])
          lineup[removeThis,3] <- as.numeric(defense$Salary[1])
          lineup[removeThis,4] <- as.numeric(defense$Value[1])
          lineup[removeThis,5] <- as.character(defense$teamAbbrev[1])
          defense <- defense[-c(1),]
          rownames(defense)=NULL
        }
      }
      
      # Replacing player, if we removed a midfielder.
      if (lineup[removeThis,1]=="M"){
        if (nrow(midfield)==0) return(NULL)
        lineup[removeThis,2] = as.character(midfield$Name[1])
        lineup[removeThis,3] = as.numeric(midfield$Salary[1])
        lineup[removeThis,4] = as.numeric(midfield$Value[1])
        lineup[removeThis,5] = as.character(midfield$teamAbbrev[1])
        removeThisMidfield <- which(util$Name==midfield$Name[1])
        midfield <- midfield[-c(1),]
        rownames(midfield)=NULL
        util <- util[-c(removeThisMidfield),]
        rownames(util)=NULL
        while (length(unique(lineup[,2])) != 11){
          lineup[removeThis,2] <- as.character(midfield$Name[1])
          lineup[removeThis,3] <- as.numeric(midfield$Salary[1])
          lineup[removeThis,4] <- as.numeric(midfield$Value[1])
          lineup[removeThis,5] <- as.character(midfield$teamAbbrev[1])
          midfield <- midfield[-c(1),]
          rownames(midfield)=NULL
        }
      }
      
      # Replacing player, if we removed a forward.
      if (lineup[removeThis,1] == "F"){
        if (nrow(forward)==0) return(NULL)
        lineup[removeThis,2] = as.character(forward$Name[1])
        lineup[removeThis,3] = as.numeric(forward$Salary[1])
        lineup[removeThis,4] = as.numeric(forward$Value[1])
        lineup[removeThis,5] = as.character(forward$teamAbbrev[1])
        removeThisForward <- which(util$Name==forward$Name[1])
        forward <- forward[-c(1),]
        rownames(forward)=NULL
        util <- util[-c(removeThisForward),]
        rownames(util)=NULL
        while (length(unique(lineup[,2])) != 11){
          lineup[removeThis,2] <- as.character(forward$Name[1])
          lineup[removeThis,3] <- as.numeric(forward$Salary[1])
          lineup[removeThis,4] <- as.numeric(forward$Value[1])
          lineup[removeThis,5] <- as.character(forward$teamAbbrev[1])
          forward <- forward[-c(1),]
          rownames(forward)=NULL
        }
      }
      
      # Replacing player, if we removed a utility.
      if (lineup[removeThis,1] == "UTIL"){
        if (nrow(util)==0) return(NULL)
        lineup[removeThis,2] = as.character(util$Name[1])
        lineup[removeThis,3] = as.numeric(util$Salary[1])
        lineup[removeThis,4] = as.numeric(util$Value[1])
        lineup[removeThis,5] = as.character(util$teamAbbrev[1])
        util <- util[-c(1),]
        rownames(util)=NULL
        while (length(unique(lineup[,2])) != 11){
          lineup[removeThis,2] <- as.character(util$Name[1])
          lineup[removeThis,3] <- as.numeric(util$Salary[1])
          lineup[removeThis,4] <- as.numeric(util$Value[1])
          lineup[removeThis,5] <- as.character(util$teamAbbrev[1])
          util <- util[-c(1),]
          rownames(util)=NULL
        }
      }
      budget = sum(as.integer(lineup[,3]),na.rm=TRUE)
    }
    
    # Output Team Salary and Points
    colnames(lineup) = c("Position","Player","Salary","Value", "Team")
    salary <- sum(as.numeric(lineup[,3]),na.rm = TRUE)
    points <- sum(as.numeric(lineup[,4])/10000 * as.numeric(lineup[,3]),na.rm = TRUE)
    return(list(lineup,salary,points))
  }
  
  findLineupMMA <- function(df=DKSalaries,exclude1="",exclude2="",exclude3="",exclude4="",exclude5=""){
    # Adds value variable to our dataframe. Multiply by 1000 just to make the numbers larger (doesn't actually matter)
    df$Value <- df$AvgPointsPerGame/df$Salary *1000

    # Remove fighters from cancelled matches
    cancelledMatches <- which(df$GameInfo=="Cancelled")
    df<-df[-c(cancelledMatches),]
    rownames(df)=NULL
    
    # Exclude user-selected players
    # This code was written prior to the change of exclude player to a dropdown menu, which explains its structure
    if (exclude1 != ""){
      if (length(which(df$Name==exclude1))==1){
        remove <- which(df$Name==exclude1)
        df <- df[-c(remove),]
      }
    }
    if (exclude2 != ""){
      if (length(which(df$Name==exclude2))==1){
        remove <- which(df$Name==exclude2)
        df <- df[-c(remove),]
      }
    }
    if (exclude3 != ""){
      if (length(which(df$Name==exclude3))==1){
        remove <- which(df$Name==exclude3)
        df <- df[-c(remove),]
      }
    }
    if (exclude4 != ""){
      if (length(which(df$Name==exclude4))==1){
        remove <- which(df$Name==exclude4)
        df <- df[-c(remove),]
      }
    }
    if (exclude5 != ""){
      if (length(which(df$Name==exclude5))==1){
        remove <- which(df$Name==exclude5)
        df <- df[-c(remove),]
      }
    }
    
    # Initalizes lineup matrix to be 5 rows and 3 columns.
    lineup <- matrix(nrow=5,ncol=3)
    
    # Creates dataframe of fighters, and orders based on average points per game. 
    fighters <- subset(df, df$Position==FALSE)
    fighters <- fighters[order(fighters$AvgPointsPerGame,decreasing=T),]
    rownames(fighters)=NULL
    
    # Loop creates our initial lineup 
    for (i in 1:5){
      # Chooses the top 5 fighters in average points per game, assigns name to column 2, salary to column 3 and value to column 4. 
      lineup[i,1] = as.character(fighters$Name[1])
      lineup[i,2] = as.numeric(fighters$Salary[1])
      lineup[i,3] = as.numeric(fighters$Value[1])
      #Eliminates player from dataframe once they are assigned to our lineup.
      fighters <- fighters[-c(1),]
      rownames(fighters)=NULL
    }
    
    # We need to find budget. 
    budget = sum(as.integer(lineup[,2]),na.rm=TRUE)

    # We remove the player of lowest value, and add the next best player. 
    while (budget > 50000){
      # Removing the player of lowest value. 
      lowestValue = min(as.numeric(lineup[,3]))
      removeThis = which(as.numeric(lineup[,3])==lowestValue)
      lineup[removeThis,1] = NA
      lineup[removeThis,2] = NA
      lineup[removeThis,3] = NA
      
      # Replacing player.
      if (nrow(fighters)==0) return(NULL)
      lineup[removeThis,1] = as.character(fighters$Name[1])
      lineup[removeThis,2] = as.numeric(fighters$Salary[1])
      lineup[removeThis,3] = as.numeric(fighters$Value[1])
      fighters <- fighters[-c(1),]
      rownames(fighters)=NULL
      
      # Recalculating budget.
      budget = sum(as.integer(lineup[,2]),na.rm=TRUE)
    }            
    
    # Output Team Salary and Points
    colnames(lineup) = c("Player","Salary","Value")
    salary <- sum(as.numeric(lineup[,2]),na.rm=TRUE)
    points <- sum(as.numeric(lineup[,3])/1000 * as.numeric(lineup[,2]),na.rm=TRUE)
    return(list(lineup,salary,points))
  }
  
  findLineupNASCAR <- function(df=DKSalaries,exclude1="",exclude2="",exclude3="",exclude4="",exclude5=""){
    # Adds value variable to our dataframe. Multiply by 1000 just to make the numbers larger (doesn't actually matter)
    df$Value <- df$AvgPointsPerGame/df$Salary *1000

    # Exclude user-selected players
    # This code was written prior to the change of exclude player to a dropdown menu, which explains its structure
    if (exclude1 != ""){
      if (length(which(df$Name==exclude1))==1){
        remove <- which(df$Name==exclude1)
        df <- df[-c(remove),]
      }
    }
    if (exclude2 != ""){
      if (length(which(df$Name==exclude2))==1){
        remove <- which(df$Name==exclude2)
        df <- df[-c(remove),]
      }
    }
    if (exclude3 != ""){
      if (length(which(df$Name==exclude3))==1){
        remove <- which(df$Name==exclude3)
        df <- df[-c(remove),]
      }
    }
    if (exclude4 != ""){
      if (length(which(df$Name==exclude4))==1){
        remove <- which(df$Name==exclude4)
        df <- df[-c(remove),]
      }
    }
    if (exclude5 != ""){
      if (length(which(df$Name==exclude5))==1){
        remove <- which(df$Name==exclude5)
        df <- df[-c(remove),]
      }
    }
    
    # Initalizes lineup matrix to be 6 rows and 3 columns.
    lineup <- matrix(nrow=6,ncol=3)
    
    # Creates dataframe of drivers, orders based on average points per race. 
    drivers <- subset(df, df$Position=="D")
    drivers <- drivers[order(drivers$AvgPointsPerGame,decreasing=T),]
    rownames(drivers)=NULL
    
    # Loop creates our initial lineup 
    for (i in 1:6){
      # Chooses the top six drivers in average points per game, assigns name to column 2, salary to column 3 and value to column 4. 
      lineup[i,1] = as.character(drivers$Name[1])
      lineup[i,2] = as.numeric(drivers$Salary[1])
      lineup[i,3] = as.numeric(drivers$Value[1])
      #Eliminates player from dataframe once they are assigned to our lineup.
      drivers <- drivers[-c(1),]
      rownames(drivers)=NULL
    }
    
    # We need to find budget. 
    budget = sum(as.integer(lineup[,2]),na.rm=TRUE)
    # We remove the player of lowest value, and add the next best player. 
    while (budget > 50000){
      # Removing the player of lowest value. 
      lowestValue = min(as.numeric(lineup[,3]))
      removeThis = which(as.numeric(lineup[,3])==lowestValue)
      lineup[removeThis,1] = NA
      lineup[removeThis,2] = NA
      lineup[removeThis,3] = NA
      
      # Replacing player.
      if (nrow(drivers)==0) return(NULL)
      lineup[removeThis,1] = as.character(drivers$Name[1])
      lineup[removeThis,2] = as.numeric(drivers$Salary[1])
      lineup[removeThis,3] = as.numeric(drivers$Value[1])
      drivers <- drivers[-c(1),]
      rownames(drivers)=NULL
      
      # Recalculating budget.
      budget = sum(as.integer(lineup[,2]),na.rm=TRUE)
    } 
        
    # Output Team Salary and Points
    colnames(lineup) = c("Player","Salary","Value")
    salary <- sum(as.numeric(lineup[,2]),na.rm=TRUE)
    points <- sum(as.numeric(lineup[,3])/1000 * as.numeric(lineup[,2]),na.rm=TRUE)
    return(list(lineup,salary,points))
  }
  
  findLineupNBA <- function(df=DKSalaries,exclude1="",exclude2="",exclude3="",exclude4="",exclude5=""){
    # Adds value variable to our dataframe. Multiply by 10,000 just to make the numbers larger (doesn't actually matter)
    df$Value <- df$AvgPointsPerGame/df$Salary *1000

    # Exclude user-selected players
    # This code was written prior to the change of exclude player to a dropdown menu, which explains its structure
    if (exclude1 != ""){
      if (length(which(df$Name==exclude1))==1){
        remove <- which(df$Name==exclude1)
        df <- df[-c(remove),]
      }
    }
    if (exclude2 != ""){
      if (length(which(df$Name==exclude2))==1){
        remove <- which(df$Name==exclude2)
        df <- df[-c(remove),]
      }
    }
    if (exclude3 != ""){
      if (length(which(df$Name==exclude3))==1){
        remove <- which(df$Name==exclude3)
        df <- df[-c(remove),]
      }
    }
    if (exclude4 != ""){
      if (length(which(df$Name==exclude4))==1){
        remove <- which(df$Name==exclude4)
        df <- df[-c(remove),]
      }
    }
    if (exclude5 != ""){
      if (length(which(df$Name==exclude5))==1){
        remove <- which(df$Name==exclude5)
        df <- df[-c(remove),]
      }
    }

    # Initalizes lineup matrix to be 8 rows and 5 columns. Sets first column to the positions we are looking to fill. 
    lineup <- matrix(nrow=8,ncol=5)
    lineup[,1] <- c("PG","SG","SF","PF","C","G","F","UTIL")
    
    # Creates dataframe of only the point guards, and orders based on average points per game. 
    pointguards <- subset(df, df$Position=="PG")
    pointguards <- pointguards[order(pointguards$AvgPointsPerGame,decreasing=T),]
    rownames(pointguards)=NULL
    
    # Creates dataframe of only the shooting guards, and orders based on average points per game.
    shootingguards <- subset(df, df$Position=="SG")
    shootingguards <- shootingguards[order(shootingguards$AvgPointsPerGame,decreasing=T),]
    rownames(shootingguards)=NULL
    
    # Creates dataframe of only the small forwards, and orders based on average points per game. 
    shootingforwards <- subset(df, df$Position=="SF")
    shootingforwards <- shootingforwards[order(shootingforwards$AvgPointsPerGame,decreasing=T),]
    rownames(shootingforwards)=NULL
    
    # Creates dataframe of only the power forwards, and orders based on average points per game.
    powerforwards <- subset(df, df$Position=="PF")
    powerforwards <- powerforwards[order(powerforwards$AvgPointsPerGame,decreasing=T),]
    rownames(powerforwards)=NULL
    
    # Creates dataframe of only the centers, and orders based on average points per game.
    centers <- subset(df, df$Position=="C")
    centers <- centers[order(centers$AvgPointsPerGame,decreasing=T),]
    rownames(centers)=NULL
    
    # Creates dataframe of all guards, and orders based on average points per game. 
    guards <- subset(df, df$Position == "PG"| df$Position == "SG")
    guards <- guards[order(guards$AvgPointsPerGame,decreasing=T),]
    rownames(guards)=NULL
    
    # Creates dataframe of all forwards, and orders based on average points per game. 
    forwards <- subset(df, df$Position == "SF" | df$Position=="PF")
    forwards <- forwards[order(forwards$AvgPointsPerGame,decreasing=T),]
    rownames(forwards)=NULL
    
    # Creates dataframe of all players for the utility wildcard slot, and orders based on average points per game. 
    util <- subset(df)
    util <- util[order(util$AvgPointsPerGame,decreasing=T),]
    rownames(util)=NULL
    
    # Loop creates our initial lineup 
    for (i in 1:8){
      # Chooses the top point guard in average points per game, assigns name to column 2, salary to column 3 and value to column 4. 
      if (i==1){
        lineup[i,2] = as.character(pointguards$Name[1])
        lineup[i,3] = as.numeric(pointguards$Salary[1])
        lineup[i,4] = as.numeric(pointguards$Value[1])
        lineup[i,5] = as.character(pointguards$teamAbbrev[1])
        #Eliminates player from dataframe once they are assigned to our lineup.
        removeThisPG <- which(guards$Name==pointguards$Name[1])
        removeThisPGUtil <- which(util$Name==pointguards$Name[1])
        pointguards <- pointguards[-c(1),]
        rownames(pointguards)=NULL
        guards <- guards[-c(removeThisPG),]
        rownames(guards)=NULL
        util <- util[-c(removeThisPGUtil),]
        rownames(util)=NULL
      }
      # Chooses the top shooting guard in average points per game, assigns name to column 2, salary to column 3 and value to column 4. 
      if (i==2){
        lineup[i,2] = as.character(shootingguards$Name[1])
        lineup[i,3] = as.numeric(shootingguards$Salary[1])
        lineup[i,4] = as.numeric(shootingguards$Value[1])
        lineup[i,5] = as.character(shootingguards$teamAbbrev[1])
        #Eliminates player from dataframe once they are assigned to our lineup.
        removeThisSG <- which(guards$Name==shootingguards$Name[1])
        removeThisSGUtil <- which(util$Name==shootingguards$Name[1])
        shootingguards <- shootingguards[-c(1),]
        rownames(shootingguards)=NULL
        guards <- guards[-c(removeThisSG),]
        rownames(guards)=NULL
        util <- util[-c(removeThisSGUtil),]
        rownames(util)=NULL
      }
      # Chooses the top small forward in average points per game, assigns name to column 2, salary to column 3 and value to column 4. 
      if (i==3){
        lineup[i,2] = as.character(shootingforwards$Name[1])
        lineup[i,3] = as.numeric(shootingforwards$Salary[1])
        lineup[i,4] = as.numeric(shootingforwards$Value[1])
        lineup[i,5] = as.character(shootingforwards$teamAbbrev[1])
        #Eliminates player from dataframe once they are assigned to our lineup.
        removeThisSF <- which(forwards$Name==shootingforwards$Name[1])
        removeThisSFUtil <- which(util$Name==shootingforwards$Name[1])
        shootingforwards <- shootingforwards[-c(1),]
        rownames(shootingforwards)=NULL
        forwards <- forwards[-c(removeThisSF),]
        rownames(forwards)=NULL
        util <- util[-c(removeThisSFUtil),]
        rownames(util)=NULL
      }
      # Chooses the top power forward player in average points per game, assigns name to column 2, salary to column 3 and value to column 4. 
      if (i==4){
        lineup[i,2] = as.character(powerforwards$Name[1])
        lineup[i,3] = as.numeric(powerforwards$Salary[1])
        lineup[i,4] = as.numeric(powerforwards$Value[1])
        lineup[i,5] = as.character(powerforwards$teamAbbrev[1])
        #Eliminates player from dataframe once they are assigned to our lineup.
        removeThisPF <- which(forwards$Name==powerforwards$Name[1])
        removeThisPFUtil <- which(util$Name==powerforwards$Name[1])
        powerforwards <- powerforwards[-c(1),]
        rownames(powerforwards)=NULL
        forwards <- forwards[-c(removeThisPF),]
        rownames(forwards)=NULL
        util <- util[-c(removeThisPFUtil),]
        rownames(util)=NULL
      }
      # Chooses the top center in average points per game, assigns name to column 2, salary to column 3 and value to column 4. 
      if (i==5){
        lineup[i,2] = as.character(centers$Name[1])
        lineup[i,3] = as.numeric(centers$Salary[1])
        lineup[i,4] = as.numeric(centers$Value[1])
        lineup[i,5] = as.character(centers$teamAbbrev[1])
        #Eliminates player from dataframe once they are assigned to our lineup.
        removeThisCUtil <- which(util$Name==centers$Name[1])
        centers <- centers[-c(1),]
        rownames(centers)=NULL
        util <- util[-c(removeThisCUtil),]
        rownames(util)=NULL
      }
      
      # Chooses the top guard in average points per game, assigns name to column 2, salary to column 3 and value to column 4. 
      if (i==6){
        lineup[i,2] = as.character(guards$Name[1])
        lineup[i,3] = as.numeric(guards$Salary[1])
        lineup[i,4] = as.numeric(guards$Value[1])
        lineup[i,5] = as.character(guards$teamAbbrev[1])
        #Eliminates player from dataframe once they are assigned to our lineup.
        
        removeThisGUtil <- which(util$Name==guards$Name[1])
        guards <- guards[-c(1),]
        rownames(centers)=NULL
        util <- util[-c(removeThisGUtil),]
        rownames(util)=NULL
      }
      
      # Chooses the top forward in average points per game, assigns name to column 2, salary to column 3 and value to column 4.
      if (i==7){
        lineup[i,2] = as.character(forwards$Name[1])
        lineup[i,3] = as.numeric(forwards$Salary[1])
        lineup[i,4] = as.numeric(forwards$Value[1])
        lineup[i,5] = as.character(forwards$teamAbbrev[1])
        #Eliminates player from dataframe once they are assigned to our lineup.
        removeThisFUtil <- which(util$Name==forwards$Name[1])
        forwards <- forwards[-c(1),]
        rownames(forwards)=NULL
        util <- util[-c(removeThisFUtil),]
        rownames(util)=NULL
      }
      
      # Chooses the top utility player in average points per game, assigns name to column 2, salary to column 3 and value to column 4.
      if (i==8){
        lineup[i,2] = as.character(util$Name[1])
        lineup[i,3] = as.numeric(util$Salary[1])
        lineup[i,4] = as.numeric(util$Value[1])
        lineup[i,5] = as.character(util$teamAbbrev[1])
        #Eliminates player from dataframe once they are assigned to our lineup.
        util <- util[-c(1),]
        rownames(util)=NULL
      }
    }
    
    # We need to find budget. 
    budget = sum(as.integer(lineup[,3]),na.rm=TRUE)

    # We remove the player of lowest value, and add the next best player. 
    while (budget > 50000){
      # Removing the player of lowest value. 
      lowestValue = min(as.numeric(lineup[,4]))
      removeThis = which(as.numeric(lineup[,4])==lowestValue)
      lineup[removeThis,2] = NA
      lineup[removeThis,3] = NA
      lineup[removeThis,4] = NA
      lineup[removeThis,5] = NA
      
      # Replacing player, if we removed a point guard.
      if (lineup[removeThis,1]=="PG"){
        if (nrow(pointguards)==0) return(NULL)
        lineup[removeThis,2] = as.character(pointguards$Name[1])
        lineup[removeThis,3] = as.numeric(pointguards$Salary[1])
        lineup[removeThis,4] = as.numeric(pointguards$Value[1])
        lineup[removeThis,5] = as.character(pointguards$teamAbbrev[1])
        removeThisPG <- which(guards$Name==pointguards$Name[1])
        removeThisPGUtil <- which(util$Name==pointguards$Name[1])
        pointguards <- pointguards[-c(1),]
        rownames(pointguards)=NULL
        guards <- guards[-c(removeThisPG),]
        rownames(guards)=NULL
        util <- util[-c(removeThisPGUtil),]
        rownames(util)=NULL
        while (length(unique(lineup[,2])) != 8){
          lineup[removeThis,2] <- as.character(pointguards$Name[1])
          lineup[removeThis,3] <- as.numeric(pointguards$Salary[1])
          lineup[removeThis,4] <- as.numeric(pointguards$Value[1])
          lineup[removeThis,5] <- as.character(pointguards$teamAbbrev[1])
          pointguards <- pointguards[-c(1),]
          rownames(pointguards)=NULL
        }
      }
      
      # Replacing player, if we removed a shooting guard.
      if (lineup[removeThis,1]=="SG"){
        if (nrow(shootingguards)==0) return(NULL)
        lineup[removeThis,2] = as.character(shootingguards$Name[1])
        lineup[removeThis,3] = as.numeric(shootingguards$Salary[1])
        lineup[removeThis,4] = as.numeric(shootingguards$Value[1])
        lineup[removeThis,5] = as.character(shootingguards$teamAbbrev[1])
        removeThisSG <- which(guards$Name==shootingguards$Name[1])
        removeThisSGUtil <- which(util$Name==shootingguards$Name[1])
        shootingguards <- shootingguards[-c(1),]
        rownames(shootingguards)=NULL
        guards <- guards[-c(removeThisSG),]
        rownames(guards)=NULL
        util <- util[-c(removeThisSGUtil),]
        rownames(util)=NULL
        while (length(unique(lineup[,2])) != 8){
          lineup[removeThis,2] <- as.character(shootingguards$Name[1])
          lineup[removeThis,3] <- as.numeric(shootingguards$Salary[1])
          lineup[removeThis,4] <- as.numeric(shootingguards$Value[1])
          lineup[removeThis,5] <- as.character(shootingguards$teamAbbrev[1])
          shootingguards <- shootingguards[-c(1),]
          rownames(shootingguards)=NULL
        }
      }
      
      # Replacing player, if we removed a small forward.
      if (lineup[removeThis,1]=="SF"){
        if (nrow(shootingforwards)==0) return(NULL)
        lineup[removeThis,2] = as.character(shootingforwards$Name[1])
        lineup[removeThis,3] = as.numeric(shootingforwards$Salary[1])
        lineup[removeThis,4] = as.numeric(shootingforwards$Value[1])
        lineup[removeThis,5] = as.character(shootingforwards$teamAbbrev[1])
        removeThisSF <- which(forwards$Name==shootingforwards$Name[1])
        removeThisSFUtil <- which(util$Name==shootingforwards$Name[1])
        shootingforwards <- shootingforwards[-c(1),]
        rownames(shootingforwards)=NULL
        forwards <- forwards[-c(removeThisSF),]
        rownames(forwards)=NULL
        util <- util[-c(removeThisSFUtil),]
        rownames(util)=NULL
        while (length(unique(lineup[,2])) != 8){
          lineup[removeThis,2] <- as.character(shootingforwards$Name[1])
          lineup[removeThis,3] <- as.numeric(shootingforwards$Salary[1])
          lineup[removeThis,4] <- as.numeric(shootingforwards$Value[1])
          lineup[removeThis,5] <- as.character(shootingforwards$teamAbbrev[1])
          shootingforwards <- shootingforwards[-c(1),]
          rownames(shootingforwards)=NULL
        }
      }
      
      # Replacing player, if we removed a power forward.
      if (lineup[removeThis,1]=="PF"){
        if (nrow(powerforwards)==0) return(NULL)
        lineup[removeThis,2] = as.character(powerforwards$Name[1])
        lineup[removeThis,3] = as.numeric(powerforwards$Salary[1])
        lineup[removeThis,4] = as.numeric(powerforwards$Value[1])
        lineup[removeThis,5] = as.character(powerforwards$teamAbbrev[1])
        removeThisPF <- which(forwards$Name==powerforwards$Name[1])
        removeThisPFUtil <- which(util$Name==powerforwards$Name[1])
        powerforwards <- powerforwards[-c(1),]
        rownames(powerforwards)=NULL
        forwards <- forwards[-c(removeThisPF),]
        rownames(forwards)=NULL
        util <- util[-c(removeThisPFUtil),]
        rownames(util)=NULL
        while (length(unique(lineup[,2])) != 8){
          lineup[removeThis,2] <- as.character(powerforwards$Name[1])
          lineup[removeThis,3] <- as.numeric(powerforwards$Salary[1])
          lineup[removeThis,4] <- as.numeric(powerforwards$Value[1])
          lineup[removeThis,5] <- as.character(powerforwards$teamAbbrev[1])
          shootingforwards <- powerforwards[-c(1),]
          rownames(powerforwards)=NULL
        }
      }
      
      # Replacing player, if we removed a center.
      if (lineup[removeThis,1]=="C"){
        if (nrow(centers)==0) return(NULL)
        lineup[removeThis,2] = as.character(centers$Name[1])
        lineup[removeThis,3] = as.numeric(centers$Salary[1])
        lineup[removeThis,4] = as.numeric(centers$Value[1])
        lineup[removeThis,5] = as.character(centers$teamAbbrev[1])
        removeThisCUtil <- which(util$Name==centers$Name[1])
        centers <- centers[-c(1),]
        rownames(centers)=NULL
        util <- util[-c(removeThisCUtil),]
        rownames(util)=NULL
        while (length(unique(lineup[,2])) != 8){
          lineup[removeThis,2] <- as.character(centers$Name[1])
          lineup[removeThis,3] <- as.numeric(centers$Salary[1])
          lineup[removeThis,4] <- as.numeric(centers$Value[1])
          lineup[removeThis,5] <- as.character(centers$teamAbbrev[1])
          centers <- centers[-c(1),]
          rownames(centers)=NULL
        }
      }
      
      # Replacing player, if we removed a guard.
      if (lineup[removeThis,1]=="G"){
        if (nrow(guards)==0) return(NULL)
        lineup[removeThis,2] = as.character(guards$Name[1])
        lineup[removeThis,3] = as.numeric(guards$Salary[1])
        lineup[removeThis,4] = as.numeric(guards$Value[1])
        lineup[removeThis,5] = as.character(guards$teamAbbrev[1])
        removeThisGUtil <- which(util$Name==guards$Name[1])
        guards <- guards[-c(1),]
        rownames(guards)=NULL
        util <- util[-c(removeThisGUtil),]
        rownames(util)=NULL
        while (length(unique(lineup[,2])) != 8){
          lineup[removeThis,2] <- as.character(guards$Name[1])
          lineup[removeThis,3] <- as.numeric(guards$Salary[1])
          lineup[removeThis,4] <- as.numeric(guards$Value[1])
          lineup[removeThis,5] <- as.character(guards$teamAbbrev[1])
          guards <- guards[-c(1),]
          rownames(guards)=NULL
        }
      }
      
      # Replacing player, if we removed a forward.
      if (lineup[removeThis,1]=="F"){
        if (nrow(forwards)==0) return(NULL)
        lineup[removeThis,2] = as.character(forwards$Name[1])
        lineup[removeThis,3] = as.numeric(forwards$Salary[1])
        lineup[removeThis,4] = as.numeric(forwards$Value[1])
        lineup[removeThis,5] = as.character(forwards$teamAbbrev[1])
        removeThisFUtil <- which(util$Name==forwards$Name[1])
        forwards <- forwards[-c(1),]
        rownames(forwards)=NULL
        util <- util[-c(removeThisFUtil),]
        rownames(util)=NULL
        while (length(unique(lineup[,2])) != 8){
          lineup[removeThis,2] <- as.character(forwards$Name[1])
          lineup[removeThis,3] <- as.numeric(forwards$Salary[1])
          lineup[removeThis,4] <- as.numeric(forwards$Value[1])
          lineup[removeThis,5] <- as.character(forwards$teamAbbrev[1])
          forwards <- forwards[-c(1),]
          rownames(forwards)=NULL
        }
      }
      
      # Replacing player, if we removed a utility.
      if (lineup[removeThis,1]=="UTIL"){
        if (nrow(util)==0) return(NULL)
        lineup[removeThis,2] = as.character(util$Name[1])
        lineup[removeThis,3] = as.numeric(util$Salary[1])
        lineup[removeThis,4] = as.numeric(util$Value[1])
        lineup[removeThis,5] = as.character(util$teamAbbrev[1])
        util <- util[-c(1),]
        rownames(util)=NULL
        while (length(unique(lineup[,2])) != 8){
          lineup[removeThis,2] <- as.character(util$Name[1])
          lineup[removeThis,3] <- as.numeric(util$Salary[1])
          lineup[removeThis,4] <- as.numeric(util$Value[1])
          lineup[removeThis,5] <- as.character(util$teamAbbrev[1])
          util <- util[-c(1),]
          rownames(util)=NULL
        }
      }
      budget = sum(as.integer(lineup[,3]),na.rm=TRUE)
    }
    
    # Output Team Salary and Points
    colnames(lineup) = c("Position","Player","Salary","Value", "Team")
    salary <- sum(as.numeric(lineup[,3]),na.rm = TRUE)
    points <- sum(as.numeric(lineup[,4])/10000 * as.numeric(lineup[,3]),na.rm = TRUE)
    return(list(lineup,salary,points))
  }
  
  # Store uploaded csv as a dataframe
  Data <- reactive({
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)}
    read.csv(inFile$datapath, header = TRUE,
                     sep = ",", quote = '"')
    })
  
  # Store user-selected player exclusions
  output$exclusion1 <- renderUI({
    if (is.null(Data())) return(NULL)
    selectInput("exclude1", label = h3("Exclude a First Player?"), 
                choices = c("",split(Data()$Name,Data()$Name)), 
                selected = 1)
  })
  
  output$exclusion2 <- renderUI({
    if (is.null(Data())) return(NULL)
    selectInput("exclude2", label = h3("Exclude a Second Player?"), 
                choices = c("",split(Data()$Name,Data()$Name)), 
                selected = 1)
  })
  
  output$exclusion3 <- renderUI({
    if (is.null(Data())) return(NULL)
    selectInput("exclude3", label = h3("Exclude a Third Player?"), 
                choices = c("",split(Data()$Name,Data()$Name)),
                selected = 1)
  })
  
  output$exclusion4 <- renderUI({
    if (is.null(Data())) return(NULL)
    selectInput("exclude4", label = h3("Exclude a Fourth Player?"), 
                choices = c("",split(Data()$Name,Data()$Name)),
                selected = 1)
  })
  
  output$exclusion5 <- renderUI({
    if (is.null(Data())) return(NULL)
    selectInput("exclude5", label = h3("Exclude a Fifth Player?"), 
                choices = c("",split(Data()$Name,Data()$Name)),
                selected = 1)
  })
  
  # Create matrix to be output as display for the suggested lineup
  output$matrix <- renderTable({
    if (input$sport=="NHL"){    
      if (is.null(Data())){
        return(matrix(c("C","C","W","W","W","D","D","UTIL","G",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),9,3))
        }
      else if (length(which(Data()$Position=="LW"))==0){
        return(matrix(c("C","C","W","W","W","D","D","UTIL","G",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),9,3))
        }
      else{
        return(findLineupNHL(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]])
        }
      }
    
    if (input$sport=="PGA"){    
      if (is.null(Data())){
        return(matrix(c("G","G","G","G","G","G",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),6,3))
        }
      else if (length(which(Data()$Position=="G"))!=NROW(Data()$Position)){
        return(matrix(c("G","G","G","G","G","G",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),6,3))
      }
      else{
        return(findLineupPGA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]])
        }
      }
    
    if (input$sport=="SOC"){    
      if (is.null(Data())){
        return(matrix(c("GK","D","D","D","M","M","M","F","F", "UTIL", "UTIL",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),11,3))
        }
      else if (length(which(Data()$Position=="GK"))==0){
        return(matrix(c("GK","D","D","D","M","M","M","F","F", "UTIL", "UTIL",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),11,3))
      }
      else{
        return(findLineupSOC(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]])
        }
      }
    
    if (input$sport=="MMA"){    
      if (is.null(Data())){
        return(matrix(c("F","F","F","F","F",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),5,3))
      }
      else if (length(which(Data()$Position==FALSE))!=NROW(Data()$Position)){
        return(matrix(c("F","F","F","F","F",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),5,3))
      }
      else{
        return(findLineupMMA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]])
      }
    }
    
    if (input$sport=="NASCAR"){    
      if (is.null(Data())){
        return(matrix(c("D","D","D","D","D","D",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),6,3))
      }
      else if (length(which(Data()$Position=="D"))!=NROW(Data()$Position)){
        return(matrix(c("D","D","D","D","D","D",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),6,3))
      }
      else{
        return(findLineupNASCAR(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]])
      }
    }
    
    if (input$sport=="NBA"){    
      if (is.null(Data())){
        return(matrix(c("PG","SG","SF","PF","C","G","F","UTIL",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),8,3))
      }
      else if (length(which(Data()$Position=="PG"))==0){
        return(matrix(c("PG","SG","SF","PF","C","G","F","UTIL",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),8,3))
      }
      else{
          return(findLineupNBA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]])
      }
    }
    })

  # Calculate total salary of suggested lineup
  output$salary <- renderPrint({
    if (input$sport=="NHL"){
      if (is.null(Data())){
        return("Enter a Dataset!")
      }
      else if (length(which(Data()$Position=="LW"))==0){
        return("You did not enter a dataset compatible with the NHL.")
      }
      else if (is.null(findLineupNHL(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return("The algorithm is unable to converge in this case. Please see the About tab for an explanation of why this has occured.")
      }

      else{
        return(findLineupNHL(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[2]])
        }
      }
    
    if (input$sport=="PGA"){    
      if (is.null(Data())){
        return("Enter a Dataset!")
      }
      else if (length(which(Data()$Position=="G"))!=NROW(Data()$Position)){
        return("You did not enter a dataset compatible with the PGA.")
      }
      else if (is.null(findLineupPGA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return("The algorithm is unable to converge in this case. Please see the About tab for an explanation of why this has occured.")
      }

      else{
        return(findLineupPGA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[2]])
        }
      }
    
    if (input$sport=="SOC"){
      if (is.null(Data())){
        return("Enter a Dataset!")
      }
      else if (length(which(Data()$Position=="GK"))==0){
        return("You did not enter a dataset compatible with soccer.")
      }
      else if (is.null(findLineupSOC(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return("The algorithm is unable to converge in this case. Please see the About tab for an explanation of why this has occured.")
      }
      else{
        return(findLineupSOC(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[2]])
        }
      }
    
    if (input$sport=="MMA"){    
      if (is.null(Data())){
        return("Enter a Dataset!")
      }
      else if (length(which(Data()$Position==FALSE))!=NROW(Data()$Position)){
        return("You did not enter a dataset compatible with MMA.")
      }
      else if (is.null(findLineupMMA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return("The algorithm is unable to converge in this case. Please see the About tab for an explanation of why this has occured.")
      }

      else{
        return(findLineupMMA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[2]])
      }
    }
    
    if (input$sport=="NASCAR"){    
      if (is.null(Data())){
        return("Enter a Dataset!")
      }
      else if (length(which(Data()$Position=="D"))!=NROW(Data()$Position)){
        return("You did not enter a dataset compatible with NASCAR")
      }
      else if (is.null(findLineupNASCAR(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return("The algorithm is unable to converge in this case. Please see the About tab for an explanation of why this has occured.")
      }
      else{
        return(findLineupNASCAR(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[2]])
      }
    }
    
    if (input$sport=="NBA"){
      if (is.null(Data())){
        return("Enter a Dataset!")
      }
      else if (length(which(Data()$Position=="PG"))==0){
        return("You did not enter a dataset compatible with the NBA")
      }
      else if (is.null(findLineupNBA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return("The algorithm is unable to converge in this case. Please see the About tab for an explanation of why this has occured.")
      }

      else{
        return(findLineupNBA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[2]])
      }
    }
    })

  # Calculate total expected number of points for suggested lineup
  output$points <- renderPrint({
    if (input$sport=="NHL"){
      if (is.null(Data())){
        return("Enter a Dataset!")
      }
      else if (length(which(Data()$Position=="LW"))==0){
        return("Please try again by uploading the appropriate file.")
      }
      else if (is.null(findLineupNHL(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return("The algorithm is unable to converge in this case. Please see the About tab for an explanation of why this has occured.")
      }

      else {
        return(findLineupNHL(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[3]])
        }
      }
    
    if (input$sport=="PGA"){    
      if (is.null(Data())){
        return("Enter a Dataset!")
      }
      else if (length(which(Data()$Position=="G"))!=NROW(Data()$Position)){
        return("Please try again by uploading the appropriate file.")
      }
      else if (is.null(findLineupPGA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return("The algorithm is unable to converge in this case. Please see the About tab for an explanation of why this has occured.")
      }
      else{
        return(findLineupPGA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[3]])
        }
      }
    
    if (input$sport=="SOC"){
      if (is.null(Data())){
        return("Enter a Dataset!")
      }
      else if (length(which(Data()$Position=="GK"))==0){
        return("Please try again by uploading the appropriate file.")
      }
      else if (is.null(findLineupSOC(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return("The algorithm is unable to converge in this case. Please see the About tab for an explanation of why this has occured.")
      }
      else {
        return(findLineupSOC(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[3]])
        }
      }
    
    if (input$sport=="MMA"){    
      if (is.null(Data())){
        return("Enter a Dataset!")
      }
      else if (length(which(Data()$Position==FALSE))!=NROW(Data()$Position)){
        return("Please try again by uploading the appropriate file.")
      }
      else if (is.null(findLineupMMA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return("The algorithm is unable to converge in this case. Please see the About tab for an explanation of why this has occured.")
      }

      else{
        return(findLineupMMA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[3]])
      }
    }
    
    if (input$sport=="NASCAR"){    
      if (is.null(Data())){
        return("Enter a Dataset!")
      }
      else if (length(which(Data()$Position=="D"))!=NROW(Data()$Position)){
        return("Please try again by uploading the appropriate file.")
      }
      else if (is.null(findLineupNASCAR(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return("The algorithm is unable to converge in this case. Please see the About tab for an explanation of why this has occured.")
      }

      else{
        return(findLineupNASCAR(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[3]])
      }
    }
    
    if (input$sport=="NBA"){
      if (is.null(Data())){
        return("Enter a Dataset!")
      }
      else if (length(which(Data()$Position=="PG"))==0){
        return("Please try again by uploading the appropriate file.")
      }
      else if (is.null(findLineupNBA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return("The algorithm is unable to converge in this case. Please see the About tab for an explanation of why this has occured.")
      }

      else {
        return(findLineupNBA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[3]])
      }
    }
    })
  
  # Generate barplot for salary breakdown by player
  output$salaryviz <- renderPlot({
    if (input$sport=="NHL"){
      if (is.null(Data())){
        return(NULL)
      }
      else if (length(which(Data()$Position=="LW"))==0){
        return("Please try again by uploading the appropriate file.")
      }
      else if (is.null(findLineupNHL(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return(NULL)
      }

      else {
        return(barplot(as.numeric(findLineupNHL(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,3]),
                       names.arg = findLineupNHL(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,2],
                       cex.names = 0.5, ylim = c(0,10000), ylab = "Player Price", main = "Lineup Salary Breakdown", col = "blue"
                       ))
      }
    }
    
    if (input$sport=="PGA"){    
      if (is.null(Data())){
        return(NULL)
      }
      else if (length(which(Data()$Position=="G"))!=NROW(Data()$Position)){
        return("Please try again by uploading the appropriate file.")
      }
      else if (is.null(findLineupPGA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return(NULL)
      }

      else{
        return(barplot(as.numeric(findLineupPGA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,2]),
                       names.arg = findLineupPGA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,1],
                       cex.names = 0.75, ylim = c(0,12000), ylab = "Player Price", main = "Lineup Salary Breakdown", col = "blue"
        ))
      }
    }
    
    if (input$sport=="SOC"){
      if (is.null(Data())){
        return(NULL)
      }
      else if (length(which(Data()$Position=="GK"))==0){
        return("Please try again by uploading the appropriate file.")
      }
      else if (is.null(findLineupSOC(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return(NULL)
      }

      else {
        return(barplot(as.numeric(findLineupSOC(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,3]),
                       names.arg = findLineupSOC(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,1],
                       cex.names = 1, ylim = c(0,10000), ylab = "Player Price", main = "Lineup Salary Breakdown", col = "blue"
        ))
      }
    }
    
    if (input$sport=="MMA"){    
      if (is.null(Data())){
        return(NULL)
      }
      else if (length(which(Data()$Position==FALSE))!=NROW(Data()$Position)){
        return("Please try again by uploading the appropriate file.")
      }
      else if (is.null(findLineupMMA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return(NULL)
      }

      else{
        return(barplot(as.numeric(findLineupMMA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,2]),
                       names.arg = findLineupMMA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,1],
                       cex.names = 0.75, ylim = c(0,12000), ylab = "Player Price", main = "Lineup Salary Breakdown", col = "blue"
        ))
      }
    }
    
    if (input$sport=="NASCAR"){    
      if (is.null(Data())){
        return(NULL)
      }
      else if (length(which(Data()$Position=="D"))!=NROW(Data()$Position)){
        return("Please try again by uploading the appropriate file.")
      }
      else if (is.null(findLineupNASCAR(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return(NULL)
      }

      else{
        return(barplot(as.numeric(findLineupNASCAR(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,2]),
                       names.arg = findLineupNASCAR(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,1],
                       cex.names = 0.75, ylim = c(0,12000), ylab = "Player Price", main = "Lineup Salary Breakdown", col = "blue"
        ))
      }
    }
    
    if (input$sport=="NBA"){
      if (is.null(Data())){
        return(NULL)
      }
      else if (length(which(Data()$Position=="PG"))==0){
        return("Please try again by uploading the appropriate file.")
      }
      else if (is.null(findLineupNBA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return(NULL)
      }
 
      else {
        return(barplot(as.numeric(findLineupNBA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,3]),
                       names.arg = findLineupNBA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,2],
                       cex.names = 0.5, ylim = c(0,10000), ylab = "Player Price", main = "Lineup Salary Breakdown", col = "blue"
        ))
      }
    }
  })
  
  # Generate barplot for value breakdown by player
  output$valueviz <- renderPlot({
    if (input$sport=="NHL"){
      if (is.null(Data())){
        return(NULL)
      }
      else if (length(which(Data()$Position=="LW"))==0){
        return("Please try again by uploading the appropriate file.")
      }
      else if (is.null(findLineupNHL(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return(NULL)
      }

      else {
        return(barplot(as.numeric(findLineupNHL(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,4]),
                       names.arg = findLineupNHL(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,2],
                       cex.names = 0.5, ylim = c(0,10), ylab = "Player Value", main = "Lineup Salary Breakdown", col = "blue"
        ))
      }
    }
    
    if (input$sport=="PGA"){    
      if (is.null(Data())){
        return(NULL)
      }
      else if (length(which(Data()$Position=="G"))!=NROW(Data()$Position)){
        return("Please try again by uploading the appropriate file.")
      }
      else if (is.null(findLineupPGA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return(NULL)
      }
 
      else{
        return(barplot(as.numeric(findLineupPGA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,3]),
                       names.arg = findLineupPGA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,1],
                       cex.names = 0.75, ylim = c(0,15), ylab = "Player Value", main = "Lineup Value Breakdown", col = "blue"
        ))
      }
    }
    
    if (input$sport=="SOC"){
      if (is.null(Data())){
        return(NULL)
      }
      else if (length(which(Data()$Position=="GK"))==0){
        return("Please try again by uploading the appropriate file.")
      }
      else if (is.null(findLineupSOC(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return(NULL)
      }

      else {
        return(barplot(as.numeric(findLineupSOC(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,4]),
                       names.arg = findLineupSOC(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,1],
                       cex.names = 1, ylim = c(0,70), ylab = "Player Value", main = "Lineup Value Breakdown", col = "blue"
        ))
      }
    }
    
    if (input$sport=="MMA"){    
      if (is.null(Data())){
        return(NULL)
      }
      else if (length(which(Data()$Position==FALSE))!=NROW(Data()$Position)){
        return("Please try again by uploading the appropriate file.")
      }
      else if (is.null(findLineupMMA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return(NULL)
      }

      else{
        return(barplot(as.numeric(findLineupMMA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,3]),
                       names.arg = findLineupMMA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,1],
                       cex.names = 0.75, ylim = c(0,15), ylab = "Player Value", main = "Lineup Value Breakdown", col = "blue"
        ))
      }
    }
    
    if (input$sport=="NASCAR"){    
      if (is.null(Data())){
        return(NULL)
      }
      else if (length(which(Data()$Position=="D"))!=NROW(Data()$Position)){
        return("Please try again by uploading the appropriate file.")
      }
      else if (is.null(findLineupNASCAR(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return(NULL)
      }

      else{
        return(barplot(as.numeric(findLineupNASCAR(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,3]),
                       names.arg = findLineupNASCAR(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,1],
                       cex.names = 0.75, ylim = c(0,15), ylab = "Player Value", main = "Lineup Value Breakdown", col = "blue"
        ))
      }
    }
    
    if (input$sport=="NBA"){
      if (is.null(Data())){
        return(NULL)
      }
      else if (length(which(Data()$Position=="PG"))==0){
        return("Please try again by uploading the appropriate file.")
      }
      else if (is.null(findLineupNBA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5))){
        return(NULL)
      }

      else {
        return(barplot(as.numeric(findLineupNBA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,4]),
                       names.arg = findLineupNBA(Data(),input$exclude1, input$exclude2, input$exclude3, input$exclude4, input$exclude5)[[1]][,2],
                       cex.names = 0.5, ylim = c(0,10), ylab = "Player Value", main = "Lineup Value Breakdown", col = "blue"
        ))
      }
    }
  })
  
  # Generate interactive d3 scatterplot for exploratory analysis
  output$scatter <- renderScatterD3({
    if (input$sport=="NHL"){
      if (is.null(Data())){
        return(NULL)
      }
      else if (length(which(Data()$Position=="LW"))==0){
        return(NULL)
      }
      else {
        return(scatterD3(Data()$Salary, Data()$AvgPointsPerGame,
                         point_size = input$pointsize,
                         point_opacity = input$opacity,
                         lab = Data()$Name,
                         labels_size = input$labelsize,
                         col_var = Data()$Position,
                         xlab = "Salary",
                         ylab = "Projected Points",
                         col_lab = "Position",
                         transitions = input$transition)
        )
      }
    }
    
    if (input$sport=="PGA"){    
      if (is.null(Data())){
        return(NULL)
      }
      else if (length(which(Data()$Position=="G"))!=NROW(Data()$Position)){
        return(NULL)
      }
      else{
        return(scatterD3(Data()$Salary, Data()$AvgPointsPerGame,
                         point_size = input$pointsize,
                         point_opacity = input$opacity,
                         lab = Data()$Name,
                         labels_size = input$labelsize,
                         col_var = Data()$Position,
                         xlab = "Salary",
                         ylab = "Projected Points",
                         col_lab = "Position",
                         transitions = input$transition)
        )
      }
    }
    
    if (input$sport=="SOC"){
      if (is.null(Data())){
        return(NULL)
      }
      else if (length(which(Data()$Position=="GK"))==0){
        return(NULL)
      }
      else {
        return(scatterD3(Data()$Salary, Data()$AvgPointsPerGame,
                         point_size = input$pointsize,
                         point_opacity = input$opacity,
                         lab = Data()$Name,
                         labels_size = input$labelsize,
                         col_var = Data()$Position,
                         xlab = "Salary",
                         ylab = "Projected Points",
                         col_lab = "Position",
                         transitions = input$transition)
        )
      }
    }
    
    if (input$sport=="MMA"){    
      if (is.null(Data())){
        return(NULL)
      }
      else if (length(which(Data()$Position==FALSE))!=NROW(Data()$Position)){
        return("Please try again by uploading the appropriate file.")
      }
      else{
        return(scatterD3(Data()$Salary, Data()$AvgPointsPerGame,
                         point_size = input$pointsize,
                         point_opacity = input$opacity,
                         lab = Data()$Name,
                         labels_size = input$labelsize,
                         col_var = Data()$Position,
                         xlab = "Salary",
                         ylab = "Projected Points",
                         col_lab = "Position",
                         transitions = input$transition)
        )
      }
    }
    
    if (input$sport=="NASCAR"){    
      if (is.null(Data())){
        return(NULL)
      }
      else if (length(which(Data()$Position=="D"))!=NROW(Data()$Position)){
        return("Please try again by uploading the appropriate file.")
      }
      else{
        return(scatterD3(Data()$Salary, Data()$AvgPointsPerGame,
                         point_size = input$pointsize,
                         point_opacity = input$opacity,
                         lab = Data()$Name,
                         labels_size = input$labelsize,
                         col_var = Data()$Position,
                         xlab = "Salary",
                         ylab = "Projected Points",
                         col_lab = "Position",
                         transitions = input$transition)
        )
      }
    }
    
    if (input$sport=="NBA"){
      if (is.null(Data())){
        return(NULL)
      }
      else if (length(which(Data()$Position=="PG"))==0){
        return("Please try again by uploading the appropriate file.")
      }
      else {
        return(scatterD3(Data()$Salary, Data()$AvgPointsPerGame,
                         point_size = input$pointsize,
                         point_opacity = input$opacity,
                         lab = Data()$Name,
                         labels_size = input$labelsize,
                         col_var = Data()$Position,
                         xlab = "Salary",
                         ylab = "Projected Points",
                         col_lab = "Position",
                         transitions = input$transition)
        )
      }
    }
  })
  })