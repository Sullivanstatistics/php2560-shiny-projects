# Optimal design points - mock model

# libraries
library(GPfit)
library(geometry)
library(lattice)


# 0. The microsimulation model - this can be changed to a real example
mock.model1 <- function(x) {
  # 1 dim mock example
  y <- log(x+0.1)+sin(5*pi*x)
  #y <- log(x+0.1)+sin(7*pi*x)
  return(y)
}

mock.model2 <- function(x) {
  # 1 dim mock example
  #y <- log(x+0.1)+sin(5*pi*x)
  y <- log(x+0.1)+sin(7*pi*x)
  #y <- 50*x+exp(7*x)*sin(5*pi*x)
  return(y)
}

mock.model3 <- function(x) {
  # 1 dim mock example
  #y <- log(x+0.1)+sin(5*pi*x)
  #y <- log(x+0.1)+sin(7*pi*x)
  y <- 50*x+exp(7*x)*sin(5*pi*x)
  return(y)
}



# 1. The initial design points
init.set.up <- function(micro.sim.model,n.init.dp, n.dim) {
  # gets evenly spaced inputs
  
  x <- matrix(rep(seq(0,1,length.out=n.init.dp), n.dim), nrow=n.init.dp, ncol=n.dim)
  y <- micro.sim.model(x)
  
  output <- list(x=x, y=y)
  return(output)
}


# 2. Candidate points - midpoints between current design points
candidate.points <- function(x, meta.model) {
  # Inputs:
  # x is a matrix of the inputs for the design points
  
  # Returns:
  # cand.points is a matrix of the values of the candidate input points
  
  x <- as.matrix(x[order(x),], nrow=nrow(x), ncol=1) 
  temp.cand.points <- x[-nrow(x),] + apply(x,2,diff)/2
  cand.points <-  temp.cand.points

  output <- list(cand.points=cand.points)
  return(output)
}


# 3. The meta-model for jackknife
theta.jk <- function(i.jk, x.corners, y.corners, x.no.corners, y.no.corners, cand.points) {
  
  x.jk <- t(t(c(x.no.corners[-(i.jk)], x.corners)))
  y.jk <- t(t(c(y.no.corners[-(i.jk)], y.corners)))
  
  
  model.jk <- GP_fit(x.jk, y.jk)
  y_pred <- predict(model.jk, cand.points)$Y_hat
  
  output <- list(y_pred=y_pred)
  return(output)
}


# 4. The jackknife - keeps "corners"
jack.knife <- function(x, y, cand.points) {
  # Inputs:
  # x is a matrix of the inputs for the design points
  
  # Returns:
  # y_hat.jk is a matrix of the predictions for each candidate when each i-th design point has been removed
  #   with i rows and number of columns = number of design points
  
  # Notes:
  # n.jk is the number of jackknife "procedures" 
  #   keep "corners", but cycle through all other design points (so number design points - 2 for 1 dim and ndim*2 for >1 dim)
  
  # "corners" stay
  x.corners.index <- which(x==min(x)| x==max(x))
  x.corners <- t(t(x[x.corners.index,]))
  x.no.corners <- t(t(x[-x.corners.index,]))
  y.corners <- y[x.corners.index,]
  y.no.corners <- y[-x.corners.index,]
  
  n.jk <- nrow(x.no.corners)
  
  y_hat.jk <- matrix(unlist(sapply(1:n.jk, theta.jk, x.corners, y.corners, x.no.corners, y.no.corners, cand.points)), 
                     byrow=T, nrow=n.jk,ncol=nrow(cand.points))
  
  return (y_hat.jk)
}


# 5. Get max se of candidate points from jackknife procedure
max.se.jk <- function (x, y_hat0, y_hat.jk) {
  # Inputs:
  # x is a matrix of the inputs for the design points
  # y_hat0 is a vector of the prediction for each candidate with all design points (0 eliminated)
  # y_hat.jk is a matrix of the predictions for each candidate when each i-th design point has been removed 
  #  [from jack.knife function]
  
  # Returns:
  # cand.pseudo.jk is a matrix of the "jackknife's pseudo value" of each candidate
  # mean.cand.jk is a vector of the means of the jackknife pseudo values for each candidate
  # se.cand.jk is a vector of the se for the jackknife for each candidate
  # max.se.jk is a scalar with the max se among the candidate points
  # cand.max.se.jk is a scalar indicating which candidate point has the max se
  
  # Notes:
  # n.jk is a scalar of the number of jackknife "procedures" 
  # n.cand is a scalar of the number of candidate input point
  
  # From Kleijnen and Van Beers Journal of the Operational Research Society 2004, eq 10
  # y_j,i = n_c * Y_hat_j(-0) - (n_c - 1) * Y_hat_j(-i) with j= 1,...,c and i = 1,...,n_c 
  # Y_hat_j(-0) is the original prediction for candidate j based on complete set of 
  # observations (0 observations eliminated). Y_hat_j(-i) is the prediction when the i-th
  # design point has been removed
  
  
  
  n.jk <- nrow(x)-2
  n.cand <- nrow(x)-1
  
  
  cand.pseudo.jk <- matrix(NA, nrow=n.jk, ncol=ncol(y_hat.jk))
  for (i in 1: n.jk) {
    cand.pseudo.jk[i,] <- n.cand * y_hat0 - (n.cand-1)*y_hat.jk[i,]
  }
  
  mean.cand.jk <- apply(cand.pseudo.jk, 2, mean)
  se.cand.jk <- apply((cand.pseudo.jk-mean.cand.jk)^2, 2, sum)/(n.cand*(n.cand-1))
  max.se.jk <- max(se.cand.jk)
  cand.max.se.jk <- which(se.cand.jk==max.se.jk)
  
  output <- list(#cand.pseudo.jk=cand.pseudo.jk, mean.cand.jk=mean.cand.jk, se.cand.jk=se.cand.jk,
    max.se.jk=max.se.jk, cand.max.se.jk=cand.max.se.jk)
  return (output)
}

# 6. Check if the SE of the chosen JK point meets threshold criteria
check.jk.thresh <- function(x, y, jk.max.se, cand.points, micro.sim.model,jk.threshold) {
  # Inputs:
  # x is a matrix of the inputs for the design points
  # y is a matrix of the output of the design points
  # jk.max.se is the list of results from max.se.jk function
  # jk.threshold is the criteria - need to figure out what is reasonable
  
  # Returns:
  # x (matrix) updated with chosen candidate point, if threshold not met (otherwise no change)
  # y (matrix) updated with corresponding output, if threshold not met (otherwise no change)
  
  # Notes:
  # max.se.jk is a scalar with the max se among the candidate points [from max.se.jk function]
  # cand.max.se.jk is a scalar indicating which candidate point has the max se [from max.se.jk function]
  # x.new.jk is the candidate point with the max se from jk
  
  
  max.se.jk <- jk.max.se$max.se.jk
  cand.max.se.jk <- jk.max.se$cand.max.se.jk
  
  if (max.se.jk > jk.threshold) {  
    x.new.jk <- cand.points[cand.max.se.jk,]
    
    x.jk <- rbind(x,x.new.jk)
    y.jk <- micro.sim.model(x.jk)
    
    run.jk <- TRUE
  } else{
    x.jk <- x
    y.jk <- y
    
    run.jk <- FALSE
  }
  
  output <- list(x=x.jk, y=y.jk, run.jk=run.jk)
  return(output)
  
}



# 7. Check if the SE of the chosen se point meets threshold criteria
check.se.thresh <- function(x, y, meta.model, cand.points, micro.sim.model, se.threshold=0) {
  # Inputs:
  # x is a matrix of the inputs for the design points
  # y is a matrix of the output of the design points
  # meta.model is a list of the metamodel results with original (all) design points
  # cand.points is a matrix of the values of the candidate input points (midpoints for now)
  # se.threshold is the criteria - need to figure out what is reasonable
  
  # Returns:
  # x (matrix) updated with chosen candidate point, if threshold not met (otherwise no change)
  # y (matrix) updated with corresponding output, if threshold not met (otherwise no change)
  # run.jk (boolean) to indicate if the threshold criteria was met, and should now check jk again
  
  # Notes:
  # max.se is a scalar of the max se among the candidate points
  # cand.max.se is a scalar indicating which candidate point has the max se
  mse.cand <- predict.GP(meta.model, cand.points)$MSE
  max.se <- max(mse.cand)
  cand.max.se <- min(which(mse.cand==max.se))
  
  if (max.se > se.threshold) {
    x.new.se <- cand.points[cand.max.se,]
    
    x.se <- rbind(x,x.new.se)
    y.se <- micro.sim.model(x.se)
    
    run.jk <- TRUE
  } else{
    x.se <- x
    y.se <- y
    
    run.jk <- FALSE
  }
  
  output <- list(run.jk=run.jk, x=x.se, y=y.se)
  return(output)
  
}

#####
# Wrapper
#####
run.optimal.dp <- function (micro.sim.model=mock.model1, n.init.dp, n.dim=1, max.num.dp=2, 
                            jk.threshold=0.4,se.threshold=0,
                            have.dp=FALSE, user.init.dp=NULL) {
  #set-up for initial model
  if (have.dp==FALSE) {
    # get evenly spaced initial design points if user doesnt specify them
    dp.init <- init.set.up(micro.sim.model,n.init.dp, n.dim)
    x <- dp.init$x
    y <- dp.init$y
  } else {
    x <- matrix(user.init.dp[,"x"], nrow=n.init.dp, ncol=n.dim)
    y <- matrix(user.init.dp[,"y"], nrow=n.init.dp, ncol=1)
  }
    
  run.jk <- TRUE
  iteration = 1
  res.GP.model <- vector("list") # placeholder for results
  meta.model <- GP_fit(x,y)
  res.GP.model[[1]] <- meta.model
  
  while (iteration < max.num.dp) {
    iteration <- iteration+1
    
    #candidate points
    cand.points <- candidate.points(x, meta.model)$cand.points

    if(run.jk==TRUE) {
      ####
      # Part 1: first jackknife to get "location" of model
      ####
      # 1.1 Run jacknife and get predictions for each candidate point
      y_hat0 <- predict(meta.model, cand.points)$Y_hat
      y_hat.jk <- jack.knife(x, y, cand.points)
      
      # 1.2 Choose candidate with the max jackknife se
      jk.max.se <- max.se.jk(x, y_hat0, y_hat.jk)
      
      # 1.3 Keep point if total volume <threshold
      check.jk <- check.jk.thresh(x,y,jk.max.se, cand.points, micro.sim.model,jk.threshold) 
      
      run.jk <- check.jk$run.jk
      if(run.jk==TRUE) {
        x <- as.matrix(check.jk$x)
        y <- as.matrix(check.jk$y)
      } else {iteration <- iteration-1; next}

    } else{
      ####
      # Part 2: Check if variance ok
      ####
      # 2.1 Get a point that minimizes total volume (temp get midpoint with max se)
      check.se <- check.se.thresh(x,y,meta.model, cand.points, micro.sim.model, se.threshold)
      x <- as.matrix(check.se$x)
      y <- as.matrix(check.se$y)
      run.jk <- check.se$run.jk

    }

    meta.model <- GP_fit(x,y)
    res.GP.model[[iteration]] <- meta.model
    
  }
  
  #output
  output <- list(res.GP.model=res.GP.model)
  return(output)
}