###--- Uncertainty Functions ---###

# Calculate U-Deviation

ydeviation <- function(MODEL, derivative, PREDSET, REFSET = PREDSET){

  library(pls)
  x.cal.scores <- MODEL$scores  # Scores of reference + prediction sets

  x.val.scores <- predict(MODEL, newdata = PREDSET, type = "scores")
  y.val.pred <- predict(MODEL, newdata = PREDSET)

  y.val.pred <- y.val.pred[,1,] # Predictions
  loadings <- MODEL$loadings    # Model loadings
  loadings.mod <- matrix(data = NA, nrow = ncol( MODEL$model[[ 2 ]]), ncol = MODEL$ncomp)
  for(i in 1 : MODEL$ncomp){  loadings.mod[ , i] <- as.numeric(loadings[ , i])  }

  spccol <- which(PREDSET$wl %in% gsub("X", "", colnames(MODEL$model[[ 2 ]]))) # get model columns

  if( derivative == "spc")  x.val <- data.table( PREDSET$spc )[ , spccol, with = F]          # Spectra of prediction set
  if( derivative == "1st")  x.val <- data.table( PREDSET$spc1st )[ , spccol, with = F]          # Spectra of prediction set
  if( derivative == "2nd")  x.val <- data.table( PREDSET$spc2nd )[ , spccol, with = F]          # Spectra of prediction set

  # obs <- PREDSET$data[,substance, with = F]         # Lab data for prediction set
  ncalobj <- nrow(REFSET$data)       # Number of callibration samples

  # Get Leverage
  Hi <- getLeverage(x.cal.scores, x.val.scores)

  # Get ResXvalSamp
  if( derivative == "spc")  ResXValSamp <- getResXValSamp(data.table(PREDSET$spc)[ , spccol, with = F]
                                                          , data.table(REFSET$spc)[ , spccol, with = F]
                                                          , x.val.scores, loadings.mod)
  if( derivative == "1st")  ResXValSamp <- getResXValSamp(data.table(PREDSET$spc1st)[ , spccol, with = F]
                                                          , data.table(REFSET$spc1st)[ , spccol, with = F]
                                                          , x.val.scores, loadings.mod)
  if( derivative == "2nd")  ResXValSamp <- getResXValSamp(data.table(PREDSET$spc2nd)[ , spccol, with = F]
                                                          , data.table(REFSET$spc2nd)[ , spccol, with = F]
                                                          , x.val.scores, loadings.mod)

  # Get ResXValTot
  if( derivative == "spc")  ResXValTot <- getTotResXCal( data.table( REFSET$spc)[ , spccol, with = F], x.cal.scores, loadings.mod)
  if( derivative == "1st")  ResXValTot <- getTotResXCal( data.table( REFSET$spc1st)[ , spccol, with = F], x.cal.scores, loadings.mod)
  if( derivative == "2nd")  ResXValTot <- getTotResXCal( data.table( REFSET$spc2nd)[ , spccol, with = F], x.cal.scores, loadings.mod)

  # Get ResYValVar
  ResYValVar <- MSEP(MODEL, intercept=FALSE)$val[1,1,]

  # Get U-Deviation
  ydev <- getYdev(ResYValVar, ResXValSamp, ResXValTot, Hi, ncalobj)

  if( MODEL$ncomp > 1 ) returnlist <- list(as.numeric(y.val.pred[,MODEL$ncomp]), ydev[,MODEL$ncomp])
  if( MODEL$ncomp == 1 ) returnlist <- list(as.numeric(y.val.pred), ydev[,MODEL$ncomp])

  names( returnlist ) <- c("prediction", "ydeviation")
  return(returnlist)

}

#1 Xresidual in validation objects
getResXValSamp <- function(x.val.mat,x.cal.mat,x.val.scores,x.cal.loadings){
  nobj <- dim(x.val.mat)[1]
  ncomp <- dim(x.val.scores)[2]
  npred <- dim(x.cal.loadings)[1]
  res.val <- matrix(0, nrow=nobj, ncol=ncomp)
  Xmeans.cal <- colMeans(x.cal.mat) #compare predictors average from calib object with predictors from each object
  # to get predictor redisuals.
  X.center <- x.val.mat - matrix(rep(Xmeans.cal, each = nobj), nrow=nobj)
  for(i in 1:ncomp){
    x.fac.load.wts <- x.val.scores[,1:i, drop=FALSE] %*% t(x.cal.loadings[,1:i,drop=FALSE])
    #res.val[,i] <- rowMeans((-x.fac.load.wts + X.center)^2)
    res.val[,i] <- rowSums((-x.fac.load.wts + X.center)^2)/(npred-i)

  }
  return(res.val)
}



#2 Total X residual in Validation sets
#extracted from getXvalres function by using a colMeans function
#use residuals based on xcal rather than cross-validation to make it more deterministic

getTotResXCal <- function(x.cal.mat,x.cal.scores,x.cal.loadings){
  nobj <- dim(x.cal.mat)[1]
  ncomp <- dim(x.cal.scores)[2]
  npred <- dim(x.cal.loadings)[1]
  res.val <- matrix(0, nrow=nobj, ncol=ncomp)
  #Xmeans.cal <- colMeans(x.cal.mat) #compare predictors average from calib object with predictors from each object
  # to get predictor redisuals.
  #X.center <- x.cal.mat - matrix(rep(Xmeans.cal, each = nobj), nrow=nobj)
  X.center <- scale(x.cal.mat, scale=FALSE)
  for(i in 1:ncomp){
    x.fac.load.wts <- x.cal.scores[,1:i, drop=FALSE] %*% t(x.cal.loadings[,1:i,drop=FALSE])
    #res.val[,i] <- rowMeans((-x.fac.load.wts + X.center)^2)
    res.val[,i] <- rowSums((-x.fac.load.wts + X.center)^2)/(npred-i)

  }
  tot.res <- colMeans(res.val)
  return(tot.res)
}


#3
#note that pred.mat should be m * n matrix with m the number of object and n the number of components
getResYValVar <- function(val.resp, pred.mat){
  ncomp <- dim(pred.mat)[2]
  nobj <- dim(pred.mat)[1]
  res.val <- matrix(0, nrow=nobj, ncol=ncomp)
  #Y.center <- scale(val.resp, scale=FALSE)
  res.val <- sapply(1:ncomp, function(x){mean((pred.mat[,x] - val.resp)^2)})
  return(res.val)
}

#4. Leverage corresponding to best PLS -- This function gives the leverage for each component. The total leverage is
# the sum of individual component leverage for each prediction samples
#************ Gives a cumulative leverage based on the number of PC used ***************#
getLeverage <- function(scores.calib, scores.valid){
  ta.calib <- diag(crossprod(scores.calib))
  ta.calib1 <- matrix(rep(ta.calib, each = nrow(scores.valid)), nrow=nrow(scores.valid))
  ncal <- dim(scores.calib)[1]
  Hi <- scores.valid^2 / ta.calib1
  ncomp <- dim(scores.valid)[2]
  nobj <- dim(scores.valid)[1]
  Hi.pr <- matrix(0, nrow=nobj, ncol=ncomp)
  for(i in 1:ncomp){
    if(i == 1){
      Hi.pr[,1] <- Hi[,1]
    }
    else {
      Hi.pr[,i] <- rowSums(Hi[,1:i])
    }
  }
  Hi.pr <- Hi.pr + (1/ncal)
  return(Hi.pr)
}



#5 : Compute prediction error ydev
getYdev <- function(ResYValVar, ResXValSamp, ResXValTot, Hi, ncalobj){
  nobj <- dim(ResXValSamp)[1]
  ncomp <- dim(ResXValSamp)[2]
  ydev <- matrix(0, nrow= nobj, ncol=ncomp)
  for( i in 1:ncomp){
    ydev[,i] <- sqrt(ResYValVar[i] * (ResXValSamp[,i]/ResXValTot[i] + Hi[,i] + 1/ncalobj) * (1- (i+1)/ncalobj))
  }
  return(ydev)
}
