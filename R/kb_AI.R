kb_AI <- function(p.u
                  , pca.reference
                  , spc.dat
                  , limit.PC = 1
                  , limit.level = 2
                  , lambda
                  , ncomp
                  , pred.analysis
                  , colp
                  , beverage
                  , location
                  , line
                  , pl
                  , parameter
                  , plot
                  , scaling_factor = 10
                  , conf.lim = .95){

  # p.u = Produktion, die untersucht wird
  # p.b = Produktion, die als Basis dient
  p.b = 1:(p.u - 1)
  p.b <- unique(sort(p.b))
  p.b <- p.b[ p.b > 0]

  pchp.warning <- 2
  pchp.alarm <- 7

  dat <- list()

  dat$ID <- unique( spc.dat$data$ID )
  spc.analysis <- spc.dat$spc1st[ spc.dat$data$ID == dat$ID[ p.u ], spc.dat$wl %in% lambda, with = F]
  date.analysis <- spc.dat$data$datetime[ spc.dat$data$ID == dat$ID[ p.u ]]

  jj <- c(p.b, p.u)
  jj <- unique(sort(jj))
  jj <- jj[ jj > 0]
  for(j in jj){
    dat$pca.prediction[[ j ]] <- predict(pca.reference
                                         , spc.dat$spc1st[ spc.dat$data$ID == dat$ID[ j ], spc.dat$wl %in% lambda, with = F])
  }

  dat$ellipse[[ max(p.b) ]] <- ellipse.dt(x1 = pca.reference$calres$scores[ , ncomp[ 1 ]]
                                          , y1 = pca.reference$calres$scores[ , ncomp[ 2 ]]
                                          , conf.lim = conf.lim, scaling_factor = scaling_factor
                                          , col = "black", lwd = 1.5, plot = F)

  dat$point_outside_ellipse[[ p.u ]] <- mapply(function(x1, y1) point_outside_ellipse(point = c(x1
                                                                                                , y1)
                                                                                      , ellipse_info = dat$ellipse[[ max(p.b) ]])
                                               , x1 = dat$pca.prediction[[ p.u ]]$scores[ , ncomp[ 1 ]]
                                               , y1 = dat$pca.prediction[[ p.u ]]$scores[ , ncomp[ 2 ]]
                                               , SIMPLIFY = F)

  warning.influence <- which(dat$pca.prediction[[ p.u ]]$T2[ , limit.PC] > pca.reference$T2lim[limit.level, limit.PC] |
                               dat$pca.prediction[[ p.u ]]$Q[ , limit.PC] > pca.reference$Qlim[limit.level, limit.PC])
  outlier.influence <- which(dat$pca.prediction[[ p.u ]]$T2[ , limit.PC] > pca.reference$T2lim[limit.level, limit.PC] &
                               dat$pca.prediction[[ p.u ]]$Q[ , limit.PC] > pca.reference$Qlim[limit.level, limit.PC])

  warning.influence <- warning.influence[ warning.influence %in% which(unlist( dat$point_outside_ellipse[[ p.u ]])) ]
  outlier.influence <- outlier.influence[ outlier.influence %in% which(unlist( dat$point_outside_ellipse[[ p.u ]])) ]

  if(plot){
    colp.prod <- rep(colp[ p.u ], nrow(spc.analysis))
    colp.prod[ warning.influence ] <- "black"
    colp.prod[ outlier.influence ] <- "black"
    pch.prod <- rep(1, nrow(spc.analysis))
    pch.prod[ warning.influence ] <- pchp.warning
    pch.prod[ outlier.influence ] <- pchp.alarm

    png(pngname <- paste0(paste(beverage, location, line, "Produktion"
                                , formatC(p.u, width = 3, format = "d", flag = "0"), "Analyse.png", sep = "_"))
        ,xxx<-4800,xxx/16*9,"px",12,"white",res=500,"Eurostile Next LT Pro",T,"cairo")

    layout(matrix( c(1,1,2,3,4,4), byrow = T, ncol = 2), heights = c(.2, .5, .4))

    par(mar = c(0,0,0,0))
    plot(1,1,type="n",xlab="",ylab="",axes=F,ylim=c(0,1),xlim=c(0,1))
    text(.5, .92
         , paste0("Ausreißer-Identifizierung mittels Referenz-PCA")
         , col = dauschblue
         , cex = 1.6, xpd = T, font = 2)

    text(.5, .72
         , paste0("Standort ", location, ", Linie ", line, ", Getränk ", beverage, ", Pfadlänge = ", pl)
         , col = dauschblue#, adj = c(0, 1)
         , cex = 1.6, xpd = T, font = 2)

    legend("bottomleft", c(xrange.date(date = date.analysis, ID = dat$ID[ p.u ])
                           , "Confidenz-Ellipse / Influence-Plot Grenzen")
           , pch = c(1, NA)
           , lty = c(NA, 3)
           , col = c(colp[ p.u ], "black"), bty = "n", cex = 1.1, ncol = 1)

    legend("bottomright", c("Warnung"
                            , "Alarm")
           , pch = c(pchp.warning, pchp.alarm)
           # , lty = c(NA, NA)
           , col = c("black", "black"), bty = "n", cex = 1.1, ncol = 1)

    xlimp <- range( unlist( lapply(dat$pca.prediction, function( x ) range(x$scores[ , 1]))))
    ylimp <- range( unlist( lapply(dat$pca.prediction, function( x ) range(x$scores[ , 2]))))

    par(mar = c(4, 4, 1.5, 1), cex.main = 1, cex.lab = 1, cex.axis = 1)
    plot( x1 <- dat$pca1st[[ 1 ]]$calres$scores[ , ncomp[ 1 ]]
          , y1 <- dat$pca1st[[ 1 ]]$calres$scores[ , ncomp[ 2 ]]
          , xlab = ""
          , ylab = ""
          , col = colp[ 1 ]
          , xlim = xlimp
          , ylim = ylimp
          , main = "Scores-Plot", type = "n")
    for(j in c(p.b, p.u)){
      if(j != p.u) points(dat$pca.prediction[[ j ]]$scores[ , ncomp[ 1 ]], dat$pca.prediction[[ j ]]$scores[ , ncomp[ 2 ]]
                          , col = colp[ j ], cex = .75)
      if(j == p.u) points(dat$pca.prediction[[ j ]]$scores[ , ncomp[ 1 ]], dat$pca.prediction[[ j ]]$scores[ , ncomp[ 2 ]]
                          , col = colp.prod, pch = pch.prod, cex = 1)
    }
    mtext(paste0("PC", ncomp[ 1 ], ", ( ", round(pca.reference$calres$expvar[ ncomp[ 1 ] ], 1), " %)")
          , 1, 2.5, cex = .65, font = 2)
    mtext(paste0("PC", ncomp[ 2 ], ", ( ", round(pca.reference$calres$expvar[ ncomp[ 2 ] ], 1), " %)")
          , 2, 2.5, cex = .65, font = 2)

    lines(dat$ellipse[[ max(p.b) ]][[ 1 ]], col = "black", lty = 3)

    # Influence Plot
    xlimp <- c(ifelse(min(dat$pca.prediction[[ p.u ]]$T2[ , 1]) < pca.reference$T2lim[2,1]
                      , min(dat$pca.prediction[[ p.u ]]$T2[ , 1]) * 0.75
                      , pca.reference$T2lim[2,1] * 0.75)
               , ifelse(max(dat$pca.prediction[[ p.u ]]$T2[ , 1]) > pca.reference$T2lim[2,1]
                        , max(dat$pca.prediction[[ p.u ]]$T2[ , 1]) * 1.25
                        , pca.reference$T2lim[2,1] * 1.25))
    ylimp <- c(ifelse(min(dat$pca.prediction[[ p.u ]]$Q[ , 1]) < pca.reference$Qlim[2,1]
                      , min(dat$pca.prediction[[ p.u ]]$Q[ , 1]) * 0.75
                      , pca.reference$Qlim[2,1] * 0.75)
               , ifelse(max(dat$pca.prediction[[ p.u ]]$Q[ , 1]) > pca.reference$Qlim[2,1]
                        , max(dat$pca.prediction[[ p.u ]]$Q[ , 1]) * 1.25
                        , pca.reference$Qlim[2,1] * 1.25))

    plot(dat$pca.prediction[[ p.u ]]$T2[ , 1]
         , dat$pca.prediction[[ p.u ]]$Q[ , 1]
         , col = colp.prod
         , pch = pch.prod
         , log = "xy"
         , xlim = xlimp, ylim = ylimp
         , xlab = "", ylab = "", main = "Influence-Plot")
    mtext(paste0("Hotelling's T2, PC ", 1)
          , 1, 2.5, cex = .65, font = 2)
    mtext(paste0("Q-Residuals, PC ", 1)
          , 2, 2.5, cex = .65, font = 2)
    abline(h = pca.reference$Qlim[2,1]
           , v = pca.reference$T2lim[2,1], lty = 3)

    # Prediction Plot
    t2 <- date.analysis
    y3 <- pred.analysis
    ylimp <- c(70, 130)

    plot(t2
         , y3
         , col = colp.prod, pch = pch.prod
         , ylim = ylimp
         , xlab = "", ylab = "", main = paste0(parameter, " Prediction")
         , axes = F
         , cex = .5)

    mtext(paste0(parameter, " in %")
          , 2, 2.5, cex = .65, font = 2)
    abline(h = c(80, 90, 110, 120), lty = 3, col = c("red", "orange", "orange", "red"), lwd = 1)
    axis(2)
    axis.POSIXct(1, t2, format = "%d.%b.%y %H:%M", cex.axis = .9)
    box()

    if(any(y3 < ylimp[ 1 ])){

      points( t2[ which(y3 < ylimp[ 1 ])]
              , rep(par("usr")[3], length( which(y3 < ylimp[ 1 ]) ))
              , pch = pch.prod[ which(y3 < ylimp[ 1 ])]
              , xpd = T)
    }

    if(any(y3 > ylimp[ 2 ])){

      points( t2[ which(y3 > ylimp[ 2 ])]
              , rep(par("usr")[4], length( which(y3 > ylimp[ 2 ]) ))
              , pch = pch.prod[ which(y3 > ylimp[ 2 ])]
              , xpd = T)
    }

    if(any(y3 < ylimp[ 1 ]) | any(y3 > ylimp[ 2 ]))
      text(par("usr")[2] - diff(par("usr")[1:2]) * .15
           , par("usr")[3] - diff(par("usr")[3:4]) * .325
           , paste0("*Punkte auf der oberen/unteren x-Achse sind > ",  ylimp[ 2 ]
                    , " % bzw. < ", ylimp[ 1 ], " %")
           , xpd = "T", cex = .8)

    dev.off()
  }

  return(outlier.influence)
}

ellipse.dt <- function(x1, y1, conf.lim = 0.99, scaling_factor = 3, col = "black", lty = 3, lwd = 1, plot = T){

  x1 <- as.numeric(x1)
  y1 <- as.numeric(y1)
  s1 <- sd(x1)^2
  s2 <- sd(y1)^2
  nobj <- length(x1)
  CI <- scaling_factor * (2 * (nobj - 1)/(nobj - 2)) * qf(conf.lim, 2, (nobj - 2))
  xc <- mean(x1)
  yc <- mean(y1)
  a <- sqrt(CI * s1)
  b <- sqrt(CI * s2)
  t <- seq(0, 2 * pi, 0.01)
  x <- xc + a * cos(t)
  y <- yc + b * sin(t)
  if(plot) lines(x, y, lty = lty, col = col, lwd = lwd)

  ellipse_matrix <- matrix(c(x, y), ncol = 2)

  return(list(ellipse = ellipse_matrix, center = c(xc, yc), major_axis = a, minor_axis = b))
}

# Function to check if a point is outside of the ellipse
point_outside_ellipse <- function(point, ellipse_info) {
  center <- ellipse_info$center
  major_axis <- ellipse_info$major_axis
  minor_axis <- ellipse_info$minor_axis

  distance_x <- (point[1] - center[1])^2
  distance_y <- (point[2] - center[2])^2
  distance <- sqrt(distance_x + distance_y)

  if ((distance_x / major_axis^2) + (distance_y / minor_axis^2) > 1) {
    return(TRUE)  # Point is outside the ellipse
  } else {
    return(FALSE) # Point is inside the ellipse
  }
}

# Datumsfunktion
xrange.date <- function(date = dt$spc$raw$spc$date, ID = dt$para$ID){
  xrange.d <- range( as.Date(date ))
  if( any( duplicated( xrange.d ))){

    main.date <- paste0("Produktion (ID = ", ID, ") am "
                        , format( xrange.d[ 1 ], "%d. %B %Y"))

  } else{

    main.date <- paste0("Produktion (ID = ", ID, ") vom "
                        , format( xrange.d[ 1 ], "%d. %B %Y"), " bis zum "
                        , format( xrange.d[ 2 ], "%d. %B %Y"))
  }
  return(main.date)
}
