influence_plot <- function( pca.input
                            , T2lim = c(1,2)
                            , Qlim = c(1,2)
                            , ncomp = 1
                            , legendtext
                            , png = F
                            , plot = T
                            , pngtext = NA
                            , col = NA
                            , return = T){

  if(is.na(col)) col = "black"
  if( is.na( pngtext )) pngtext <- ""

  T2 <- pca.input$calres$T2[ , ncomp]
  Q <- pca.input$calres$Q[ , ncomp]
  T2lim <- pca.input$T2lim[T2lim , ncomp]
  Qlim <- pca.input$Qlim[Qlim , ncomp]

  xlim <- ifelse( max( T2 , na.rm = T) > max(T2lim)
                  , max( T2 , na.rm = T)
                  , max(T2lim)) * 1.1
  xlim <- c(0, xlim)

  ylim <- ifelse( max( Q , na.rm = T) > max(Qlim)
                  , max( Q , na.rm = T)
                  , max(Qlim)) * 1.1
  ylim <- c(0, ylim)

  if( png ) png(paste0("Influence_Plot_"
                       , pngtext, ".png")
                ,xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")

  if( plot ){
    plot( T2, Q, xlim = xlim, ylim = ylim
          , xlab = ""
          , ylab = ""
          , main = paste("Influence Plot, PC", ncomp, pngtext)
          , col = col)
    mtext(paste0("Hotelling's T2, PC ", ncomp)
          , 1, 2.5, cex = .65, font = 2)
    mtext(paste0("Q-Residuals, PC ", ncomp)
          , 2, 2.5, cex = .65, font = 2)
    abline( h = Qlim, v = T2lim
            , lty = 2, col = "red")

  }

  valid.vector <- rep("valid", length(T2))

  if(length( T2lim ) == 2 & length( Qlim ) == 2){

    valid.vector[ which(T2 > T2lim[ 2 ])] <- "warning"
    valid.vector[ which(Q > Qlim[ 2 ])] <- "warning"
    valid.vector[ which(T2 > T2lim[ 2 ] & Q > Qlim[ 2 ])] <- "alarm"
  }

  if(length( T2lim ) == 1 & length( Qlim ) == 1){

    valid.vector[ which(T2 > T2lim[ 1 ])] <- "warning"
    valid.vector[ which(Q > Qlim[ 1 ])] <- "warning"
    valid.vector[ which(T2 > T2lim[ 1 ] & Q > Qlim[ 1 ])] <- "alarm"
  }


  if(png) dev.off()
  if(return) return(valid.vector)
}

xlim_wl <- function( legendtext){

  xlimp <- c( legendtext$wl1, legendtext$wl2, legendtext$wl3, legendtext$wl4)
  xlimp <- xlimp[ which( xlimp != 0 | xlimp != "0") ]
  xlimp <- xlimp[ !is.na( xlimp ) ]
  return( xlimp )
}

ylim_wl <- function( y, wl, xlim_wl){

  if( is.null( nrow( y ))){
    if( length( xlim ) == 2) ylimp <- c(min( t( data.frame(y))[ , which( wl %in% xlim )[ 1 ] : which( wl %in% xlim )[ 2 ]], na.rm = T)
                                        , max( t( data.frame(y))[ , which( wl %in% xlim )[ 1 ] : which( wl %in% xlim )[ 2 ]], na.rm = T))
    if( length( xlim ) == 4) ylimp <- c(min( t( data.frame(y))[ , c(which( wl %in% xlim )[ 1 ] : which( wl %in% xlim )[ 2 ], which( wl %in% xlim )[ 3 ] : which( wl %in% xlim )[ 4 ])], na.rm = T)
                                        , max( t( data.frame(y))[ , c(which( wl %in% xlim )[ 1 ] : which( wl %in% xlim )[ 2 ], which( wl %in% xlim )[ 3 ] : which( wl %in% xlim )[ 4 ])], na.rm = T))
  }

  if( !is.null( nrow( y ))){
    if( length( xlim ) == 2) ylimp <- c(min( data.frame(y)[ , which( wl %in% xlim )[ 1 ] : which( wl %in% xlim )[ 2 ]], na.rm = T)
                                        , max( data.frame(y)[ , which( wl %in% xlim )[ 1 ] : which( wl %in% xlim )[ 2 ]], na.rm = T))
    if( length( xlim ) == 4) ylimp <- c(min( data.frame(y)[ , c(which( wl %in% xlim )[ 1 ] : which( wl %in% xlim )[ 2 ], which( wl %in% xlim )[ 3 ] : which( wl %in% xlim )[ 4 ])], na.rm = T)
                                        , max( data.frame(y)[ , c(which( wl %in% xlim )[ 1 ] : which( wl %in% xlim )[ 2 ], which( wl %in% xlim )[ 3 ] : which( wl %in% xlim )[ 4 ])], na.rm = T))
  }
  return( ylimp )

}


