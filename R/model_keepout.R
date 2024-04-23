keep.out.unsb <- function(model, wl1, wl2, wl3, wl4, wl5, wl6){

  if( !is.na(wl3)) if( wl3 == "NA") wl3 <- NA
  if( !is.na(wl4)) if( wl4 == "NA") wl4 <- NA
  if( !is.na(wl5)) if( wl5 == "NA") wl5 <- NA
  if( !is.na(wl6)) if( wl6 == "NA") wl6 <- NA

  ncolp <- ncol(model$data) - 1

  if(wl1 != min(model$wl)){
    keepout.1 <- ncolp + 1
    keepout.2 <- max(which(model$wl < wl1)) + ncolp
  }

  if(wl1 == min(model$wl)){
    keepout.1 = max(which(model$wl < wl2)) + ncolp
  }

  suppressMessages(ifelse(all(!is.na(wl3)), check.wl3 <- T, check.wl3 <- F))
  suppressMessages(ifelse(all(!is.na(wl5)), check.wl5 <- T, check.wl5 <- F))

  if(!check.wl3 & !check.wl5){

    keepout.7 <- min(which(model$wl > wl2)) + ncolp

    }

  if(check.wl3 & !check.wl5){
    keepout.3 <- min(which(model$wl > wl2 & model$wl < wl3)) + ncolp
    keepout.4 <- max(which(model$wl > wl2 & model$wl < wl3)) + ncolp

    keepout.7 <- min(which(model$wl > wl4)) + ncolp
  }

  if(check.wl5){
    keepout.3 <- min(which(model$wl > wl2 & model$wl < wl3)) + ncolp
    keepout.4 <- max(which(model$wl > wl2 & model$wl < wl3)) + ncolp

    keepout.5 <- min(which(model$wl > wl4 & model$wl < wl5)) + ncolp
    keepout.6 <- max(which(model$wl > wl4 & model$wl < wl5)) + ncolp

    keepout.7 <- min(which(model$wl > wl6)) + ncolp
  }


  if(max( model$wl ) != max(sort( c(wl1, wl2, wl3, wl4, wl5, wl6))))  keepout.max <- length(model$wl) + ncolp
  if(max( model$wl ) == max(sort( c(wl1, wl2, wl3, wl4, wl5, wl6)))) stop()

  if(check.wl5 & wl1 != min(model$wl)) keepout.return <- paste0(keepout.1, "-", keepout.2
                                                                , ",", keepout.3, "-", keepout.4
                                                                , ",", keepout.5, "-", keepout.6
                                                                , ",", keepout.7, "-", keepout.max)
  if(check.wl3 & !check.wl5 & wl1 != min(model$wl)) keepout.return <- paste0(keepout.1, "-", keepout.2, ",", keepout.3, "-", keepout.4, ",", keepout.7, "-", keepout.max)
  if(!check.wl3 & wl1 != min(model$wl)) keepout.return <- paste0(keepout.1, "-", keepout.2, ",", keepout.7, "-", keepout.max)
  if(!check.wl3 & wl1 == min(model$wl)) keepout.return <- paste0(keepout.1, "-", keepout.max)

  return(keepout.return)
}
