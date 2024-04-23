model_opt_lab_lin_prod_modellist <- function( model.trs = dt$model$trs
                                              , columns = NA){


  model <- list()
  model$data <- data.frame(model.trs$data)
  # vector
  levellist <- lapply( columns, function( y ) levels( factor( model$data[ , grep( y, colnames( model$data))[ 1 ]])))
  seqp <- list()

  for(i in 1 : length( columns)){

    seqp[[ i ]] <- list()

    seqp[[ i ]][[  1 ]] <- 1:nrow(model$data)

    for(j in 1 : length( levellist[[ i ]])){

      seqp[[ i ]][[ j + 1 ]] <-   grep( levellist[[ i ]][ j ], model$data[ , grep( columns[ i ], colnames( model$data))[ 1 ]], invert = T)
    }

    seqp[[ i ]] <- lapply(  seqp[[ i ]], unlist)
  }

  names( seqp ) <- columns

  seqp <- do.call( c, seqp)

  dt$var.model$names <- list()
  for(i in 1 : length( columns)) dt$var.model$names[[ i ]] <- paste0( columns[[ i ]], "_", levellist[[ i ]])

  names( seqp ) <- c("all", unlist( dt$var.model$names))


  returnmodellist <- lapply(seqp, function( x ) list(data = model$data[x,]
                                                     , wl = model.trs$wl
                                                     , spc = model.trs$spc[x,]
                                                     , spc1st = model.trs$spc1st[x,]
                                                     , spc2nd = model.trs$spc2nd[x,]))
  return( returnmodellist )
}


model_opt_lab_lin_prod_match <- function(lab_date, lab_para
                                         , prod_data
                                         , LG3_time, LG3_timerange
                                         , unit){

  opt <- list()
  match_date <- list()
  match_subdat <- list()
  match_subdatx <- list()
  match_labmedian <- list()
  subdat <- list()

  for(i in 1:length(unit)){

    if(substr(prod_data[[i]]$datetime, 11,11)[1] != " ")  prod_data[[i]]$datetime <- as.POSIXct(strptime(prod_data[[i]]$datetime, "%Y-%m-%dT%H:%M:%S") )

    if(LG3_time[[i]] >= LG3_timerange[[i]])
      match_subdat[[i]] <- lapply(lab_date[[i]], function(x) which( prod_data[[i]]$datetime >= (x - LG3_time[[i]] - LG3_timerange[[i]] ) &
                                                                      prod_data[[i]]$datetime <= (x - LG3_time[[i]] + LG3_timerange[[i]] )))
    if(LG3_time[[i]] < LG3_timerange[[i]])
      match_subdat[[i]] <- lapply(lab_date[[i]], function(x) which( prod_data[[i]]$datetime <= (x - LG3_time[[i]] + LG3_timerange[[i]] )))

    lab_date[[i]] <- lab_date[[i]][which(lapply(match_subdat[[i]], length) != 0)]
    lab_para[[i]] <- lab_para[[i]][which(lapply(match_subdat[[i]], length) != 0)]

    subdat[[i]] <- prod_data[[i]][sort(unique(unlist(match_subdat[[i]]))) , ]
    subdat[[i]] <- transfer_csv(csv.file = subdat[[i]])

    if(LG3_time[[i]] >= LG3_timerange[[i]])
      match_subdatx[[i]] <- lapply(lab_date[[i]], function(x) which( subdat[[i]]$data$datetime >= (x - LG3_time[[i]] - LG3_timerange[[i]] ) &
                                                                       subdat[[i]]$data$datetime <= (x - LG3_time[[i]] + LG3_timerange[[i]] )))

    if(LG3_time[[i]] < LG3_timerange[[i]])
      match_subdatx[[i]] <- lapply(lab_date[[i]], function(x) which( subdat[[i]]$data$datetime <= (x - LG3_time[[i]] + LG3_timerange[[i]] )))

    match_labmedian[[i]]<- median(lab_para[[i]])

  }

  returnlist <- list(lab_date, lab_para, subdat, match_subdatx, match_labmedian)
  return( returnlist )
}

model_opt_lab_lin_prod_calc <- function(lab_date, lab_para
                                        , prod_data
                                        , LG3_time, LG3_timerange
                                        , unit, directory
                                        , lin_csv
                                        , lin_dat
                                        , pls_model
                                        , pls_substance
                                        , pls_wl
                                        , pls_ncomp
                                        , matchfun
                                        , wiederholbarkeit){

  library(pls)
  opt <- list()
  match_date <- list()
  match_subdat <- list()
  match_subdatx <- list()
  match_labmedian <- list()
  subdat <- list()

  lab_date = matchfun[[1]]
  lab_para = matchfun[[2]]
  subdat = matchfun[[3]]
  match_subdatx = matchfun[[4]]
  match_labmedian = matchfun[[5]]

  plslist <- list()
  pred_lin <- list()
  match_sd_wiederholbarkeit <- list()
  match_diff_wiederholbarkeit <- list()

  for(z in 1:length(pls_model)){
    plslist <- pls_model(csv_transfered = pls_model[[z]]
                         , substance = pls_substance
                         , wlr = pls_wl
                         , ncomp = pls_ncomp)

    pred_lin <- pls_prediction(csv_transfered = lin_csv
                               , pls_model = plslist
                               , ncomp = pls_ncomp
                               , messaget = F)


    pred_subdat <- list()
    match_min <- list()
    match_sd <- list()
    match_sd_mod <- list()
    match_cor <- list()
    for(i in 1:length(unit)){

      match_sd_wiederholbarkeit[[i]] <- list()
      match_diff_wiederholbarkeit[[i]] <- list()

      pred_subdat[[i]] <- pls_prediction(csv_transfered = subdat[[i]], pls_model = plslist, ncomp = pls_ncomp, messaget = F)
      match_min[[i]] <- lapply(pred_subdat[[i]]$prediction, function(b) lapply(b, function(a) mapply(function(x, z) apply(a, 2, function(y) y[ x ][ which.min( abs ( y[ x ] - z ))])
                                                                                                     , x = match_subdatx[[i]]
                                                                                                     , z = lab_para[[i]])))
      match_sd[[i]] <- lapply(match_min[[i]], function(a) lapply(a, function(b) apply(b, 1, function(c) sd( lab_para[[i]] - c))))
      match_cor[[i]] <- lapply(match_min[[i]], function(a) lapply(a, function(b) apply(b, 1, function(c) cor( lab_para[[i]], c))))

      for(p in 1 : length( wiederholbarkeit[[ i ]])){

        match_diff_wiederholbarkeit[[i]][[ p ]] <- lapply(match_min[[i]], function(a) lapply(a, function(b) apply(b, 1, function(c) abs( sum( lab_para[[i]][ wiederholbarkeit[[ i ]][[ p ]] ] - c[ wiederholbarkeit[[ i ]][[ p ]] ])))))
        match_sd_wiederholbarkeit[[i]][[ p ]] <- lapply(match_min[[i]], function(a) lapply(a, function(b) apply(b, 1, function(c) sd( lab_para[[i]][ wiederholbarkeit[[ i ]][[ p ]] ] - c[ wiederholbarkeit[[ i ]][[ p ]] ]))))

      }

      match_sd_mod[[ i ]] <- list()

      for(k in 1:length(match_sd[[i]])){
        if(k == 1) spc = "spc"
        if(k == 2) spc = "1st"
        if(k == 3) spc = "2nd"

        match_sd_mod[[i]][[k]] <- list()

        for(j in 1:length(match_sd[[i]][[k]])){
          match_sd_mod[[i]][[k]][[j]] <- list()
          match_sd_mod[[i]][[k]][[j]] <- data.frame(spc = spc, ncomp = j
                                                    , wl1 = substr(names(match_sd[[i]][[k]][[j]]), 1, 3)
                                                    , wl2 = substr(names(match_sd[[i]][[k]][[j]]), 5, 7)
                                                    , wl3 = substr(names(match_sd[[i]][[k]][[j]]), 9, 11)
                                                    , wl4 = substr(names(match_sd[[i]][[k]][[j]]), 13, 15)
                                                    , wl5 = substr(names(match_sd[[i]][[k]][[j]]), 17, 19)
                                                    , wl6 = substr(names(match_sd[[i]][[k]][[j]]), 21, 23)
                                                    , sd = round(match_sd[[i]][[k]][[j]], 2))

          for(p in 1 : length( wiederholbarkeit[[ i ]]))
            match_sd_mod[[i]][[k]][[j]] <- cbind(match_sd_mod[[i]][[k]][[j]], round(match_sd_wiederholbarkeit[[i]][[ p ]][[ k ]][[ j ]], 2))

          names( match_sd_mod[[i]][[k]][[j]] )[ (length( match_sd_mod[[i]][[k]][[j]] ) - length( wiederholbarkeit[[ i ]]) + 1) : length( match_sd_mod[[i]][[k]][[j]] ) ] <- paste0("sd.d", formatC( 1 : length( wiederholbarkeit[[ i ]]), width = 4, format = "d", flag = "0"))

          for(p in 1 : length( wiederholbarkeit[[ i ]]))
            match_sd_mod[[i]][[k]][[j]] <- cbind(match_sd_mod[[i]][[k]][[j]], round(match_diff_wiederholbarkeit[[i]][[ p ]][[ k ]][[ j ]], 2))

          names( match_sd_mod[[i]][[k]][[j]] )[ (length( match_sd_mod[[i]][[k]][[j]] ) - length( wiederholbarkeit[[ i ]]) + 1) : length( match_sd_mod[[i]][[k]][[j]] ) ] <- paste0("diff.d", formatC( 1 : length( wiederholbarkeit[[ i ]]), width = 4, format = "d", flag = "0"))

        }


      }
      match_sd_mod[[i]] <- do.call(rbind, lapply(match_sd_mod[[i]], function(x) do.call(rbind, x)))
      names(match_sd_mod[[i]])[grep("sd", names(match_sd_mod[[i]]))[ 1 ]] <- paste0("sd.", unit[[i]])
    }

    # R2 und lin range ####
    lin_minmax <- lapply(pred_lin$prediction, function(x) lapply(x, function(y) apply(y, 2, function(z) z[length(z)] - z[1])))
    lin_R2 <- lapply(pred_lin$prediction, function(x) lapply(x, function(y) apply(y, 2, function(z) cor(z, lin_dat) ^ 2)))

    lin_sd <- lapply(pred_lin$prediction, function(x) lapply(x, function(y) apply(y, 2, function(z) sd ( lin_dat - z))))

    # merge ####


    if( length( unit ) > 1){

      match_merge <- match_sd_mod[[ 1 ]]

      for( i in 2 : length( unit)){

        tocbind <- match_sd_mod[[ i ]][ , names( match_sd_mod[[ i ]] ) != c("spc", "ncomp", "wl1", "wl2", "wl3", "wl4", "wl5", "wl6")]
        names( tocbind )[ 1 ] <- paste0("sd.", unlist(unit[ i ]))

        match_merge <- cbind(match_merge, tocbind)

      }
    } else { match_merge <- match_sd_mod[[1]]}

    match_merge <- cbind(match_merge, lin_range = unlist(lin_minmax), lin_R2 = unlist(lin_R2), lin_sd = unlist(lin_sd))
    match_merge <- data.frame(match_merge)

    RMSEspc <- lapply( plslist$spc, function( x ) pls::RMSEP( x ) )
    RMSEspc1st <- lapply( plslist$spc1st, function( x ) pls::RMSEP( x ) )
    RMSEspc2nd <- lapply( plslist$spc2nd, function( x ) pls::RMSEP( x ) )

    RMSEspcsub <- list()
    RMSEspc1stsub <- list()
    RMSEspc2ndsub <- list()
    lmsubspc <- list()
    lmsubspc1st <- list()
    lmsubspc2nd <- list()

    for(i in 1:pls_ncomp){

      RMSEspcsub[[ i ]] <- lapply(RMSEspc, function( x ) x$val[,, i+1 ])
      RMSEspc1stsub[[ i ]] <- lapply(RMSEspc1st, function( x ) x$val[,, i+1 ])
      RMSEspc2ndsub[[ i ]] <- lapply(RMSEspc2nd, function( x ) x$val[,, i+1 ])
      lmsubspc[[i]] <- lapply(plslist$spc, function( x ) summary( lm( x$fitted.values[,,i] ~ x$model$`pls$x`))$r.squared)
      lmsubspc1st[[i]] <- lapply(plslist$spc1st, function( x ) summary( lm( x$fitted.values[,,i] ~ x$model$`pls$x`))$r.squared)
      lmsubspc2nd[[i]] <- lapply(plslist$spc2nd, function( x ) summary( lm( x$fitted.values[,,i] ~ x$model$`pls$x`))$r.squared)

    }

    match_merge <- cbind( match_merge
                          , model_RMSE = c(unlist( RMSEspcsub), unlist( RMSEspc1stsub), unlist( RMSEspc2ndsub))
                          , model_R2 = c(unlist( lmsubspc), unlist( lmsubspc1st), unlist( lmsubspc2nd )))

    rownames(match_merge) <- 1:nrow(match_merge)

    if(length(grep("sd", names(match_merge))) > 1) match_merge$sd.sum <- apply(match_merge[ , grep("sd", names(match_merge))],1,function(x) sum(x, na.rm =T))
    # if(length(grep("sd", names(match_merge))) > 1) match_merge <- match_merge[order(match_merge$sd.sum) , ]
    # if(length(grep("sd", names(match_merge))) == 1) match_merge <- match_merge[order(match_merge[ , grep("sd", names(match_merge))] ) , ]

    setwd(directory)

    match_merge$lin_range <- round(match_merge$lin_range, 2)
    match_merge$lin_R2 <- round(match_merge$lin_R2, 2)
    match_merge$lin_sd <- round(match_merge$lin_sd, 2)
    match_merge$model_RMSE <- round(match_merge$model_RMSE, 2)
    match_merge$model_R2 <- round(match_merge$model_R2, 2)
    match_merge$sd.sum <- round(match_merge$sd.sum, 2)

    fwrite(cbind(ID = 1 : nrow(match_merge), match_merge)
           , paste0(kk <- substr( gsub(" ", "", gsub(":", "", gsub("-", "", Sys.time()))), 3, 14), "_analysis_", formatC(z, width = 4, format = "d", flag = "0"), ".csv")
           , row.names = F
           , dec = ","
           , sep = ";")
    if(z == 1) kk0 <- kk
    if(z == 1) kk0 <- as.numeric(gsub("_","",substr(kk0, 1, 13)))

    write.csv2(data.frame(cbind(pls_model[[ z ]]$data, pls_model[[ z ]]$spc)), paste0(kk, "_modelmatrix_", formatC(z, width = 4, format = "d", flag = "0"), ".csv"), row.names = F)
    rm(match_merge)
    gc()
    message(paste(z, "finished at",Sys.time()))
  }

  setwd(directory)
  export_files <- dir()[which(as.numeric(gsub("_","",substr(dir(), 1, 13))) >= kk0)]
  export_files <- export_files[grep("_analysis", export_files)]
  export_files <- export_files[grep("merge", export_files, invert = T)]

  export_ID <- as.numeric( substr( export_files
                                   , unlist(lapply(gregexpr("_", export_files), function( x ) x[ length( x )] + 1))
                                   , unlist(gregexpr(".csv", export_files)) - 1))

  export_raw <- lapply(export_files, function( x ) fread(x, sep = ";", dec = ","))

  export_raw <- mapply(function(x, y) cbind(x, matrix = as.numeric(y))
                       , x = export_raw
                       , y = as.list(export_ID)
                       , SIMPLIFY = F)

  export_raw <- rbindlist(export_raw, fill = T)

  fwrite(export_raw, paste0(substr( gsub(" ", "", gsub(":", "", gsub("-", "", Sys.time()))), 3, 14), "_analysis_merge.csv"), row.names = F, sep = ";", dec = ",")
  file.remove(export_files)

  gc()

  return(export_raw)

}
