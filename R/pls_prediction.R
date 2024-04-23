pls_prediction <- function(csv_transfered, pls_model, ncomp, messaget = F) {
  options(warn = -1)

  csv_transfered$spc <- as.data.frame(csv_transfered$spc)
  csv_transfered$spc1st <- as.data.frame(csv_transfered$spc1st)
  csv_transfered$spc2nd <- as.data.frame(csv_transfered$spc2nd)

  if("spc" %in% names(pls_model)) predict_plsmodel_df <- data.frame()
  if("spc1st" %in% names(pls_model)) predict_plsmodel_1st_df <- data.frame()
  if("spc2nd" %in% names(pls_model)) predict_plsmodel_2nd_df <- data.frame()

  if("spc" %in% names(pls_model)) predict_plsmodel <- list()
  if("spc1st" %in% names(pls_model)) predict_plsmodel_1st <- list()
  if("spc2nd" %in% names(pls_model)) predict_plsmodel_2nd <- list()

  if("spc" %in% names(pls_model)) if(is.null(colnames(csv_transfered$spc))) csv_transfered$spc <- t(csv_transfered$spc)
  if("spc1st" %in% names(pls_model)) if(is.null(colnames(csv_transfered$spc))) csv_transfered$spc1st <- t(csv_transfered$spc1st)
  if("spc2nd" %in% names(pls_model)) if(is.null(colnames(csv_transfered$spc))) csv_transfered$spc2nd <- t(csv_transfered$spc2nd)

  spcpos <- grep("spc", names(pls_model))
  spcpos1 <- spcpos[1]

  wlr <- data.frame(wl1 = substr(names(pls_model[[spcpos1]]), 1, 3)
                    , wl2 = substr(names(pls_model[[spcpos1]]), 5, 7)
                    , wl3 = substr(names(pls_model[[spcpos1]]), 9, 11)
                    , wl4 = substr(names(pls_model[[spcpos1]]), 13, 15)
                    , wl5 = substr(names(pls_model[[spcpos1]]), 17, 19)
                    , wl6 = substr(names(pls_model[[spcpos1]]), 21, 23))

  wlr$wl3[which(wlr$wl3 == "")] <- NA
  wlr$wl4[which(wlr$wl4 == "")] <- NA

  wlr$wl3[which(wlr$wl3 == "0_0")] <- NA
  wlr$wl4[which(wlr$wl4 == "0_0")] <- NA

  wlr$wl5[which(wlr$wl5 == "")] <- NA
  wlr$wl6[which(wlr$wl6 == "")] <- NA

  wlr$wl5[which(wlr$wl5 == "0_0")] <- NA
  wlr$wl6[which(wlr$wl6 == "0_0")] <- NA

  wlrl <- apply(wlr, 1, function(x) c(x[1]:x[2]
                                      , if(!is.na(x[3])) x[3]:x[4]
                                      , if(!is.na(x[5])) x[5]:x[6]))

  ifelse(nrow(wlr) > 1
         , wlrlp <- lapply(wlrl, function(x) which(csv_transfered$wl %in% x))
         , wlrlp <- which(csv_transfered$wl %in% wlrl))

    if(nrow(wlr) == 1) wlrlp <- list(wlrlp)

  predict_plsmodel_df <- list()
  predict_plsmodel_1st_df  <- list()
  predict_plsmodel_2nd_df  <- list()

  for(j in 1:ncomp){

    if("spc" %in% names(pls_model)){
      if(!is.data.table( csv_transfered$spc ))
      predict_plsmodel[[j]] <- mapply(function(x, z) predict(object = x
                                                             , newdata = as.matrix(csv_transfered$spc[ , z])
                                                             , ncomp = j
                                                             , se.fit = T, interval = "confidence", level = 0.95)
                                      , x = pls_model$spc
                                      , z = wlrlp)

      if(is.data.table( csv_transfered$spc ))
        predict_plsmodel[[j]] <- mapply(function(x, z) predict(object = x
                                                               , newdata = as.matrix(csv_transfered$spc[ , z, with = F])
                                                               , ncomp = j
                                                               , se.fit = T, interval = "confidence", level = 0.95)
                                        , x = pls_model$spc
                                        , z = wlrlp)

      predict_plsmodel_df[[j]] <- data.frame(wlr
                                             , ncomp = j
                                             , mean = unlist(apply(predict_plsmodel[[j]], 2, function(y) mean(y, na.rm = T)))
                                             , sd = unlist(apply(predict_plsmodel[[j]], 2, function(y) sd(y, na.rm = T)))
                                             , median = unlist(apply(predict_plsmodel[[j]], 2, function(y) median(y, na.rm = T)))
                                             , mad = unlist(apply(predict_plsmodel[[j]], 2, function(y) mad(y, na.rm = T)))
                                             , min = unlist(apply(predict_plsmodel[[j]], 2, function(y) min(y, na.rm = T)))
                                             , max = unlist(apply(predict_plsmodel[[j]], 2, function(y) max(y, na.rm = T))))
      if(messaget == T) message(paste0("Prediction of PC", j, " for spc finished"))
      }

    if("spc1st" %in% names(pls_model)){
      if(!is.data.table( csv_transfered$spc1st ))
        predict_plsmodel_1st[[j]] <- mapply(function(x, z) predict(object = x
                                                                 , newdata = as.matrix(csv_transfered$spc1st[ , z])
                                                                 , ncomp = j
                                                                 , se.fit = T, interval = "confidence", level = 0.95)
                                          , x = pls_model$spc1st
                                          , z = wlrlp)
      if(is.data.table( csv_transfered$spc1st ))
        predict_plsmodel_1st[[j]] <- mapply(function(x, z) predict(object = x
                                                                   , newdata = as.matrix(csv_transfered$spc1st[ , z, with = F])
                                                                   , ncomp = j
                                                                   , se.fit = T, interval = "confidence", level = 0.95)
                                            , x = pls_model$spc1st
                                            , z = wlrlp)

      predict_plsmodel_1st_df[[j]] <- data.frame(wlr
                                                 , ncomp = j
                                                 , mean = unlist(apply(predict_plsmodel_1st[[j]], 2, function(y) mean(y, na.rm = T)))
                                                 , sd = unlist(apply(predict_plsmodel_1st[[j]], 2, function(y) sd(y, na.rm = T)))
                                                 , median = unlist(apply(predict_plsmodel_1st[[j]], 2, function(y) median(y, na.rm = T)))
                                                 , mad = unlist(apply(predict_plsmodel_1st[[j]], 2, function(y) mad(y, na.rm = T)))
                                                 , min = unlist(apply(predict_plsmodel_1st[[j]], 2, function(y) min(y, na.rm = T)))
                                                 , max = unlist(apply(predict_plsmodel_1st[[j]], 2, function(y) max(y, na.rm = T))))
      if(messaget == T) message(paste0("Prediction of PC", j, " for 1st derivative finished"))
      }

    if("spc2nd" %in% names(pls_model)){
      if(!is.data.table( csv_transfered$spc2nd ))
        predict_plsmodel_2nd[[j]] <- mapply(function(x, z) predict(object = x
                                                                 , newdata = as.matrix(csv_transfered$spc2nd[ , z])
                                                                 , ncomp = j
                                                                 , se.fit = T, interval = "confidence", level = 0.95)
                                          , x = pls_model$spc2nd
                                          , z = wlrlp)

      if(is.data.table( csv_transfered$spc2nd ))
        predict_plsmodel_2nd[[j]] <- mapply(function(x, z) predict(object = x
                                                                   , newdata = as.matrix(csv_transfered$spc2nd[ , z, with = F])
                                                                   , ncomp = j
                                                                   , se.fit = T, interval = "confidence", level = 0.95)
                                            , x = pls_model$spc2nd
                                            , z = wlrlp)


      predict_plsmodel_2nd_df[[j]] <- data.frame(wlr
                                                 , ncomp = j
                                                 , mean = unlist(apply(predict_plsmodel_2nd[[j]], 2, function(y) mean(y, na.rm = T)))
                                                 , sd = unlist(apply(predict_plsmodel_2nd[[j]], 2, function(y) sd(y, na.rm = T)))
                                                 , median = unlist(apply(predict_plsmodel_2nd[[j]], 2, function(y) median(y, na.rm = T)))
                                                 , mad = unlist(apply(predict_plsmodel_2nd[[j]], 2, function(y) mad(y, na.rm = T)))
                                                 , min = unlist(apply(predict_plsmodel_2nd[[j]], 2, function(y) min(y, na.rm = T)))
                                                 , max = unlist(apply(predict_plsmodel_2nd[[j]], 2, function(y) max(y, na.rm = T))))
      if(messaget == T)  message(paste0("Prediction of PC", j, " for 2nd derivative finished"))
    }
    if(messaget == T)  if(j != ncomp) message(paste0("Prediction of PC", j, " finished, ", ncomp - j, " to go"))
    if(messaget == T) if(j == ncomp) message(paste0("Prediction finished"))
  }

  if("spc" %in% names(pls_model)) predict_plsmodel_df <- do.call(rbind, predict_plsmodel_df)
  if("spc1st" %in% names(pls_model)) predict_plsmodel_1st_df <- do.call(rbind, predict_plsmodel_1st_df)
  if("spc2nd" %in% names(pls_model)) predict_plsmodel_2nd_df <- do.call(rbind, predict_plsmodel_2nd_df)

  predict_plsmodel_df <- rbind(if("spc" %in% names(pls_model)) cbind(spc="spc",predict_plsmodel_df),
                               if("spc1st" %in% names(pls_model)) cbind(spc="1st",predict_plsmodel_1st_df),
                               if("spc2nd" %in% names(pls_model)) cbind(spc="2nd",predict_plsmodel_2nd_df)
  )
  predict_plsmodel_df <- predict_plsmodel_df[order(predict_plsmodel_df$mad), ]
  rownames(predict_plsmodel_df) <- 1:nrow(predict_plsmodel_df)

  prediction_all <- list(if("spc" %in% names(pls_model)) predict_plsmodel,
                         if("spc1st" %in% names(pls_model)) predict_plsmodel_1st,
                         if("spc2nd" %in% names(pls_model)) predict_plsmodel_2nd)

  if(!"spc2nd" %in% names(pls_model)) prediction_all[[3]] <- NULL
  if(!"spc1st" %in% names(pls_model)) prediction_all[[2]] <- NULL
  if(!"spc" %in% names(pls_model)) prediction_all[[1]] <- NULL

  names(prediction_all) <- names(pls_model)[spcpos]
  # prediction_all <- prediction_all[which(names(prediction_all) %in% names(pls_model))]

  prediction_return <- list(predict_plsmodel_df,prediction_all)
  names(prediction_return) <- c("predict_parameter","prediction")
  return(prediction_return)
}
