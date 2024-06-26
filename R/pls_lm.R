pls_lm <- function(pls_model, csv_transfered, substance, wlr, ncomp){

  pls_lm <- list()
  pls_lm$spc <- list()
  pls_lm$spc1st <- list()
  pls_lm$spc2nd <- list()
  pls_lm$plsRMSE <- list()
  pls_lm$pls_1stRMSE <- list()
  pls_lm$pls_2ndRMSE <- list()
  pls_lm$pls_slope <- list()
  pls_lm$pls_1stslope <- list()
  pls_lm$pls_2ndslope <- list()
  pls_lm$plsR2 <- list()
  pls_lm$pls_1stR2 <- list()
  pls_lm$pls_2ndR2 <- list()
  pls_lm$ncomp <- list()
  pls_lm$ncomp_1st <- list()
  pls_lm$ncomp_2nd <- list()

  pls_lm_summary <- data.frame()

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

  wlr$wl5[which(wlr$wl5 == "")] <- NA
  wlr$wl6[which(wlr$wl6 == "")] <- NA

for(j in 1:ncomp){
    if("spc" %in% names(pls_model)){
      pls_lm$spc[[j]] <- lapply(pls_model$spc, function(x) lm(x$fitted.values[,,j] ~ x$model$`pls$x`))
      pls_lm$plsRMSE[[j]] <- lapply(pls_model$spc, function(x) pls::RMSEP(x,estimate="train")$val[,,j+1])
   #   pls_lm$pls_slope[[j]] <- lapply(pls_lm$spc[[j]], function(x) coef(x)[2])
      pls_lm$plsR2[[j]] <- lapply(pls_lm$spc[[j]], function(x) summary(x)$r.squared)
      pls_lm$ncomp[[j]] <- rep(j, length(pls_lm$spc[[j]]))
    }

    if("spc1st" %in% names(pls_model)){
      pls_lm$spc1st[[j]] <- lapply(pls_model$spc1st, function(x) lm(x$fitted.values[,,j] ~ x$model$`pls$x`))
      pls_lm$pls_1stRMSE[[j]] <- lapply(pls_model$spc1st, function(x) pls::RMSEP(x,estimate="train")$val[,,j+1])
   #   pls_lm$pls_1stslope[[j]] <- lapply(pls_lm$spc1st[[j]], function(x) coef(x)[2])
      pls_lm$pls_1stR2[[j]] <- lapply(pls_lm$spc1st[[j]], function(x) summary(x)$r.squared)
      pls_lm$ncomp_1st[[j]] <- rep(j, length(pls_lm$spc1st[[j]]))
    }

    if("spc2nd" %in% names(pls_model)){
      pls_lm$spc2nd[[j]] <- lapply(pls_model$spc2nd, function(x) lm(x$fitted.values[,,j] ~ x$model$`pls$x`))
      pls_lm$pls_2ndRMSE[[j]] <- lapply(pls_model$spc2nd, function(x) pls::RMSEP(x,estimate="train")$val[,,j+1])
   #   pls_lm$pls_2ndslope[[j]] <- lapply(pls_lm$spc2nd[[j]], function(x) coef(x)[2])
      pls_lm$pls_2ndR2[[j]] <- lapply(pls_lm$spc2nd[[j]], function(x) summary(x)$r.squared)
      pls_lm$ncomp_2nd[[j]] <- rep(j, length(pls_lm$spc2nd[[j]]))
    }
  }

  if("spc" %in% names(pls_model))
    pls_lm_summary <- cbind(ncomp = unlist( pls_lm$ncomp )
                               , wlr
                               , spc = "spc"
                  #             , Slope = unlist( pls_lm$pls_slope )
                               , RMSE = unlist( pls_lm$plsRMSE )
                               , R2 = unlist( pls_lm$plsR2 ))

  if("spc1st" %in% names(pls_model))
    pls_lm_summary1st <- cbind(ncomp = unlist( pls_lm$ncomp_1st )
                               , wlr
                               , spc = "1st"
                   #            , Slope = unlist( pls_lm$pls_1stslope )
                               , RMSE = unlist( pls_lm$pls_1stRMSE )
                               , R2 = unlist( pls_lm$pls_1stR2 ))

  if("spc2nd" %in% names(pls_model))
    pls_lm_summary2nd <- cbind(ncomp = unlist( pls_lm$ncomp_2nd )
        , wlr
        , spc = "2nd"
    #    , Slope = unlist( pls_lm$pls_2ndslope )
        , RMSE = unlist( pls_lm$pls_2ndRMSE )
        , R2 = unlist( pls_lm$pls_2ndR2 ))

  pls_lm_summary <- rbind(if("spc" %in% names(pls_model)) pls_lm_summary
                          , if("spc1st" %in% names(pls_model)) pls_lm_summary1st
                          , if("spc2nd" %in% names(pls_model)) pls_lm_summary2nd)

  rownames(pls_lm_summary) <- 1:nrow(pls_lm_summary)
  # pls_lm_summary$slope <- round(pls_lm_summary$Slope,3)
  pls_lm_summary$R2 <- round(pls_lm_summary$R2,3)
  pls_lm_summary$RMSE <- round(pls_lm_summary$RMSE,3)

  return(pls_lm_summary)
}
