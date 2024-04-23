pls_lm_lin <- function(pls_model){

  # Create Empty List ####
  pls_lm <- list()

  # Wavelength ####
  wlr <- data.frame(wl1 = substr(names(pls_model), 1, 3)
                    , wl2 = substr(names(pls_model), 5, 7)
                    , wl3 = substr(names(pls_model), 9, 11)
                    , wl4 = substr(names(pls_model), 13, 15))

  # check for non numeric and make them NA
  wlr <- apply(wlr, 2, function(x) as.numeric(gsub('\\D', '', x)))
  wlr[ , 3][which(nchar(wlr[,3]) < 2 | is.na(nchar(wlr[,3])))] <- NA
  wlr[ , 4][which(nchar(wlr[,4]) < 2 | is.na(nchar(wlr[,4])))] <- NA

  # read ncomp from names
  ncomp <- as.numeric(gsub('\\D', "", substr(names(pls_model), nchar(names(pls_model)) - 1, nchar(names(pls_model)))))

  # read spc from names
  spc <- gsub("_", "",  substr(names(pls_model)
                               , unlist(lapply(gregexpr("_", names(pls_model)), function(x) x[length(x) - 1]))
                               , unlist(lapply(gregexpr("_", names(pls_model)), max))))

  # calculate summary data
  pls_lm$lm <- mapply(function(x, y) summary(lm(x$fitted.values[,,y] ~ x$model$`pls$x`))
                      , x = pls_model
                      , y = as.list(ncomp)
                      , SIMPLIFY = F)

  pls_lm$RMSE <- unlist(mapply(function(x, y) RMSEP(x,estimate="train")$val[,,y+1]
                               , x = pls_model
                               , y = ncomp))

  pls_lm$slope <- as.numeric(unlist(lapply(pls_lm$lm, function(x)  coef(x)[2])))

  pls_lm$R2 <- as.numeric(unlist(lapply(pls_lm$lm, function(x)  x$r.squared)))

  # merge in data frame ####
  pls_lm_summary <-   data.frame(wlr
                                 , spc = spc
                                 , ncomp = ncomp
                                 , Slope = pls_lm$slope
                                 , RMSE = pls_lm$RMSE
                                 , R2 = unlist(pls_lm$R2))
  pls_lm_summary <- data.frame(pls_lm_summary)

  rownames(pls_lm_summary) <- 1:nrow(pls_lm_summary)
  pls_lm_summary$Slope <- round(pls_lm_summary$Slope,3)
  pls_lm_summary$R2 <- round(pls_lm_summary$R2,3)
  pls_lm_summary$RMSE <- round(pls_lm_summary$RMSE,2)

  return(pls_lm_summary)
}
