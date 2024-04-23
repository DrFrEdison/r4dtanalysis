use_modelmda_on_device <- function(customer, beverage, LG, parameter, csv_transfered
                                   , alpha = 0.05, return_type = "prediction"){

  originalwd <- getwd()
  require(readODS)
  setwd(unlist(wd$master)[1])
  model_overview <-  read_ods("r4dt_model.ods")

  if(parameter == "Coffein") parameter <- "offein"
  if(parameter == "Koffein") parameter <- "offein"

  rowp <- which(model_overview$customer %in% customer & model_overview$beverage %in% beverage & model_overview$LG %in% LG)
  rowp <- rowp[ rowp %in% grep(parameter, model_overview$substance) ]

  model_overviewp <- model_overview[rowp,]

  if(nrow(model_overviewp)) model_overviewp <- model_overviewp[1,]

  setwd(unlist(wd$model[[ 1 ]])[1])
  setwd(paste0(setwd(unlist(wd$model[[ 1 ]])[1]),"/",model_overviewp$customer))

  txtlist <- grep(parameter,dir(),value = T)
  if(LG != "SG"){ txtlist <- grep(paste0("LG",LG),txtlist,value=T) } else{ grep(paste0(LG),txtlist,value=T) }
  txtlist <- grep(beverage,txtlist,value=T)
  txtlist <- grep(customer,txtlist,value=T)

  txtlist <- txtlist[1]

  dat <- fread(txtlist,dec=",")

  # if(length(grep("offein",parameter))>0){
  #   if(length(grep(parameter,names(dat)))==0) colnump <- names(dat)[grep(gsub("C","K",parameter),names(dat))]  else colnump <- names(dat)[grep(parameter,names(dat))]} else colnump <- names(dat)[grep(parameter,names(dat))]


  colnumpp <- grep(parameter, names(dat))
  setDF(dat)

  if(!is.na(model_overviewp$subset)) dat <- dat[eval(parse(text=paste0("c(",gsub("-",":",as.character(model_overviewp$subset)),")"))),]

  dat <- dat[!is.na(dat[ , colnumpp]), ]
  names(dat)[colnumpp] <- parameter
  names(dat)[1] <- "ID"

  dat <- transfer_csv(csv.file = dat,
                      p = ifelse(is.na(model_overviewp$p) | model_overviewp$p == 0, 2, model_overviewp$p),
                      n1 = ifelse(is.na(model_overviewp$n1) | model_overviewp$n1 == 0, 7, model_overviewp$n1),
                      n2 = ifelse(is.na(model_overviewp$n2) | model_overviewp$n2 == 0, 11, model_overviewp$n2))
  matrixtochoose <- NA
  whichnot <- NA
  substance <- parameter
  wlr <- data.frame(wl1=model_overviewp$wl1, wl2=model_overviewp$wl2)
  if(!is.na(model_overviewp$wl3))   wlr <- data.frame(wl1=model_overviewp$wl1,wl2=model_overviewp$wl2,wl3=model_overviewp$wl3,wl42=model_overviewp$wl4)

  ncomp <- model_overviewp$ncomp
  derivative <- as.character(model_overviewp$spc)
  namecolumn <- "ID"

  pls_function_obj <- pls_function.mda(csv_transfered = dat, substance = substance, wlr = wlr,ncomp = ncomp, spc = derivative, alpha = alpha)

  # pls_lm <- plsmda_lm_function(pls_function_obj = pls_function_obj, csv_transfered = dat, wlr = wlr, ncomp = ncomp)

  setwd(setwd(unlist(wd$model)[1]))
  setwd(originalwd)

  if(pls_function_obj[[5]][[1]]$ncomp != ncomp) ncomp <- pls_function_obj[[5]][[1]]$ncomp

  suppressMessages(prediction <- produktion_prediction_mda(csv_transfered = csv_transfered, ncomp = ncomp,  pls_function_obj = pls_function_obj))

  if(return_type == "prediction") return( as.numeric(prediction$prediction[[grep(derivative, names(prediction$prediction))]][[ncomp]][[1]]$y.pred[ , ncomp,]) )

  if(return_type == "model"){
    setwd(originalwd)
    model <- list()
    model$model <- pls_function_obj[[grep(derivative, names(pls_function_obj))]][[1]]
    model$predpara <- prediction$prediction[[grep(derivative, names(prediction$prediction))]][[ncomp]][[1]]$xdecomp
    model$pred <- as.numeric(prediction$prediction[[grep(derivative, names(prediction$prediction))]][[ncomp]][[1]]$y.pred[ , ncomp, ])
    model$csv <- dat
    model$name <- txtlist
    model$para <- model_overviewp
    if(is.na(model$para$wl3)) model$wl <- c(model$para$wl1 : model$para$wl2)
    if(!is.na(model$para$wl3)) model$wl <- c(model$para$wl1 : model$para$wl2, model$para$wl3 : model$para$wl4)

    model$val$model <- data.frame(Q = model$model$Qlim[1,ncomp], T2 = model$model$T2lim[1,ncomp])
    model$val$val <- data.frame(Q = model$predpara$Q[,ncomp], T2 = model$predpara$T2[,ncomp])
    model$val$val$FRes <- sqrt( model$val$val$Q / length( model$wl ) )
    model$val$model$Fres <-  sqrt( model$val$model$Q / length( model$wl ) )

    message(paste("Parameter =", model_overviewp$substance
                  , "wl1 =", model_overviewp$wl1
                  , "wl2 =", model_overviewp$wl2
                  , "wl3 =", model_overviewp$wl3
                  , "wl4 =", model_overviewp$wl4
                  , "PC =", model_overviewp$ncomp
                  , "derivative =", model_overviewp$spc))
    return(model)}
}

