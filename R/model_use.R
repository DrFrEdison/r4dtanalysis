model_use <- function(customer
                      , beverage
                      , substance
                      , pl = "00300" # Standard path length
                      , LG = 3
                      , model_version = NA
                      , csv_transfered
                      , return_type = c("model_output", "prediction", "ydeviation", "pred+ydev", "keepout", "overview", "beverages")){

  require(readODS)

  # save path and library
  originalwd <- getwd()
  setwd( wd$model[[ 1 ]] )


  model_overview <-  openxlsx::read.xlsx( xlsxFile = grep("xlsx", dir(pattern = "Q-xx-UEB-00001"), value = T)[ length( grep("xlsx", dir(pattern = "Q-xx-UEB-00001"), value = T) )]
                                          , sheet = "Modellliste"
                                          , startRow = 1
                                          , na.strings = "")

  # return beverages  ####
  if( return_type == "beverage"){
    model_overview <- model_overview[ which(model_overview$customer %in% customer ),]
    return( unique(model_overview$beverage) )
  }

  # return overview  ####
  if( return_type == "overview"){
    model_overview <- model_overview[ which(model_overview$customer %in% customer & model_overview$beverage %in% beverage),]
    model_overview <- model_overview[ , 1 : grep( "Slope", names( model_overview ))]
    return( model_overview )
  }

  # LG Spelling mistakes
  if( LG == "LG3") LG <- "LG3"
  if( LG == "LG2") LG <- "LG2"
  if( LG == "2") LG <- "LG2"
  if( LG == "3") LG <- "LG3"

  # Coffein spelling
  if(substance == "Coffein") substance <- "offein"
  if(substance == "Koffein") substance <- "offein"
  if(substance == "Caffeine") substance <- "offein"

  # Search for model by customer, beverage, pl and LG
  model_overview <- model_overview[ which(model_overview$customer %in% customer
                                          & model_overview$beverage %in% beverage
                                          & model_overview$pl %in% pl
                                          & model_overview$LG %in% LG), ]
  model_overview <- model_overview[ grep(substance, model_overview$substance), ]

  # Search for model by model_version
  if( is.na(model_version)) model_overview <- model_overview[ length( model_overview$version ) , ]
  if( !is.na(model_version) & nrow(model_overview) > 1){
    model_overview <- model_overview[ model_overview$version == model_version , ]
    if(model_overview$version != model_version) stop()
  }

  # modify model_overview
  model_overview$ncomp <- as.numeric(model_overview$ncomp)
  model_overview$derivative <- as.character(model_overview$derivative)

  if( !is.na(model_overview$p)) warning(paste0("Polynomial order is not constant but ", model_overview$p, ", check csv_transfered parameter!"), immediate. = T)
  if( !is.na(model_overview$n1)) warning(paste0("Window size for first derivation not constant but ", model_overview$n1, ", check csv_transfered parameter!"), immediate. = T)
  if( !is.na(model_overview$n2)) warning(paste0("Window size for second derivation not constant but ", model_overview$n2, ", check csv_transfered parameter!"), immediate. = T)

  # Serach for model file ####
  setwd( wd$model[[ 1 ]] )
  setwd( paste0("./", model_overview$customer))

  model_file <- grep( substance, dir(), value = T)
  model_file <- grep( paste0("_", LG), model_file, value = T)
  model_file <- grep( beverage, model_file, value=T)
  model_file <- grep( pl, model_file, value=T)
  model_file <- grep( beverage, model_file, value=T)

  if(substance == "offein") model_file <- grep(paste0("_C", substance), model_file, value = T)
  if(substance != "offein") model_file <- grep(paste0("_", substance), model_file, value = T)

  if( is.na(model_version)){
    model_versionlist <-   substr( model_file
                                   , lapply( model_file, function( x ) unlist( gregexpr( "_V", x)[ length( gregexpr( "_V", x) )]) + 1)
                                   , lapply( model_file, function( x ) unlist( gregexpr( "_V", x)[ length( gregexpr( "_V", x) )]) + 6))
    model_versionlist <- sort(model_versionlist)[ length( model_versionlist)]} else{ model_versionlist <- model_version }

  model_file <- grep(   paste0("_", model_versionlist), model_file, value=T)
  model_file <- model_file[1]

  # read model, find substance column and setDF ####
  model_read <- fread(model_file,dec=",")
  model_y_col <- grep(substance, names(model_read))
  if( length( model_y_col ) < 1 ) stop("substance column in model file not found")
  setDF(model_read)
  model_read <- model_read[!is.na(model_read[ , model_y_col]), ]
  names(model_read)[model_y_col] <- substance

  # subset rows (old relict subsetting rows, not according to SOP-202)
  if(!is.na(model_overview$subset)) model_read <- model_read[eval(parse(text=paste0("c(",gsub("-",":",as.character(model_overview$subset)),")"))),]

  # trs model file
  model_read <- transfer_csv(csv.file = model_read,
                             p = ifelse(is.na(model_overview$p) | model_overview$p == 0, 2, model_overview$p),
                             n1 = ifelse(is.na(model_overview$n1) | model_overview$n1 == 0, 7, model_overview$n1),
                             n2 = ifelse(is.na(model_overview$n2) | model_overview$n2 == 0, 11, model_overview$n2))

  # return keepout ####
  if( return_type == "keepout"){
    model_read.keep.out <- keep.out.unsb(model = model_read, wl1 = model_overview$wl1, wl2= model_overview$wl2, wl3 = model_overview$wl3, wl4 = model_overview$wl4, wl5 = model_overview$wl5, wl6 = model_overview$wl6)
    model_read.para <- data.frame(customer = model_overview$customer, beverage = model_overview$beverage, substance = model_overview$substance, pl = model_overview$pl, ncomp = model_overview$ncomp, spc = model_overview$derivative
                                  , wl1 = model_overview$wl1, wl2= model_overview$wl2, wl3 = model_overview$wl3, wl4 = model_overview$wl4, wl5 = model_overview$wl5, wl6 = model_overview$wl6)

    return(list(model_read.keep.out, model_read.para))
  }


  # wavelength
  wlr <- data.frame(wl1 = model_overview$wl1, wl2 = model_overview$wl2,
                    wl3 = model_overview$wl3, wl4 = model_overview$wl4,
                    wl5 = model_overview$wl5, wl6 = model_overview$wl6)


  # pls ####
  pls_model <- pls_model(csv_transfered = model_read, substance = substance, wlr = wlr, ncomp = model_overview$ncomp, spc = model_overview$derivative)

  if(return_type == "model_output"){

    library(pls)
    pls_lm <- pls_lm(pls_model = pls_model, csv_transfered = model_read, wlr = wlr, ncomp = model_overview$ncomp)
    pls_lm <- pls_lm[ nrow(pls_lm) , ]

    model_output <- list()
    model_output$model <- pls_model$pls[[1]]
    model_output$csv <- model_read
    model_output$name <- model_file
    model_output$para <- model_overview
    model_output$lm <- pls_lm
    if(is.na(model_output$para$wl3)) model_output$wl <- c(model_output$para$wl1 : model_output$para$wl2)
    if(!is.na(model_output$para$wl3) & is.na(model_output$para$wl5)) model_output$wl <- c(model_output$para$wl1 : model_output$para$wl2, model_output$para$wl3 : model_output$para$wl4)
    if(!is.na(model_output$para$wl5)) model_output$wl <- c(model_output$para$wl1 : model_output$para$wl2, model_output$para$wl3 : model_output$para$wl4, model_output$para$wl5 : model_output$para$wl6)

    return(model_output)}

  if(return_type == "prediction"){
    suppressMessages(prediction <- pls_prediction(csv_transfered = csv_transfered, pls_model = pls_model, ncomp = model_overview$ncomp))

    prediction_final <- pls_prediction_clean(prediction
                                             , model_overview$wl1, model_overview$wl2, model_overview$wl3, model_overview$wl4, model_overview$wl5, model_overview$wl6
                                             , model_overview$ncomp, model_overview$derivative)

    message(paste0("Customer = ", model_overview$customer
                   , ", Beverage = ", model_overview$beverage
                   , ", substance = ", model_overview$substance
                   , ", wavelength = ", model_overview$wl1, "|", model_overview$wl2
                   , if( !is.na( model_overview$wl3 )){ paste0(" & ", model_overview$wl3, "|", model_overview$wl4)}
                   , if( !is.na( model_overview$wl5 )){ paste0(" & ", model_overview$wl5, "|", model_overview$wl6)}
                   , ", PC = ", model_overview$ncomp
                   , ", derivative = ", model_overview$derivative))
    if(return_type == "prediction") return(prediction_final)
  }

  if(return_type == "ydeviation"){
    ydev <- ydeviation(MODEL = pls_model$pls[[ 1 ]]
                       , derivative = model_overview$derivative
                       , PREDSET = csv_transfered
                       , REFSET = model_read)

    return(ydev$ydeviation)}

  if(return_type == "pred+ydev"){
    ydev <- ydeviation(MODEL = pls_model$pls[[ 1 ]]
                       , derivative = model_overview$derivative
                       , PREDSET = csv_transfered
                       , REFSET = model_read)

    return(ydev$ydeviation)}

  setwd(originalwd)


}
#
# debug(model_use)
#
# model_use(customer = "CCEP"
#           , beverage = "Monster_Energy_Ultra"
#           , substance = "Sorbicacid"
#           , LG = "LG2"
#           , pl = "00200"
#           , csv_transfered = dt$spc$trs$spc$Karlsruhe_BCANKD
#           , return_type = "prediction" )
#
# undebug(model_use)
