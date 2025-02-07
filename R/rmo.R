rmo.prediction <- function( model.ID = dt$mop$model_choose
                            , model.filter = dt$mop$merge
                            , model.list = dt$rmo$var
                            , substance = dt$choose$substance
                            , prod.dat
                            , lin.dat
                            , SOLL = 100){

  model.filter <- model.filter[ model.filter$ID == model.ID , ]

  library(pls)
  pred.prod <- pls_prediction_new_model(modell_csv_transfered = model.list[[ model.filter$matrix ]]
                                        , substance = substance
                                        , wl1 = model.filter$wl1
                                        , wl2 = model.filter$wl2
                                        , wl3 = model.filter$wl3
                                        , wl4 = model.filter$wl4
                                        , wl5 = model.filter$wl5
                                        , wl6 = model.filter$wl6
                                        , ncomp = model.filter$ncomp
                                        , derivative = model.filter$spc
                                        , csv_transfered = prod.dat)

  pred.lin <- pls_prediction_new_model(modell_csv_transfered = model.list[[ model.filter$matrix ]]
                                       , substance = substance
                                       , wl1 = model.filter$wl1
                                       , wl2 = model.filter$wl2
                                       , wl3 = model.filter$wl3
                                       , wl4 = model.filter$wl4
                                       , wl5 = model.filter$wl5
                                       , wl6 = model.filter$wl6
                                       , ncomp = model.filter$ncomp
                                       , derivative = model.filter$spc
                                       , csv_transfered = lin.dat)

  bias.prod <-  bias( median( pred.prod, na.rm = T), 0, SOLL, LG = 3)
  bias.lin <- bias( median( pred.lin, na.rm = T), 0, median( lin.dat ), LG = 3)

  returnlist <- list( pred.prod, pred.lin, bias.prod, bias.lin)
  names( returnlist ) <- c("pred.prod", "pred.lin", "bias.prod", "bias.lin")
  return( returnlist )
}

getraenk_angelegt <- function(beverage){

  originalwd <- getwd()
  setwd( wd$model[[ 1 ]] )

  model_overview <-  openxlsx::read.xlsx( xlsxFile = dir(pattern = "Q-xx-UEB-00001")[ length( dir(pattern = "Q-xx-UEB-00001")) ]
                                          , sheet = "Modellliste"
                                          , startRow = 1
                                          , na.strings = "")

  model_overview <- model_overview[ model_overview$beverage == beverage , ]
  model_overview <- model_overview[ , !names( model_overview ) %in% c("location", "line", "p", "n1", "n2", "seg", "Slope", "subset")]

  return(model_overview[ model_overview$beverage == beverage , ])

  setwd( originalwd )

}



soll_warn_sperr <- function( q_xx_ueb = dt$para$q_xx_ueb
                             , unit = q_xx_ueb$unit){

  SOLL <- list()
  WARN <- list()
  SPERR <- list()

  for(i in 1 : length( unit)){

    SOLL[[ i ]] <-  ifelse( unit[ i ] == "%", q_xx_ueb$SOLLp[ i ], q_xx_ueb$SOLLa[ i ])


    WARN[[ i ]] <-  if( unit[ i ] == "%"){
      q_xx_ueb[ i , c("warn_minp", "warn_maxp")]
    } else{
      q_xx_ueb[ i , c("warn_mina", "warn_maxa")]
    }

    SPERR[[ i ]] <-  if( unit[ i ] == "%"){
      q_xx_ueb[ i , c("sperr_minp", "sperr_maxp")]
    } else{
      q_xx_ueb[ i , c("sperr_mina", "sperr_maxa")]
    }


  }

  dat <- data.frame( substance = q_xx_ueb$substance
                     , SOLL = unlist(SOLL)
                     , WARN_min = unlist(lapply(WARN, function(x) x[[ 1 ]]))
                     , WARN_max = unlist(lapply(WARN, function(x) x[[ 2 ]]))
                     , SPERR_min = unlist(lapply(SPERR, function(x) x[[ 1 ]]))
                     , SPERR_max = unlist(lapply(SPERR, function(x) x[[ 2 ]])))

  return( dat )
}

absolut_zu_prozent <- function( substance, SOLL, model = dt$ausmischung$raw){

  model <- data.frame(model)
  model[ , substance] <- model[ , substance] / SOLL * 100
  model <- data.table(model)
  return(model)
}

prozent_zu_absolut <- function( substance, SOLL, model = dt$ausmischung$raw){

  model <- data.frame(model)
  model[ , substance] <- model[ , substance] * SOLL / 100
  model <- data.table(model)
  return(model)
}

pathlength_find <- function( pfad ){

  pl.list <- c("50000", "10000", "05000", "02000", "01000", "00500", "00300", "00200", "00100", "00075")
  pl.grep <- lapply( pl.list, function( x ) gregexpr( x, pfad))
  pl.grep <- unlist( lapply(pl.grep, function( x ) x[[ 1 ]]))

  return( pl.list[ pl.grep > 0 ] )
}

pathlength_numeric <- function( pl )  return( as.numeric( paste0(substr(pl, 1,2), ".", substr(pl, 3,5)) ) )

wlr_function_collect <- function(wl = 210 : 350, wlr1 = 10, wlr2 = 20, wlr3 = 20){

  library(plyr)
  wl1 <- wlr_function(wl, wl, wlr1)
  wl2 <- wlr_function2(wl, wl, wlr2)
  wl3 <- wlr_function3(wl, wl, wlr3)
  return( rbind.fill(wl1, wl2, wl3) )
}




getraenk_angelegt <- function(beverage){

  originalwd <- getwd()
  setwd( wd$model[[ 1 ]] )

  model_overview <-  openxlsx::read.xlsx( xlsxFile = dir(pattern = "Q-xx-UEB-00001")[ length( dir(pattern = "Q-xx-UEB-00001")) ]
                                          , sheet = "Modellliste"
                                          , startRow = 1
                                          , na.strings = "")

  model_overview <- model_overview[ model_overview$beverage == beverage , ]
  model_overview <- model_overview[ , !names( model_overview ) %in% c("location", "line", "p", "n1", "n2", "seg", "Slope", "subset")]

  return(model_overview[ model_overview$beverage == beverage , ])

  setwd( originalwd )

}



soll_warn_sperr <- function( q_xx_ueb = dt$para$q_xx_ueb
                             , unit = q_xx_ueb$unit){

  SOLL <- list()
  WARN <- list()
  SPERR <- list()

  for(i in 1 : length( unit)){

    SOLL[[ i ]] <-  ifelse( unit[ i ] == "%", q_xx_ueb$SOLLp[ i ], q_xx_ueb$SOLLa[ i ])


    WARN[[ i ]] <-  if( unit[ i ] == "%"){
      q_xx_ueb[ i , c("warn_minp", "warn_maxp")]
    } else{
      q_xx_ueb[ i , c("warn_mina", "warn_maxa")]
    }

    SPERR[[ i ]] <-  if( unit[ i ] == "%"){
      q_xx_ueb[ i , c("sperr_minp", "sperr_maxp")]
    } else{
      q_xx_ueb[ i , c("sperr_mina", "sperr_maxa")]
    }


  }

  dat <- data.frame( substance = q_xx_ueb$substance
                     , SOLL = unlist(SOLL)
                     , WARN_min = unlist(lapply(WARN, function(x) x[[ 1 ]]))
                     , WARN_max = unlist(lapply(WARN, function(x) x[[ 2 ]]))
                     , SPERR_min = unlist(lapply(SPERR, function(x) x[[ 1 ]]))
                     , SPERR_max = unlist(lapply(SPERR, function(x) x[[ 2 ]])))

  return( dat )
}

absolut_zu_prozent <- function( substance, SOLL, model = dt$ausmischung$raw){
  model[ , (substance) := get(substance) / SOLL * 100]

  return(model)
}

prozent_zu_absolut <- function( substance, SOLL, model = dt$ausmischung$raw){
  model[ , (substance) := get(substance) * SOLL / 100]

  return(model)
}

pathlength_find <- function( pfad ){

  pl.list <- c("50000", "10000", "05000", "02000", "01000", "00500", "00300", "00200", "00100", "00075")
  pl.grep <- lapply( pl.list, function( x ) gregexpr( x, pfad))
  pl.grep <- unlist( lapply(pl.grep, function( x ) x[[ 1 ]]))

  return( pl.list[ pl.grep > 0 ] )
}

pathlength_numeric <- function( pl )  return( as.numeric( paste0(substr(pl, 1,2), ".", substr(pl, 3,5)) ) )

wlr_function_collect <- function(wl = 210 : 350, wlr1 = 10, wlr2 = 20, wlr3 = 20){

  library(plyr)
  wl1 <- wlr_function(wl, wl, wlr1)
  wl2 <- wlr_function2(wl, wl, wlr2)
  wl3 <- wlr_function3(wl, wl, wlr3)
  return( rbind.fill(wl1, wl2, wl3) )
}

source_scan <- function(file, start = 1, pattern_end = "wlr_function_collect"){

  file.lines <- scan(file, what = character(), sep = "\n")
  file.lines <- file.lines[ start : grep( pattern_end, file.lines) - 1 ]

  source(textConnection(file.lines))

}



