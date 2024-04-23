model_parameter_write <- function(customer
                                  , location = NA
                                  , line = NA
                                  , beverage
                                  , pl
                                  , LG
                                  , substance
                                  , wl1
                                  , wl2
                                  , wl3
                                  , wl4
                                  , wl5
                                  , wl6
                                  , ncomp
                                  , spc
                                  , unsb
                                  , modelversion = "V01-00"
                                  , model){

  # write to model data
  setwd(wd$master)
  dt$model.overview <- read_ods("r4dt_model.ods")

  if( length( LG ) > 1) LG = paste(LG, collapse = "_")

  setwd("./Archiv")
  write_ods(x = dt$model.overview, path = paste0(date.dt(), "_", gsub("\\:", "", substr(Sys.time(), 12, 19)), "_dt_model_overview.ods"), overwrite = F)
  setwd(wd$master)

  if(length(which(dt$model.overview$beverage == beverage & dt$model.overview$substance == substance)) == 0){
    dt$model.overview <- rbind(dt$model.overview
                               , data.frame(customer = customer
                                            , location = location
                                            , line = line
                                            , beverage = beverage
                                            , unsb = paste0(unsb, "_", pl, "_", modelversion, ".unsb")
                                            , pl = pl
                                            , LG = LG
                                            , substance = substance
                                            , wl1 = wl1
                                            , wl2 = wl2
                                            , wl3 = wl3
                                            , wl4 = wl4
                                            , wl6 = wl6
                                            , wl6 = wl6
                                            , ncomp = ncomp
                                            , spc = spc
                                            , p = NA, n1 = NA, n2 = NA, seg = NA, Slope = NA, subset = NA
                                            , SOLLp = NA, sperr_maxp = NA, sperr_minp = NA
                                            , warn_maxp = NA, warn_minp = NA
                                            , unit = NA
                                            , SOLLa_unit = NA, SOLLa = NA
                                            , sperr_maxa = NA, sperr_mina = NA
                                            , warn_maxa = NA, warn_mina = NA)
    )
  }

  if(length(which(dt$model.overview$beverage == beverage & dt$model.overview$substance == substance)) > 0){

    rowp <- which(dt$model.overview$beverage == beverage &
                    dt$model.overview$substance == substance &
                    dt$model.overview$pl == pl)[ 1 ]
    dt$model.overview[ rowp , "wl1"] <- wl1
    dt$model.overview[ rowp , "wl2"] <- wl2
    dt$model.overview[ rowp , "wl3"] <- wl3
    dt$model.overview[ rowp , "wl4"] <- wl4
    dt$model.overview[ rowp , "wl5"] <- wl5
    dt$model.overview[ rowp , "wl6"] <- wl6
    dt$model.overview[ rowp , "ncomp"] <- ncomp
    dt$model.overview[ rowp , "spc"] <- spc
    dt$model.overview[ rowp , "unsb"] <- paste0(unsb, "_", pl, "_", modelversion, ".unsb")
    dt$model.overview[ rowp , "version"] <- modelversion
  }

  dt$model.overview <- dt$model.overview[ order(dt$model.overview$customer, dt$model.overview$beverage, dt$model.overview$substance),]
  write_ods(x = dt$model.overview, path = "dt_model_overview.ods", overwrite = T)

  setwd("./model")
  setwd(paste0("./", customer))

  fwrite(x = cbind(model$data, model$spc)
         , file = paste0( paste(beverage
                                , substance
                                , pl
                                , modelversion
                                , LG
                                , sep = "_"), ".csv")
         , sep = ";", dec = ",", na = NA)
}
