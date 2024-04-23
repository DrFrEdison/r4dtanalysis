pred_prod_lin_vas <- function( trs_list
                               , names = c("prod", "lin", "vas")
                               , modell_csv_transfered = dt$mop$model.choose
                               , substance = dt$para$substance[ dt$para$i ]
                               , wl1 = dt$mop$wl1
                               , wl2 = dt$mop$wl2
                               , wl3 = dt$mop$wl3
                               , wl4 = dt$mop$wl4
                               , ncomp = dt$mop$ncomp
                               , derivative = dt$mop$derivative
                               , T2 = F){
  
  names( trs_list ) <- names
  prod = NA
  lin = NA
  vas = NA
  
  if( length( trs_list$prod) > 0) prod <- lapply( trs_list$prod, function(x) pred_of_new_model(modell_csv_transfered
                                                                                               , substance
                                                                                               , wl1
                                                                                               , wl2
                                                                                               , wl3, wl4
                                                                                               , ncomp
                                                                                               , derivative
                                                                                               , x))
  
  if( length( trs_list$lin) > 0) lin <- pred_of_new_model(modell_csv_transfered
                                                          , substance
                                                          , wl1
                                                          , wl2
                                                          , wl3, wl4
                                                          , ncomp
                                                          , derivative
                                                          , trs_list$lin)
  
  if( length( trs_list$vas) > 0) vas <- pred_of_new_model(modell_csv_transfered
                                                          , substance
                                                          , wl1
                                                          , wl2
                                                          , wl3, wl4
                                                          , ncomp
                                                          , derivative
                                                          , trs_list$vas)
  
  returnlist <- list(prod, lin, vas)
  names( returnlist ) <- names
  return( returnlist )
}
