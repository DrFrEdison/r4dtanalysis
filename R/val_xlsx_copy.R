validation_xlsx_copy <- function( type = c("FO", "CCEP", "Vorlage")
                                   , V = "01-00"
                                   , subfolder
                                   , pattern){

  root.wd <- getwd()

  if( type == "FO"){

    print( "FO-223" )
    filenamep <- paste0(r4dt::date(), "_Validierung_", dt$para$customer, "_", dt$para$beverage, "_", dt$para$substance[ dt$para$i ], "_", dt$para$model$raw.pl
                        , "_V", V, ".xltx")
    filenamep <- gsub( "xltx", "xlsx", filenamep)
    print( root.wd )
    # file.copy(sourcep <- paste0("D:/OneDrive - Dausch Technologies GmbH/Dokumentation/QM/06_FO_Formblaetter/"
    #                  , dir(path = "D:/OneDrive - Dausch Technologies GmbH/Dokumentation/QM/06_FO_Formblaetter/", pattern = "FO-223"))
    #           , filenamep <- paste0(r4dt::date(), "_Validierung_", dt$para$customer, "_", dt$para$beverage, "_", dt$para$substance[ dt$para$i ], "_", dt$para$model$raw.pl
    #                                 , "_V", V, ".xltx")
    #           , overwrite = T)

  }

  if( type == "CCEP"){

    file.copy(sourcep <- "D://OneDrive - Dausch Technologies GmbH/FE_Methoden/BEV_CCEP/Mastermodelle/KA-TSW-QS-FB41-DT Liquiguard Auswertung Tabelle.xlsx"
              , filenamep <- paste0(r4dt::date(), "_Validierung_", dt$para$customer, "_", dt$para$beverage, "_", dt$para$substance[ dt$para$i ], "_", dt$para$model$raw.pl
                                    , "_V", V, ".xlsx")
              , overwrite = T)

  }

  if( type == "Vorlage"){

    setwd("..")
    setwd( paste0( "./", subfolder))

    file.copy( sourcep <- dir( pattern = pattern )
               , filenamep <- paste0(root.wd, "/", r4dt::date(), "_Validierung_", dt$para$customer, "_", dt$para$beverage, "_", dt$para$substance[ dt$para$i ], "_", dt$para$model$raw.pl
                                     , "_V", V, ".xlsx")
               , overwrite = T)

			         }

  setwd(root.wd)

  if( type != "FO")   message( sourcep, "\ncopied to\n", paste0(root.wd, "/", filenamep))

  return(filenamep)
}
