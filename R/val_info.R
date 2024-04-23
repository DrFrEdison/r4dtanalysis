validation_info <- function(customer = NA){

  if( customer == "CCEP"){

    infoexport <- c(dt$para$location
                    , dt$para$line
                    , dt$para$samplingtime
                    , dt$para$timerange
                    , dt$para$beverage
                    , dt$para$beverage
                    , dt$para$substance[ dt$par$i ]
                    , dt$labfile
                    , format( Sys.Date(), "%d.%m.%Y")
                    , basename( dt$xlsx$file )
                    , dt$para$model.name
                    , "Ja"
                    , dir(this.path::this.dir(), pattern = "RSV")[ length( dir(this.path::this.dir(), pattern = "RSV") ) ]
                    , "MK"
                    , "MS")

  }

  if( customer != "CCEP"){

    infoexport <- c(as.character( dt$para$location[ j ] )
                    , as.character( dt$para$line[ j ] )
                    , dt$para$beverage
                    , dt$para$beverage
                    , dt$para$substance[ dt$par$i ]
                    , dt$para$unit[ dt$par$i ]
                    , dt$labfile
                    , format( Sys.Date(), "%d.%m.%Y")
                    , basename( dt$xlsx$file )
                    , dt$para$model.name
                    , "Ja")

  }

  return(infoexport)
}
