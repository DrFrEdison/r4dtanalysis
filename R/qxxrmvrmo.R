QxxRMORMV <- function( pattern = "Q-xx-RMO"
                       , Dummy = paste0( wd$fe[[ 1 ]], "Allgemein/Dummy_R_github")
                       , copyto = dt$wd.git){
  
  Qfiles <- list.files(wd$git, recursive = T, pattern = pattern)
  Qfiles <- basename( Qfiles )
  
  QID <- sort( substr( Qfiles
                       , unlist( lapply( gregexpr( pattern, Qfiles), function( x ) x[[ 1 ]] + 9) )
                       , unlist( lapply( gregexpr( pattern, Qfiles), function( x ) x[[ 1 ]] + 13))))
  
  suppressWarnings( QID <- QID[ !is.na( as.numeric( QID ))])
  QID <- QID[ length( QID )]
  QID <- as.numeric( QID )
  
  QID.new <- formatC(QID + (1 : length( dt$para$substance )), width = 5, flag = "0")
  
  Qxx <- list.files( Dummy, pattern = pattern)
  Qfiles.new <- paste0( paste0( copyto, "/", unlist( lapply( QID.new, function( x ) gsub("XXXXX", x, Qxx)))))
  Qfiles.new <- gsub( "beverage", dt$para$beverage, Qfiles.new)
  Qfiles.new <- gsub( "YYYYY", dt$para$model$raw.pl, Qfiles.new)
  Qfiles.new <- mapply( function( substance, Qfiles) gsub( "parameter", substance, Qfiles)
                        , substance = dt$para$substance
                        , Qfiles = Qfiles.new
                        , SIMPLIFY = F)
  
  Qfiles.new <- Qfiles.new[unlist( lapply( lapply( lapply( dt$para$substance, function( x ) grep( x, grep( dt$para$beverage, Qfiles, value = T))), length), function( x ) x == 0))]
  
  file.copy( from = paste0( wd$fe[[ 1 ]], "Allgemein/Dummy_R_github/", Qxx)
             , to = unlist( Qfiles.new)
             , overwrite = F)
  
  message( paste0(length( Qfiles.new ), " ", pattern, "-files copied to ", copyto))
  
}