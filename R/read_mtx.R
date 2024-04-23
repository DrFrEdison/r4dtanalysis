mtx_add <- function(qxxmtx, coltoremove = c("X1", "X2"), sheet = "Zugabe_p"){

  qxxmtx <- lapply(qxxmtx, function( x ) openxlsx::read.xlsx( x , sheet = sheet))

  rangep <- 1 : length(coltoremove)

  qxxmtx <- lapply(qxxmtx, function(x) x[ , c(rangep, which( apply( x[ , -c(rangep) ], 2, function(y) sum(y, na.rm = T)) != 0) + max( rangep ))])

  if(length(qxxmtx) > 1){
    qxxmtx <- do.call(cbind, qxxmtx)
    lp = T} else{
      qxxmtx <- qxxmtx[[ 1 ]]
      lp = F
    }

  if(lp) qxxmtxcol <- qxxmtx[ , names( qxxmtx ) %in% c( coltoremove)][ , 1:(ncol(qxxmtx[ , names( qxxmtx ) %in% c( coltoremove)]) / 2)]
  if(!lp) qxxmtxcol <- qxxmtx[ , names( qxxmtx ) %in% c( coltoremove)][ , 1:(ncol(qxxmtx[ , names( qxxmtx ) %in% c( coltoremove)]))]

  qxxmtx <- qxxmtx[ , !names( qxxmtx ) %in% c( coltoremove)]

  qxxmtx <- qxxmtx[ which( apply(qxxmtx, 1, function(x) sum( x, na.rm = T)) != 0) , ]

  parameter <- names( qxxmtx )

  qxxmtx <- cbind( qxxmtxcol[ 1:nrow(qxxmtx) , ], qxxmtx)

  qxxmtx <- list(qxxmtx, parameter)

  names(qxxmtx) <- c("QXXMTX", "parameter")

  return( qxxmtx )

}

mtx_folder <- function(dir, parameter){

  for(i in 1:length(parameter)){
    setwd(dir)
    dir.create(parameter[i], showWarnings = F)
    setwd( paste0( "./", parameter[i]))
  }

}

mtx_move_spc <- function( dir.source, dir.target, parameter, filter = "VAS", filteron = T, SL = "SL"){

  setwd(dir.source)
  spc.files <- dir( pattern = ".spc$")

  parameter <- c(parameter, "H2O")

  for(i in 1:length(parameter)){

    file.to.copy <- grep( paste0("_", parameter[i], "_"), spc.files, value = T)

    if(filteron) file.to.copy <- grep( paste0("_", filter, "_"), file.to.copy, value = T, invert = T)
    if(!filteron) file.to.copy <- grep( paste0("_", filter, "_"), file.to.copy, value = T, invert = F)

    file.to.copy.SL <- grep( paste0( "_", "SL", "_" ), file.to.copy, value = T)
    file.to.copy <- grep( paste0( "_", "SL", "_" ), file.to.copy, value = T, invert = T)

    setwd( dir.target )
    setwd( paste0("./", parameter[i]))

    if( length(file.to.copy) > 0)
      file.copy(from = paste0( dir.source, file.to.copy)
                , to = basename(file.to.copy)
                , overwrite = T
                , recursive = F)

    if( length(file.to.copy.SL) > 0)
      file.copy(from = paste0( dir.source, file.to.copy.SL)
                , to = basename(file.to.copy.SL)
                , overwrite = T
                , recursive = F)
  }

}

mtx_plot_spc <- function( dir
                          , beverage
                          , parameter = NA
                          , baseline = NA
                          , pngplot = F
                          , plotlyplot = T
                          , filestext = NA
                          , colp = NA
                          , subfiles = NA
                          , recursive = T
                          , write = T
                          , path = getwd()
                          , FG = T){

  # Plot and write spectra ####
  spc.files <- list.files(dir, pattern = ".spc$", recursive = recursive)

  if(FG) fg.files <- grep("_FG_", spc.files, value = T)
  if(!FG) spc.files <- grep(parameter, spc.files, value = T)
  if( !is.na(parameter)) spc.files <- c(fg.files, grep(parameter, spc.files, value = T))

  stop_quietly <- function() {
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }

  if( length(spc.files) == 0 ){ stop_quietly() }

  filestext <- substr(basename( spc.files )
                      , unlist(gregexpr("_DT", basename( spc.files ), ignore.case = F)) + 8
                      , unlist(gregexpr("01_00", basename( spc.files ), ignore.case = F)) - 3)
  filestext <- filestext [which(nchar(filestext) > 0)]

    setwd( dir)
    dtR4spc::read_spc(working_directory = dir
                      , spc.write = T
                      , return.R = T
                      , recursive = T)

  # if( is.na(parameter)) if(length(dir( pattern = paste0(date.dt(), "_ref.html") )) > 0) file.rename(dir( pattern = paste0(date.dt(), "_ref.html") ), paste0(date.dt(), "_", beverage, "_", "ref.html"))
  # if( is.na(parameter)) if(length(dir( pattern = paste0(date.dt(), "_drk.html") )) > 0) file.rename(dir( pattern = paste0(date.dt(), "_drk.html") ), paste0(date.dt(), "_", beverage, "_", "drk.html"))
  # if( is.na(parameter)) if(length(dir( pattern = paste0(date.dt(), "_spc.html") )) > 0) file.rename(dir( pattern = paste0(date.dt(), "_spc.html") ), paste0(date.dt(), "_", beverage, "_", "spc.html"))
  # if( is.na(parameter)) if(length(dir( pattern = paste0(date.dt(), "_trans.html") )) > 0) file.rename(dir( pattern = paste0(date.dt(), "_trans.html") ), paste0(date.dt(), "_", beverage, "_", "trans.html"))
  #
  if( is.na(parameter)) if(length(dir( pattern = paste0(date.dt(), "_ref.csv") )) > 0) file.rename(dir( pattern = paste0(date.dt(), "_ref.csv") ), paste0(date.dt(), "_", beverage, "_", "ref.csv"))
  if( is.na(parameter)) if(length(dir( pattern = paste0(date.dt(), "_drk.csv") )) > 0) file.rename(dir( pattern = paste0(date.dt(), "_drk.csv") ), paste0(date.dt(), "_", beverage, "_", "drk.csv"))
  if( is.na(parameter)) if(length(dir( pattern = paste0(date.dt(), "_spc.csv") )) > 0) file.rename(dir( pattern = paste0(date.dt(), "_spc.csv") ), paste0(date.dt(), "_", beverage, "_", "spc.csv"))
  if( is.na(parameter)) if(length(dir( pattern = paste0(date.dt(), "_trans.csv") )) > 0) file.rename(dir( pattern = paste0(date.dt(), "_trans.csv") ), paste0(date.dt(), "_", beverage, "_", "trans.csv"))

  # if( !is.na(parameter)) if(length(dir( pattern = paste0(date.dt(), "_ref.html") )) > 0) file.rename(dir( pattern = paste0(date.dt(), "_ref.html") ), paste0(date.dt(), "_", beverage, "_", parameter, "_ref.html"))
  # if( !is.na(parameter)) if(length(dir( pattern = paste0(date.dt(), "_drk.html") )) > 0) file.rename(dir( pattern = paste0(date.dt(), "_drk.html") ), paste0(date.dt(), "_", beverage, "_", parameter, "_drk.html"))
  # if( !is.na(parameter)) if(length(dir( pattern = paste0(date.dt(), "_spc.html") )) > 0) file.rename(dir( pattern = paste0(date.dt(), "_spc.html") ), paste0(date.dt(), "_", beverage, "_", parameter, "_spc.html"))
  # if( !is.na(parameter)) if(length(dir( pattern = paste0(date.dt(), "_trans.html") )) > 0) file.rename(dir( pattern = paste0(date.dt(), "_trans.html") ), paste0(date.dt(), "_", beverage, "_", parameter, "_trans.html"))
  #
  if( !is.na(parameter)) if(length(dir( pattern = paste0(date.dt(), "_ref.csv") )) > 0) file.rename(dir( pattern = paste0(date.dt(), "_ref.csv") ), paste0(date.dt(), "_", beverage, "_", parameter, "_ref.csv"))
  if( !is.na(parameter)) if(length(dir( pattern = paste0(date.dt(), "_drk.csv") )) > 0) file.rename(dir( pattern = paste0(date.dt(), "_drk.csv") ), paste0(date.dt(), "_", beverage, "_", parameter, "_drk.csv"))
  if( !is.na(parameter)) if(length(dir( pattern = paste0(date.dt(), "_spc.csv") )) > 0) file.rename(dir( pattern = paste0(date.dt(), "_spc.csv") ), paste0(date.dt(), "_", beverage, "_", parameter, "_spc.csv"))
  if( !is.na(parameter)) if(length(dir( pattern = paste0(date.dt(), "_trans.csv") )) > 0) file.rename(dir( pattern = paste0(date.dt(), "_trans.csv") ), paste0(date.dt(), "_", beverage, "_", parameter, "_trans.csv"))

}

mtx_match_SA_Acid <- function( mtx
                               , ist.in.prozent
                               , type = c("NaOH", "Total")
                               , type.name = c("TA", "TTA")
                               , NaOH.fac = 10){

  mtx.acid <- list()

  ist.in.prozent <- cbind(ist.in.prozent, type = NA)
  names( ist.in.prozent )[ ncol( ist.in.prozent )] <- type.name

  mtx.acid$xlsx <- lapply( mtx, function( x ) read.xlsx( x, sheet = "SA"))
  mtx.acid$acid <- lapply( mtx.acid$xlsx, function( x ) x[ , c( grep("X1", colnames( x )), grep( type, colnames( x )))])
  mtx.acid$acid <- suppressWarnings( lapply( mtx.acid$acid, function( x ) x[ which( !is.na( as.numeric( x[ , 2] ) ) ) , ]) )

  for(i in 1 : length( mtx )) mtx.acid$acid[[ i ]][ , 2] <- as.numeric( mtx.acid$acid[[ i ]][ , 2] )
  if( type == "NaOH") for(i in 1 : length( mtx )) mtx.acid$acid[[ i ]][ , 2] <- mtx.acid$acid[[ i ]][ , 2] * NaOH.fac

  for(i in 1 : length( mtx ))
    if( "FG" %in% mtx.acid$acid[[ i ]][ , 1] )
      mtx.acid$acid[[ i ]][ , 1][ mtx.acid$acid[[ i ]][ , 1] %in% "FG" ] <- "FG 100 %"

  for(j in 1 : length(mtx) ){
    for(i in 1 : length( ist.in.prozent$Probe_Anteil ) ){

      if( ist.in.prozent$Probe_Anteil[ i ] %in% mtx.acid$acid[[ j ]][ , 1] ){
        ist.in.prozent[ , ncol(ist.in.prozent)][ i ] <- mtx.acid$acid[[ j ]][ which( mtx.acid$acid[[ j ]][ , 1] %in% ist.in.prozent$Probe_Anteil[ i ] ) , 2]
      }
    }
  }

  return( ist.in.prozent)
}

