model_name <- function( dir = getwd()
                        , pattern = dt$para$substance[dt$para$i]
                        , modelfile = c(".unsb", ".41M")){

  modelp <- list()
  modelp$files <- list.files( path = dir)

  modelp$grep$pattern = grep(pattern, modelp$files, value = T)
  if( pattern == "Coffein" | pattern == "Koffein") modelp$grep$EN <- grep("Caffeine", modelp$files, value = T)


  if( length( modelp$grep$EN ) != 0) modelp$name <- modelp$grep$EN
  if( length( modelp$grep$pattern ) != 0) modelp$name <- modelp$grep$pattern

  modelp$name <- grep( modelfile, modelp$name, value = T)

  for(i in 1:length(modelfile)) modelp$name <- gsub( modelfile[[ i ]], "",  modelp$name)

  return(modelp$name)
}
