pls_prediction_clean <- function(prediction, wl1, wl2, wl3, wl4, wl5, wl6, ncomp, derivative){

  final_prediction <- as.numeric(prediction$prediction[[1]][[ncomp]])

  return(final_prediction)
}
