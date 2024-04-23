pls_analyse_plot <- function(pls_model,
                             model_matrix = NA,
                             colp = NA,
                             wl1, wl2, wl3, wl4, wl5, wl6,
                             ncomp,
                             derivative,
                             pc_scores = c(1,2),
                             plot_loadings = T,
                             pl_regression_and_pred_vs_ref = ncomp,
                             var_xy = "y",
                             val = F,
                             pngname = paste0(date.dt(), "_PC", ncomp, "_", derivative, "_", wl1, "_", wl2, "_", wl3, "_", wl4, "_", wl5, "_", wl6
                                              , "_PC", pc_scores[1], "_vs_PC", pc_scores[2])){

  if(ncomp == 1) pc_scores = c(1,1)

  if(length(pls_model) < 5){
    plstochoose <- pls_model[[grep(derivative, names(pls_model))[1]]]
    if(is.na(wl3)) plstochoose <- plstochoose[[grep(paste(wl1, wl2, sep="_"),names(plstochoose))]]
    if(!is.na(wl3) & is.na(wl3)) plstochoose <- plstochoose[[grep(paste(wl1, wl2, wl3, wl4, sep="_"),names(plstochoose))]]
    if(!is.na(wl5)) plstochoose <- plstochoose[[grep(paste(wl1, wl2, wl3, wl4, wl5, wl6, sep="_"),names(plstochoose))]]
  }

  if(length(pls_model) > 5){ plstochoose <- pls_model}
  # Explained Variance Y
  explvar_y <- c(round(drop(R2(plstochoose, estimate = "train", intercept = FALSE)$val)*100,1)[1],
                 round(diff(drop(R2(plstochoose, estimate = "train", intercept = FALSE)$val)*100),1))
  png(filename = paste0(pngname,".png"),width = 7*1.5, height = 7/1.1,type="cairo",units="in",pointsize=12,res=500)
  par(cex.axis = 1.2, cex.lab=1.5, font.lab=2)

  layout(matrix(c(1,1,0,2), 2, 2, byrow = TRUE))
  if(any(!is.na(model_matrix))){
    layout(matrix(c(1,2,3,
                    4,5,3), ncol = 3, byrow =T), heights = c(3,3), widths = c(3, 3, 1))
  } else{
    layout(matrix(c(1,2,
                    3,4), ncol = 2, byrow =T)
    )
  }
  par(mar = c(4,5,1,1))

  if(any(!is.na(model_matrix))){
    colp1 <- rainbow(length(levels(factor(model_matrix$data[,grep(colp, names(model_matrix$data))[1]]))))
    colp2 <- colp1[factor(model_matrix$data[,grep(colp, names(model_matrix$data))[1]])]
    legendt <- paste(levels(factor(model_matrix$data[,grep(colp, names(model_matrix$data))[1]])))} else{
      colp1 <- "blue"
      colp2 <- "blue"
    }

  plot(plstochoose$scores[,pc_scores[1]], plstochoose$scores[,pc_scores[2]],
       xlab = paste0("PC",pc_scores[1], " (",round(explvar(plstochoose)[pc_scores[1]],1),"%, ", explvar_y[pc_scores[1]],"%)"),
       ylab = paste("PC",pc_scores[2], "(",round(explvar(plstochoose)[pc_scores[2]],1),"%, ", explvar_y[pc_scores[2]],"%)"),
       cex = 1.75, pch = 20, col = colp2, main = "Score Plot")

  if(plot_loadings == F){
    if(is.na(wl3))
      plot(wl1:wl2,plstochoose$coefficients[,,pl_regression_and_pred_vs_ref[1]], lwd = 2, col = "blue", type = "l", lty = 1,
           xlab = "Lambda in nm", ylab = paste("Regression coefficient, PC",pl_regression_and_pred_vs_ref), main = paste("Regression coefficient, PC",pl_regression_and_pred_vs_ref))
    if(!is.na(wl3) & is.na(wl5))
      plot(c(wl1:wl2, wl3:wl4),plstochoose$coefficients[,,pl_regression_and_pred_vs_ref[1]], lwd = 2, col = "blue", type = "l", lty = 1,
           xlab = "Lambda in nm", ylab = paste("Regression coefficient, PC",pl_regression_and_pred_vs_ref), main = paste("Regression coefficient, PC",pl_regression_and_pred_vs_ref))

    if(!is.na(wl5))
      plot(c(wl1:wl2, wl3:wl4, wl5:wl6),plstochoose$coefficients[,,pl_regression_and_pred_vs_ref[1]], lwd = 2, col = "blue", type = "l", lty = 1,
           xlab = "Lambda in nm", ylab = paste("Regression coefficient, PC",pl_regression_and_pred_vs_ref), main = paste("Regression coefficient, PC",pl_regression_and_pred_vs_ref))
  }


  if(plot_loadings == T){
    if(is.na(wl3))
      plot(wl1:wl2,plstochoose$loadings[,pl_regression_and_pred_vs_ref[1]], lwd = 2, col = "blue", type = "l", lty = 1,
           xlab = "Lambda in nm", ylab = paste("Regression coefficient, PC",pl_regression_and_pred_vs_ref), main = paste("Loadings, PC",pl_regression_and_pred_vs_ref[1]))
    if(!is.na(wl3) & is.na(wl5))
      plot(c(wl1:wl2, wl3:wl4),plstochoose$loadings[ , pl_regression_and_pred_vs_ref[1]], lwd = 2, col = "blue", type = "l", lty = 1,
           xlab = "Lambda in nm", ylab = paste("Loadings, PC",pl_regression_and_pred_vs_ref), main = paste("Loadings, PC",pl_regression_and_pred_vs_ref[1]))
    if(!is.na(wl5))
      plot(c(wl1:wl2, wl3:wl4, wl5:wl6),plstochoose$loadings[ , pl_regression_and_pred_vs_ref[1]], lwd = 2, col = "blue", type = "l", lty = 1,
           xlab = "Lambda in nm", ylab = paste("Loadings, PC",pl_regression_and_pred_vs_ref), main = paste("Loadings, PC",pl_regression_and_pred_vs_ref[1]))
  }
  if(any(!is.na(model_matrix))){
    par(mar = c(0,0,0,0))
    plot(1,1,type="n", axes = F, xlab = "", ylab = "")
    legend("left", legendt, col = colp1, pch = 20, cex = 1.5, bty = "n", ncol = ceiling(length(legendt)/30), xpd = T
           , text.width	= .1, pt.cex = 1.5)
  }

  par(mar = c(4,5,1,1))
  if(var_xy == "x") plot(0:ncomp,as.numeric(c(0,cumsum(explvar(plstochoose)[1:ncomp]))), type = "b", col ="blue", lwd = 2,
                         xlab = "Factors", ylab = "X-Variance", main = "Explained Variance")
  if(var_xy == "y") plot(0:ncomp, as.numeric(c(0,drop(R2(plstochoose, estimate = "train", intercept = FALSE)$val)[1:ncomp])*100), type = "b", col ="blue", lwd = 2,
                         xlab = "Factors", ylab = "Y-Variance", main = "Explained Variance")

  plot(plstochoose$model$`pls$x`,
       plstochoose$fitted.values[,,pl_regression_and_pred_vs_ref],
       col = colp2, pch = 20, cex = 1.25,
       xlab = paste0("Reference Y, PC ",pl_regression_and_pred_vs_ref),
       ylab = paste0("Predicted Y, PC ",pl_regression_and_pred_vs_ref),
       main = "Predicted vs. Reference")

  if(val == T){
    points(plstochoose$model$`pls$x`,
           plstochoose$validation$pred[,,pl_regression_and_pred_vs_ref],
           col = "red", pch = 19, cex = 1.75,
    )
  }
  dev.off()
}

pls_analyse_plot_rmo <- function(csv_transfered = dt$mop$model.choose
                                 , wl1 = dt$mop$wl1, wl2 = dt$mop$wl2, wl3 = dt$mop$wl3, wl4 = dt$mop$wl4, wl5 = dt$mop$wl5, wl6 = dt$mop$wl6
                                 , ncomp = dt$mop$ncomp
                                 , derivative = dt$mop$derivative
                                 , substance = dt$para$substance[ dt$para$i ]
                                 , pc_scores = c(1,2)
                                 , plot_loadings = T
                                 , pl_regression_and_pred_vs_ref = ncomp
                                 , var_xy = "y"
                                 , val = F
                                 , colp = "Probe"
                                 , pngname = paste0(date.dt(), "_PC", ncomp, "_", derivative, "_", wl1, "_", wl2, "_", wl3, "_", wl4, "_", wl5, "_", wl6
                                                    , "_PC", pc_scores[1], "_vs_PC", pc_scores[2])
                                 , png = F){

  library(pls)
  if(ncomp == 1) pc_scores = c(1,1)

  pls_model <- pls_model(csv_transfered = csv_transfered
                         , substance = substance
                         , wlr = data.frame( wl1, wl2, wl3, wl4, wl5, wl6)
                         , ncomp = ncomp
                         , spc = derivative)

  plstochoose <- pls_model[[grep(derivative, names(pls_model))[1]]]
  if(is.na(wl3)) plstochoose <- plstochoose[[grep(paste(wl1, wl2, sep="_"),names(plstochoose))]]
  if(!is.na(wl3) & is.na(wl5)) plstochoose <- plstochoose[[grep(paste(wl1, wl2, wl3, wl4, sep="_"),names(plstochoose))]]
  if(!is.na(wl5)) plstochoose <- plstochoose[[grep(paste(wl1, wl2, wl3, wl4, wl5, wl6, sep="_"),names(plstochoose))]]

  # Explained Variance Y
  explvar_y <- c(round(drop(R2(plstochoose, estimate = "train", intercept = FALSE)$val)*100,1)[1],
                 round(diff(drop(R2(plstochoose, estimate = "train", intercept = FALSE)$val)*100),1))
  if(png) png(filename = paste0(pngname,".png"),width = 7*1.5, height = 7/1.1,type="cairo",units="in",pointsize=12,res=500)
  par(cex.axis = 1.2, cex.lab=1.5, font.lab=2)

  layout(matrix(c(1,1,0,2), 2, 2, byrow = TRUE))
  if(any(!is.na(csv_transfered))){
    layout(matrix(c(1,2,3,
                    4,5,3), ncol = 3, byrow =T), heights = c(3,3), widths = c(3, 3, 1))
  } else{
    layout(matrix(c(1,2,
                    3,4), ncol = 2, byrow =T)
    )
  }
  par(mar = c(4,5,1,1))

  if(any(!is.na(csv_transfered))){
    colp1 <- rainbow(length(levels(factor(csv_transfered$data[,grep(colp, names(csv_transfered$data))[1]]))))
    colp2 <- colp1[factor(csv_transfered$data[,grep(colp, names(csv_transfered$data))[1]])]
    legendt <- paste(levels(factor(csv_transfered$data[,grep(colp, names(csv_transfered$data))[1]])))} else{
      colp1 <- "blue"
      colp2 <- "blue"
    }

  plot(plstochoose$scores[,pc_scores[1]], plstochoose$scores[,pc_scores[2]],
       xlab = paste0("PC",pc_scores[1], " (",round(explvar(plstochoose)[pc_scores[1]],1),"%, ", explvar_y[pc_scores[1]],"%)"),
       ylab = paste("PC",pc_scores[2], "(",round(explvar(plstochoose)[pc_scores[2]],1),"%, ", explvar_y[pc_scores[2]],"%)"),
       cex = 1.75, pch = 20, col = colp2, main = "Score Plot")

  if(plot_loadings == F){
    if(is.na(wl3))
      plot(wl1:wl2,plstochoose$coefficients[,,pl_regression_and_pred_vs_ref[1]], lwd = 2, col = "blue", type = "l", lty = 1,
           xlab = "Lambda in nm", ylab = paste("Regression coefficient, PC",pl_regression_and_pred_vs_ref), main = paste("Regression coefficient, PC",pl_regression_and_pred_vs_ref))
    if(!is.na(wl3) & is.na(wl5))
      plot(c(wl1:wl2, wl3:wl4),plstochoose$coefficients[,,pl_regression_and_pred_vs_ref[1]], lwd = 2, col = "blue", type = "l", lty = 1,
           xlab = "Lambda in nm", ylab = paste("Regression coefficient, PC",pl_regression_and_pred_vs_ref), main = paste("Regression coefficient, PC",pl_regression_and_pred_vs_ref))
    if(!is.na(wl5))
      plot(c(wl1:wl2, wl3:wl4, wl5:wl6),plstochoose$coefficients[,,pl_regression_and_pred_vs_ref[1]], lwd = 2, col = "blue", type = "l", lty = 1,
           xlab = "Lambda in nm", ylab = paste("Regression coefficient, PC",pl_regression_and_pred_vs_ref), main = paste("Regression coefficient, PC",pl_regression_and_pred_vs_ref))
  }

  if(plot_loadings == T){
    if(is.na(wl3))
      plot(wl1:wl2,plstochoose$loadings[,pl_regression_and_pred_vs_ref[1]], lwd = 2, col = "blue", type = "l", lty = 1,
           xlab = "Lambda in nm", ylab = paste("Regression coefficient, PC",pl_regression_and_pred_vs_ref), main = paste("Loadings, PC",pl_regression_and_pred_vs_ref[1]))
    if(!is.na(wl3) & is.na(wl5))
      plot(c(wl1:wl2, wl3:wl4),plstochoose$loadings[ , pl_regression_and_pred_vs_ref[1]], lwd = 2, col = "blue", type = "l", lty = 1,
           xlab = "Lambda in nm", ylab = paste("Loadings, PC",pl_regression_and_pred_vs_ref), main = paste("Loadings, PC",pl_regression_and_pred_vs_ref[1]))
    if(!is.na(wl5))
      plot(c(wl1:wl2, wl3:wl4, wl5:wl6),plstochoose$loadings[ , pl_regression_and_pred_vs_ref[1]], lwd = 2, col = "blue", type = "l", lty = 1,
           xlab = "Lambda in nm", ylab = paste("Loadings, PC",pl_regression_and_pred_vs_ref), main = paste("Loadings, PC",pl_regression_and_pred_vs_ref[1]))
  }
  if(any(!is.na(csv_transfered))){
    par(mar = c(0,0,0,0))
    plot(1,1,type="n", axes = F, xlab = "", ylab = "")
    legend("left", legendt, col = colp1, pch = 20, cex = 1.5, bty = "n", ncol = ceiling(length(legendt)/30), xpd = T
           , text.width	= .1, pt.cex = 1.5)
  }

  par(mar = c(4,5,1,1))
  if(var_xy == "x") plot(0:ncomp,as.numeric(c(0,cumsum(explvar(plstochoose)[1:ncomp]))), type = "b", col ="blue", lwd = 2,
                         xlab = "Factors", ylab = "X-Variance", main = "Explained Variance")
  if(var_xy == "y") plot(0:ncomp, as.numeric(c(0,drop(R2(plstochoose, estimate = "train", intercept = FALSE)$val)[1:ncomp])*100), type = "b", col ="blue", lwd = 2,
                         xlab = "Factors", ylab = "Y-Variance", main = "Explained Variance")

  plot(x <- plstochoose$model$`pls$x`,
       y <- plstochoose$fitted.values[,,pl_regression_and_pred_vs_ref],
       col = colp2, pch = 20, cex = 1.25,
       xlab = paste0("Reference Y, PC ",pl_regression_and_pred_vs_ref),
       ylab = paste0("Predicted Y, PC ",pl_regression_and_pred_vs_ref),
       main = "Predicted vs. Reference")


  legend("topleft", c(paste0("R2 =", as.character( round(summary(lm(y~x))$r.squared, 3)))
                      , paste0( "RMSE =", round( RMSEP(plstochoose)$val[ , , ncomp + 1], 3))))
  if(val == T){
    points(plstochoose$model$`pls$x`,
           plstochoose$validation$pred[,,pl_regression_and_pred_vs_ref],
           col = "red", pch = 19, cex = 1.75,
    )
  }
  if(png) dev.off()
  par( mfrow = c(1,1))
}
