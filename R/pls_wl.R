wlr_function <- function(wlr1, wlr2, seqp){

  wlr1 <- seq(min(wlr1), max(wlr1),seqp)
  wlr2 <- seq(min(wlr2), max(wlr2),seqp)
  wlr <- expand.grid(wlr1, wlr2)

  if(length(which(wlr[,1] >= wlr[,2])) > 0) wlr <- wlr[-which(wlr[,1] >= wlr[,2]),]
  return(wlr)

}

wlr_function2 <- function(wlr1, wlr2, seqp){

  wlr3 <- c(min(wlr1), max(wlr2))

  wlr1 <- seq(min(wlr1), max(wlr1),seqp)
  wlr2 <- seq(min(wlr2), max(wlr2),seqp)
  wlr3 <- seq(min(wlr3), max(wlr3),seqp)
  wlr4 <- rev(wlr3)
  wlr <- expand.grid(wlr1, wlr2, wlr3, wlr4)

  if(length(which(wlr$Var1 >= wlr$Var2)) > 0) wlr <- wlr[-which(wlr$Var1 >= wlr$Var2),]
  if(length(which(wlr$Var3<=wlr$Var2)) >0 ) wlr <- wlr[-which(wlr$Var3<=wlr$Var2) , ]
  if(length(which(wlr$Var3<=wlr$Var1)) >0 ) wlr <- wlr[-which(wlr$Var3<=wlr$Var1) , ]
  if(length(which(wlr$Var4<=wlr$Var3)) >0 ) wlr <- wlr[-which(wlr$Var4<=wlr$Var3) , ]

  return(wlr)
}

wlr_function3 <- function(wlr1, wlr2, seqp){

  wlr3 <- c(min(wlr1), max(wlr2))
  wlr5 <- c(min(wlr3), max(wlr3))

  wlr1 <- seq(min(wlr1), max(wlr1),seqp)
  wlr2 <- seq(min(wlr2), max(wlr2),seqp)
  wlr3 <- seq(min(wlr3), max(wlr3),seqp)
  wlr4 <- rev(wlr3)
  wlr5 <- seq(min(wlr5), max(wlr5),seqp)
  wlr6 <- rev(wlr5)
  wlr <- expand.grid(wlr1, wlr2, wlr3, wlr4, wlr5, wlr6)

  if(length(which(wlr$Var1 >= wlr$Var2)) > 0) wlr <- wlr[-which(wlr$Var1 >= wlr$Var2),]
  if(length(which(wlr$Var3<=wlr$Var2)) >0 ) wlr <- wlr[-which(wlr$Var3<=wlr$Var2) , ]
  if(length(which(wlr$Var3<=wlr$Var1)) >0 ) wlr <- wlr[-which(wlr$Var3<=wlr$Var1) , ]
  if(length(which(wlr$Var4<=wlr$Var3)) >0 ) wlr <- wlr[-which(wlr$Var4<=wlr$Var3) , ]
  if(length(which(wlr$Var5<=wlr$Var3)) >0 ) wlr <- wlr[-which(wlr$Var5<=wlr$Var3) , ]
  if(length(which(wlr$Var6<=wlr$Var5)) >0 ) wlr <- wlr[-which(wlr$Var6<=wlr$Var5) , ]
  if(length(which(wlr$Var5<=wlr$Var4)) >0 ) wlr <- wlr[-which(wlr$Var5<=wlr$Var4) , ]

  return(wlr)
}
