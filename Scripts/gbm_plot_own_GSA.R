# Own version of gbm.plot 
# Modified from Kristina Kviles version

gbm.plot.own <- function(gbm.object, dat, smooth = TRUE, rug = TRUE, 
                         s.col = "aquamarine4",
                         ylim = c(-2.5,3), y.label = "", varnames=NULL, 
                         with_yax=c(1,7),
                         xlim_n=0, range_n=c(-2,71)) 
{
  
  n <- length(gbm.object$var.names)
  if(is.null(varnames)){
    varnames <- gbm.object$contributions$var
  }
  
  for(j in 1:n){
    k <- match(gbm.object$contributions$var[j],gbm.object$gbm.call$predictor.names)
    varname <- varnames[j]
    contrib <- gbm.object$contributions$rel.inf[j]
    xlab <- paste0(varname, " (", round(contrib), "%)")
    response.matrix <- gbm::plot.gbm(gbm.object, k, return.grid = TRUE)
    response.matrix[,2] <- response.matrix[,2] - mean(response.matrix[,2])
    smoothed <- loess(response.matrix[,2] ~ response.matrix[,1], span = 0.3)
    
    vardat <- dat[, gbm.object$gbm.call$gbm.x[k]]
    #vardat_pres <- vardat[dat$PresAbs==1]
    
    if(j==xlim_n){
      plot(response.matrix, type="l", ylab="", xlab="", ylim = ylim, xlim=range_n, lty="dotted", col=s.col, lwd = 1.5);
      mtext(side=1, xlab, outer = FALSE, cex = 0.7, line = 2.5)
    }else if (j%in%with_yax){
      plot(response.matrix, type="l", ylab="", xlab=xlab, ylim = ylim, lty="dotted", col=s.col, lwd = 1.5);
      mtext(side=1, xlab, outer = FALSE, cex = 0.7, line = 2.5)
    }else{
      plot(response.matrix, type="l", ylab="", xlab=xlab, ylim = ylim, lty="dotted", col=s.col, axes=FALSE,frame=TRUE, lwd = 1.5);
      Axis(side=1);Axis(side=2,labels = FALSE);mtext(side=1, xlab, outer = FALSE, cex = 0.7, line = 2.5)}
    if(smooth){points(response.matrix[,1],smoothed$fitted, type = "l", lwd=2)}
    if(rug){rug(vardat, col=s.col)}
  }
  # mtext(side=2, "Partial effect", outer = TRUE, cex = 0.7, line = 1)
}


