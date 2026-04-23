# Own version of gbm.plot 
# Modified from Kristina Kviles version

gbm.plot.own <- function(gbm.object, dat, smooth = TRUE, rug = TRUE, tag = "A", xrange_pres = FALSE,
                         s.col = "aquamarine4",ylim = c(-1,1), varnames=NULL, showmean = TRUE, 
                         with_yax=c(1,7),species="kelp") 
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
    print(varname)
    idx <- which.min(abs(response.matrix$y))
    print(response.matrix[[1]][idx])
    
    vardat <- dat[, gbm.object$gbm.call$gbm.x[k]]
    vardat_pres <- vardat[dat$Tetthet>0]
    vardat_mean <- mean(vardat[dat$Tetthet>0], na.rm=T)
    
    if (xrange_pres == TRUE){
      xlims =  range(vardat_pres, na.rm=T)
    }else{
      xlims =  range(vardat, na.rm=T)}
    
    if (j%in%with_yax){
      plot(response.matrix, type="l", ylab="", xlab=xlab, xlim = xlims, ylim = ylim, col=s.col, lwd = 1);
      mtext(side=1, xlab, outer = FALSE, cex = 0.7, line = 2.5)
    }else{
      plot(response.matrix, type="l", ylab="", xlab=xlab, xlim = xlims, ylim = ylim, col=s.col,  axes=FALSE,frame=TRUE, lwd = 1);
      Axis(side=1);Axis(side=2,labels = FALSE);mtext(side=1, xlab, outer = FALSE, cex = 0.7, line = 2.5)}
    if(smooth){points(response.matrix[,1],smoothed$fitted, type = "l", lwd=2)}
    if(rug){rug(vardat_pres, col=s.col)}
    if(showmean){abline(v = vardat_mean, col=s.col, lty="dotted", , lwd = 0.75)}
    if(j==1){mtext(side = 3, tag, outer = FALSE, line = 0,  cex = 1.25, adj = -0.4)}  
  }
  mtext(side=2, paste0("Marginal effect on ",species), outer = TRUE, cex = 0.7, line = 1)
}


