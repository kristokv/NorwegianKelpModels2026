
# Filled-color interaction surface + labeled contours for a GBM
# Made by co-pilot 23.01.2026
# Mirrors the contour branch of dismo::gbm.perspec(), but:
# - adds xlab/ylab
# - draws a filled color field + labeled contours
# - lets you control breaks, palette, etc.
gbm_contour_filled_labeled <- function(gbm.object, x, y,
                                       xlab = NULL, ylab = NULL,
                                       n.grid = 20,
                                       n.trees = NULL,
                                       z.range = NULL,
                                       breaks = NULL,
                                       palette = NULL,
                                       contour.col = "grey10",
                                       contour.lwd = 0.8,
                                       contour.labcex = 0.8,
                                       add_legend = TRUE,
                                       legend.args = list(),
                                       ...) {
  # ---- 1) Build grid like gbm.perspec() ----
  gbm.call <- gbm.object$gbm.call
  data <- gbm.call$dataframe[, gbm.call$gbm.x, drop = FALSE]
  pred.names <- gbm.object$gbm.call$predictor.names
  if (is.null(n.trees)) {
    n.trees <- gbm.call$best.trees  # same default used by gbm.perspec()
  }
  
  make_axis <- function(v) {
    if (is.vector(v)) {
      seq(min(v, na.rm = TRUE), max(v, na.rm = TRUE), length.out = n.grid)
    } else {
      factor(names(table(v)), levels = levels(v))
    }
  }
  
  x.var <- make_axis(data[[x]])
  y.var <- make_axis(data[[y]])
  grid_df <- expand.grid(list(x.var, y.var))
  names(grid_df) <- c(pred.names[x], pred.names[y])
  
  # Fill the remaining predictors at mean/mode (as gbm.perspec does)
  n.preds <- ncol(data); col_i <- 3
  for (k in seq_len(n.preds)) {
    if (k != x && k != y) {
      if (is.vector(data[[k]])) {
        grid_df[, col_i] <- mean(data[[k]], na.rm = TRUE)
      } else {
        tmp <- sort(table(data[[k]]), decreasing = TRUE)
        grid_df[, col_i] <- rep(names(tmp)[1], length(x.var) * length(y.var))
        grid_df[, col_i] <- as.factor(grid_df[, col_i])
      }
      names(grid_df)[col_i] <- pred.names[k]
      col_i <- col_i + 1
    }
  }
  
  # ---- 2) Predict on link scale as in gbm.perspec() ----
  z <- gbm::predict.gbm(gbm.object, grid_df, n.trees = n.trees, type = "link")
  zmat <- matrix(z, nrow = length(x.var), ncol = length(y.var), byrow = FALSE)
  
  # ---- 3) Ranges, breaks, palette ----
  if (is.null(z.range)) z.range <- range(z, finite = TRUE)
  if (is.null(breaks))  breaks <- pretty(z.range, n = 10)
  if (is.null(palette)) palette <- grDevices::colorRampPalette(c("#F7FBFF", "#6BAED6", "#08306B"))
  
  cols <- palette(length(breaks) - 1)
  
  # ---- 4) Draw filled colors + labeled contours ----
  # image() handles filled colors; overlay contour with labels
  graphics::image(x = as.numeric(x.var), y = as.numeric(y.var), z = zmat,
                  xlab = if (is.null(xlab)) pred.names[x] else xlab,
                  ylab = if (is.null(ylab)) pred.names[y] else ylab,
                  col = cols, breaks = breaks, useRaster = TRUE, ...)
  graphics::contour(x = as.numeric(x.var), y = as.numeric(y.var), z = zmat,
                    levels = breaks, drawlabels = TRUE, labcex = contour.labcex,
                    col = contour.col, lwd = contour.lwd, add = TRUE)
  
  # ---- 5) Optional legend (simple base legend) ----
  if (isTRUE(add_legend)) {
    # A compact horizontal color bar legend
    usr <- par("usr")
    xleft <- usr[1]; xright <- usr[2]; ybottom <- usr[3]
    h <- 0.06 * (usr[4] - usr[3])  # legend height
    ytop <- ybottom + h
    
    # Legend rectangles
    bx <- seq(xleft, xright, length.out = length(cols) + 1)
    for (i in seq_along(cols)) {
      rect(bx[i], ybottom, bx[i+1], ytop, col = cols[i], border = NA, xpd = NA)
    }
    rect(xleft, ybottom, xright, ytop, border = "grey40", xpd = NA)
    
    # Tick marks with labels at the same 'breaks'
    at_x <- seq(xleft, xright, length.out = length(breaks))
    axis(side = 1, at = at_x, labels = format(breaks), line = -1.2, tick = FALSE, cex.axis = 0.75)
    # Optional legend title
    if (!is.null(legend.args$title)) {
      mtext(legend.args$title, side = 1, line = -3.0, cex = 0.8, font = 2)
    }
  }
  
  invisible(list(x = x.var, y = y.var, z = zmat, breaks = breaks, cols = cols))
}
