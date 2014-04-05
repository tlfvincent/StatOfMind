# This can be used to plot radarcharts.
# An example of applying this script to produce radarcharts can be found at the following webpage: http://statofmind.wordpress.com/2014/01/26/mapping-the-taste-profile-of-scottish-whishkeys/


'radarchart' <- function (df, axistype = 0, seg = 4, pty = 16, pcol = 1:8, plty = 1:6,
plwd = 1, pdensity = NULL, pfcol = NA, cglty = 3, cglwd = 1,
cglcol = "navy", axislabcol = "blue", title = "", maxmin = TRUE,
na.itp = TRUE, centerzero = FALSE, vlabels = NULL, caxislabels = NULL,
paxislabels = NULL, cex=3, ...)
{
  if (!is.data.frame(df)) {
    cat("The data must be given as dataframe.\n")
    return()
  }
  if ((n <- length(df)) < 3) {
    cat("The number of variables must be 3 or more.\n")
    return()
  }
  if (maxmin == FALSE) {
    dfmax <- apply(df, 2, max)
    dfmin <- apply(df, 2, min)
    df <- rbind(dfmax, dfmin, df)
  }
  plot(c(-1.2, 1.2), c(-1.2, 1.2), type = "n", frame.plot = FALSE,
  axes = FALSE, xlab = "", ylab = "", main = title, cex.main=3, asp = 1,
  ...)
  theta <- seq(90, 450, length = n + 1) * pi/180
  theta <- theta[1:n]
  xx <- cos(theta)
  yy <- sin(theta)
  CGap <- ifelse(centerzero, 0, 1)
  for (i in 0:seg) {
    polygon(xx * (i + CGap)/(seg + CGap), yy * (i + CGap)/(seg +
    CGap), lty = cglty, lwd = cglwd, border = cglcol)
    if (axistype == 1 | axistype == 3)
    CAXISLABELS <- paste(i/seg * 100, "(%)")
    if (axistype == 4 | axistype == 5)
    CAXISLABELS <- sprintf("%3.2f", i/seg)
    if (!is.null(caxislabels) & (i < length(caxislabels)))
    CAXISLABELS <- caxislabels[i + 1]
    if (axistype == 1 | axistype == 3 | axistype == 4 | axistype ==
    5)
    text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS,
    col = axislabcol)
  }
  if (centerzero) {
    arrows(0, 0, xx * 1, yy * 1, lwd = cglwd, lty = cglty,
    length = 0, col = cglcol)
  }
  else {
    arrows(xx/(seg + CGap), yy/(seg + CGap), xx * 1, yy *
    1, lwd = cglwd, lty = cglty, length = 0, col = cglcol)
  }
  PAXISLABELS <- df[1, 1:n]
  if (!is.null(paxislabels))
  PAXISLABELS <- paxislabels
  if (axistype == 2 | axistype == 3 | axistype == 5) {
    text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol)
  }
  VLABELS <- colnames(df)
  if (!is.null(vlabels))
  VLABELS <- vlabels
  text(xx * 1.2, yy * 1.2, VLABELS, cex=1.8)
  series <- length(df[[1]])
  if (length(pty) < (series - 2)) {
    ptys <- rep(pty, series - 2)
    pcols <- rep(pcol, series - 2)
    pltys <- rep(plty, series - 2)
    plwds <- rep(plwd, series - 2)
    pdensities <- rep(pdensity, series - 2)
    pfcols <- rep(pfcol, series - 2)
  }
  else {
    ptys <- pty
    pcols <- pcol
    pltys <- plty
    plwds <- plwd
    pdensities <- pdensity
    pfcols <- pfcol
  }
  for (i in 3:series) {
    xxs <- xx
    yys <- yy
    scale <- CGap/(seg + CGap) + (df[i, ] - df[2, ])/(df[1,
    ] - df[2, ]) * seg/(seg + CGap)
    if (sum(!is.na(df[i, ])) < 3) {
      cat(sprintf("[DATA NOT ENOUGH] at %d\n%g\n", i, df[i,
      ]))
    }
    else {
      for (j in 1:n) {
        if (is.na(df[i, j])) {
          if (na.itp) {
            left <- ifelse(j > 1, j - 1, n)
            while (is.na(df[i, left])) {
              left <- ifelse(left > 1, left - 1, n)
            }
            right <- ifelse(j < n, j + 1, 1)
            while (is.na(df[i, right])) {
              right <- ifelse(right < n, right + 1, 1)
            }
            xxleft <- xx[left] * CGap/(seg + CGap) +
            xx[left] * (df[i, left] - df[2, left])/(df[1,
            left] - df[2, left]) * seg/(seg + CGap)
            yyleft <- yy[left] * CGap/(seg + CGap) +
            yy[left] * (df[i, left] - df[2, left])/(df[1,
            left] - df[2, left]) * seg/(seg + CGap)
            xxright <- xx[right] * CGap/(seg + CGap) +
            xx[right] * (df[i, right] - df[2, right])/(df[1,
            right] - df[2, right]) * seg/(seg + CGap)
            yyright <- yy[right] * CGap/(seg + CGap) +
            yy[right] * (df[i, right] - df[2, right])/(df[1,
            right] - df[2, right]) * seg/(seg + CGap)
            if (xxleft > xxright) {
              xxtmp <- xxleft
              yytmp <- yyleft
              xxleft <- xxright
              yyleft <- yyright
              xxright <- xxtmp
              yyright <- yytmp
            }
            xxs[j] <- xx[j] * (yyleft * xxright - yyright *
            xxleft)/(yy[j] * (xxright - xxleft) - xx[j] *
            (yyright - yyleft))
            yys[j] <- (yy[j]/xx[j]) * xxs[j]
          }
          else {
            xxs[j] <- 0
            yys[j] <- 0
          }
        }
        else {
          xxs[j] <- xx[j] * CGap/(seg + CGap) + xx[j] *
          (df[i, j] - df[2, j])/(df[1, j] - df[2, j]) *
          seg/(seg + CGap)
          yys[j] <- yy[j] * CGap/(seg + CGap) + yy[j] *
          (df[i, j] - df[2, j])/(df[1, j] - df[2, j]) *
          seg/(seg + CGap)
        }
      }
      polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i -
      2], border = pcols[i - 2], density = pdensities[i -
      2], col = pfcols[i - 2])
      points(xx * scale, yy * scale, pch = ptys[i - 2],
      col = pcols[i - 2])
    }
  }
}
