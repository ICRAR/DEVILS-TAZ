blurCOSMIC<-function (x, sigma = NULL, ..., normalise = FALSE, bleed = TRUE, 
          varcov = NULL) 
{
  stopifnot(is.im(x))
  sigma.given <- !is.null(sigma)
  varcov.given <- !is.null(varcov)
  if (sigma.given) {
    stopifnot(is.numeric(sigma))
    stopifnot(length(sigma) %in% c(1, 2))
    stopifnot(all(sigma > 0))
  }
  if (varcov.given) 
    stopifnot(is.matrix(varcov) && nrow(varcov) == 2 && ncol(varcov) == 
                2)
  ngiven <- varcov.given + sigma.given
  switch(ngiven + 1L, {
    sigma <- (1/8) * min(diff(x$xrange), diff(x$yrange))
  }, {
    if (sigma.given && length(sigma) == 2) varcov <- diag(sigma^2)
    if (!is.null(varcov)) sigma <- NULL
  }, {
    stop(paste("Give only one of the arguments", sQuote("sigma"), 
               "and", sQuote("varcov")))
  })
  X <- fillNA(x, 0)
  Y <- second.moment.calc(X, sigma = sigma, ..., varcov = varcov, 
                          what = "smooth")
  if (!bleed) 
    Y$v[is.na(x$v)] <- NA
  if (!normalise) 
    return(Y)
  Xone <- x
  isna <- is.na(x$v)
  Xone$v[isna] <- 0
  Xone$v[!isna] <- 1
  Ydenom <- second.moment.calc(Xone, sigma = sigma, ..., varcov = varcov, 
                               what = "smooth")
  Z <- eval.im(Y/Ydenom)
  return(Z)
}