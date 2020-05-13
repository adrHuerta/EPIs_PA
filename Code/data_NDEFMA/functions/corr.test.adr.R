corr.test.adr<-function (x, y = NULL, use = "pairwise", method = "pearson", 
          adjust = "holm", alpha = 0.05) 
{
  cl <- match.call()
  if (is.null(y)) {
    r <- cor(x, use = use, method = method)
    sym <- TRUE
    n <- t(!is.na(x)) %*% (!is.na(x))
  }
  else {
    r <- cor(x, y, use = use, method = method)
    sym = FALSE
    n <- t(!is.na(x)) %*% (!is.na(y))
  }
  if ((use == "complete") | (min(n) == max(n))) 
    n <- min(n)
  t <- (r * sqrt(n - 2))/sqrt(1 - r^2)
  p <- 2 * (1 - pt(abs(t), (n - 2)))
  se <- sqrt((1 - r * r)/(n - 2))
  nvar <- ncol(r)
  p[p > 1] <- 1
  if (adjust != "none") {
    if (is.null(y)) {
      lp <- upper.tri(p)
      pa <- p[lp]
      pa <- p.adjust(pa, adjust)
      p[upper.tri(p, diag = FALSE)] <- pa
    }
    else {
      p[] <- p.adjust(p, adjust)
    }
  }
  z <- fisherz(r[lower.tri(r)])
  if (min(n) < 4) {
    warning("Number of subjects must be greater than 3 to find confidence intervals.")
  }
  alpha <- 1 - alpha/2
  dif <- qnorm(alpha)
  if (sym) {
    if (is.matrix(n)) {
      se <- 1/sqrt(n[lower.tri(n)] - 3)
    }
    else {
      se <- 1/sqrt(n - 3)
    }
    lower <- fisherz2r(z - dif * se)
    upper <- fisherz2r(z + dif * se)
    ci <- data.frame(lower = lower, r = r[lower.tri(r)], 
                     upper = upper, p = p[lower.tri(p)])
    cnR <- abbreviate(colnames(r), minlength = 20)
    k <- 1
    for (i in 1:(nvar - 1)) {
      for (j in (i + 1):nvar) {
        rownames(ci)[k] <- paste(cnR[i], cnR[j], sep = "-")
        k <- k + 1
      }
    }
  }
  else {
    z <- fisherz(r)
    se <- 1/sqrt(n - 3)
    lower <- as.vector(fisherz2r(z - dif * se))
    upper <- as.vector(fisherz2r(z + dif * se))
    ci <- data.frame(lower = lower, r = as.vector(r), upper = upper, 
                     p = as.vector(p))
    cnR <- abbreviate(rownames(r), minlength = 20)
    cnC <- abbreviate(colnames(r), minlength = 20)
    k <- 1
    for (i in 1:ncol(y)) {
      for (j in 1:ncol(x)) {
        rownames(ci)[k] <- paste(cnR[j], cnC[i], sep = "-")
        k <- k + 1
      }
    }
  }
  result <- list(r = r, n = n, t = t, p = p, se = se, adjust = adjust, 
                 sym = sym, ci = ci, Call = cl)
  class(result) <- c("psych", "corr.test")
  return(result)
}
