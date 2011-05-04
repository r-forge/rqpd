"rqpd" <-
function(formula, panel = panel(), data=parent.frame(), na.action, subset, 
    contrasts=NULL, control = NULL, ...)
{

  # "Reveal" options in panel  ## Also 223--225 & 263--264
  for (opt in 1:length(panel)) 
    assign(names(panel)[opt], panel[[opt]])
  # This is clever, but the reader has no idea at this point what variables
  # are set by this loop -- Ok, how about this, then:

  # opts <- c("method", "taus", "tauw", "lambda", "cre", "ztol")
  # if (!all(names(panel) %in% opts) | length(panel) != length(opts))
  #   stop("Unexpected input in 'panel'. Use panel() function.")
  # for (opt in opts) assign(opt, panel[[opt]])

  # Process and check Formula according to method.
  oformula <- formula # original formula
  formula  <- switch(class(formula)[1], Formula=formula, Formula(formula))
  if (method == "pfe") {
    # Expected layout: y ~ x1 + ... | s
    if (any(length(formula) != c(1, 2)) | 
        length(all.vars(formula(formula, lhs=0, rhs=2))) > 1)
      stop("Formula must on the form 'y ~ x + ... | s', see documentation.")
  } else if (method == "cre") {
    # Expected layout: y ~ x + ... | s | z + ...
    if (any(length(formula) != c(1, 3)) | 
        length(all.vars(formula(formula, lhs=0, rhs=2))) > 1)
      stop(paste("Formula must be on the form 'y ~ x + ... | s | z + ...'",
              "see documentation."))
  } else { 
    stop("Formula processing not implemeted for this method.")
  }

  ## Process Call
  call <- match.call()
  m    <- match.call(expand = FALSE)
  temp <- c("", "formula", "data", "subset", "na.action")
  m    <- m[match(temp, names(m), nomatch = 0)]

  ## Evaluate model.frame
  m$formula <- formula
  m[[1L]]   <- as.name("model.frame")
  m         <- eval(m, parent.frame())

  y <- model.response(m, "any")

  ## Make X matrix
  require(MatrixModels)
  mt  <- terms(formula, data = data)
  mtX <- terms(formula, data = data, rhs = 1L)
  if(((lambda <= 0)) && method == "pfe")
        attr(mtX, "intercept") <- 0L
  X <- model.Matrix(mtX, m, contrasts, sparse = TRUE)
  Xnames <- dimnames(X)[[2]]
  X <- as(X ,"matrix.csr")

  ## Make Z matrix
  rhs.idx <- switch(method, pfe=2L, cre=c(2L, 3L), stop("Not implemented.")) 
  mtZ <- delete.response(terms(formula, data = data, rhs = rhs.idx))
  attr(mtZ, "intercept") <- 0L
  ids <- m[,attr(mtZ,"term.labels")[1]]
  Z <- switch(method,
      pfe = model.Matrix(mtZ, m, sparse = TRUE),
      cre = model.cre(mtZ, m, ids, contrasts, panel$cre),
      stop("Not implemented."))
  Znames <- dimnames(Z)[[2]]
  Z <- as(Z ,"matrix.csr")

  ## Additional method-specific checking
  if (method == "pfe") {
    if (det(t(X) %*% X) < ztol) stop("Singular design.")
    # Perhaps deal with lambda=0, and singular design here.
  } else if (method == "cre") {
    if (det(t(cbind(X, Z)) %*% cbind(X, Z)) < ztol) stop("Singular design.")
  } 

  ## Set default control parameters
  if(is.null(control)) control <- sfn.control(warn.mesg = FALSE)

  ## Call fitting routines:
  f <- switch(method, 
      pfe = rqpd.fit.pfe(X, Z, y, taus, tauw, lambda, control, ...),
      cre = rqpd.fit.cre(X, Z, y, taus, control, ...),
      stop("No fitting routine for method."))

  fit <- list(coefficients = f$coef, residuals = f$resid)
  class(fit) <- c(paste("rqpd.", method, sep=""), "rqpd")
  
  if (method == "pfe") {
    names(fit$coef) <- c(paste(rep(Xnames, length(taus)), "[",
      rep(taus, each=length(Xnames)),"]", sep = ""), Znames)
  } else if (method == "cre") {
    names(fit$coef) <- paste(rep(c(Xnames, Znames), length(taus)), "[",
      rep(taus, each=length(Xnames)+length(Znames)),"]", sep = "")
  }

  fit$X       <- X
  fit$Z       <- Z
  fit$y       <- y
  fit$ids     <- ids
  fit$panel   <- panel
  fit$control <- control
  fit$formula <- oformula
  fit$call    <- call
  fit$rank    <- sum(abs(f$coef) > ztol)
  attr(fit, "na.message") <- attr(m, "na.message")
  fit
}

"panel" <-
function(method="pfe", taus=1:3/4, tauw=c(.25,.5,.25), 
    lambda=1, cre="m", ztol=1e-5)
{
  # Argument validating and defaults:
  method <- switch(tolower(method), pfe="pfe", cre="cre", "pfe")
  eps    <- .Machine$double.eps^(2/3)
  taus   <- pmin(pmax(taus, eps), 1-eps)  # eps < tau < 1-eps
  tauw   <- abs(tauw)/sum(abs(tauw))      # positive tauw summing to 1.
  lambda <- pmax(0, abs(lambda))[1]       # single non-negative number
  cre    <- switch(tolower(cre), m="m", crem="m", ad="ad", "m")
  ztol   <- pmax(pmin(ztol, 1), 0)[1]        # between 0 and 1?

  if (method == "pfe" & length(tauw)!=length(taus))
    stop("lengths of tauw and taus do not match.")

  # return a list
  list(method=method, taus=taus, tauw=tauw, lambda=lambda, cre=cre, ztol=ztol)
}

"rqpd.fit.cre" <- 
function(X, Z, y, taus, control, ...)
{
  p <- ncol(X) + ncol(Z)
  K <- length(taus)
  res <- do.call(cbind, lapply(taus, function(tau) {
      tmp <- rq.fit.sfn(cbind(X, Z), y, tau=tau, control=control) 
      c(tmp$it, tmp$ierr, tmp$coef)
    }))  
  fit <- list(coefficients=c(res[-(1:2),]), it=res[1,], ierr=res[2,])
  if(any((fit$ierr != 0) && (fit$it < 5))) 
    warning(paste("Dubious convergence:", sfnMessage(fit$ierr)))
  fit$residuals <- rep(y, K) - 
    (as(K, "matrix.diag.csr") %x% cbind(X, Z)) %*% fit$coef
  fit$contrasts <- attr(X, "contrasts")
  fit
}

"rqpd.fit.pfe" <- 
function(X, Z, y, taus, tauw, lambda, control, ...)
{
    N <- length(y)
    p <- ncol(X)
    n <- ncol(Z) 
    K <- length(taus)
      
    y <- tauw %x% y 
    D <- cbind(as(tauw, "matrix.diag.csr") %x% X, tauw %x% Z)
    rhs <- c((tauw*(1 - taus)) %x% (t(X)%*%rep(1, N)),
        sum(tauw*(1 - taus)) * (t(Z) %*% rep(1, N)))
    if (lambda > 0) {
        D <- rbind(D, cbind(as.matrix.csr(0, n, K*p), lambda*as(n, "matrix.diag.csr")))
        y <- c(y, rep(0, n))
        rhs <- rhs + c(rep(0,p*K), lambda * rep(1/2, n))
        }
    f <- rq.fit.sfn(D, y, rhs = rhs, control = control)

    if((f$ierr != 0) && (f$it < 5)) 
        warning(paste("Dubious convergence:", sfnMessage(f$ierr)))
    fit <- list(coefficients = f$coef, ierr = f$ierr, it = f$it)
    fit$contrasts <- attr(X, "contrasts")
    fit$resid <- c(y - D %*% fit$coef)
    fit
}

"print.rqpd" <-
function(x, ...)
{
  taus <- x$panel$taus

	if(!is.null(cl <- x$call)) {
		cat("Call:\n")
		dput(cl)
	}
	coef <- coef(x)[1:(ncol(x$X)*length(taus))]
	cat("\nCoefficients:\n")
	print(coef, ...)
	rank <- x$rank
	nobs <- length(x$y)
	rdf <- nobs - rank
	cat("\nDegrees of freedom:", nobs, "total;", rdf, "residual\n")
	if(!is.null(attr(x, "na.message")))
		cat(attr(x, "na.message"), "\n")
	invisible(x)
}

"print.rqpd.cre" <-
function(x, ...)
{
	if(!is.null(cl <- x$call)) {
		cat("Call:\n")
		dput(cl)
	}
	coef <- coef(x)
	cat("\nCoefficients:\n")
	print(coef, ...)
	rank <- x$rank
	nobs <- length(x$y)
	rdf <- nobs - rank
	cat("\nDegrees of freedom:", nobs, "total;", rdf, "residual\n")
	if(!is.null(attr(x, "na.message")))
		cat(attr(x, "na.message"), "\n")
	invisible(x)
}

"coef.rqpd" <- 
function(object, ...) object$coef

"boot.rqpd"<-
function (ids, X, Z, y, panel, control, 
	R = 200, bsmethod = "wxy", ...)
{
  # "Reveal" options in panel
  for (opt in 1:length(panel)) 
    assign(names(panel)[opt], panel[[opt]])

  p <- switch(method,
      pfe = ncol(X) * length(taus) + ncol(Z),
      cre = (ncol(X) + ncol(Z))*length(taus),
      stop("Bootstrap not implemented for method."))
  L <- length(levels(as.factor(ids)))
  M <- table(ids)

  B <- matrix(NA, R, p)
  if (bsmethod == "wxy") {
    for(i in 1:R){
      w <- rep(rexp(L, 1), M)
      B[i,] <- switch(method,
        pfe = rqpd.fit.pfe(w*X, w*Z, w*y, taus, tauw, lambda, control)$coef,
        cre = rqpd.fit.cre(w*X, w*Z, w*y, taus, control)$coef,
        stop("Method not implemented."))
    }
  } else stop("Your specified bootstrap method is not implemented")

  # Alternative to lines 234--243 with multicore option.
  # If multicore is loaded and cores are set, e.g. options(cores=XX).

# if (!bsmethod == "wxy") stop("The specified bsmethod if not implemented.")
# applyfun <- if ("multicore" %in% .packages()) mclapply else lapply
# B <- do.call(rbind, applyfun(1:R, function(iter) {
#   w <- rep(rexp(L, 1), M)
#   switch(method,
#     pfe = rqpd.fit.pfe(w*X, w*Z, w*y, taus, tauw, lambda, control)$coef,
#     cre = rqpd.fit.cre(w*X, w*Z, w*y, taus, control)$coef,
#     stop("Method not implemented."))
# }))

  B
}

"summary.rqpd" <- function (object, se = "boot", covariance = FALSE, ...)
{
  for (elem in 1:length(object)) 
    assign(names(object)[elem], object[[elem]])
  
  cnames <- names(coef)
  n <- length(y)
  p <- length(coef)
  rdf <- n - rank
  if(NCOL(ids) > 1) stop("Only one-way layouts for ids allowed for summary")
  if (se == "boot") {
      B <- boot.rqpd(ids, X, Z, y, panel, control, ...)
      cov <- cov(B)
      serr <- sqrt(diag(cov))
  }
  else stop("Only boot method for rqpd objects")
  coef <- array(coef, c(p, 4))
  dimnames(coef) <- list(cnames, 
      c("Value", "Std. Error", "t value", "Pr(>|t|)"))
  coef[, 2] <- serr
  coef[, 3] <- coef[, 1]/coef[, 2]
  coef[, 4] <- if(rdf > 0) 2 * (1 - pt(abs(coef[, 3]), rdf)) else NA
  object <- object[c("call")]
  if (covariance == TRUE) object$cov <- cov 
  object$coefficients <- coef
  object$rdf <- rdf
  object$panel <- panel
  object$ncolX <- ncol(X)
  class(object) <- c(paste("summary.rqpd.", panel$method, sep=""), 
      "summary.rqpd")
  object
}

"print.summary.rqpd" <-
function(x, digits = max(5, .Options$digits - 2), ...)
{
  taus <- x$panel$taus
  tauw <- x$panel$tauw
	cat("\nCall: ")
	dput(x$call)
	coef <- x$coef[1:(x$ncolX*length(taus)), ]
	cat("\ntaus: ")
	print(format(round(taus, digits = digits)), quote = FALSE, ...)
	cat("\ntau weights: ")
	print(format(round(tauw, digits = digits)), quote = FALSE, ...)
	cat("\nCoefficients:\n")
	print(format(round(coef, digits = digits)), quote = FALSE, ...)
	invisible(x)
}

"print.summary.rqpd.cre" <-
function(x, digits = max(5, .Options$digits - 2), ...)
{
  taus <- x$panel$taus
	cat("\nCall: ")
	dput(x$call)
	coef <- x$coef
	cat("\ntaus: ")
	print(format(round(taus, digits = digits)), quote = FALSE, ...)
	cat("\nCoefficients:\n")
	print(format(round(coef, digits = digits)), quote = FALSE, ...)
	invisible(x)
}
