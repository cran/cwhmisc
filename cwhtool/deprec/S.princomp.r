S.princomp <- function(form,data,cor,na.action) {
  if (class(form)=="formula") {
    ind <- formula2Rterm.names(form)
    x <- data.frame(data[,ind])
  } else x <- form
  na.act <- deparse(substitute(na.action))
  if (na.act=="na.omit") {
    ok <- complete.cases(x)
  } else ok <- TRUE
  princomp(x[ok,],cor=cor)
}

S.summary.princomp <- function(object, loadings = FALSE, cutoff = 0.1, ...)

	sdev <- object$sdev
	vars <- sdev^2
	vars <- vars/sum(vars)
	if(loadings) {
		if(is.numeric(loadings)) {
			loadings <- min(loadings, dim(object$loadings)[2])
			lds <- object$loadings[, 1:loadings, drop = FALSE]
		}
		else lds <- object$loadings
	}
	else lds <- NULL
	varmat <- rbind("Standard deviation" = sdev, "Proportion of Variance"
		 = vars, "Cumulative Proportion" = cumsum(vars))
	ans <- list(varmat = varmat, loadings = lds, cutoff = cutoff)
	class(ans) <- "summary.princomp"
	ans
}

print.loadings1 <- function (x, digits = 3, cutoff = 0.1, sort = FALSE, ...) 
{
    Lambda <- unclass(x)
    p <- nrow(Lambda)
    factors <- ncol(Lambda)
    if (sort) {
        mx <- max.col(abs(Lambda))
        ind <- cbind(1:p, mx)
        mx[abs(Lambda[ind]) < 0.5] <- factors + 1
        Lambda <- Lambda[order(mx, 1:p), ]
    }
    cat("\nLoadings:\n")
    fx <- format(round(Lambda, digits))
    names(fx) <- NULL
    nc <- nchar(fx[1])
    fx[abs(Lambda) < cutoff] <- paste(rep(" ", nc), collapse = "")
    print(fx, quote = FALSE, ...)
    invisible(x)
}

S.rotate.princomp <- function(y, ..., n = dim(xmat)[2])
{ x <- y
	xmat <- loadings(x)
	if(length(attr(xmat, "tmat")))
		if(n != dim(attr(xmat, "tmat"))[1])
			stop("parameter 'n' must be identical for oblique rotations"
				)
	xmat <- xmat[, 1:n, drop = FALSE]
	tmat <- loadings(x)
	attr(xmat, "correlation") <- attr(tmat, "correlation")
	attr(xmat, "tmat") <- attr(tmat, "tmat")
	newrot <- S.rotate.default(xmat, ...)
	x$loadings[, 1:n] <- newrot$rmat
	if(length(newrot$correlation)) {
		attr(x$loadings, "correlation") <- newrot$correlation
		attr(x$loadings, "tmat") <- newrot$tmat
	}
	if(length(scor <- x$scores)) {
		sa <- attributes(scor)
		if(length(newrot$correlation))
			scor[, 1:n] <- NA
		else {
			scorminor <- scor[, 1:n, drop = FALSE]
			nobs <- dim(scorminor)[1] - 1
			ssq <- sqrt(nobs/(nobs + 1))/x$factor.sdev[1:n]
			qq <- newrot$tmat/ssq
			ssq2 <- sqrt(apply(qq * qq, 2, sum))
			scorminor <- scorminor %*% (ssq * t(t(newrot$tmat) * 
				ssq2))
			x$coef[, 1:n] <- x$coef[, 1:n] %*% (ssq * t(t(newrot$
				tmat) * ssq2))
			scor[, 1:n] <- scorminor
			x$factor.sdev[1:n] <- ssq2 * sqrt(nobs/(nobs + 1))
		}
		attributes(scor) <- sa
		x$scores <- scor
	}
	x$sdev[1:n] <- NA
	this.call <- match.call()
	this.call[["x"]] <- x$call
	x$call <- this.call
	x
}

S.rotate.default <- function(x, rotation = NULL, orthogonal = TRUE, parameters = NULL, normalize = TRUE)
{	choices <- c("varimax", "quartimax", "equamax", "parsimax", "orthomax", 
		"covarimin", "biquartimin", "quartimin", "oblimin", 
		"crawford.ferguson", "procrustes", "promax", "none")
	dx <- dim(x)
	p <- dx[1]
	k <- dx[2]
	lpar <- length(parameters)
	procrustflag <- FALSE
	if(lrotation <- length(rotation)) {
		if(lrotation > 1) {
			rotation <- rotation[1]
			warning("only the first element of \"rotation\" used")
		}
		index <- charmatch(rotation, choices, nomatch = NA)
		if(is.na(index))
			stop(paste("the rotation \"", rotation, 
				"\" doesn't match any known rotation", sep = ""
				))
		if(index == 0) {
			minds <- match(substring(choices, 1, last = nchar(
				rotation)), rotation, nomatch = 0) > 0
			stop(paste("ambiguous match for rotation, matches:", 
				paste(choices[minds], collapse = ", ")))
		}
		rotation <- choices[index]
		switch(rotation,
			varimax = {
				orthogonal <- TRUE
				parameters <- 1
				if(lpar)
				  warning("input parameters ignored")
			}
			,
			quartimax = {
				orthogonal <- TRUE
				parameters <- 0
				if(lpar)
				  warning("input parameters ignored")
			}
			,
			equamax = {
				orthogonal <- TRUE
				parameters <- k/2
				if(lpar)
				  warning("input parameters ignored")
			}
			,
			parsimax = {
				orthogonal <- TRUE
				parameters <- (p * (k - 1))/(p + k - 2)
				if(lpar)
				  warning("input parameters ignored")
			}
			,
			orthomax = {
				orthogonal <- TRUE
				if(!lpar)
				  parameters <- 1
			}
			,
			covarimin = {
				orthogonal <- FALSE
				parameters <- c(1/p, 1, -1/p, -1)
				if(lpar)
				  warning("input parameters ignored")
			}
			,
			biquartimin = {
				orthogonal <- FALSE
				parameters <- c(0.5/p, 1, -0.5/p, -1)
				if(lpar)
				  warning("input parameters ignored")
			}
			,
			quartimin = {
				orthogonal <- FALSE
				parameters <- c(0, 1, 0, -1)
				if(lpar)
				  warning("input parameters ignored")
			}
			,
			oblimin = {
				orthogonal <- FALSE
				if(lpar != 4)
				  if(lpar == 1)
				    parameters <- c(parameters/p, 1,  - 
				      parameters/p, -1)
				  else parameters <- c(1/p, 1, -1/p, -1)
			}
			,
			crawford.ferguson = {
				orthogonal <- FALSE
				if(lpar != 4)
				  if(lpar == 2)
				    parameters <- c(0, parameters[1], 
				      parameters[2],  - sum(parameters))
				  else {
				    if(lpar)
				      stop(
				        "parameters for Crawford-Ferguson must be of length 2 or 4"
				        )
				    parameters <- c(0, 1, 0, -1)
				  }
			}
			,
			procrustes = {
				if(!is.matrix(parameters))
				  stop(
				    "must give a matrix as a target for procrustes rotations"
				    )
				procrustflag <- TRUE
			}
			,
			promax = {
				switch(lpar + 1,
				  {
				    orthpar <- 1
				    pow <- 4
				  }
				  ,
				  {
				    orthpar <- parameters[1]
				    pow <- 4
				  }
				  ,
				  {
				    orthpar <- parameters[1]
				    pow <- parameters[2]
				  }
				  ,
				  stop(
				    "bad length for parameters for the promax rotation"
				    ))
				target <- rotate.default(x, orthog = TRUE, 
				  normalize = normalize, parameters = orthpar)$
				  rmat
				parameters <- sign(target) * abs(target)^pow
				procrustflag <- TRUE
			}
			,
			none = return(list(rmat = x)))
	}
	else {
		if(orthogonal) {
			if(!lpar)
				parameters <- 1
			else if(lpar > 1)
				stop(paste("bad length (", lpar, 
				  ") for parameters", sep = ""))
		}
		else {
			if(lpar != 4) {
				if(lpar == 1)
				  parameters <- c(parameters/p, 1,  - 
				    parameters/p, -1)
				else if(lpar == 0)
				  parameters <- c(1/p, 1, -1/p, -1)
				else stop(paste("bad length (", lpar, 
				    ") for parameters", sep = ""))
			}
		}
	}
	if(procrustflag)
		return(procrustes(x, target = parameters, orthogonal = orthogonal))
	if(orthogonal) {
#		orthomax(x, gamma = parameters, normalize = normalize)
        }  
	else obliquemin(x, kappa = parameters, normalize = normalize)
}

#orthomax <- function(amat, gamma = 1, normalize = TRUE, iter.max = 100, eps = 1e-05)
#{
#	if(length(attr(amat, "correlation")))
#		stop("orthogonal rotation of obliquely rotated loadings not allowed"
#			)
#	dA <- dim(amat)
#	if(length(dA) != 2 || prod(dA) != length(amat))
#		stop("amat must be a matrix")
#	if(length(gamma) > 1) {
#		gamma <- gamma[1]
#		warning("only the first element of gamma used")
#	}
#	storage.mode(amat) <- "double"
#	value <- .C("S_orthomax",
#		as.integer(dA[1]),
#		as.integer(dA[2]),
#		rmat = amat,
#		gamma = as.double(gamma),
#		as.double(eps),
#		normalize = as.integer(normalize),
#		iterations = as.integer(iter.max))[c("rmat", "gamma", 
#		"normalize", "iterations")]
#	value$tmat <- solve(amat, value$rmat)
#	value$normalize <- normalize
#	value$orthogonal <- TRUE
#	value
#}

