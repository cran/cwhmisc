dtweedie <- function(y, mu, phi=1, power=1.5)
{
#  Density or probability mass function for Tweedie
#  exponential dispersion models.
#
#  mu is the mean, and phi is the dispersion of the distribution
#  We have var(y) = phi * mu^p
#
#  GKS  3 June 98.  Last revised 10 Nov 99.
#
        if(power == 0) {
                return( dnorm(y, mu, phi) )
        }
        if(power == 1) {
                return( dpois(y/phi, mu/phi) )
        }
        if(power>1 & power<2) {
                return( dpoisgam(y, mu, phi, power) )
        }
        if(power == 2) {
                alpha <- 1/phi
                beta <- mu/alpha
                return( dgamma(y/beta, alpha)/beta )
        }
        if(power == 3) {
                return( dinvgauss(y, mu, 1/phi) )
        }
        saddle.tweedie(y, mu, phi, power)
}

ptweedie <- function(q, mu, phi=1, power=1.5)
{
#  Cumulative distribution function for Tweedie
#  exponential dispersion models.
#
#  mu is the mean, and phi is the dispersion of the distribution
#  We have var(y) = phi * mu^p
#
#  GKS  30 Mar 99
#
        if(power == 0) {
                return( pnorm(q, mu, phi) )
        }
        if(power == 1) {
                return( ppois(q/phi, mu/phi) )
        }
        if(power>1 & power<2) {
                return( ppoisgam(q, mu, phi, power) )
        }
        if(power == 2) {
                alpha <- 1/phi
                beta <- mu/alpha
                return( pgamma(q/beta, alpha) )
        }
        if(power == 3) {
                return( pinvgauss(q, mu, 1/phi) )
        }
#
#       Modified residual r* tail approximation
        tail.tweedie(q, mu, phi, power)
}

qtweedie <- function(p, mu, phi=1, power=1.5)
{
#  Quantiles for Tweedie exponential dispersion models
#
#  mu is the mean and phi is the dispersion of the distribution
#  We have var(y) = phi * mu^power
#
#  This function is currently reasonable only for the normal, Poisson, gamma
#  and inverse-Gaussian special cases.
#
#  GKS  28 Mar 99, 4 Oct 99
#
        if(power == 0) {
                return( qnorm(p, mu, phi) )
        }
        if(power == 1) {
                return( phi * qpois(p, mu/phi) )
        }
        if(power == 2) {
                alpha <- 1/phi
                beta <- mu/alpha
                return( beta * qgamma(p, alpha) )
        }
        if(power == 3) {
                return( qinvgauss(p, mu, 1/phi) )
        }
#
#       Simple normal approximation - need to improve!
        qnorm(p, mu, phi*mu^power)
}

saddle.tweedie <- function(y, mu, phi, p)
{
#  Saddlepoint approximation for Tweedie densities
#  Gordon Smyth, U of Queensland, gks@maths.uq.edu.au
#  3 June 1998.  Last revised 10 Nov 1999.
#
        y1 <- ifelse(y>0,y,1/6)
        exp( - devi.tweedie(y, mu, p)/2/phi)/sqrt(2 * pi * phi * y1^p)
}

devi.tweedie <- function(y, mu = 1, p = 0)
{
#  Unit deviance of Tweedie family
#  GKS  3 June 98
#
        y1 <- y + (y == 0)
        if(p == 1)
                theta <- log(y1/mu)
        else theta <- (y1^(1 - p) - mu^(1 - p))/(1 - p)
        if(p == 2)
                kappa <- log(y1/mu)
        else kappa <- (y^(2 - p) - mu^(2 - p))/(2 - p)
        2 * (y * theta - kappa)
}

tail.tweedie <- function(x, ..., mu = 1, phi = 1, p = 0)
{
#  Tail area approximation based on modified deviance residual
#  See Jørgensen (1997) page 143
#  Gordon Smyth, U of Queensland, gks@maths.uq.edu.au
#  28 April 2000
#
        if(phi==0) return(NA)
        e <- x-mu
        s <- sqrt(phi)
        z <- x/mu
        u <- mu^(1-p/2) * z^(p/2) * ifelse(p==1,log(z),(z^(1-p)-1)/(1-p))
        r <- sign(e) * sqrt(devi.tweedie(x,mu,p))
        rmod <- r/s + ifelse(abs(e/mu) > 1e-8, s/r*log(u/r), p*s*mu^(p/2-1)/6)
        pnorm(rmod)
}

tweedie <- function(var.power = 0, link.power = 1)
{
#       Tweedie generalized linear model family
#       Gordon Smyth, U of Queensland, gks@maths.uq.edu.au
#  March 1996
#
        if (is.null(sys.call()$link.power)) link.power <- 1 - var.power

        if (link.power != 0) tw.lnk <- list(
                names = "Box-Cox: mu^q",
                link = substitute(
                        function(mu, q = l.p) mu^q,
                        list(l.p = link.power)),
                inverse = substitute(
                        function(eta, q = l.p) eta^(1/q),
                        list(l.p = link.power)),
                deriv = substitute(
                        function(mu, q = l.p) q*mu^(q-1),
                        list(l.p = link.power)),
                initialize = expression({
                        y <- as.numeric(y)
                        mu <- y + (y == 0)/6}))

        if (link.power == 0) tw.lnk <- list(
                names = "Log: log(mu)",
                link = function(mu) log(mu),
                inverse = function(eta) care.exp(eta),
                deriv = function(mu) 1/mu,
                initialize = expression({
                        y <- as.numeric(y)
                        mu <- y + (y == 0)/6}))

        tw.var <- list(
                names = "mu^p",
                variance = substitute(
                        function(mu, p = v.p) mu^p,
                        list(v.p = var.power)),
                deviance = substitute(
                        function(mu, y, w, residuals = F, p = v.p) {
                                y1 <- y + (y == 0)
                                if (p == 1)
                                        theta <- log(y1/mu)
                                else
                                        theta <- ( y1^(1-p) - mu^(1-p) ) / (1-p)
                                if (p == 2)
                                        kappa <- log(y1/mu)
                                else
                                        kappa <- ( y^(2-p) - mu^(2-p) ) / (2-p)
                                devi <- y*theta - kappa
                                if(residuals) sign(y-mu)*sqrt(2*w*abs(devi)) else 2*sum(w*devi)},
                        list(v.p = var.power)))

        make.family("Tweedie", link = tw.lnk, variance = tw.var)
}

##  glm.weight <- function(link, variance)
##  {
##  #  This function fixes a bug in S-Plus 2000 Release 1.
##  #  It is not required in earlier or later versions of S-Plus.
##  #  Gordon Smyth, U of Queensland, gks@maths.uq.edu.au
##  #  5 Nov 1999.
##  #
##          default <- expression(w/((sqrt(family$variance(mu)) * family$deriv(mu))^2))
##          dnames <- dimnames(glm.weights)
##          if(!match(link, dnames[[1]], F))
##                  return(default)
##          if(!match(variance, dnames[[2]], F))
##                  return(default)
##          ww <- glm.weights[link, variance]
##          if(as.character(ww) == "NULL")
##                  default
##          else ww
##  }
