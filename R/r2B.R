roundB <- function(x, base = 10, rnd = 0) {
  sign(x) * round(abs(x) * base^rnd) * base^(-rnd)
}  # roundB

r2B <- function(x, base = 10, rnd = 0, space = 0, plus = "", lead = "", 
  just = c("right", "left", "center", "none")) {
  stopifnot( 2 <= base & base <= 60 )
  LIMITSMALL <- if ( rnd == 0 ) 0.5 else 1.0 # check for "0" after "."
  Jst  <- match.arg(just)
  Lad  <- if (missing(lead))  lead <- "$" else lead  # placeholder, see 'gsub' below
  Lfin <- is.finite(x)
  dot  <- if ( rnd >= 0.5 )  "." else ""
  rndt <- trunc(rnd)  # change 0.5 to 0
  Ssig <- ifelse(signp(x) == 1, plus, "-")
  nsig <- max(nchar(Ssig), na.rm = TRUE)
  Lx0  <- !is.na(x) & (x == 0)
  Nx   <- length(x)
  Absx <- abs(x)
  Lsml <- ifelse(Lx0 | is.na(x), FALSE, ifelse(Absx < LIMITSMALL, TRUE, FALSE))
  Absx <- Absx + Lsml
     # make '0.x' 1.x to show leading '0's AAAA
  N0   <- Absx * base^rndt
  NN   <- roundB(N0, base )
  if (rndt < 0) {
    NN <- NN * base^(-rndt)
    Lsml <- Lsml | (NN < LIMITSMALL)
    NN <- NN + Lsml
  }
  Lsml <- Lsml | !Lx0 & (NN < LIMITSMALL)
  sig  <- sigplaces(NN, base )
  spce <- if (missing(space)) 
    sig else max(sig, space)
  Ndn  <- Nd(NN, base)
  Ndig <- ifelse(!Lfin, spce, sig)
  Ncol <- max(sig + nsig, spce)
  nspc <- max(Ncol, spce)
  Mm   <- matrix(Lad, nrow = Nx, ncol = Ncol)
  if (nsig) 
    Mm[, 1] <- Ssig
  for (kk in seqm(Ncol, nsig + 1, -1)) {
    # decimal digits, from least significant to most significant
    NNdiv <- NN%/%base
    NNmod <- mod(NN, base)
    Islead0After <- (NNmod == 0) & (kk <= Ncol - Ndig + 1)  # is leading '0' ?
    Mm[, kk] <- ifelse(Islead0After, Lad, HexagesDig[NNmod + 1])
    NN <- NNdiv
  }  # END kk #
  res <- x  # only structure
  first <- max(Ncol - rndt, 1)
  for (ii in 1:Nx) {
    if (!Lfin[ii]) {
      RR <- paste00(NA2str(x[ii]))
    } else if (Lx0[ii]) {
      RR <- if (Jst=="right") "0 " else " 0"
    } else { # now numbers no dot
      if (dot != ".") {
        RR <- paste00(Mm[ii, ])
      } else {
        RR <- paste0(c(Mm[ii, seqm(1, first)], dot, Mm[ii, seqm(first + 
          1, Ncol)]), collapse = "")
      }  # else #
      if (Lsml[ii]) {
        RR <- sub("1", "0", RR)  # compensate AAAAA
      } # Lsml
      RR <- gsub("\\$", "", paste00(RR))  # eliminate placeholder
    }  # numbers
#    if (Jst != "none") {
      res[ii] <- justify(RR, nspc + nchar(dot ), loc = Jst, with = " ")
#    } else res[ii] <- RR
  }  # END ii
  names(res) <- names(x)
  return( list(s=res,base=base) )
}  # r2B

r2Be <- function(x, base=10, space=4,plus =  "+", just=c("right", "left","center", "none") )
{
  stopifnot(2 <= base & base <= 60 )
  stopifnot(4 <= space)
  just <- match.arg(just)
  me <- rbind( x+0.0, normalize( x, base=base ) ) # x, a, e, b
  Ssign <- ifelse(signp(x) == 1,plus,"-")
  Nsign  <- max(nchar(Ssign)) # no. of chars in sign
  
  Ss <- apply(me,2,function(xx) {
    .Lmant <- function( xx ) { # length for mantissa xx 
       return( space - Nsign - sigplaces( xx, base ) - ifelse(xx < 0,1,0) - nchar(EXPCHAR) ) } # "." !
    .rndB <- function( XX ) {
      Lmant <- .Lmant( XX )
      if ( Lmant <= 0 ) {
        res <- justify( paste00( Ssign( x0 ), "0."), space,just,"0" )
      } else {# Lmant
        rndMant <- max( 0, Lmant - 2.0 )  # adjust for "x."
        res <- roundB( XX, base=base, rnd=rndMant )
      }
      return( res )
    } # rndB
    .Eformat <- function( xx, base ) { # 
      mant <- xx[2]; expo <- xx[3]
      repeat {
        rnd  <- max( 0, .Lmant( expo ) - 1.5 )
        rndt <- trunc(rnd)  # change 0.5 to 0
        N0   <- abs( mant )
        manOld <- mant <- roundB(N0, base, rndt )
#            manOld <- mant <- .rndB( mant )
        Lmant  <- .Lmant( mant )
        expOld <- expo
        if ( abs( manOld ) >= base ) { # adjust
          mant <- manOld/base
          expo <- expOld + 1
        } else if ( abs( manOld ) < 1.0/base ) {
          mant <- manOld*base
          expo <- expOld - 1
        } # adjust
        if ( mant == manOld& expo == expOld ) break   
      } # repeat
      return( list( m = mant, e = expo ) )
    } # Eformat

  x0 <- xx[1]
  mantissa <- xx[2]; exponent <- xx[3]
  if ( is.finite( x0 ) ) {
    if ( x0 == 0 )
      return( justify( "0 ", space, loc = just, with = " ") )
    manOld <- mantissa
    expOld <- exponent
#    sigplExpold <- sigplManold <- -1
    expNew <- exponent
    manNew <- mantissa
    SigplX <- sigplaces( x0,base )
    if ( SigplX <= space-1 & abs(exponent) <= space-1)
    { # enough space for *F-format*
      rndFformat <- space - SigplX - Nsign - 0.5
      corr <- ( frac( rndFformat ) == 0.5 ) & manOld != 0
      res <- r2B( xx[1], base=base, rnd=rndFformat, space=space-corr, plus=plus,just=just)$s
    } else {  #  no f-format
      EF <- .Eformat( xx, base )
      mm <- EF$m
      ee <- EF$e
      Lmant <- .Lmant( ee )
      CmanN <- r2B( mm, base=base, 
                space=Lmant,rnd=max( 0, Lmant - 1.5 ),
                plus=plus,just=just)$s
      CexpN <- r2B( ee, base=base )$s
      res <- paste0( CmanN, if (base==10) "e" else EXPCHAR, CexpN )
    } # end no f-format,  expon
    res <- justify( res, space, just, " " )
  } else { # NaN or Inf
    MA  <- justify( NA2str(mantissa), space-1, just, " " )    
    res <- paste0( MA, " " )
  } # NaN
  return( res )
} # function x  
) # apply
  names(Ss) <- NULL
  return ( list(s=Ss,base=base) )
} # r2Be

strB2r <- function( STR , base=10 ) { # "2017-03-21, 10:18:29"
  oldop <- options(warn = -1) # avoid warnings on NA
  on.exit(options(oldop))
Ss <- sapply( STR, function( str ) {
  str <- unlist( str )
  Ss <- gsub('[^\\\\.[:alnum:]*-]\\s*',"",str ) # eliminate blank ??
  mant <-  as.character( unlist( unlist( strsplit (Ss, "e" ) ) ) )
  if (length ( mant ) == 1 )
    mant <-  as.character( unlist( unlist( strsplit (Ss, "z" ) ) ) )
  M11 <- mant[1]
  E12 <- if ( length( mant ) == 2 ) strB2i( mant[2], base ) else NA
  doNaN <- issubstr ( str, "NA" ) | issubstr ( str, "NaN" )  | issubstr ( str, "Inf" )
  if (doNaN) return ( as.numeric( str ) )
  Mant <- unlist( strsplit( M11[1],"\\.") ) # avoid "."
  Mint <- Mant[1]; Mfraction <- Mant[2]
  r1 <- strB2i( Mint, base )
  r2 <- if ( length( Mant ) == 2 ) strB2i( Mfraction, base )/base^nchar( Mfraction) else 0
  res <-  (r1 + signp( r1 )*r2)
  if( !is.na( E12 ) ) res <- res * base^E12
return( res )
    } # function
) # sapply
  names(Ss) <- NULL
  return( Ss )
} # strB2r

strB2i <- function( STR, base=10 ) { # "2017-03-21, 10:18:29"
    stopifnot( !is.list( STR))
  oldop <- options(warn = -1) # avoid warnings on NA
  on.exit(options(oldop))
Ss <- sapply( STR, function( str ) {
  doNaN <- issubstr ( str, "NA" ) | issubstr ( str, "NaN" )  | issubstr ( str, "Inf" )
  if (doNaN) return ( as.numeric( str ) ) 
  vorz <- 1
  dig <- unlist( strsplit ( str, "" ) )
  vorz <- 1
  if ( !is.na( cpos( str, "-" ) ) ) {
#  if ( dig[1] == "-" ) {
    vorz <- -1;
    start <- 2
  } else if (dig[1] == "+" ) { start <- 2 
  } else start <- 1
  legal <- HexagesDig[1:base]
  xx <- 0
  for ( kk in start:length( dig ) ) {
    plac <- which(dig[kk]==legal )
    if( !length( plac ) == 0 ) {
      xx <- xx*base + plac - 1
    } else { # illegal character
      xx <- NaN
      break
    }
  } # kk
  return ( xx*vorz )
    } # function
) # sapply
  names(Ss) <- NULL
  return( Ss )
} # strB2i
