

getname <- function(ex,kk=1) { # from string
  Ls <- c(LETTERS,letters)
  NLs <- c(Ls,0:9)
  res <- ""; nc <- nchar(ex)
  if (kk<1 | nc==0 | kk>nc) {return(res)}
  ex <- cv(ex)
  while (kk<nc & !(ex[kk] %in% NLs)) {kk <- kk+1}
  mm <- kk
  if (kk<=nc & ex[kk] %in% Ls) { kk <- kk+1
    while (kk<=nc & ex[kk] %in% NLs) {kk <- kk+1}
    res <- ex[mm:(kk-1)]
  }
  vc(res)
} ## getname

getnamebr <- function(ex,kk=1) { # from vector of char
  ## first occurrence of name or (name), return start, end; if none found, end<-0
  Ls <- c(LETTERS,letters)
  NLs <- c(Ls,0:9)
  res <- list(start=kk,end=0)
  nc  <- length(ex)
  if (kk<1 | nc==0 | kk>nc)               {return(res)}
  while (kk<=nc & !(ex[kk] %in% c("(",Ls))) {kk <- kk+1}
  if (kk>nc)                              {return(res)}
## found?
  mm <- kk
  if (ex[mm]=="(") {  # is (
    while (kk<=nc & !(ex[kk] %in% ")")) {kk <- kk+1}
    if (kk<=nc) {
      res <- list(start=mm,end=kk)
    } else res <- list(start=mm,end=0)
  } else { # is name?
    while (kk<=nc & (ex[kk] %in% NLs)) {kk <- kk+1}
    res <- list(start=mm,end=kk-1)
  }
  res
} ## getnamebr

##  getnamebr(cv("+-/"),1) # 1 0
##  getnamebr(cv("+ a"),1) # 3 3
##  getnamebr(cv("+ (a"),1)  # 3 0
##  getnamebr(cv("+ (a)"),1) # 3 5
##  getnamebr(cv("+ a)"),1) # 3 3
##  getnamebr(cv("+ (a+B)"),1) # 3 7

treatbex <- function(ex) { # (m) -> m   (n+p) unchanged
  nc <- length(ex)
  if (nc>2 && ex[1]=="(" && ex[nc]==")") {
    if (!any(c("+","-","*","/") %in% ex[(2):(nc-1)]))
      ex <- ex[-c(1,nc)]
  }
  ex
} ## treatbex

##  vc(treatbex(cv("anc")))  # "anc"
##  vc(treatbex(cv("(anc"))) # "(anc"
##  vc(treatbex(cv("(an)"))) # "an"

treatnamebr <- function(ex,kk=1) {
  be <- getnamebr(ex)
  if (be$start < be$end) ex <- treatbex(ex[be$start:be$end])
  ex
}

##  vc(treatnamebr(cv("+ a"),1))     # "+ a"
##  vc(treatnamebr(cv("+ (a"),1))    # "+ (a"
##  vc(treatnamebr(cv("+ (a)"),1))   # "a"
##  vc(treatnamebr(cv("+ a)"),1))    # "+ a)"
##  vc(treatnamebr(cv("+ (a+B)"),1)) # "(a+B)"

getI <- function(ex,kk=1) {
  res <- list(start=kk,end=0)
  nc  <- length(ex)
  if (kk<1 | nc==0 | kk>nc)               {return(res)}
catn("ex, nc =", ex,", ", nc)
  while (kk<nc & !(ex[kk]=="I" & ex[kk+1]=="(")) {kk <- kk+1}
catn("kk =", kk)
  if (kk>=nc)                              {return(res)}
## found?
  nn <- 1
  mm <- kk
  kk <- kk+2
catn("kk, mm, nn =", kk, mm, nn)
  repeat {
    if (ex[kk]=="(") nn <- nn+1 else
    if (ex[kk]==")") nn <- nn-1
catn("kk, nn =", kk, nn)
    kk <- kk+1
    if (kk>nc | nn==0) break
  }
  if (nn==0) {
    res <- list(start=mm,end=kk-1)
  } else res <- list(start=mm,end=0)
  res
} # getI

##  getI(cv("+ (n)j"),1)    # 1 0
##  getI(cv("+ I(nM)j"),1)  # 3 7
##  getI(cv("+ I(n+M)j"),1) # 3 8
##  getI(cv("+ I(n(+)M)j"),1) # 3 10

treatI <- function(ex) { # I(...) -> ...
  nc  <- length(ex)
  if (nc>2 && ex[1]=="(" && ex[nc]==")") {
    if (!any(c("+","-","*","/") %in% ex[(2):(nc-1)]))
      ex <- ex[-c(1,nc)]
  }
  ex

} # treatI

treatsquare <- function(expr,kk=1) { # (C^2(M)) -> C^2 M
  getb <- function(ex,ii) { # search for "("
    nc <- length(ex)
    ll <- ii
    while (ll<=nc && ex[ll]!="(") { ll <- ll+1 }
    return(ll)
  } ## getb, returns nc+1 if not found
  res <- expr
  nc  <- nchar(expr)
  catn("nc =",nc)
  if (kk<1 | nc==0 | kk>nc-7) {return(expr)}
  ex <- unlist(strsplit(expr,""))
  catn("ex =",ex)
  kk <- getb(ex,kk)
  catn("kk =");print(kk)
  while (kk<=nc-7) {
    catn("while kk =",kk)
    if (ex[kk]=="(") {
      mm <- kk
  catn("mm =",mm,"\n")
      bas <- treatnamebr(ex[(mm+1):nc])
  catn("bas =",bas)
#      bas <- getname(expr,kk)
#      kk <- kk+nchar(bas)+1
      if (kk<=nc-5 && ex[kk]=="^") {
    catn("while kk =",kk, "  getb\n")
        kk <- getb(ex,kk)
        if (kk<=nc-3) {
          pp  <- kk-1  ## 2.(
    catn("pp, kk =",kk, "\n")
          while (ex[kk] != ")") kk <- kk+1
##            bas <- getname(expr,kk)  ## 
##  catn("bas =",bas)
##            kk  <- kk+nchar(bas)+1
          qq  <- kk  ## )
    catn("qq, kk =",kk, "\n")
          retain <- any(c("+","-","*","/") %in% ex[(pp+1):(qq-1)])
    catn("retain =",retain, "\n")
          if (kk<=nc-1 && ex[kk]==")" & ex[kk+1]==")") {
            if (retain) {
              ex <- ex[-c(mm,(kk+1))]
            } else{
              ex <- ex[-c(mm,kk,(kk+1))]
              ex[pp] <- " "
            }
            kk <- getb(ex,kk-1)
            nc <- length(ex)
          }
        }
      }
    }
    res <- paste(ex,collapse="")
  }
  res
} ## treatsquare

treatsquare("9 (rs^3(n)) i")
treatsquare("9 (rs^3(n+w)) i")


getfctbr <- function(form,kk=1) {
        ## find at most fct(n+p)^(m+q) -> fct^m+q (n+p)
  nc   <- length(form)
  mm   <- kk
  res  <- list(f=form, start=mm,end=nc)
  res1 <- getnamebr(form,kk)
  kk   <- res1$end
  if (kk<1 | nc==0 | kk>nc)               {return(res)}
  kk <- kk+1
  if (form[res1$start]!="(") { # fct found
    res1 <- getnamebr(form,kk)
    if (res1$end>0 && form[res1$start]=="I" && form[res1$start+1]=="(") {
#      return(form[c(1:(res1$start-1),res1$start+2:res1$end)]  ??????????
    }
  } else { # (n+p) found
    while (kk<nc & form[kk]!="^") kk <- kk+1
    if (kk<nc) {  # exponent found, remove () if there
      res1 <- getfctbr(form,kk+1)
      kk   <- res1$end
      if (kk<nc) {
        res$end <- if (form[se$start]=="(") res1$end-2
        form <- removebr(form,res1)
      }
    
    } else { # (n+p) found

    }
  }
  list(f=,start=mm,end=end)
} ## getfctbr

simplForm <- function(form) {
  ## A: (name)       -> name       | (n+p) unchanged     :  getnamebr,treatbex
  ## B: fct(name)    -> fct name   | fct(n+p) unchanged  :
  ## C: I(name)      -> name       | I(n+p) -> (n+p)     :
  ## D: fct(name)^k  -> fct^k name | fct(n+p)^k -> fct^k (n+p): A:
  ## E: fct(m)^(n+p) -> fct^n+p m  D:

  form <- deparse(form)
  nc   <- length(form)
  k <- 1
  while (k<=nc) {
    res  <- getfctbr(form,k)
    k    <- res$end+1
    form <- res$f
  }
  form
} ## simplForm


removebr <- function(form,st=1,en=length(form)) {
  if (form[st]=="(" & form[en]==")") {
    res <- list(f=form[(st+1):(en-1)],st=st+1,en=en-1)
  } else {
    res <- list(f=form,st=st,en=en)
  } ##?
  res ##?
}£ ## removebr

##  vc(removebr(cv("hkj"))$f)    # "hkj"
##  vc(removebr(cv("hkj)"))$f)   # "hkj)"
##  vc(removebr(cv("(hkj)"))$f)  # "hkj"

