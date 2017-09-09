\name{int2}
\alias{NdM}
\alias{int2ASCII}
\alias{int2B}
\alias{int2Oct}
\alias{int2Hex}
\alias{strRound}
\title{convert integers, string to integer vector}
\description{ Functions for conversion to string representation of
  integers to arbitrary bases
}
\usage{
  NdM( x, B=10 )
  int2ASCII( n)
  int2B( n, B=10, space, plus=lead, lead="",just=c("left","right","center","none") )
  int2Oct( n )
  int2Hex( n )
  strRound( str, digits = getOption("digits"), B=10)
}
\arguments{
  \item{str}{String representing a real}
  \item{n}{Integer vector}
  \item{B}{1 < integer < 17, base of representation}
  \item{space}{Integer, space for conversion}
  \item{plus}{string for signifying positive values, usually "" or "+" }
  \item{lead}{string for insertion between sign and first significant
    digit, usually "" or "0"}
  \item{just}{String for choosing kind of justification within 'space',
    partial matching allowed}
  \item{x}{Vector of reals}
  \item{digits}{no. of digits for roeunding}
}
\value{
  \code{NdM} maximum number of decimal places needed for trunc(x)\cr
  \code{int2ASCII,int2B,int2Oct,int2Hex} vector of strings  represented by 'n'\cr
  \code{strRound} real, represented by x
}
\details{
  \code{int2Oct} Convert integer to octal representation.\cr
  \code{int2Hex} Convert integer to hex representation
}
\examples{
  NdM(10^(1:4)) # 5
  int2ASCII(1:255)[121:129] # "x" "y" "z" "{" "|" "}" "~" "\177" "\200"
  int2B(1:50,2) # all of same length
  int2B(1:50*(-1)^(1:50),just="r") # left flush
  unlist(sapply(1:50,int2B,2,just="l")[1,] ) # individual lengths
  unlist(sapply(1:50,int2B,7)[1,] ) # individual lengths
  unlist(sapply(1:50,int2B)[1,] )
  unlist(sapply(1:50,int2Oct)[1,] )
  unlist(sapply(1:50,int2Hex)[1,] )
  strRound(pi*10^4,0)/10^4 == strRound(pi,4) # TRUE
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{arith}
