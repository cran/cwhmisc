\name{Julian date}
\alias{Dat2Jul}
\alias{Jul2Dat}
\alias{monthsN}
\alias{Mnames}
\alias{Dnames}
\alias{mdiny}
\alias{Wday}
\alias{Yday}
\title{calender conversions}
\description{
 calender conversions,  Julian day number from civil date and back,
 names of months, weekdays.
}
\usage{
  Dat2Jul( yr, mo, dy, hr=12 )
  Jul2Dat( JD )
  monthsN( leap )
  Mnames
  Dnames
  mdiny( dk, leap )
  Wday( JD )
  Yday( mo, dy, leap )
}
\arguments{
  \item{yr, mo, dy}{integer, year, month, day of date}
  \item{hr, JD}{real, hrs, Julian date}
  \item{leap}{Boolean, = is concerning year a leap year ?}
  \item{dk}{Integer }
}
\value{
  \code{Dat2Jul:} JD, year Y BC is to be given as -|Y-1|, e.g. 4 BC = -3 !!\cr
  \code{Jul2Dat:} date ( year, month, day, hours ).\cr
  \code{ monthsN:}  cumulative sum of days in months.\cr
  \code{ Mnames:}  names of months.\cr
  \code{ Dnames:}  names of weekdays.\cr
  \code{ mdiny:}  c(  number of month, day in (leap) year ).\cr
  \code{ Wday:}   name of weekday from dk mod 7.
  \code{ Yday:}   number of day, from 0 = Jan 1.
}
\examples{
 Dat2Jul(-4712,01,01)  # =     0  i.e. Start of Julian day numbering
 Jul2Dat( 0 )          #  -4712    1    1   12
 formatFix(Dat2Jul(c(1582,10,4,23+59/60)),8,10) # 2299160.499.. = last day of Julian calendar
 Jul2Dat( 2299160 )          # 1582  10  4   12
 Dat2Jul(c(2000,3,2))   # 2451606
 Jul2Dat(2451606.0)    # 2000    3    2   12
 mdiny(1,TRUE)    #  1 1
 mdiny(60,TRUE)   #  2 29
}
\note{ See also\cr
       http://www.onlineconversion.com/julian_date.htm\cr
  http://en.wikipedia.org/wiki/Julian_day#Converting_Julian_or_Gregorian_calendar\cr
  _date_to_Julian_Day_Number\cr\cr
Julian date is a continuous numbering of days since the biblical day of creation in 4713 BC, Jan. 1, 12 hours. The Julian calendar date 1582 Oct 4 was succeeded by the Gregorian calendar date 1582 Oct 15.\cr
       Conversion of Julian and Gregorian dates to Julian day number is done by \code{Dat2Jul}. The reverse is done by \code{Jul2Dat}.
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{math}
