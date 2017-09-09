\name{Julian date}
\alias{Dat2Jul}
\alias{Jul2Dat}
\alias{isLeap}
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
  \item{leap}{Boolean, = is given year a leap year ?}
  \item{dk}{integer, day in year }
}
\value{
  \code{ Dat2Jul:} JD, year year BC is to be given as -|year-1|, e.g. 4 BC = -3, 1 BC = 0 !!\cr
  \code{ Jul2Dat:} date ( year, month, day, hours ).\cr
  \code{ monthsN:}  cumulative sum of days in months.\cr
  \code{ Mnames:}  names of months.\cr
  \code{ Dnames:}  names of weekdays.\cr
  \code{ mdiny:}  c(  number of month, day in (leap) year ).\cr
  \code{ Wday:}   name of weekday from dk mod 7.
  \code{ Yday:}   number of day, from 0 = Jan 1.
}
\examples{
c(Jul2Dat(dd <- Dat2Jul( -4712,1,1) ) )
   # -4712, 1, 1, 12;  JD=0  i.e. Start of Julian day numbering
c(Jul2Dat(dd <- Dat2Jul( -1, 1, 1)),dd)
   #    -1,  1,  1, 12; JD=1720693 , start of last year BC
c(Jul2Dat(dd <- Dat2Jul( -1,12,31)),dd)
   #    -1, 12, 31, 12; JD=1721057 , last day BC
c(Jul2Dat(dd <- Dat2Jul( -0, 1, 1)),dd)
   #     0,  1,  1, 12; JD=1721058 , first day AD
c(Jul2Dat(dd <- Dat2Jul(  1, 1 ,1)),dd)
   #     1,  1,  1, 12; JD=1721424
c(Jul2Dat(dd <- Dat2Jul( 1582,10, 4 )),dd)
   #  1582, 10,  4, 12; 2299160, last day of Julian calendar
c(Jul2Dat(dd <- Dat2Jul( 1582,10,15)),dd)
   #  1582, 10, 15, 12; 2299161, first day of Gregorian calendar
round(c(Jul2Dat(dd <- Dat2Jul( 1582,10,15, 0.0168)),dd),1 )
   #  1582, 10, 15, 12; 2299160.5 first day of Gregorian calendar
c(Jul2Dat(dd <- Dat2Jul( 2001,1,1)),dd)
   #  2001,  1,  1, 12; 2451911
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
