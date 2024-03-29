% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wday.R
\name{wday}
\alias{wday}
\title{wday}
\usage{
wday(date = Sys.Date())
}
\arguments{
\item{date}{A Date object, with the current date as the default}
}
\value{
A integer with the weekday component of the date
}
\description{
Return the day of the week as an integer
}
\details{
This function simply wraps around the \code{as.POSIXlt} function and returns its
\code{wday} field.
}
