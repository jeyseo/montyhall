% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg.R
\name{open_goat_door}
\alias{open_goat_door}
\title{Opening the goat door}
\usage{
open_goat_door(game, a.pick)
}
\arguments{
\item{Numeric}{}
}
\value{
The function will be a numeric value that indicates a door.
}
\description{
'open_goat_door()' chooses a goat door if the car door is selected.
}
\details{
If the contestant selects a car door, one of the two goat
doors are randomly selected.
}
\examples{
  open_goat_door()
}
