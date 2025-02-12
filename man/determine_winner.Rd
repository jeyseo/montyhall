% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg.R
\name{determine_winner}
\alias{determine_winner}
\title{Determine the winner}
\usage{
determine_winner(final.pick, game)
}
\arguments{
\item{logical}{}
}
\value{
The function will be true or false, which is defined as win or lose.
}
\description{
'determine_winner()' lets us know if the contestant is the winner.
}
\details{
If the final choice is a car, the contestant wins. If it is a goat, the
contestant loses.
}
\examples{
  determine_winner()
}
