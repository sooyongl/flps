% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_desc.r
\docType{data}
\name{graded}
\alias{graded}
\title{graded.rda}
\format{
A data frame with variables:
\describe{
  \item{schid}{School ID}
  \item{id}{Student ID}
  \item{sex}{0 = boys; 1 = girls}
  \item{race}{0 = White; 1 = Others}
  \item{pretest}{Pre test scores}
  \item{stdscore}{Standardized scores}
  \item{cm_sex}{Cluster-mean of sex}
  \item{cm_race}{Cluster-mean of race}
  \item{cm_pretest}{Cluster-mean of Pre test scores}
  \item{cm_stdscore}{Cluster-mean of of Standardized scores}
  \item{trt}{Treatment assignment; 0 = control, 1 = treatment}
  \item{Y}{Outcome}
  \item{q1}{Graded response item}
  \item{q2}{Graded response item}
  \item{q3}{Graded response item}
  \item{q4}{Graded response item}
  \item{q5}{Graded response item}
  \item{q6}{Graded response item}
  \item{q7}{Graded response item}
  \item{q8}{Graded response item}
  \item{q9}{Graded response item}
  \item{q10}{Graded response item}
  \item{q11}{Graded response item}
  \item{q12}{Graded response item}
  \item{q13}{Graded response item}
  \item{q14}{Graded response item}
  \item{q15}{Graded response item}
  \item{q16}{Graded response item}
  \item{q17}{Graded response item}
  \item{q18}{Graded response item}
  \item{q19}{Graded response item}
  \item{q20}{Graded response item}
}
}
\source{
CTA1
}
\usage{
graded
}
\description{
A data set containing graded response items information.
Example data regenerated from CTA1
}
\examples{
data(graded)
summary(graded)
}
\keyword{datasets}
