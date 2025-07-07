#' Homicides Dataset
#'
#' A dataset containing homicides recorded by the Metropolitan Police from 2003-2023
#'
#' @format A data frame with 2670 rows and 9 variables:
#' \describe{
#'   \item{Age_Group}{Age group of the victim. One of: `"Adult"`, `"Adolescent/Young Adult"`, `"Child"`, `"Elderly"`.}
#'   \item{Sex}{Sex of the victim. `"Male"` or `"Female"`}
#'   \item{Method_of_Killing}{Recorded method of killing, e.g., `"Knife or Sharp Implement"`, `"Blunt Implement"`, etc.}
#'   \item{Domestic_Abuse}{Indicates whether the incident was flagged as domestic abuse.}
#'   \item{Solved_Status}{Whether the case has been solved. One of: `"Solved"`, `"Unsolved"`}
#'   \item{Borough}{London Borough where the homicide occurred}
#'   \item{Ethnicity}{Recorded ethnicity of the victim. One of `"Asian"`, `"Black"`, `"Not Reported/Not Known"`, `"Other"`, `"White"`.}
#'   \item{Year}{Year in which the incident was recorded.}
#'   \item{BCU}{Basic Command Unit where the homicide occurred (Homicide units changed from Boroughs to BCUs in 2017).}
#' }
#'
#' @source Data taken and filtered from https://www.met.police.uk/police-forces/metropolitan-police/areas/stats-and-data/stats-and-data/met/homicide-dashboard/
"homicides"
