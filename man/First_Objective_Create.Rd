% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/First_Objective.R
\name{First_Objective_Create}
\alias{First_Objective_Create}
\title{Create First Objective coefficients}
\usage{
First_Objective_Create(Parcels, Coefficients, Sense= "Min",
                              Silence=FALSE, Env= .GlobalEnv)
}
\arguments{
\item{Parcels}{A vector of strings: the name of the parcels appearing in the objective}

\item{Coefficients}{A vector of doubles: the respective coefficients of the parcels in \code{Parcels}.}

\item{Sense}{A string: "Min" means the objective is to be minimized and "Max" means the objective is to be maximized. (Default: "Min")}

\item{Silence}{A binary parameter: if \code{FALSE}, the function will report a summary message. If \code{TRUE}, the function will be silent.  (Default: FALSE)}

\item{Env}{the environment where the package should create or access variables.
By default the package works in the R's Global environment. (Default: .GlobalEnv)}
}
\value{
NULL (creates some variables in the environment but returns nothing)
}
\description{
\code{First_Objective_Create} creates the problem's first objective function.\cr
It creates a list named \code{First_Objective}, where \code{First_Objective[["Coefficients"]]} stores the coefficients of parcels
and \code{First_Objective[["Sense"]]} is equal to "Min" or "Max" showing if the objective is to be minimized or maximized.\cr
This function is not sensitive to the order the parcels are input and will automatically store parcels in \code{First_Objective[["Coefficients"]]} sorted with the same order they were stored in \code{Parcels}.\cr
\strong{Note:} The coefficients of the parcels not appearing in the input \code{Parcels} will be equal to zero.
}
\examples{
\dontrun{

First_Objective_Create(Parcels = c("Parcel_1","Parcel_2","Parcel_3"),
                       Coefficients = c(1,5,10),Sense = "Max")

## Creates {Maximize: Parcel_1 + 5*Parcel_2 + 10*Parcel_3} ##

}

}
\seealso{
Other First Objective: 
\code{\link{First_Objective_Import}()},
\code{\link{Optimize_First_Objective}()}

Other Objective Creators: 
\code{\link{Risk_Objective_Create}()},
\code{\link{Second_Objective_Create}()}
}
\concept{First Objective}
\concept{Objective Creators}
