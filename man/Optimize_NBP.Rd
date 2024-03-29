% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Optimize_NBP.R
\name{Optimize_NBP}
\alias{Optimize_NBP}
\title{Load and Optimize Nash Bargaining Problem (NBP)}
\usage{
Optimize_NBP(First_Objective=0, Second_Objective=0, Risk_Objective=0,
                    Time_limit=1e7, Solver="SCIP", Silence= FALSE, Env= .GlobalEnv)
}
\arguments{
\item{First_Objective}{A binary parameter: If \bold{1}, the first objective function (\code{First_Objective}) will be included in optimization.
If \bold{0}, it will \strong{not} be included in optimization.}

\item{Second_Objective}{A binary parameter: If \bold{1}, the second objective function (\code{Second_Objective}) will be included in optimization.
If \bold{0}, it will \strong{not} be included in optimization.}

\item{Risk_Objective}{A binary parameter: If \bold{1}, the Risk objective function (\code{Risk_Objective}) will be included in optimization.
If \bold{0}, it will \strong{not} be included in optimization.}

\item{Time_limit}{A double: the total time limit in seconds}

\item{Solver}{A string: defining the solver to be used to solve the problem. (Default: "SCIP")}

\item{Silence}{A binary parameter: if \code{FALSE}, the function will report a summary message. If \code{TRUE}, the function will be silent.  (Default: FALSE)}

\item{Env}{the environment where the package should create or access variables.
By default the package works in the R's Global environment. (Default: .GlobalEnv)}
}
\value{
A list with 8 members:\cr
\verb{[["Solution_time"]]}: a numeric value; defining the time spent in the solver in seconds.\cr
\verb{[["Status"]]}: a character; defining the status of the solution.\cr
\verb{[["Gap"]]}: a numeric value; defining the relative optimality gap of the solution.\cr
\verb{[["Result"]]}: a vector of binaries; its \code{i}'th element defines if the parcel named \code{Parcels[i]} is protected or not. Comparing these values with the values of \code{Status} defined when creating the problem,
the user can find which parcels are to be invested, divested, or remain the same.\cr
\verb{[["Firs_Objective"]]}: a numveric value; (if applicable) returns the NBP value of first objective.\cr
\verb{[["Second_Objective"]]}: a numveric value; (if applicable) returns the NBP value of second objective.\cr
\verb{[["Risk_Objective"]]}: a numveric value; (if applicable) returns the NBP value of risk objective.\cr
\verb{[["Ideal"]]}: a list with two members returning the ideal values of the input objectives found in the process of creating the NBP.\cr
\verb{[["Nadir"]]}: a list with two members returning the nadir values of the input objectives found in the process of creating the NBP.\cr
}
\description{
\code{Optimize_NBP} creates and optimizes the Nash Bargaining Problem (NBP) in Julia environment.
It requires two objective functions to be considered as two players.\cr
(For detailed information refer to the paper)
}
\details{
To create  NBP, the \bold{nadir} values of both objectives are required, which entails 4 optimization problems.

If \code{Optimize_NBP} terminates with a \code{Warning}, then there have been a problem in the process of finding the Nadir points, and
the function has terminated before even going to the NBP optimization. The corresponding found solution and status will be returned as a list.
Furhter, the \code{Warning} will report in what step the function was terminated.

If \code{Optimize_NBP} finishes with no \code{Warning}, then the Nadir points were found successfully, and the NBP was created.
Then, in the list that \code{Optimize_NBP} returns, \verb{[["Status"]]} defines the status of the returned NBP solution.\cr
If \verb{[["Status"]]="OPTIMAL"}, the NBP was solved to optimality and the Parcels' optimal status are stored in \verb{[["Result"]]}.
Also, the NBP optimal values of objectives will be stored in either \verb{[["First_Objective"]]}, \verb{[["Second_Objective"]]}, or \verb{[["Risk_Objective"]]}
, based on the input objectives.\cr
If \verb{[["Status"]]="TIME_LIMIT"}, the solver was terminated because the time limit was reached.
If any feasible solution was reported by the solver, it would be stored in \verb{[["Result"]]}. Otherwise, \verb{[["Result"]]} and objective values would be
all equal to "N/A".\cr
If \verb{[["Status"]]="INFEASIBLE"}, then there has been a problem in the solver. In this stage, the feasibility and boundedness of the problem
is certain, and therefore, any status of this kind is showing an underlying problem. \bold{Contact the Developer.}\cr

In any other case, the \verb{[["Status"]]} will be the solution status that the solver has reported.

\strong{Note 1:} If only First Objective and Second Objective are considered, \code{Optimize_NBP} needs to solve a total of \bold{5} optimization problems to find the NBP solution:
Four Linear Problem (LP), and one Second Order Cone Program (SOCP).\cr

\strong{Note 2:} If Risk objective is considered along with one of First objective or Second Objective, \code{Optimize_NBP} needs to solve \bold{3} optimization problems to find the NBP solution:
One Linear Problem (LP), one Quadratic Problem (QP), and one Second Order Cone Program (SOCP).
}
\examples{
\dontrun{

## to find NBP solution for First Objective and Second Objective: ##

Optimize_NBP(First_Objective=1, Second_Objective=1, Time_limit=1e7, Solver="SCIP", Silence= FALSE)

## to find NBP solution for First Objective and Risk Objective: ##

Optimize_NBP(First_Objective=1, Risk_Objective=1, Time_limit=1e7, Solver="SCIP", Silence= FALSE)

## to find NBP solution for Second Objective and Risk Objective: ##

Optimize_NBP(Second_Objective=1, Risk_Objective=1, Time_limit=1e7, Solver="SCIP", Silence= FALSE)
}


}
\seealso{
Other Optimizer Functions: 
\code{\link{Load_Problem}()},
\code{\link{Optimize_First_Objective}()},
\code{\link{Optimize_Second_Objective}()},
\code{\link{Optimize_WSO}()}
}
\concept{Optimizer Functions}
