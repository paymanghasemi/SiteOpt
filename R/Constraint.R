
#'
#' Create a Linear Constraint
#'
#' \code{Add_Constraint} creates a Constraint in the optimization model of the form:\cr
#' \eqn{Coefficients . Parcels \le RHS},\cr
#' where `Coefficients` is a vector of doubles, `Parcels` is the vector of parcel names, and  `RHS` is a double representing the right-hand-side value of the constraint.
#'
#'
#'
#'
#' When used for the first time, `Add_Constraint` will create a list named `Constraints`.
#'
#' Each member of the list `Constraints` represents a linear constraint.
#' Each constraint is stored as a list with three members: `Parcels` storing the names of the parcels in the constraint,
#' `Coefficients` storing the respective coefficients of the parcels, and `RHS` storing the constraints right-hand-side value.
#'
#' In other words, the information of the `i`'th constraint in `Constraints` can be accessed by \cr
#' `Constraints[[i]][["Parcels"]]`, \cr
#' `Constraints[[i]][["Coefficients"]]`, \cr
#' and `Constraints[[i]][["RHS"]]`.
#'
#' This function enables adding any types of linear constraints to the optimization model.
#'
#' \strong{Removing a constraint:} Constraint `i` can be removed using the following code:
#'
#' `Constraints[[i]]<-NULL`
#'
#' \strong{Remove all Constraints:} All of the Constraints stored in `Constraints` can be removed using the following code:
#'
#' `remove(Constraints)`
#'
#'
#' @usage Add_Constraint(Parcels, Coefficients, RHS,
#'                       Silence=FALSE, Env= .GlobalEnv)
#'
#' @param Parcels A vector of strings: the name of the parcels to appear in the constraint.
#' @param Coefficients A vector of doubles: the respective coefficients of the `Parcels`.
#' @param RHS A double: defining the right-hand-side value of the constraint.
#' @param Silence A binary parameter: if `FALSE`, the function will report a summary message. If `TRUE`, the function will be silent.  (Default: FALSE)
#' @param Env the environment where the package should create or access variables.
#' By default the package works in the R's Global environment. (Default: .GlobalEnv)
#'
#' @examples
#' \dontrun{
#'
#' ## The constraint  {Parcel_1 + Parcel_3 + Parcel_5 >= 2}
#' is the same as  {-Parcel_1 - Parcel_3 - Parcel_5 <= -2},
#' which can be created by using the following code: ##
#'
#' Add_Constraint(Parcels= c("Parcel_1","Parcel_3","Parcel_5"),
#'                Coefficients= c(-1,-1,-1),
#'                RHS=-2,
#'                Silence=FALSE)
#'
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @return NULL (creates some variables in the environment but returns nothing)
#' @family Constraints
#'
#' @export
Add_Constraint <- function(Parcels, Coefficients, RHS,Silence=FALSE, Env= .GlobalEnv) {



  ################
  #    Errors    #
  ################
  if (exists('Parcels', envir = Env) == FALSE || exists('Status', envir = Env) == FALSE) {
    stop('Parcels and Status Should be created first.')
  }

  if(length(Parcels)!=length(Coefficients)){
    stop('Parcels and Coefficients are not the same length.')
  }

  if(all(Parcels%in%Env$Parcels)==FALSE){
    stop('Undefined Parcels: "',
         paste(Parcels[which(!Parcels %in% Env$Parcels)], collapse = ' '),
         '"')
  }
  ################
  #    Reports   #
  ################

  if (exists("Constraints", envir = Env) == 0) {
    if(Silence==FALSE){
      cat("Creating Constraints.\n")}
    Env$Constraints <- list()
  } else if(Silence==FALSE){
    cat("Adding to Constraints.\n")
  }


  ################
  #     Body     #
  ################


  temp <- length(Env$Constraints) + 1
  Env$Constraints[[temp]] <- list()
  Env$Constraints[[temp]][['Parcels']] <- Parcels
  Env$Constraints[[temp]][['Coefficients']] <- Coefficients
  Env$Constraints[[temp]][['RHS']] <- RHS

  ################
  #    Reports   #
  ################

  if(Silence==FALSE){
    cat("A constraint with ",length(Env$Constraints[[temp]][['Parcels']])," parcels and RHS ",Env$Constraints[[temp]][['RHS']]," created. \n")
  }

}
