


#'
#' Create a Budget Constraint
#'
#' \code{Budget_Add_Constraint} creates a Budget Constraint in the optimization model of the form:\cr
#' \if{html}{\figure{Budget_constraint.jpg}{}}
#' \if{latex}{\figure{Budget_constraint.jpg}{options: width=2.0in}}\cr
#' where \eqn{B} and \eqn{S} are respectively the set of initially protected and unprotected parcels, and `C` is the budget limit.
#'
#'
#' When used for the first time, `Budget_Add_Constraint` or [Budget_Import_Constraint] will create a list named `Budget` in the environment.
#'  Whenever a new budget constraint is added, a new member will be added to the list `Budget`.
#'
#' Each member of the list `Budget` represents a budget constraint.
#' Each budget constraint is stored as a list with three members: `Parcels` storing the names of the parcels in the constraint,
#' `Coefficients` storing the respective coefficients of the parcels, and `Limit` storing the budget limit (right-hand-side).\cr
#'
#' In other words, the information of the `i`'th Budget constraint in `Budget` can be accessed by:\cr
#' `Budget[[i]][["Parcels"]]`,\cr
#' `Budget[[i]][["Coefficients"]]`,\cr
#'  and `Budget[[i]][["Limit"]]`.
#'
#'
#' \strong{Removing a budget:} Budget `i` can be removed using the following code:
#'
#' `Budget[[i]]<-NULL`
#'
#' \strong{Removing all budget constraints:} All budget constraints can be removed using the following code:
#'
#' `remove(Budget)`
#'
#'
#' @usage Budget_Add_Constraint(Parcels, Coefficients, Limit, Silence=FALSE, Env= .GlobalEnv)
#'
#' @param Parcels A vector of strings: the name of the parcels appearing in the constraint. The package will automatically differentiate protected from unprotected parcels.
#' @param Coefficients A vector of doubles: the respective coefficients of the parcels in `Parcels`.
#' @param Limit A double: defining the budget limit (right-hand-side)
#' @param Silence A binary parameter: if `FALSE`, the function will report a summary message. If `TRUE`, the function will be silent.  (Default: FALSE)
#' @param Env the environment where the package should create or access variables.
#' By default the package works in the R's Global environment. (Default: .GlobalEnv)
#'
#'
#' @examples
#' \dontrun{
#'
#' ## To create a budget constraint of the form
#' 5*Parcel_1 + 10*Parcel_3 + 15*Parcel_4 <= 15 ##
#'
#' Budget_Add_Constraint(Parcels= c("Parcel_1","Parcel_3","Parcel_4"),
#'                       Coefficients= c(5,10,15), Limit=15)
#'
#'}
#'
#'
#' @return NULL (creates some variables in the environment but returns nothing)
#' @family Budget Constraints
#' @family Constraints
#'
#' @export
Budget_Add_Constraint <- function(Parcels, Coefficients, Limit, Silence=FALSE, Env= .GlobalEnv) {

  ################
  #    Errors    #
  ################

  if (exists('Parcels', envir = Env) == FALSE || exists('Status', envir = Env) == FALSE) {
    stop('Parcels and Status Should be created first.')
  }

  if(length(Parcels)!=length(Coefficients)){
    stop('Parcels and Coefficients are not the same length.')
  }

  if(all(Parcels%in% Env$Parcels)==FALSE){
    stop('Undefined Parcels: "',
         paste(Parcels[which(!Parcels %in% Env$Parcels)], collapse = ' '),
         '"')
  }

  ################
  #    Reports   #
  ################

  if (exists("Budget",envir = Env) == 0) {
    if(Silence==FALSE){
      cat("Including Budget Constraints.\n")}
    Env$Budget <- list()
  } else if(Silence==FALSE){
    cat("Adding Budget Constraints.\n")
  }



  ################
  #     Body     #
  ################

  temp <- length(Env$Budget) + 1
  Env$Budget[[temp]] <- list()
  Env$Budget[[temp]][['Parcels']] <- as.character(Parcels)
  Env$Budget[[temp]][['Coefficients']] <- as.double(Coefficients)
  Env$Budget[[temp]][['Limit']] <- as.double(Limit)


  ################
  #    Reports   #
  ################

  if(Silence==FALSE){
    cat("A budget constraint with ", length(Env$Budget[[temp]][['Parcels']]), " parcels and limit= ", Env$Budget[[temp]][['Limit']], " is created.\n")
  }
}







#'
#' Import Budget Constraint's data from Excel file
#'
#' \code{Budget_Import_Constraint} imports the Budget Constraint's data from a sheet in an excel file.
#' Everytime used, it will add a member to a list named \code{Budget} which will store the budget constraints.\cr
#' (For more information, refer to [Budget_Add_Constraint])
#' \cr
#'
#'
#' The first row of the sheet will be ignored as captions. The first column of the sheet should have names of parcels in the constraint,
#' and the second column should have the parcels' respective coefficients.
#' The \bold{Budget Limit} can be passed as a Parameter when calling the `Budget_Import_Constraint`, for instance `Limit= 50`.
#' Otherwise, if `Limit = NULL`, the function will look for a row named "Limit" in the input sheet and use its value as the constraints limit.\cr
#' Here is an example Budget constraint table:
#'
#' \if{html}{\figure{Budget.jpg}{}}
#' \if{latex}{\figure{Budget.jpg}{options: width=2.0in}}\cr
#'
#'
#' \strong{Removing a budget:} Budget `i` can be removed using the following code:
#'
#' `Budget[[i]]<-NULL`
#'
#' \strong{Removing all budget constraints:} All budget constraints can be removed using the following code:
#'
#' `remove(Budget)`
#'
#' @usage Budget_Import_Constraint(Address, Sheet= "Budget", Limit= NULL,
#'                                 Silence= FALSE, Env= .GlobalEnv)
#'
#' @param Address A string: the location and name of the excel file.
#' @param Sheet A string: the name of the sheet holding the budget constraint data. (Default: "Budget")
#' @param Limit A double: defining the budget limit (right-hand-side). If `NULL`, the Limit will be read from the excel file. (Default: NULL)
#' @param Silence A binary parameter: if `FALSE`, the function will report a summary message. If `TRUE`, the function will be silent.  (Default: FALSE)
#' @param Env the environment where the package should create or access variables.
#' By default the package works in the R's Global environment. (Default: .GlobalEnv)
#'
#' @examples
#' \dontrun{
#'
#' ## To read the data and import the budget limit from the excel file ##
#'
#' Budget_Import_Constraint("C:\\example.xlsx", Sheet= "Budget", Limit= NULL)
#'
#' ## To read the data from excel file, and put the Budget limit equal to 60.
#' (It will ignore the limit defined in the excel file) ##
#'
#' Budget_Import_Constraint("C:\\example.xlsx", Sheet= "Budget", Limit= 60)
#'
#'}
#'
#' @family Budget Constraints
#'
#'
#' @return NULL (creates some variables in the environment but returns nothing)
#' @export
Budget_Import_Constraint <-
  function(Address,
           Sheet = "Budget",
           Limit = NULL,
           Silence = FALSE, Env= .GlobalEnv) {


    ################
    #    Errors    #
    ################

    if (exists('Parcels', envir = Env) == FALSE || exists('Status', envir = Env) == FALSE) {
      stop('Parcels and Status Should be created first.')
    }

    ################
    #     Body     #
    ################

    suppressMessages(mydata <-
                       as.matrix(read_excel(Address, sheet = Sheet)))

    if (is.null(Limit)) {
      Limit = as.double(mydata[match('Limit', mydata[, 1]), 2])
    }
    if (is.na(match('Limit', mydata[, 1])) == 0) {
      Data_names <- mydata[-(match('Limit', mydata[, 1])), 1]
      Data_cost <- mydata[-(match('Limit', mydata[, 1])), 2]
    } else{
      Data_names <- mydata[, 1]
      Data_cost <- mydata[, 2]
    }

    ################
    #    Errors    #
    ################

    if (all(Data_names %in% Env$Parcels) == FALSE) {
      stop('Undefined Parcels: "',
           paste(Data_names[which(!Data_names %in% Env$Parcels)], collapse = ' '),
           '"')
    }


    ################
    #    Reports   #
    ################

    if (exists("Budget",envir = Env) == 0) {
      if(Silence==FALSE){
        cat("Including Budget Constraints.\n")}
      Env$Budget <- list()
    } else if(Silence==FALSE){
      cat("Adding Budget Constraints.\n")
    }



    ################
    #     Body     #
    ################

    temp <- length(Env$Budget) + 1
    Env$Budget[[temp]] <- list()
    Env$Budget[[temp]][['Parcels']] <- as.character(Data_names)
    Env$Budget[[temp]][['Coefficients']] <- as.double(Data_cost)
    Env$Budget[[temp]][['Limit']] <- as.double(Limit)


    ################
    #    Reports   #
    ################

    if(Silence==FALSE){
      cat("A budget constraint with ", length(Env$Budget[[temp]][['Parcels']]), " parcels and limit= ", Env$Budget[[temp]][['Limit']], " is created.\n")
    }
  }
