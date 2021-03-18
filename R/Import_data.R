

#'
#' Import problem's data from Excel file
#'
#' \code{Import_data} imports the problem's data from an excel file.
#' The excel file must include a sheet named "Parcels" providing names of all parcels in the problem and their status.\cr
#' If any of the sheets named "First_Objective", "Second_Objective", "Sigma", and "Correlation" exist in the excel file, the function will automatically import the data from them.\cr
#' \strong{Note:} The sheets in the excel file must follow the structures explained in the functions in `See Also` section.
#'
#'
#'
#'
#' Based on the sheets available, \code{Import_data} will automatically call different functions.
#'
#' If "Parcels" exists, the function [Parcels_Import] will be called. It will create
#' variables \code{Parcels} representing the name of parcels and the order they are used,
#' and \code{Status} showing which parcel is or is not protected (owned).\cr
#' (For more information refer to function [Parcels_Import])
#'
#' If "First_Objective" exists, the function [First_Objective_Import] will be called, which will create
#' a list named \code{First_Objective}. `First_Objective[["Coefficients"]]` stores the the coefficient of parcels
#' and `First_Objective[["Sense"]]` stores the parameter `First_Objective_Sense`
#' showing if the objective is to be minimized or maximized.\cr
#' (For more information refer to function [First_Objective_Import])
#'
#' If "Second_Objective" exists, the function [Second_Objective_Import] will be called, which will create
#'  a list named \code{Second_Objective}. `Second_Objective[["Coefficients"]]` stores the the coefficient of parcels
#' and `Second_Objective[["Sense"]]` stores parameter `Second_Objective_Sense`
#' showing if the objective is to be minimized or maximized.\cr
#' (For more information refer to function [Second_Objective_Import])
#'
#' If both "Sigma" and "Correlation" exist, the function [Risk_Objective_Import] will be called.
#' It will creat a list named \code{Risk_Objective} will be created,
#' where `Risk_Objective[["Sigma"]]` stores the the sigmas of parcels,
#' `Risk_Objective[["Correlation"]]` stores the the Correlation of parcels.\cr
#' (For more information refer to function [Risk_Objective_Import])
#'
#'
#'
#'
#' @usage Import_data(Address, First_Objective_Sense= "Min",
#'                    Second_Objective_Sense="Min", Silence= FALSE, Env= .GlobalEnv)
#'
#' @param Address A string: the location and name of the excel file.
#' @param First_Objective_Sense A string: "Min" means the objective is to be minimized and "Max" means the objective is to be maximized. (Default: "Min")
#' @param Second_Objective_Sense A string: "Min" means the objective is to be minimized and "Max" means the objective is to be maximized. (Default: "Min")
#' @param Silence A binary parameter: if `FALSE`, the function will report a summary message. If `TRUE`, the function will be silent.  (Default: FALSE)
#' @param Env the environment where the package should create or access variables.
#' By default the package works in the R's Global environment. (Default: .GlobalEnv)
#'
#' @examples
#' \dontrun{
#'
#' Import_data(Address= "C:\\Folder\\example.xlsx",
#'             First_Objective_Sense= "Max", Second_Objective_Sense="Max")
#' }
#'
#'
#'
#' @return NULL (creates some variables in the environment but returns nothing)
#' @family Import Functions
#' @import readxl
#' @import JuliaCall
#' @import stats
#' @import utils
#' @export
Import_data <- function(Address, First_Objective_Sense= "Min", Second_Objective_Sense="Min", Silence= FALSE, Env= .GlobalEnv) {
  sheet_names <- excel_sheets(Address)
  ################
  #    Errors    #
  ################

  if ("Parcels" %in% sheet_names == FALSE) {
    stop("Parcels sheet was not found.")
  }


  ################
  #     Body     #
  ################
  Parcels_Import(Address, Silence = Silence, Env = Env)

  if("First_Objective" %in% sheet_names){
    First_Objective_Import(Address= Address, Sheet = "First_Objective", Sense = First_Objective_Sense, Silence = Silence, Env = Env)
  }else if(Silence==FALSE){
    cat("No sheet named First_Objective Found: Skipping First Objective.\n")
  }

  if("Second_Objective" %in% sheet_names){
    Second_Objective_Import(Address= Address, Sheet = "Second_Objective", Sense = Second_Objective_Sense, Silence = Silence, Env = Env)
  }else if(Silence==FALSE){
    cat("No sheet named Second_Objective Found: Skipping Second Objective.\n")
  }

  if("Sigma" %in% sheet_names && "Correlation" %in% sheet_names){
    Risk_Objective_Import(Address= Address, Sigma_Sheet = "Sigma", Correlation_Sheet = "Correlation", Silence = Silence, Env = Env)
  }else if(Silence==FALSE){
    cat("No sheet named Sigma and Correlation Found: Skipping Risk Objective.\n")
  }

  if(Silence==FALSE){
    cat("Import_data Completed.\n")
  }
}

