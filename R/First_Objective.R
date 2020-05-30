

#' Import First Objective coefficients from Excel file
#'
#'
#' \code{First_Objective_Import} imports the problem's first objective coefficients from a sheet in an excel file.\cr
#' It creates a list named \code{First_Objective}, where `First_Objective[["Coefficients"]]` stores the coefficients of parcels
#' and `First_Objective[["Sense"]]` is equal to "Min" or "Max" showing if the objective is to be minimized or maximized.\cr
#' This function is not sensitive to the order the parcels are stored in the sheet and will automatically store parcels
#' in \code{First_Objective[["Coefficients"]]} sorted with the same order they were stored in \code{Parcels}.\cr
#' \strong{Note:} The coefficients of the parcels not included in the excel file will be considered zero.
#'
#' The first row of the sheet will be ignored as captions. The first column of the sheet should include names, and the second column should include coefficients.\cr
#' Here is an example First Objective table:
#'
#' \if{html}{\figure{Objective1.jpg}{}}
#' \if{latex}{\figure{Objective1.jpg}{options: width=2.0in}}\cr
#'
#' \strong{Note:} The coefficients of the parcels not appearing in the excel file will be equal to zero.
#'
#' @usage First_Objective_Import(Address, Sheet = "First_Objective",
#'                               Sense= "Min", Silence = FALSE, Env= .GlobalEnv)
#'
#' @param Address A string: the location and name of the excel file.
#' @param Sheet A string: the name of the sheet holding the first objective's data. (Default: "First_Objective")
#' @param Sense A string: "Min" means the objective is to be minimized and "Max" means the objective is to be maximized. (Default: "Min")
#' @param Silence A binary parameter: if `FALSE`, the function will report a summary message. If `TRUE`, the function will be silent.  (Default: FALSE)
#' @param Env the environment where the package should create or access variables.
#' By default the package works in the R's Global environment. (Default: .GlobalEnv)
#'
#'
#' @examples
#' \dontrun{
#'
#' First_Objective_Import("C:\\example.xlsx", Sheet = "First_Objective", Sense= "Min")
#'}
#'
#' @return NULL (creates some variables in the environment but returns nothing)
#' @family First Objective
#' @family Import Functions
#' @export
First_Objective_Import <- function(Address,
                          Sheet = "First_Objective",
                          Sense= "Min",
                          Silence = FALSE,
                          Env= .GlobalEnv) {

  ################
  #    Errors    #
  ################
  if (exists('Parcels', envir = Env) == FALSE || exists('Status', envir = Env) == FALSE) {
    stop('Parcels and Status Should be created first.')
  }


  if(Sheet %in% excel_sheets(Address)==FALSE){
    stop('No sheet named ',Sheet,' was found.')
  }

  if(Sense != "Min" && Sense != "Max"){
    stop('Unknown Sense: Sense can only be "Min" or "Max".')
  }



  ################
  #     Body     #
  ################
  suppressMessages(my_data <-
                     as.matrix(read_excel(Address, sheet = Sheet)))

  ################
  #    Errors    #
  ################

  if (all(my_data[, 1] %in% Env$Parcels) == FALSE) {
    stop('Undefined Parcels: "',
         paste(my_data[, 1][which(!my_data[, 1] %in% Env$Parcels)], collapse = ' '),
         '"')
  }




  ################
  #     Body     #
  ################
  Env$First_Objective <- list();
  Env$First_Objective[["Sense"]] <- Sense;
  Env$First_Objective[["Coefficients"]]<- rep(0, length(Env$Parcels))
  Env$First_Objective[["Coefficients"]][match(as.character(my_data[, 1]),Env$Parcels)] <-
    as.double(my_data[, 2])

  ################
  #    Reports   #
  ################



  if (Silence == FALSE) {
    cat("First Objective: Sense is ",Sense,
      " and Coefficients of parcels ",
      Env$Parcels[1],
      " and ",
      Env$Parcels[length(Env$Parcels)],
      " are ",
      Env$First_Objective[["Coefficients"]][1],
      " and ",
      Env$First_Objective[["Coefficients"]][length(Env$Parcels)],
      ".\n"
    )
    cat("Importing First Objective Completed.\n")
  }
}


#' Create First Objective coefficients
#'
#'
#' \code{First_Objective_Create} creates the problem's first objective function.\cr
#' It creates a list named \code{First_Objective}, where `First_Objective[["Coefficients"]]` stores the coefficients of parcels
#' and `First_Objective[["Sense"]]` is equal to "Min" or "Max" showing if the objective is to be minimized or maximized.\cr
#' This function is not sensitive to the order the parcels are input and will automatically store parcels in \code{First_Objective[["Coefficients"]]} sorted with the same order they were stored in \code{Parcels}.\cr
#' \strong{Note:} The coefficients of the parcels not appearing in the input `Parcels` will be equal to zero.
#'
#' @usage First_Objective_Create(Parcels, Coefficients, Sense= "Min",
#'                               Silence=FALSE, Env= .GlobalEnv)
#'
#' @param Parcels A vector of strings: the name of the parcels appearing in the objective
#' @param Coefficients A vector of doubles: the respective coefficients of the parcels in `Parcels`.
#' @param Sense A string: "Min" means the objective is to be minimized and "Max" means the objective is to be maximized. (Default: "Min")
#' @param Silence A binary parameter: if `FALSE`, the function will report a summary message. If `TRUE`, the function will be silent.  (Default: FALSE)
#' @param Env the environment where the package should create or access variables.
#' By default the package works in the R's Global environment. (Default: .GlobalEnv)
#'
#'
#' @examples
#' \dontrun{
#'
#' First_Objective_Create(Parcels = c("Parcel_1","Parcel_2","Parcel_3"),
#'                        Coefficients = c(1,5,10),Sense = "Max")
#'
#' ## Creates {Maximize: Parcel_1 + 5*Parcel_2 + 10*Parcel_3} ##
#'
#'}
#'
#' @return NULL (creates some variables in the environment but returns nothing)
#' @family First Objective
#' @family Objective Creators
#' @export
First_Objective_Create <- function(Parcels, Coefficients, Sense= "Min", Silence=FALSE, Env= .GlobalEnv) {
  ################
  #    Errors    #
  ################
  if (exists('Parcels', envir = Env) == FALSE || exists('Status', envir = Env) == FALSE) {
    stop('Parcels and Status Should be created first.')
  }

  if(Sense != "Min" && Sense != "Max"){
    stop('Unknown Sense: Sense can only be "Min" or "Max".')
  }

  if(all(Parcels%in%Env$Parcels)==FALSE){
    stop('Undefined Parcels: "',
         paste(Parcels[which(!Parcels %in% Env$Parcels)], collapse = ' '),
         '"')
  }

  if(length(Parcels)!=length(Coefficients)){
    stop('Parcels and Coefficients do not have the same length.')
  }


  ################
  #     Body     #
  ################
  Env$First_Objective <- list();
  Env$First_Objective[["Sense"]] <- Sense;
  Env$First_Objective[["Coefficients"]]<- rep(0, length(Env$Parcels))
  Env$First_Objective[["Coefficients"]][match(as.character(Parcels),Env$Parcels)] <-
    as.double(Coefficients)



  ################
  #    Reports   #
  ################

  if (Silence == FALSE) {
    cat("First Objective: Sense is ",Sense,
        " and Coefficients of parcels ",
        Env$Parcels[1],
        " and ",
        Env$Parcels[length(Env$Parcels)],
        " are ",
        Env$First_Objective[["Coefficients"]][1],
        " and ",
        Env$First_Objective[["Coefficients"]][length(Env$Parcels)],
        ".\n"
    )
    cat("Creating First Objective Completed.\n")
  }

}
