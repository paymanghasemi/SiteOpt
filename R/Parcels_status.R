



#' Import parcel status data from Excel file
#'
#'
#' \code{Parcels_Import} imports the parcels' data from a sheet
#' in an excel file. It will create some variables  in the input `Env`.
#' The variables are \code{Parcels} storing the name of all parcels and the
#' order they are used, and \code{Status} storing the status of the parcels.\cr
#' \strong{Note:} These variables are essential to create
#' the optimization problem. So, if not created properly, using other functions will
#' result in an error.
#'
#'
#'
#'
#' In the input excel file, the first row of the sheet will be ignored as captions. The first column of
#' the sheet should include names of parcels, and the second column should include their status.
#'  Value \bold{1} means the parcel is already protected (owned), and value
#' \bold{0} means the parcel is not protected (unowned). These values are
#' essential in creating optimization variables.\cr Here is an example table of a problem with 5 parcels:
#'
#'
#' \if{html}{\figure{Assignment.jpg}{Our logo}}
#' \if{latex}{\figure{Assignment.jpg}{options: width=2.0in}}\cr
#'
#' In the optimization model, protected (owned) and unprotected (unowned) parcels
#' will be represented by the binary decision variables \code{(1-y)} and
#' \code{x}. In the final solution after optimization, \code{(1-y)=1} means that
#' the parcel which was initially protected (owned) remains protected, and
#' \code{(1-y)=0} means that the parcel is divested (sold).\cr Similarly, \code{x=1} means
#' that the parcel that was initially unprotected (unowned) will become protected (invested), and
#' \code{x=0} means that the parcel will remain unprotected.\cr (For more
#' information refer to the Paper)
#'
#' @usage Parcels_Import(Address, Sheet= "Parcels", Silence= FALSE, Env= .GlobalEnv)
#'
#' @param Address A string: the location and name of the excel file.
#' @param Sheet A string: the name of the sheet holding the parcels' data.
#'   (Default: "Parcels")
#' @param Silence A binary parameter: if `FALSE`, the function will report a
#'   summary message. If `TRUE`, the function will be silent.  (Default: FALSE)
#' @param Env the environment where the package should create or access variables.
#' By default the package works in the R's Global environment. (Default: .GlobalEnv)
#'
#' @examples
#' \dontrun{
#'
#' Parcels_Import("example.xlsx",Sheet= "Parcels")
#' }
#'
#'
#' @return NULL (creates some variables in the environment but returns nothing)
#' @family Parcels Status
#' @family Import Functions
#'
#' @export
Parcels_Import <-
  function(Address,
           Sheet = "Parcels",
           Silence = FALSE, Env=.GlobalEnv) {
    ################
    #    Errors    #
    ################

    if (Sheet %in% excel_sheets(Address) == FALSE) {
      stop('No sheet named ', Sheet, ' was found.')
    }



    ################
    #     Body     #
    ################
    suppressMessages(my_data <-
                       as.matrix(readxl::read_excel(Address, sheet = Sheet)))



    Env$Parcels <- my_data[, 1]
    Env$Status <- as.integer(my_data[, 2])


    # assign('Parcel_names',my_data[, 1],envir = env)
    # assign('N_Parcels',as.integer(length(env$Parcel_names)),envir = env)
    # assign('.Parcels_map',setNames(env$Parcel_names, as.integer(1:env$N_Parcels)),envir = env)
    # assign('.Map_parcels',setNames(as.integer(1:env$N_Parcels), env$Parcel_names),envir = env)
    # assign('Parcel_assignment',as.integer(my_data[, 2]),envir = env)


    # assign('Parcel_names',my_data[, 1],envir = as.environment(env))
    # assign('N_Parcels',as.integer(length(Parcel_names)),envir = as.environment(env))
    # assign('.Parcels_map',setNames(Parcel_names, as.integer(1:N_Parcels)),envir = as.environment(env))
    # assign('.Map_parcels',setNames(as.integer(1:N_Parcels), Parcel_names),envir = as.environment(env))
    # assign('Parcel_assignment',as.integer(my_data[, 2]),envir =as.environment(env))

    #Parcel_names <<- my_data[, 1]
    # N_Parcels <<- as.integer(length(Parcel_names))
    # .Parcels_map <<- setNames(Parcel_names, as.integer(1:N_Parcels))
    # .Map_parcels <<- setNames(as.integer(1:N_Parcels), Parcel_names)
    # Parcel_assignment <<- as.integer(my_data[, 2])

    ################
    #    Reports   #
    ################

    if (Silence == FALSE) {
      cat("Number of Parcels= ",length(Env$Parcels), " . \n")
      cat(
        "The problem has ",
        sum(Env$Status),
        " protected and ",
        length(Env$Parcels) - sum(Env$Status),
        " unprotected parcels.\n"
      )
      cat("Import Parcels Completed.\n")
    }

  }


#' Create Parcels and their Status
#'
#'
#' \code{Parcels_Create} manually creates the problem's data. It
#' will create variables named \code{Parcels} storing the name of parcels and the order they are used, and
#' \code{Status} showing whether parcels are protected or not.\cr
#' \strong{Note:} These variables are essential to create the optimization
#' problem. So, if not created, calling other functions will result in an error.
#'
#'
#' `Parcels` should include the name of all parcels appearing in the problem,
#' and `Status` should include all the parcels' status, where value
#' \bold{1} means the parcel is protected (owned), and value \bold{0} means the
#' parcel is unprotected (unowned). These values are essential in creating
#' optimization variables.\cr
#'
#' In the optimization model, protected (owned) and unprotected (unowned) parcels
#' will be represented by the binary decision variables \code{(1-y)} and
#' \code{x}. In the final solution after optimization, \code{(1-y)=1} means that
#' the parcel which was initially protected (owned) remains protected, and
#' \code{(1-y)=0} means that the parcel is divested (sold).\cr Similarly, \code{x=1} means
#' that the parcel that was initially unprotected (unowned) will become protected (invested), and
#' \code{x=0} means that the parcel will remain unprotected.\cr (For more
#' information refer to the Paper)
#'
#'
#' @usage Parcels_Create(Parcels, Status, Silence= FALSE, Env= .GlobalEnv)
#'
#' @param Parcels A vector of strings: the name of the parcels appearing in the problem
#' @param Status A vector of binary: the respective status of the parcels in `Parcels`.
#' Value \bold{1} means the parcel is protected (owned), and value \bold{0} means the
#' parcel is unprotected (unowned).
#' @param Silence A binary parameter: if `FALSE`, the function will report a
#' summary message. If `TRUE`, the function will be silent.  (Default: FALSE)
#' @param Env the environment where the package should create or access variables.
#' By default the package works in the R's Global environment. (Default: .GlobalEnv)
#'
#'
#' @examples
#' \dontrun{
#'
#' Parcels_Create(Parcels=c("Parcel_1","Parcel_2","Parcel_3","Parcel_4","Parcel_5"),
#'                   Status= c(0,1,0,1,1))
#' }
#'
#'
#' @return NULL (creates some variables in the environment but returns nothing)
#' @family Parcels Status
#'
#' @export
Parcels_Create <-
  function(Parcels,
           Status,
           Silence = FALSE, Env= .GlobalEnv) {
    ################
    #    Errors    #
    ################

    if (length(Parcels)!= length(Status)) {
      stop('Parcels and Status are not from the same length.')
    }



    ################
    #     Body     #
    ################


    Env$Parcels <- Parcels
    Env$Status <- as.integer(Status)

    ################
    #    Reports   #
    ################


    if (Silence == FALSE) {
      cat("Number of Parcels= ",length(Env$Parcels), " . \n")
      cat(
        "The problem has ",
        sum(Env$Status),
        " protected and ",
        length(Env$Parcels) - sum(Env$Status),
        " unprotected parcels.\n"
      )
      cat("Creating Parcels Completed.\n")
    }

  }
