





#' Import Risk Objective data from Excel file
#'
#'
#' \code{Risk_Objective_Import} imports the Risk objective's data from an excel file.
#' The required data are a sheet containing parcels' \bold{Sigma} and a sheet containing parcels' \bold{Correlation}.\cr
#' Function creates a list named \code{Risk_Objective}, where `Risk_Objective[["Sigma"]]` stores the the sigmas of parcels,
#' `Risk_Objective[["Correlation"]]` stores the the Correlation of parcels.\cr
#' This function is not sensitive to the order the parcels are stored in the sheet and will automatically store parcels sorted with the same order they were stored in \code{Parcels}.
#'
#'
#' Risk objective is a quadratic function representing the portfolio optimization problem, and this function is to be \bold{Minimized}.
#' The optimal value for this objective is \bold{zero} and the optimal solution is to have all parcels \bold{divested (unprotected)}.
#' Therefore, there is no single objective optimizer for this objective, as its optimal solution is priorly known. (For more information refer to the paper)\cr
#'
#' To import the data, the excel file should include two sheets. A sheet including parcels' sigmas, and the name of the sheet is passed as `Sigma_Sheet`.\cr
#' The first row of this sheet will be ignored as captions. The first column of the sheet should include names, and the second column should include sigmas.\cr
#' Here is an example Sigma table:
#'
#' \if{html}{\figure{Variance.jpg}{}}
#' \if{latex}{\figure{Variance.jpg}{options: width=2.0in}}\cr
#'
#' The second sheet should include the matrix of parcels' correlation, and the name of the sheet is passed as `Correlation_Sheet`.\cr
#' The first row of this sheet will be ignored. If the problem has \code{n} parcels, then the correlation sheet should
#'  have \code{n+1} columns. The first column of the sheet should include names and the other \code{n} columns should have the
#'  \code{n*n} correlation matrix. The Correlation matrix should be a symmetric matrix whose diogonal elements are all one.\cr
#'  The member in the \code{i+1}'th row and \code{j+1}'th column will then be the correlation between the parcels whose names are in the \code{i+1} and \code{j+1}
#'  row in the first column. Remember the first column has the parcel names.\cr
#'  Here is an example Correlation table:
#'
#' ![](Correlation.jpg "Example Plot Title")\cr
#'
#'
#'
#' @usage Risk_Objective_Import(Address, Sigma_Sheet = "Sigma",
#'                              Correlation_Sheet= "Correlation", Silence = FALSE,
#'                              Env= .GlobalEnv)
#'
#' @param Address A string: the location and name of the excel file.
#' @param Sigma_Sheet A string: the name of the sheet holding the parcels' sigmas' data. (Default: "Sigma")
#' @param Correlation_Sheet A string: the name of the sheet holding the parcels' Correlations' data. (Default: "Correlation")
#' @param Silence A binary parameter: if `FALSE`, the function will report a summary message. If `TRUE`, the function will be silent.  (Default: FALSE)
#' @param Env the environment where the package should create or access variables.
#' By default the package works in the R's Global environment. (Default: .GlobalEnv)
#'
#'
#' @examples
#' \dontrun{
#'
#' Risk_Objective_Import("C:\\example.xlsx", Sigma_Sheet = "Sigma",
#'                       Correlation_Sheet= "Correlation")
#'}
#'
#' @return NULL (creates some variables in the environment but returns nothing)
#' @family Risk Objective
#' @family Import Functions
#' @export
Risk_Objective_Import <- function(Address, Sigma_Sheet = "Sigma", Correlation_Sheet= "Correlation", Silence = FALSE, Env= .GlobalEnv) {

  ################
  #    Errors    #
  ################
  if (exists('Parcels', envir = Env) == FALSE || exists('Status', envir = Env) == FALSE) {
    stop('Parcels and Status Should be created first.')
  }

  if(Sigma_Sheet %in% excel_sheets(Address)==FALSE){
    stop('No sheet named ',Sigma_Sheet,' was found.')
  }else if(Correlation_Sheet %in% excel_sheets(Address)==FALSE){
    stop('No sheet named ',Correlation_Sheet,' was found.')
  }



  ################
  #     Body     #
  ################
  Env$Risk_Objective <-list()
  Env$Risk_Objective[["Sigma"]]<-.Import_Sigma(Address,Sigma_Sheet,Silence,Env)
  Env$Risk_Objective[["Correlation"]]<-.Import_Correlation(Address,Correlation_Sheet,Silence,Env)

  ################
  #    Reports   #
  ################

  if (Silence == FALSE) {
    cat('Importing Risk Objective Completed. \n')
  }
}


#' Create Risk Objective
#'
#'
#' \code{Risk_Objective_Create} creates the problem's Risk objective function.
#' It creates a list named \code{Risk_Objective}, where `Risk_Objective[["Sigma"]]` stores the the sigmas of parcels, and\cr
#' `Risk_Objective[["Correlation"]]` stores the the Correlation of parcels.\cr
#' This function is not sensitive to the order the parcels are stored in the sheet and will automatically store parcels sorted with the same order they were stored in \code{Parcels}.
#'
#'
#'
#'
#' @usage Risk_Objective_Create(Parcels, Sigma, Correlation,
#'                              Silence=FALSE, Env= .GlobalEnv)
#'
#' @param Parcels A vector of strings: the name of the parcels appearing in the objective
#' @param Sigma A vector of doubles: the respective sigmas of the parcels in `Parcels`.
#' @param Correlation A matrix of doubles: a symmetric matrix with diogonal elements equal to one.
#' The element in `i`'th row and `j`'th column is the correlation between the `i` and `j` member of `Parcels`.
#' @param Silence A binary parameter: if `FALSE`, the function will report a summary message. If `TRUE`, the function will be silent.  (Default: FALSE)
#' @param Env the environment where the package should create or access variables.
#' By default the package works in the R's Global environment. (Default: .GlobalEnv)
#'
#'
#' @examples
#' \dontrun{
#'
#' Risk_Objective_Create(Parcels = c("Parcel_1","Parcel_2","Parcel_3"),
#'                       Sigma = c(1,2,3),
#'                      Correlation = cbind(c(1,0.5,-0.7),c(0.5,1,0.2),c(-0.7,0.2,1)))
#'
#' ## Correlation matrix in this example is
#'  | +1.0 | +0.5 | -0.7 |
#'  | +0.5 | +1.0 | +0.2 |
#'  | -0.7 | +0.2 | +1.0 | ##
#'}
#'
#'
#' @return NULL (creates some variables in the environment but returns nothing)
#' @family Risk Objective
#' @family Objective Creators
#' @export
Risk_Objective_Create <- function(Parcels, Sigma, Correlation, Silence=FALSE, Env= .GlobalEnv) {
  ################
  #    Errors    #
  ################
  if (exists('Parcels', envir = Env) == FALSE || exists('Status', envir = Env) == FALSE) {
    stop('Parcels and Status Should be created first.')
  }


  if(all(Parcels%in%Env$Parcels)==FALSE){
    stop('Undefined Parcels: "',
         paste(Parcels[which(!Parcels %in% Env$Parcels)], collapse = ' '),
         '"')
  }

  if(all( Env$Parcels %in% Parcels)==FALSE){
    stop('No input found for: "',
         paste(Env$Parcels[which(!Env$Parcels %in% Parcels)], collapse = ' '),
         '"')
  }

  if(length(Sigma) != length(Parcels)){
    stop('Sigma should be same length as Parcels. \n')
  }

  if(length(Correlation) != length(Parcels)^2 || length(Correlation[1,])!=length(Parcels) || length(Correlation[,1])!=length(Parcels)){
    stop('Correlation should be a ',length(Parcels),'*',length(Parcels),' matrix.\n')
  }



  ################
  #     Body     #
  ################
  Env$Risk_Objective <- list();
  Env$Risk_Objective[["Sigma"]]<- rep(0, length(Env$Parcels))
  Env$Risk_Objective[["Sigma"]][match(Parcels,Env$Parcels)] <-
    as.double(Sigma)

  rownames(Correlation)<-match(Parcels,Env$Parcels)
  colnames(Correlation)<-match(Parcels,Env$Parcels)
  Correlation<-Correlation[order(as.integer(rownames(Correlation))),]
  Correlation<-Correlation[,order(as.integer(colnames(Correlation)))]
  rownames(Correlation)<-NULL
  colnames(Correlation)<-NULL
  Env$Risk_Objective[["Correlation"]] <-as.matrix(apply(Correlation,c(1,2),FUN = as.double))



  ################
  #    Reports   #
  ################

  if (Silence == FALSE) {
    cat("Risk Objective: Sigmas of parcels ",
        Env$Parcels[1],
        " and ",
        Env$Parcels[length(Env$Parcels)],
        " are ",
        Sigma[1],
        " and ",
        Sigma[length(Env$Parcels)],
        ".\n"
    )
    cat("Correlation between parcels ",
        Env$Parcels[1],
        " and ",
        Env$Parcels[length(Env$Parcels)],
        " is ",
        Env$Risk_Objective[["Correlation"]][1,length(Env$Parcels)],
        ".\n"
    )
    cat('Creating Risk Objective Completed.')
  }


}
