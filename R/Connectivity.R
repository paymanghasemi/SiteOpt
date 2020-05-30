#'
#' Create a Connectivity Constraint
#'
#' \code{Connectivity_Add_Constraint} creates a Connectivity Constraint in the optimization model of the form:\cr
#' \eqn{Target \le \sum (Condition)  }.\cr
#' This constraint means that the parcel `Target` can be protected (invested)
#' \bold{only if} at least one of the parcels in `Condition` are protected (invested).
#'
#'
#'
#' When used for the first time, `Connectivity_Add_Constraint` or [Connectivity_Import_Constraint]
#'  will create a list named `Connectivity` in the environment.
#' When, for the first time, a connectivity constraint is added for a `Target` parcel, for instance named "Parcel_1",
#' a new member named "Parcel_1" will be added to the `Connectivity` list, i.e. `Connectivity[["Parcel_1"]]`.
#'
#' Then, if `Repetition=FALSE`, whenever `Connectivity_Add_Constraint` or [Connectivity_Import_Constraint] are used for "Parcel_1", the `Condition`
#' parcels will be added to the "Parcel_1"'s first connectivity constraint, i.e. `Connectivity[["Parcel_1"]][[1]]`.
#' Otherwise, if `Repetition=TRUE`, the `Condition` parcels will create a new constraint for "Parcel_1", i.e. `Connectivity[["Parcel_1"]][[i+1]]`,
#' where `i` is the number of "Parcel_1"'s existing connectivity constraints.
#'
#'
#'
#' \strong{Removing a specific connectivity constraint of a `Target`:} Connectivity constraint `i` of a parcel named "Parcel_j"
#' can be removed using the following code:
#'
#' `Connectivity[["Parcel_j"]][[i]]<-NULL`
#'
#' \strong{Removing all Connectivity constraints of a `Target`:} Connectivity constraints of a parcel named "Parcel_j"
#' can be removed using the following code:
#'
#' `Connectivity[["Parcel_j"]]<-NULL`
#'
#'
#' \strong{Remove all Connectivity Constraints:} All of the Connectivity constraints can be removed using the following code:
#'
#' `remove(Connectivity)`
#'
#' @usage Connectivity_Add_Constraint(Target, Condition, Repetition = TRUE,
#'                                    Silence=FALSE, Env= .GlobalEnv)
#'
#' @param Target A string: the name of a parcel which can only be protected (invested) only if at least one of the `Condition` parcels are protected (invested)
#' @param Condition A vector of strings: the names of the parcels that if at least one of them are protected (invested), the `Target` can be protected (invested).
#' @param Repetition A binary parameter: If `TRUE`, the `Condition` parcels will create a new constraint for `Target`.
#' If `FALSE`, the `Condition` parcels will be added to the \bold{first} existing connectivity constraint of `Target`, i.e. `Connectivity[["Target"]][[1]]`. (Default: TRUE)
#' @param Silence A binary parameter: if `FALSE`, the function will report a summary message. If `TRUE`, the function will be silent.  (Default: FALSE)
#' @param Env the environment where the package should create or access variables.
#' By default the package works in the R's Global environment. (Default: .GlobalEnv)
#'
#'
#' @examples
#'\dontrun{
#'
#' Connectivity_Add_Constraint(Target= "Parcel_1", Condition= "Parcel_4",
#'                            Repetition=TRUE)
#'
#' ## This will create Connectivity[["Parcel_1"]][[1]] and
#' add the following constraint to the optimization model
#' Parcel_1 <= Parcel_4 . Now, if the code ##
#'
#' Connectivity_Add_Constraint(Target= "Parcel_1", Condition= c("Parcel_2","Parcel_3"),
#'                             Repetition=TRUE)
#'
#' ## is run, Connectivity[["Parcel_1"]][[2]] will be created,
#' and the following constraint will be added to the optimization model
#' Parcel_1 <= Parcel_2 + Parcel_3 . However, if the code ##
#'
#' Connectivity_Add_Constraint(Target= "Parcel_1", Condition= c("Parcel_2","Parcel_3"),
#'                             Repetition=FALSE)
#'
#' ## is run, only Connectivity[["Parcel_1"]][[1]] will be updated,
#'  and its existing constraint will be updated in the optimization model as
#' Parcel_1 <= Parcel_4 + Parcel_2 + Parcel_3 .
#'}
#'
#'
#' @return NULL (creates some variables in the environment but returns nothing)
#' @family Connectivity Constraints
#' @family Constraints
#'
#' @export
Connectivity_Add_Constraint <- function(Target, Condition, Repetition = TRUE,Silence=FALSE, Env= .GlobalEnv) {

  ################
  #    Errors    #
  ################
  if (exists('Parcels', envir = Env) == FALSE || exists('Status', envir = Env) == FALSE) {
    stop('Parcels and Status Should be created first.')
  }

  if(all(c(Target,Condition)%in% Env$Parcels)==FALSE){
    stop('Undefined Parcels: "',
         paste(c(Target,Condition)[which(!c(Target,Condition) %in% Env$Parcels)], collapse = ' '),
         '"')
  }
  ################
  #    Reports   #
  ################
  if (exists("Connectivity", envir = Env) == 0) {
    if(Silence==FALSE){
      cat("Including connectivity Constraints.\n")}
    Env$Connectivity <- list()
  } else if(Silence==FALSE){
    cat("Adding connectivity Constraints.\n")
  }

  if(Silence==FALSE){
    if(Repetition == TRUE || is.null(Env$Connectivity[[as.character(Target)]]) == 1){
      cat("A new connectivity constraint for ",Target," is created with ",length(unique(Condition))," conditions. \n")
    }else{
      cat("The first connectivity constraint of ",Target," is updated with ",length(setdiff(Condition,Env$Connectivity[[as.character(Target)]][[1]]))," new conditions. \n")
    }
  }

  ################
  #     Body     #
  ################


  if (Repetition == FALSE) {
    if (is.null(Env$Connectivity[[as.character(Target)]]) == 1) {
      if (length(unique(Condition)) == 1) {
        Env$Connectivity[[as.character(Target)]] <-
          as.list(unique(Condition))
      } else{
        Env$Connectivity[[as.character(Target)]] <-
          unique(Condition)
      }
    } else{
      if (length(unique(c(Env$Connectivity[[as.character(Target)]][[1]], Condition))) >
          1) {
        Env$Connectivity[[as.character(Target)]][[1]] <-
          unique(c(Env$Connectivity[[as.character(Target)]][[1]], Condition))
      }
    }


  } else if (Repetition == TRUE) {
    if (is.null(Env$Connectivity[[as.character(Target)]]) == 1) {
      if (length(unique(Condition)) == 1) {
        Env$Connectivity[[as.character(Target)]] <-
          as.list(unique(Condition))
      } else{
        Env$Connectivity[[as.character(Target)]][[1]] <-
          unique(Condition)
      }
    } else{
      Env$Connectivity[[as.character(Target)]][[length(Env$Connectivity[[as.character(Target)]]) +
                                              1]] <-
        unique(Condition)
    }

  }
}







#'
#' Import Connectivity Constraints' data from Excel file
#'
#' \code{Connectivity_Import_Constraint} imports the Connectivity Constraints' data from a sheet in an excel file.
#' Everytime used, it will update the list named \code{Connectivity} which will store the connectivity constraints.\cr
#' (For more information, refer to [Connectivity_Add_Constraint])
#' \cr
#'
#'
#' The first row of the sheet will be ignored as captions, and in the remaining rows, each row includes a Connectivity Constraint.
#' The first column of the sheet should have names of `Target` parcels,
#' and each cell in front of a `Target` includes a parcel from the group of `Condition` parcels.
#'
#'  Here is an example Connectivity constraint table:
#'
#' \if{html}{\figure{Connectivity.jpg}{}}
#' \if{latex}{\figure{Connectivity.jpg}{options: width=2.0in}}\cr
#'
#' If `Repetition=TRUE`, function creates `Connectivity[["Parcel_5"]][[1]]` and\cr
#' `Connectivity[["Parcel_5"]][[2]]`, and the following constraints will be added to the optimization model\cr
#' `Parcel__5 <= Parcel__1`,\cr
#' `Parcel__5 <= Parcel__4`.\cr
#' If `Repetition=FALSE`, only `Connectivity[["Parcel_1"]][[1]]` will be created,
#' and only the following constraint will be added to the optimization model\cr
#' `Parcel_5 <= Parcel_1 + Parcel_4`.\cr
#'
#'
#' \strong{Removing a specific connectivity constraint of a `Target`:} Connectivity constraint `i` of a parcel named "Parcel_j"
#' can be removed using the following code:
#'
#' `Connectivity[["Parcel_j"]][[i]]<-NULL`
#'
#' \strong{Removing all connectivity constraints of a `Target`:} Connectivity constraints of a parcel named "Parcel_j"
#' can be removed using the following code:
#'
#' `Connectivity[["Parcel_j"]]<-NULL`
#'
#'
#' \strong{Remove all Connectivity Constraints:} All of the Connectivity constraints can be removed using the following code:
#'
#' `remove(Connectivity)`
#'
#'
#'
#' @usage Connectivity_Import_Constraint(Address, Sheet= "Connectivity",
#'                                       Repetition= TRUE, Silence= FALSE, Env= .GlobalEnv)
#'
#' @param Address A string: the location and name of the excel file.
#' @param Sheet A string: the name of the sheet holding the connectivity constraints data. (Default: "Connectivity")
#' @param Repetition A binary parameter: If `TRUE`, the `Condition` parcels will create a new constraint for `Target`.
#' If `FALSE`, the `Condition` parcels will be added to the \bold{first} existing Connectivity constraint of `Target`, i.e. `Connectivity[["Target"]][[1]]`. (Default: TRUE)
#' @param Silence A binary parameter: if `FALSE`, the function will report a summary message. If `TRUE`, the function will be silent.  (Default: FALSE)
#' @param Env the environment where the package should create or access variables.
#' By default the package works in the R's Global environment. (Default: .GlobalEnv)
#'
#' @examples
#' \dontrun{
#'
#' Connectivity_Import_Constraint("C:\\example.xlsx", Sheet= "Connectivity", Repetition= TRUE)
#'
#'}
#'
#'
#' @family Connectivity Constraints
#'
#'
#' @return NULL (creates some variables in the environment but returns nothing)
#' @export
Connectivity_Import_Constraint <-
  function(Address,
           Sheet= "Connectivity",
           Repetition = TRUE,
           Silence = FALSE,
           Env= .GlobalEnv) {


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

    ################
    #    Errors    #
    ################
    if (all(mydata[which(!mydata[,] %in% NA)] %in% Env$Parcels) == FALSE) {
      stop('Undefined Parcels: "', paste(mydata[which(!mydata %in% Env$Parcels &
                                                        !mydata %in% NA)], collapse = ' '), '"')
    }

    ################
    #    Reports   #
    ################
    if (exists("Connectivity", envir = Env) == 0) {
      if (Silence == FALSE) {
        cat("Including connectivity Constraints.\n")
      }
      Env$Connectivity <- list()
    } else if (Silence == FALSE) {
      cat("Adding connectivity Constraints.\n")
    }

    # if(Silence==FALSE){
    #   for (i in 1:nrow(mydata)) {
    #     Temp <-
    #       c(as.vector(mydata[i, colSums(is.na(mydata[i, , drop = FALSE])) == 0]))
    #
    #     if(Repetition == TRUE || is.null(Env$Connectivity[[as.character(mydata[i, 1])]]) == 1){
    #       cat("A new connectivity constraint for ",mydata[i, 1]," is created with ",length(unique(Temp[-(1)]))," conditions. \n")
    #     }else{
    #       cat("The first connectivity constraint of ",mydata[i, 1]," is updated with ",length(setdiff(Temp[-(1)],Env$Connectivity[[as.character(mydata[i, 1])]][[1]]))," new conditions. \n")
    #     }
    #   }
    # }

    ################
    #     Body     #
    ################


    if (Repetition == FALSE) {
      for (i in 1:nrow(mydata)) {
        Temp <-
          c(as.vector(mydata[i, colSums(is.na(mydata[i, , drop = FALSE])) == 0]))

        if(Silence==FALSE){
            if(is.null(Env$Connectivity[[as.character(mydata[i, 1])]]) == 1){
              cat("A new connectivity constraint for ",mydata[i, 1]," is created with ",length(unique(Temp[-(1)]))," conditions. \n")
            }else{
              cat("The first connectivity constraint of ",mydata[i, 1]," is updated with ",length(setdiff(Temp[-(1)],Env$Connectivity[[as.character(mydata[i, 1])]][[1]]))," new conditions. \n")
            }
          }





        if (is.null(Env$Connectivity[[as.character(mydata[i, 1])]]) == 1) {
          if (length(unique(Temp[-(1)])) == 1) {
            Env$Connectivity[[as.character(mydata[i, 1])]] <-
              as.list(unique(Temp[-(1)]))
          } else{
            Env$Connectivity[[as.character(mydata[i, 1])]][[1]] <-
              unique(Temp[-(1)])
          }
        } else{
          if (length(unique(c(Env$Connectivity[[as.character(mydata[i, 1])]][[1]], Temp[-(1)]))) >
              1) {
            Env$Connectivity[[as.character(mydata[i, 1])]][[1]] <-
              unique(c(Env$Connectivity[[as.character(mydata[i, 1])]][[1]], Temp[-(1)]))
          }
        }
      }

    } else if (Repetition == TRUE) {
      for (i in 1:nrow(mydata)) {
        Temp <-
          c(as.vector(mydata[i, colSums(is.na(mydata[i, , drop = FALSE])) == 0]))


        if(Silence==FALSE){

            cat("A new connectivity constraint for ",mydata[i, 1]," is created with ",length(unique(Temp[-(1)]))," conditions. \n")

        }


        if (is.null(Env$Connectivity[[as.character(mydata[i, 1])]]) == 1) {
          if (length(unique(Temp[-(1)])) == 1) {
            Env$Connectivity[[as.character(mydata[i, 1])]] <-
              as.list(unique(Temp[-(1)]))
          } else{
            Env$Connectivity[[as.character(mydata[i, 1])]][[1]] <-
              unique(Temp[-(1)])
          }
        } else{
          Env$Connectivity[[as.character(mydata[i, 1])]][[length(Env$Connectivity[[as.character(mydata[i, 1])]]) +
                                                        1]] <-
            unique(Temp[-(1)])
        }
      }
    }

  }

