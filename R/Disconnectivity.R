



#'
#' Create a Disconnectivity Constraint
#'
#' \code{Disconnectivity_Add_Constraint} creates a Disconnectivity Constraint in the optimization model of the form:\cr
#' \eqn{\sum (Condition)  + C * Target \le C},\cr
#' where \code{C} is the number of parcels in `Condition`. This constraint means that the parcel `Target` can be protected (invested)
#' \bold{only if} none of the parcels in `Condition` are protected (invested).
#'
#'
#'
#' When used for the first time, `Disconnectivity_Add_Constraint` or\cr
#' [Disconnectivity_Import_Constraint] will create a list named `Disconnectivity`.
#' When, for the first time, a disconnectivity constraint is added for a `Target` parcel, for instance named "Parcel_1",
#' a new member named "Parcel_1" will be added to the `Disconnectivity` list, i.e. `Disconnectivity[["Parcel_1"]]`.
#'
#' Then, if `Repetition=FALSE`, whenever `Disconnectivity_Add_Constraint` or\cr
#' [Disconnectivity_Import_Constraint] are used for "Parcel_1", the `Condition` parcels will be added to "Parcel_1"'s
#'  first disconnectivity constraint, i.e. `Disconnectivity[["Parcel_1"]][[1]]`.
#' Otherwise, if `Repetition=TRUE`, the `Condition` parcels will create a new constraint for "Parcel_1", i.e.\cr
#' `Disconnectivity[["Parcel_1"]][[i+1]]`, where `i` is the number of "Parcel_1"'s existing disconnectivity constraints.
#'
#'
#' \strong{Removing a specific disconnectivity constraint of a `Target`:} Disconnectivity constraint `i` of a parcel named "Parcel_j"
#' can be removed using the following code:
#'
#' `Disconnectivity[["Parcel_j"]][[i]]<-NULL`
#'
#' \strong{Removing all disconnectivity constraints of a `Target`:} Disconnectivity constraints of a parcel named "Parcel_j"
#' can be removed using the following code:
#'
#' `Disconnectivity[["Parcel_j"]]<-NULL`
#'
#'
#' \strong{Remove all Disconnectivity Constraints:} All of the Disconnectivity constraints can be removed using the following code:
#'
#' `remove(Disconnectivity)`
#'
#' @usage Disconnectivity_Add_Constraint(Target, Condition, Repetition = TRUE,
#'                                       Silence=FALSE, Env= .GlobalEnv)
#'
#' @param Target A string: the name of a parcel which can only be protected (invested) if none of the `Condition` parcels are protected (invested).
#' @param Condition A vector of strings: the names of the parcels that if at least one of them are protected (invested), the `Target` cannot be protected (invested).
#' @param Repetition A binary parameter: If `TRUE`, the `Condition` parcels will create a new constraint for `Target`.
#' If `FALSE`, the `Condition` parcels will be added to the \bold{first} existing disconnectivity constraint of `Target`, i.e. `Disconnectivity[["Target"]][[1]]`. (Default: TRUE)
#' @param Silence A binary parameter: if `FALSE`, the function will report a summary message. If `TRUE`, the function will be silent.  (Default: FALSE)
#' @param Env the environment where the package should create or access variables.
#' By default the package works in the R's Global environment. (Default: .GlobalEnv)
#'
#' @examples
#' \dontrun{
#'
#' Disconnectivity_Add_Constraint(Target= "Parcel_1", Condition= "Parcel_4", Repetition=TRUE)
#'
#' ## will create Disconnectivity[["Parcel_1"]][[1]] and
#' add the following constraint to the optimization model
#' Parcel_4 + Parcel_1 <= 1 . Now, if the code ##
#'
#' Disconnectivity_Add_Constraint(Target= "Parcel_1", Condition= c("Parcel_2","Parcel_3"),
#'                                Repetition=TRUE)
#'
#' ## is run, Disconnectivity[["Parcel_1"]][[2]] will be created,
#' and the following constraint will be added to the optimization model
#' Parcel_2 + Parcel_3 + 2*Parcel_1 <= 2 . However, if the code ##
#'
#' Disconnectivity_Add_Constraint(Target= "Parcel_1", Condition= c("Parcel_2","Parcel_3"),
#'                                Repetition=FALSE)
#'
#' ## is run, only Disconnectivity[["Parcel_1"]][[1]] will be updated,
#' and its existing constraint will be updated in the optimization model as
#' Parcel_4 + Parcel_2 + Parcel_3 + 3*Parcel_1 <= 3 . ##
#'
#'}
#'
#'
#'
#' @return NULL (creates some variables in the environment but returns nothing)
#' @family Disconnectivity Constraints
#' @family Constraints
#'
#' @export

Disconnectivity_Add_Constraint <-
  function(Target, Condition, Repetition = TRUE,Silence=FALSE, Env= .GlobalEnv) {

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
    if (exists("Disconnectivity", envir = Env) == 0) {
      if(Silence==FALSE){
        cat("Including Disconnectivity Constraints.\n")}
      Env$Disconnectivity <- list()
    } else if(Silence==FALSE){
      cat("Adding Disconnectivity Constraints.\n")
    }


    if(Silence==FALSE){
      if(Repetition == TRUE || is.null(Env$Disconnectivity[[as.character(Target)]]) == 1){
        cat("A new Disconnectivity constraint for ",Target," is created with ",length(unique(Condition))," conditions. \n")
      }else{
        cat("The first Disconnectivity constraint of ",Target," is updated with ",length(setdiff(Condition,Env$Disconnectivity[[as.character(Target)]][[1]]))," new conditions. \n")
      }
    }
    ################
    #     Body     #
    ################


    if (Repetition == FALSE) {
      if (is.null(Env$Disconnectivity[[as.character(Target)]]) == 1) {
        if (length(unique(Condition)) == 1) {
          Env$Disconnectivity[[as.character(Target)]] <-
            as.list(unique(Condition))
        } else{
          Env$Disconnectivity[[as.character(Target)]] <-
            unique(Condition)
        }
      } else{
        if (length(unique(c(Env$Disconnectivity[[as.character(Target)]][[1]], Condition))) >
            1) {
          Env$Disconnectivity[[as.character(Target)]][[1]] <-
            unique(c(Env$Disconnectivity[[as.character(Target)]][[1]], Condition))
        }
      }


    } else if (Repetition == TRUE) {
      if (is.null(Env$Disconnectivity[[as.character(Target)]]) == 1) {
        if (length(unique(Condition)) == 1) {
          Env$Disconnectivity[[as.character(Target)]] <-
            as.list(unique(Condition))
        } else{
          Env$Disconnectivity[[as.character(Target)]][[1]] <-
            unique(Condition)
        }
      } else{
        Env$Disconnectivity[[as.character(Target)]][[length(Env$Disconnectivity[[as.character(Target)]]) +
                                                   1]] <-
          unique(Condition)
      }

    }
  }






#'
#' Import Disconnectivity Constraints' data from Excel file
#'
#' \code{Disconnectivity_Import_Constraint} imports the Disconnectivity Constraints' data from a sheet in an excel file.
#' Everytime used, it will update the list named \code{Disconnectivity} which will store the disconnectivity constraints.\cr
#' (For more information, refer to [Disconnectivity_Add_Constraint])
#' \cr
#'
#'
#' The first row of the sheet will be ignored as captions, and in the remaining rows, each row includes a Disconnectivity Constraint.
#' The first column of the sheet should have names of `Target` parcels,
#' and each cell in front of a `Target` includes a parcel from the group of `Condition` parcels.
#'
#'  Here is an example Disconnectivity constraint table:
#'
#' \if{html}{\figure{Disconnectivity.jpg}{}}
#' \if{latex}{\figure{Disconnectivity.jpg}{options: width=2.0in}}\cr
#'
#' If `Repetition=TRUE`, function creates `Disconnectivity[["Parcel_1"]][[1]]` and \cr
#' `Disconnectivity[["Parcel_1"]][[2]]`, and the following constraints will be added to the optimization model\cr
#' `Parcel_4 + Parcel_1 <= 1`,\cr
#' `Parcel_2 + Parcel_3 + 2*Parcel_1 <= 2`.\cr
#' If `Repetition=FALSE`, only `Disconnectivity[["Parcel_1"]][[1]]` will be created, and only the following constraint will be added to the optimization model\cr
#' `Parcel_4 + Parcel_2 + Parcel_3 + 3*Parcel_1 <= 3`.\cr
#'
#'
#' \strong{Removing a specific disconnectivity constraint of a `Target`:} Disconnectivity constraint `i` of a parcel named "Parcel_j"
#' can be removed using the following code:
#'
#' `Disconnectivity[["Parcel_j"]][[i]]<-NULL`
#'
#' \strong{Removing all disconnectivity constraints of a `Target`:} Disconnectivity constraints of a parcel named "Parcel_j"
#' can be removed using the following code:
#'
#' `Disconnectivity[["Parcel_j"]]<-NULL`
#'
#'
#' \strong{Remove all Disconnectivity Constraints:} All of the Disconnectivity constraints can be removed using the following code:
#'
#' `remove(Disconnectivity)`
#'
#'
#'
#' @usage Disconnectivity_Import_Constraint(Address, Sheet= "Disconnectivity",
#'                                          Repetition= TRUE, Silence= FALSE,
#'                                          Env= .GlobalEnv)
#'
#' @param Address A string: the location and name of the excel file.
#' @param Sheet A string: the name of the sheet holding the disconnectivity constraints data. (Default: "Disconnectivity")
#' @param Repetition A binary parameter: If `TRUE`, the `Condition` parcels will create a new constraint for `Target`.
#' If `FALSE`, the `Condition` parcels will be added to the \bold{first} existing disconnectivity constraint of `Target`, i.e. `Disconnectivity[["Target"]][[1]]`. (Default: TRUE)
#' @param Silence A binary parameter: if `FALSE`, the function will report a summary message. If `TRUE`, the function will be silent.  (Default: FALSE)
#' @param Env the environment where the package should create or access variables.
#' By default the package works in the R's Global environment. (Default: .GlobalEnv)
#'
#' @examples
#' \dontrun{
#'
#' Disconnectivity_Import_Constraint("C:\\example.xlsx", Sheet= "Disconnectivity",
#'                                   Repetition= TRUE)
#'
#'}
#'
#'
#' @family Disconnectivity Constraints
#'
#'
#' @return NULL (creates some variables in the environment but returns nothing)
#' @export
Disconnectivity_Import_Constraint <-
  function(Address,
           Sheet= "Disconnectivity",
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
    if (exists("Disconnectivity", envir = Env) == 0) {
      if (Silence == FALSE) {
        cat("Including Disconnectivity Constraints.\n")
      }
      Env$Disconnectivity <- list()
    } else if (Silence == FALSE) {
      cat("Adding Disconnectivity Constraints.\n")
    }

    # if(Silence==FALSE){
    #   for (i in 1:nrow(mydata)) {
    #     Temp <-
    #       c(as.vector(mydata[i, colSums(is.na(mydata[i, , drop = FALSE])) == 0]))
    #
    #     if(Repetition == TRUE || is.null(Env$Disconnectivity[[as.character(mydata[i, 1])]]) == 1){
    #       cat("A new Disconnectivity constraint for ",mydata[i, 1]," is created with ",length(unique(Temp[-(1)]))," conditions. \n")
    #     }else{
    #       cat("The first Disconnectivity constraint of ",mydata[i, 1]," is updated with ",length(setdiff(Temp[-(1)],Env$Disconnectivity[[as.character(mydata[i, 1])]][[1]]))," new conditions. \n")
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
            if(is.null(Env$Disconnectivity[[as.character(mydata[i, 1])]]) == 1){
              cat("A new Disconnectivity constraint for ",mydata[i, 1]," is created with ",length(unique(Temp[-(1)]))," conditions. \n")
            }else{
              cat("The first Disconnectivity constraint of ",mydata[i, 1]," is updated with ",length(setdiff(Temp[-(1)],Env$Disconnectivity[[as.character(mydata[i, 1])]][[1]]))," new conditions. \n")
            }
        }




        if (is.null(Env$Disconnectivity[[as.character(mydata[i, 1])]]) == 1) {
          if (length(unique(Temp[-(1)])) == 1) {
            Env$Disconnectivity[[as.character(mydata[i, 1])]] <-
              as.list(unique(Temp[-(1)]))
          } else{
            Env$Disconnectivity[[as.character(mydata[i, 1])]][[1]] <-
              unique(Temp[-(1)])
          }
        } else{
          if (length(unique(c(Env$Disconnectivity[[as.character(mydata[i, 1])]][[1]], Temp[-(1)]))) >
              1) {
            Env$Disconnectivity[[as.character(mydata[i, 1])]][[1]] <-
              unique(c(Env$Disconnectivity[[as.character(mydata[i, 1])]][[1]], Temp[-(1)]))
          }
        }
      }
    }
    else if (Repetition == TRUE) {
      for (i in 1:nrow(mydata)) {
        Temp <-
          c(as.vector(mydata[i, colSums(is.na(mydata[i, , drop = FALSE])) == 0]))



        if(Silence==FALSE){
              cat("A new Disconnectivity constraint for ",mydata[i, 1]," is created with ",length(unique(Temp[-(1)]))," conditions. \n")
        }


        if (is.null(Env$Disconnectivity[[as.character(mydata[i, 1])]]) == 1) {
          if (length(unique(Temp[-(1)])) == 1) {
            Env$Disconnectivity[[as.character(mydata[i, 1])]] <-
              as.list(unique(Temp[-(1)]))
          } else{
            Env$Disconnectivity[[as.character(mydata[i, 1])]][[1]] <-
              unique(Temp[-(1)])
          }
        } else{
          Env$Disconnectivity[[as.character(mydata[i, 1])]][[length(Env$Disconnectivity[[as.character(mydata[i, 1])]]) +
                                                           1]] <-
            unique(Temp[-(1)])
        }
      }
    }
  }
