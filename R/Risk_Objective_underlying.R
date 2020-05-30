.Import_Sigma <- function(Address,
                            Sheet,
                            Silence,Env) {

  ################
  #     Body     #
  ################
  suppressMessages(my_data <-
                     as.matrix(read_excel(Address, sheet = Sheet)))

  ################
  #    Errors    #
  ################

  if (all(my_data[, 1] %in% Env$Parcels) == FALSE) {
    stop('Undefined Parcels in ',Sheet,' "',
         paste(my_data[, 1][which(!my_data[, 1] %in% Env$Parcels)], collapse = ' '),
         '"')
  }

  if (all(Env$Parcels %in% my_data[, 1]) == FALSE) {
    stop('No Sigma found for: "',
         paste(Env$Parcels[which(!Env$Parcels %in% my_data[, 1])], collapse = ' '),
         '"')
  }

  ################
  #     Body     #
  ################
  Temp_sigma<- rep(0, length(Env$Parcels))
  Temp_sigma[match(as.character(my_data[, 1]),Env$Parcels)] <-
    as.double(my_data[, 2])


  ################
  #    Reports   #
  ################

  if (Silence == FALSE) {
    cat(
      "Risk Objective: Sigma of parcels ",
      Env$Parcels[1],
      " and ",
      Env$Parcels[length(Env$Parcels)],
      " are ",
      Temp_sigma[1],
      " and ",
      Temp_sigma[length(Env$Parcels)],
      ".\n"
    )
  }
  return(Temp_sigma)
}


.Import_Correlation <-
  function(Address,
           Sheet,
           Silence,Env) {

    ################
    #     Body     #
    ################
    my_data <-
      as.matrix(read_excel(Address, sheet = "Correlation"))


    ################
    #    Errors    #
    ################

    if (all(my_data[, 1] %in% Env$Parcels) == FALSE) {
      stop('Undefined Parcels in ',Sheet,' "',
           paste(my_data[, 1][which(!my_data[, 1] %in% Env$Parcels)], collapse = ' '),
           '"')
    }

    if (all(Env$Parcels %in% my_data[, 1]) == FALSE) {
      stop('No Correlation found for: "',
           paste(Env$Parcels[which(!Env$Parcels %in% my_data[, 1])], collapse = ' '),
           '"')
    }

    if(length(my_data[1,])!= length(Env$Parcels)+1 || length(my_data[,1])!= length(Env$Parcels) ){
      stop("The Correlation should be a symmetric matrix with ",length(Env$Parcels),"*", length(Env$Parcels)," elements. \n")
    }


    ################
    #     Body     #
    ################


    Temp_cov_names <-
      setNames(as.integer(1:length(Env$Parcels)), my_data[, 1])

    Temp_cov <- my_data[, 2:(1 + length(Env$Parcels))]

    Parcel_correlation <-
      matrix(as.double(), nrow = length(Env$Parcels), ncol = length(Env$Parcels))

    for (i in 1:length(Env$Parcels)) {
      for (j in 1:length(Env$Parcels)) {
        Parcel_correlation[i, j] <-
          as.double(Temp_cov[Temp_cov_names[Env$Parcels[i]], Temp_cov_names[Env$Parcels[j]]])

      }
    }


    ################
    #    Reports   #
    ################

    if (Silence == FALSE) {
      cat(
        "Correlation of parcels {",
        Env$Parcels[1],
        " , ",
        Env$Parcels[length(Env$Parcels)],
        "} is ",
        Parcel_correlation[1, length(Env$Parcels)],
        "\n"
      )
    }

    return(Parcel_correlation)
  }


