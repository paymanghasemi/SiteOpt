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
  rownames(my_data)<-match(my_data[,1],Env$Parcels)
  my_data<-my_data[,2]
  my_data<-my_data[order(as.integer(names(my_data)))]
  names(my_data)<-NULL
  my_data<-as.double(my_data)
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
      my_data[1],
      " and ",
      my_data[length(Env$Parcels)],
      ".\n"
    )
  }
  return(my_data)
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
    rownames(my_data)<-match(my_data[,1],Env$Parcels)
    my_data<-my_data[,2:ncol(my_data)]
    colnames(my_data)<-rownames(my_data)
    my_data<-my_data[order(as.integer(rownames(my_data))),]
    my_data<-my_data[,order(as.integer(colnames(my_data)))]
    rownames(my_data)<-NULL
    colnames(my_data)<-NULL
    my_data<-as.matrix(apply(my_data,c(1,2),FUN = as.double))
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
        my_data[1, length(Env$Parcels)],
        "\n"
      )
    }

    return(my_data)
  }


