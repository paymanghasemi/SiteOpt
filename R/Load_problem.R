

#'
#' Load the optimization problem into Julia environment (optional)
#'
#' `Load_Problem` loads the optimization problem into Julia environment. When using each of the Optimize functions, `Load_Problem` will be automatically called
#' prior to solving the problem. However, the user may want to use `Load_Problem` to ensure that there is no error in the process of loading the problem and
#'  setting up Julia.
#'
#'
#'
#' `Load_Problem` sets up the Julia environment and creats the necessary variables and functions in Julia. It will also create the optimization model
#' without solving it.\cr
#'
#' Based on the existance of variables named "Budget", "Connectivity", "Disconnectivity", and "Constraints",
#' `Load_Problem` will include their respective constraints in the model. If the user does not want to include any of these constraints
#' in the model, its respective variable should not exists in the environment. In other words, if `exists("Budget", envir= *)==TRUE`, the function will create the Budget constraints
#' in the model, and if `exists("Budget", envir= *)==FALSE`, no Budget constraint will be included in the optimization model.
#' (Note: * refers to the environment passed to the function as `Env` parameter.)
#'
#' @usage Load_Problem(First_Objective=0, Second_Objective=0, Risk_Objective=0,
#'                     Solver="SCIP", Silence= FALSE, Env= .GlobalEnv)
#'
#' @param First_Objective A binary parameter: If \bold{1}, the first objective function (`First_Objectove`) will be included in optimization.
#' If \bold{0}, it will \strong{not} be included in optimization.
#' @param Second_Objective A binary parameter: If \bold{1}, the second objective function (`Second_Objectove`) will be included in optimization.
#' If \bold{0}, it will \strong{not} be included in optimization.
#' @param Risk_Objective A binary parameter: If \bold{1}, the Risk objective function (`Risk_Objectove`) will be included in optimization.
#' If \bold{0}, it will \strong{not} be included in optimization.
#' @param Solver A string: defining the solver to be used to solve the problem. (Default= "SCIP")
#' @param Silence A binary parameter: if `FALSE`, the function will report a summary message. If `TRUE`, the function will be silent.  (Default: FALSE)
#' @param Env the environment where the package should create or access variables.
#' By default the package works in the R's Global environment. (Default: .GlobalEnv)
#'
#' @examples
#' \dontrun{
#'
#' ## to load objective one and two, but not the risk objective, into Julia environment ##
#'
#' Load_Problem(First_Objective=1, Second_Objective=1, Risk_Objective=0, Solver="SCIP")
#'
#'}
#'
#'
#' @return NULL (creates some variables in the environment but returns nothing)
#' @family Optimizer Functions
#'
#' @export
Load_Problem<-function(First_Objective=0, Second_Objective=0, Risk_Objective=0, Solver="SCIP", Silence= FALSE, Env= .GlobalEnv){
  ################
  #    Errors    #
  ################
  if (exists('Parcels', envir = Env) == FALSE || exists('Status', envir = Env) == FALSE) {
    stop('Parcels and Status Should be created first.')
  }

  if(First_Objective==1 && exists('First_Objective', envir = Env) == FALSE){
    stop("First objective has not been defined in the environment ",Env,".\n
         create `First_Objective`")
  }

  if(Second_Objective==1 && exists('Second_Objective', envir = Env) == FALSE){
    stop("Second objective has not been defined in the environment ",Env,".\n
         create 'Second_Objective'")
  }

  if(Risk_Objective==1 && exists('Risk_Objective', envir = Env) == FALSE){
    stop("Risk objective has not been defined in the environment ",Env,".\n
         create 'Risk_Objective'")
  }


  ################
  #     Body     #
  ################
  julia <- julia_setup()
  julia_library("JuMP")
  julia_library(Solver)
  julia_assign("Parcel_Status",Env$Status)
  julia_assign("N_Parcels",length(Env$Parcels))
  julia_assign("N_Objectives",as.integer(2))
  julia_assign("Error","Error!!!!")
  julia_assign('N_protected', as.integer(sum(Env$Status)))
  Solver=paste("model=Model(",Solver,".Optimizer);",sep='')
  julia_command(Solver)
  julia_command(" @variable(model, x[i=1:(N_Parcels-N_protected)], Bin);")
  julia_command(" @variable(model, y[i=1:N_protected], Bin);")
  .Dict_Variables()


  if(First_Objective==1){
    .First_objective_creator(Env);
    if(Silence==FALSE){
      cat("First Objective Loaded. \n")
    }
  }
  if(Second_Objective==1){
    .Second_objective_creator(Env);
    if(Silence==FALSE){
      cat("Second Objective Loaded. \n")
    }
  }

  if(Risk_Objective==1){
    .Risk_objective_creator(Env);
    if(Silence==FALSE){
      cat("Risk objective Loaded. \n")
    }
  }

  if(exists("Budget")){
    .Budget_constraint(Env);
    if(Silence==FALSE){
      cat("Budget Constraints Loaded. \n")
    }
  }

  if(exists("Connectivity")){
    .Connectivity_constraint(Env);
    if(Silence==FALSE){
      cat("Connectivity Constraints Loaded. \n")
    }
  }
  if(exists("Disconnectivity")){
    .Disconnectivity_constraint(Env);
    if(Silence==FALSE){
      cat("Disconnectivity Constraints Loaded. \n")
    }
  }

  if(exists("Constraints")){
    .Constraints(Env);
    if(Silence==FALSE){
      cat("Linear Constraints Loaded. \n")
    }
  }
  if(Silence==FALSE){
    cat("Problem loaded Successfully. \n")
  }

}






.Dict_Variables<-function(){
  julia_command("Counter_protected=0; ")
  julia_command(" Counter_unprotected=0;")
  julia_command("Dict_Variables=Dict();")
  julia_command("for i= 1:N_Parcels
        if Parcel_Status[i] == 0
          global  Counter_unprotected += 1
          global Dict_Variables[i]=x[Counter_unprotected]
        else
          global  Counter_protected += 1
          global Dict_Variables[i]=(1-y[Counter_protected])
        end
    end; ")
}

.Budget_constraint<-function(Env){
  julia_assign("counter",as.integer(length(Env$Budget)))
  julia_command('Budget=Array{Any,1}(UndefInitializer(),counter);')
  for(i in 1:length(Env$Budget)){
    julia_assign("left",as.integer(match(as.character(Env$Budget[[i]][["Parcels"]]),Env$Parcels)))
    julia_assign("Coefficients",as.double(Env$Budget[[i]][["Coefficients"]]))
    julia_assign("right",as.double(Env$Budget[[i]][["Limit"]]))
    julia_assign("counter",as.integer(i))
    julia_command("Budget[counter]=@constraint(model, sum(Coefficients[findfirst(x->x==i,left)]*(Dict_Variables[i]-Parcel_Status[i]) for i in left)<= right);")
  }
}

.Connectivity_constraint<-function(Env){
  temp_counter<-0;
  for( i in 1:length(names(Env$Connectivity))){
    temp_counter= temp_counter+ length(Env$Connectivity[[as.character(names(Env$Connectivity)[i])]])}
  julia_assign("counter",as.integer(temp_counter))
  julia_command('Connectivity=Array{Any,1}(UndefInitializer(),counter);')
  julia_assign("counter",as.integer(1))
  for(i in 1:length(names(Env$Connectivity))){
    julia_assign("left",as.integer(match(as.character(names(Env$Connectivity)[i]),Env$Parcels)))
    for(j in 1:length(Env$Connectivity[[as.character(names(Env$Connectivity)[i])]])){
      julia_assign("right",as.integer(match(as.character(Env$Connectivity[[as.character(names(Env$Connectivity)[i])]][[j]]),Env$Parcels)))
      julia_command("Connectivity[counter]=@constraint(model, Dict_Variables[left] - sum(Dict_Variables[i] for i in right)<= 0);")
      julia_command("counter+=1;")}
  }
}

.Disconnectivity_constraint<-function(Env){
  temp_counter<-0;
  for( i in 1:length(names(Env$Disconnectivity))){
    temp_counter= temp_counter+ length(Env$Disconnectivity[[as.character(names(Env$Disconnectivity)[i])]])}
  julia_assign("counter",as.integer(temp_counter))
  julia_command('Disconnectivity=Array{Any,1}(UndefInitializer(),counter);')
  julia_assign("counter",as.integer(1))
  for(i in 1:length(names(Env$Disconnectivity))){
    julia_assign("left",as.integer(match(as.character(names(Env$Disconnectivity)[i]),Env$Parcels)))
    for(j in 1:length(Env$Disconnectivity[[as.character(names(Env$Disconnectivity)[i])]])){
      julia_assign("right",as.integer(match(as.character(Env$Disconnectivity[[as.character(names(Env$Disconnectivity)[i])]][[j]]),Env$Parcels)))
      julia_command("Disconnectivity[counter]=@constraint(model, length(right)*Dict_Variables[left] + sum(Dict_Variables[i] for i in right)<= length(right));")
      julia_command("counter+=1;")}
  }
}

.Constraints<-function(Env){
  julia_assign("counter",as.integer(length(Env$Constraints)))
  julia_command('Constraints=Array{Any,1}(UndefInitializer(),counter);')
  for(i in 1:length(Env$Constraints)){
    julia_assign("left",as.integer(match(as.character(Env$Constraints[[i]][['Parcels']]),Env$Parcels)))
    julia_assign("Coefficients",as.double(Env$Constraints[[i]][['Coefficients']]))
    julia_assign("right",as.double(Env$Constraints[[i]][['RHS']]))
    julia_assign("counter",as.integer(i))
    julia_command("Constraints[counter]=@constraint(model, sum(Coefficients[findfirst(x->x==i,left)]*Dict_Variables[i] for i in left)<= right);")
  }
}

.First_objective_creator<-function(Env){
  julia_assign("First_Objective_Coefficients",Env$First_Objective[["Coefficients"]])
  julia_command(" First_Objective=@expression(model, sum(First_Objective_Coefficients[i]*Dict_Variables[i] for i=1:N_Parcels ));")
}

.Second_objective_creator<-function(Env){
  julia_assign("Second_Objective_Coefficients",Env$Second_Objective[["Coefficients"]])
  julia_command(" Second_Objective=@expression(model, sum(Second_Objective_Coefficients[i]*Dict_Variables[i] for i=1:N_Parcels ));")
}

.Risk_objective_creator<-function(Env){
    julia_assign("Parcel_Sigma",Env$Risk_Objective[["Sigma"]])
    julia_assign("Parcel_correlation",Env$Risk_Objective[["Correlation"]])
    julia_command(" Risk_Objective=@expression(model,sum((Parcel_Sigma[i]^2)*Dict_Variables[i] for i=1:N_Parcels) + 2*sum(Parcel_Sigma[i]*Dict_Variables[i]*sum(Parcel_correlation[i,j]*Parcel_Sigma[j]*Dict_Variables[j] for j=(i+1):N_Parcels ) for i=1:N_Parcels));")
}
