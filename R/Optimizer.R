

#'
#' Load and Optimize the first Objective Function
#'
#' `Optimize_First_Objective` loads the optimization problem and optimizes `First_Objective` in Julia environment.\cr
#'
#'
#' In the list that `Optimize_First_Objective` returns, `[["Status"]]` defines the status of the returned solution.\cr
#' If `[["Status"]]="OPTIMAL"`, the problem was solved to optimality and the parcels' optimal status are stored in `[["Result"]]`.
#' Also, the optimal value will be stored in `[["Objective_Value"]]`.\cr
#' If `[["Status"]]="TIME_LIMIT"`, the solver was terminated because the time limit was reached.
#' If any feasible solution was reported by the solver, it would be stored in `[["Result"]]`. Otherwise, `[["Result"]]` and `[["Objective_Value"]]` would be
#' both equal to "N/A".\cr
#' If `[["Status"]]="INFEASIBLE"`, the problem is infeasible. So, no solution would be reported.\cr
#'
#' In any other case, the `[["Status"]]` will be the solution status that the solver has reported.
#'
#' @usage Optimize_First_Objective(Time_limit=1e7, Solver="SCIP",
#'                                 Silence= FALSE, Env= .GlobalEnv)
#'
#'
#' @param Time_limit A double: the total time limit in seconds
#' @param Solver A string: defining the solver to be used to solve the problem. (Default: "SCIP")
#' @param Silence A binary parameter: if `FALSE`, the function will report a summary message. If `TRUE`, the function will be silent.  (Default: FALSE)
#' @param Env the environment where the package should create or access variables.
#' By default the package works in the R's Global environment. (Default: .GlobalEnv)
#'
#'
#' @examples
#' \dontrun{
#'
#' Optimization_Result = Optimize_First_Objective(Time_limit=1e7, Solver="SCIP")
#'}
#'
#' @return A list with 5 members:\cr
#'  `[["Solution_time"]]`: a numeric value; defining the time spent in the solver in seconds.\cr
#'  `[["Status"]]`: a character; defining the status of the solution.\cr
#'  `[["Gap"]]`: a numeric value; defining the relative optimality gap of the solution.\cr
#'  `[["Result"]]`: a vector of binaries; its `i`'th element defines if the parcel named `Parcels[i]` is protected or not.\cr
#'  `[["Objective_Value"]]`: a numveric value; the returned value of First objective.
#'
#'
#' @family Optimizer Functions
#' @family First Objective
#' @export
Optimize_First_Objective<- function(Time_limit=1e7, Solver="SCIP", Silence= FALSE, Env= .GlobalEnv){
  ################
  #     Body     #
  ################

  Load_Problem(First_Objective=1 ,Solver= Solver,Silence= Silence,Env = Env);

  ################
  #    Reports   #
  ################
  if(Silence==FALSE){
    cat("Optimizing First Objective.\n");
  }else{
    julia_command('set_silent(model);')
  }

  ################
  #     Body     #
  ################
  if(Env$First_Objective[["Sense"]]=="Max"){
  julia_command("@objective(model, Max, First_Objective);")
  }else if(Env$First_Objective[["Sense"]]=="Min"){
    julia_command("@objective(model, Min, First_Objective);")
  }

  Optimization<-.optimize(Time_limit= Time_limit,Skip_solution = 0);
  if(Optimization[["Gap"]]!=-1){
    Optimization[["Objective_Value"]]=julia_eval('objective_value(model)')}else{
      Optimization[["Objective_Value"]]="N/A";
    }


  ################
  #    Reports   #
  ################
  if(Silence==FALSE){
    if(Optimization[["Status"]]=="OPTIMAL"){
      cat("Optimal Solution found. \n");
    }else if(Optimization[["Status"]]=="TIME_LIMIT"){
      cat("Time Limit reached while optimizing the problem. \n");
    }else if(Optimization[["Status"]]=="INFEASIBLE"){
      cat("The problem is Infeasible. \n");
    }else{
      cat('Could not solve the problem. Check [["Status"]] and the solvers manual.')
    }
  }

  return(Optimization);
}







#'
#' Load and Optimize the second Objective Function
#'
#' `Optimize_Second_Objective` loads the optimization problem and optimizes `Second_Objective` in Julia environment.\cr
#'
#'
#' In the list that `Optimize_Second_Objective` returns, `[["Status"]]` defines the status of the returned solution.\cr
#' If `[["Status"]]="OPTIMAL"`, the problem was solved to optimality and the parcels' optimal status are stored in `[["Result"]]`.
#' Also, the optimal value will be stored in `[["Objective_Value"]]`.\cr
#' If `[["Status"]]="TIME_LIMIT"`, the solver was terminated because the time limit was reached.
#' If any feasible solution was reported by the solver, it would be stored in `[["Result"]]`. Otherwise, `[["Result"]]` and `[["Objective_Value"]]` would be
#' both equal to "N/A".\cr
#' If `[["Status"]]="INFEASIBLE"`, the problem is infeasible. So, no solution would be reported.\cr
#'
#' In any other case, the `[["Status"]]` will be the solution status that the solver has reported.
#'
#' @usage Optimize_Second_Objective(Time_limit=1e7, Solver="SCIP",
#'                                  Silence= FALSE, Env= .GlobalEnv)
#'
#'
#' @param Time_limit A double: the total time limit in seconds
#' @param Solver A string: defining the solver to be used to solve the problem. (Default: "SCIP")
#' @param Silence A binary parameter: if `FALSE`, the function will report a summary message. If `TRUE`, the function will be silent.  (Default: FALSE)
#' @param Env the environment where the package should create or access variables.
#' By default the package works in the R's Global environment. (Default: .GlobalEnv)
#'
#'
#' @examples
#' \dontrun{
#'
#' Optimization_Result = Optimize_Second_Objective(Time_limit=1e7, Solver="SCIP")
#'}
#'
#' @return A list with 5 members:\cr
#'  `[["Solution_time"]]`: a numeric value; defining the time spent in the solver in seconds.\cr
#'  `[["Status"]]`: a character; defining the status of the solution.\cr
#'  `[["Gap"]]`: a numeric value; defining the relative optimality gap of the solution.\cr
#'  `[["Result"]]`: a vector of binaries; its `i`'th element defines if the parcel named `Parcels[i]` is protected or not.\cr
#'  `[["Objective_Value"]]`: a numveric value; the returned value of Second objective.
#'
#'
#' @family Optimizer Functions
#' @family Second Objective
#' @export
Optimize_Second_Objective<- function(Time_limit=1e7, Solver="SCIP", Silence= FALSE, Env= .GlobalEnv){
  ################
  #     Body     #
  ################

  Load_Problem(Second_Objective=1 ,Solver= Solver,Silence= Silence,Env = Env);

  ################
  #    Reports   #
  ################
  if(Silence==FALSE){
    cat("Optimizing Second Objective.\n");
  }else{
    julia_command('set_silent(model);')
  }

  ################
  #     Body     #
  ################
  if(Env$Second_Objective[["Sense"]]=="Max"){
    julia_command("@objective(model, Max, Second_Objective);")
  }else if(Env$Second_Objective[["Sense"]]=="Min"){
    julia_command("@objective(model, Min, Second_Objective);")
  }

  Optimization<-.optimize(Time_limit= Time_limit,Skip_solution = 0);
  if(Optimization[["Gap"]]!=-1){
    Optimization[["Objective_Value"]]=julia_eval('objective_value(model)')}else{
      Optimization[["Result"]]="N/A";
      Optimization[["Objective_Value"]]="N/A";
    }


  ################
  #    Reports   #
  ################
  if(Silence==FALSE){
    if(Optimization[["Status"]]=="OPTIMAL"){
      cat("Optimal Solution found. \n");
    }else if(Optimization[["Status"]]=="TIME_LIMIT"){
      cat("Time Limit reached while optimizing the problem. \n");
    }else if(Optimization[["Status"]]=="INFEASIBLE"){
      cat("The problem is Infeasible. \n");
    }else{
      cat('Could not solve the problem. Check [["Status"]] and the solvers manual.')
    }
  }

  return(Optimization);
}




.optimize<-function(Time_limit,Skip_solution=0){
  julia_assign('Time_limit',Time_limit)
  julia_command('set_time_limit_sec(model, Time_limit);')
  julia_assign('Optimization_status',"Error")
  julia_assign('Skip_solution',Skip_solution)
  julia_assign('Gap',-1)
  julia_assign('Solution_time',-1)
  julia_command('Result_Status=fill(-1,N_Parcels);')
  julia_command('optimize!(model)')
  julia_command('if termination_status(model) == MOI.OPTIMAL
        Optimization_status="OPTIMAL"
        Solution_time= solve_time(model)
        if Skip_solution==0
          for i=1:N_Parcels
            global Result_Status[i]=round(value(Dict_Variables[i]))
          end
        end
        Gap=0
    elseif termination_status(model) == MOI.TIME_LIMIT
      Solution_time=Time_limit
      if has_values(model)
        Optimization_status="TIME_LIMIT"
        Gap=MOI.get(model, MOI.RelativeGap())
        if Skip_solution==0
          for i=1:N_Parcels
            global Result_Status[i]=value(Dict_Variables[i])
          end
        end
        else
        Optimization_status="TIME_LIMIT"
        end
    elseif termination_status(model) == MOI.INFEASIBLE_OR_UNBOUNDED || termination_status(model) == MOI.INFEASIBLE
          Optimization_status="INFEASIBLE"
    else
          Optimization_status=termination_status(model)
    end;')
  Optimization<-list("Solution_time"=julia_eval('Solution_time'), "Status"=julia_eval('Optimization_status'),"Gap"=julia_eval('Gap'),"Result"=julia_eval('Result_Status'))
  return(Optimization)
}
