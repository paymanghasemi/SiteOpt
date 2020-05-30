
#'
#' Load and Optimize Weighted Sum Optimization (WSO)
#'
#' `Optimize_WSO` creates and optimizes the Weighted Sum Optimization (WSO) in Julia environment.
#' It requires two objective functions and their respective positive weights.\cr
#' (For detailed information refer to the paper)
#'
#'
#'
#' In the list that `Optimize_WSO` returns, `[["Status"]]` defines the status of the returned WSO solution.\cr
#' If `[["Status"]]="OPTIMAL"`, the WSO was solved to optimality and the Parcels' optimal status are stored in `[["Result"]]`.
#' Also, the WSO optimal values of objectives will be stored in either `[["First_Objective"]]`, `[["Second_Objective"]]`, or `[["Risk_Objective"]]`
#' , based on the input objectives.\cr
#' If `[["Status"]]="TIME_LIMIT"`, the solver was terminated because the time limit was reached.
#' If any feasible solution was reported by the solver, it would be stored in `[["Result"]]`. Otherwise, `[["Result"]]` and objective values would be
#' all equal to "N/A".\cr
#' If `[["Status"]]="INFEASIBLE"`, the problem is infeasible. So, no solution would be reported.\cr
#'
#' In any other case, the `[["Status"]]` will be the solution status that the solver has reported.
#'
#'
#' \strong{Note 1:} The weights should be non negative, and two objectives should have positive weights.\cr
#'
#'
#'
#' @usage Optimize_WSO(First_Objective=0, Second_Objective=0, Risk_Objective=0,
#'                     Time_limit=1e7, Solver="SCIP", Silence= FALSE, Env= .GlobalEnv)
#'
#' @param First_Objective A float parameter: defining the weight of the first objective function (`First_Objective`) in the optimization.
#' If \bold{0}, it will \strong{not} be included in optimization.
#' @param Second_Objective A float parameter: defining the weight of the second objective function (`Second_Objective`) in the optimization.
#' If \bold{0}, it will \strong{not} be included in optimization.
#' @param Risk_Objective A float parameter: defining the weight of the Risk objective function (`Risk_Objective`) in the optimization.
#' If \bold{0}, it will \strong{not} be included in optimization.
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
#' ## to find WSO solution for First Objective and Second Objective with weights equal to 1 and 2: ##
#'
#' Optimize_WSO(First_Objective=1, Second_Objective=2, Time_limit=1e7, Solver="SCIP", Silence= FALSE)
#'
#' ## to find WSO solution for First Objective and Risk Objective with weights equal to 1 and 2: ##
#'
#' Optimize_WSO(First_Objective=1, Risk_Objective=2, Time_limit=1e7, Solver="SCIP", Silence= FALSE)
#'
#' ## to find WSO solution for Second Objective and Risk Objective with weights equal to 1 and 2: ##
#'
#' Optimize_WSO(Second_Objective=1, Risk_Objective=2, Time_limit=1e7, Solver="SCIP", Silence= FALSE)
#'}
#'
#'
#' @return A list with 8 members:\cr
#'  `[["Solution_time"]]`: a numeric value; defining the time spent in the solver in seconds.\cr
#'  `[["Status"]]`: a character; defining the status of the solution.\cr
#'  `[["Gap"]]`: a numeric value; defining the relative optimality gap of the solution.\cr
#'  `[["Result"]]`: a vector of binaries; its `i`'th element defines if the parcel named `Parcels[i]` is protected or not.\cr
#'  `[["Firs_Objective"]]`: a numveric value; (if applicable) returns the WSO value of first objective.\cr
#'  `[["Second_Objective"]]`: a numveric value; (if applicable) returns the WSO value of second objective.\cr
#'  `[["Risk_Objective"]]`: a numveric value; (if applicable) returns the WSO value of risk objective.\cr
#'
#' @family Optimizer Functions
#'
#' @export
Optimize_WSO<-function(First_Objective=0, Second_Objective=0, Risk_Objective=0, Time_limit=1e7, Solver="SCIP", Silence= FALSE, Env= .GlobalEnv){

  ################
  #    Errors    #
  ################

  if(First_Objective * Second_Objective * Risk_Objective !=0){
    stop('Only two objectives can be input.\n')
  }

  if(First_Objective <0 || Second_Objective <0 || Risk_Objective <0){
    stop('Only non-negative weights can be input.\n')
  }

  ################
  #     Body     #
  ################
  Load_Problem(First_Objective= (First_Objective>0), Second_Objective= (Second_Objective>0), Risk_Objective = (Risk_Objective>0), Solver, Silence, Env = Env);
  if(Silence==TRUE){
    julia_command('set_silent(model);')
  }

  ################
  #     Body     #
  ################


  Optimization<- .WSO_delegate(First_Objective,Second_Objective,Risk_Objective,Time_limit, Silence, Env= Env)

  ################
  #    Reports   #
  ################
  if(Silence==FALSE){
    if(Optimization[["Status"]]=="OPTIMAL"){
      cat("Optimal Solution found. \n");
    }else if(Optimization[["Status"]]=="TIME_LIMIT"){
      cat("Time Limit reached while optimizing the problem. \n");
    }else if(Optimization[["Status"]]=="INF_or_UNBOUNDED"){
      cat("The problem is either Infeasible or Unbounded. \n");
    }else{
      cat('Could not solve the problem. Check [["Status"]] and the solvers manual.')
    }
  }

  return(Optimization)

}

.WSO_delegate<- function(First_power, Second_power, Risk_power, Time_limit, Silence, Env){
  if(Silence==FALSE){
    cat("Optimizing the Weighted Sum Problem between: ");}

  if(First_power * Second_power >0){
    if(Silence==FALSE){
      cat("First Objective and Second Objective.\n")}
    return(.WSO_Optimizer(Name_1 = "First_Objective",Sense_1 = Env$First_Objective[["Sense"]],
                          Weight_1=First_power, Name_2 = "Second_Objective",
                          Sense_2 = Env$Second_Objective[["Sense"]],Weight_2=Second_power, Time_limit))
  }else if(First_power * Risk_power >0){
    if(Silence==FALSE){
      cat("First Objective and Risk Objective.\n")}
    return(.WSO_Optimizer(Name_1 = "First_Objective",Sense_1 = Env$First_Objective[["Sense"]],
                          Weight_1=First_power, Name_2 = "Risk_Objective", Sense_2 = "Min",
                          Weight_2=Risk_power, Time_limit))
  }else if(Second_power*Risk_power>0){
    if(Silence==FALSE){
      cat("Second Objective and Risk Objective.\n")}
    return(.WSO_Optimizer(Name_1 = "Second_Objective",Sense_1 = Env$Second_Objective[["Sense"]],
                          Weight_1=Second_power, Name_2 = "Risk_Objective", Sense_2 = "Min",
                          Weight_2=Risk_power, Time_limit))
  }
}

