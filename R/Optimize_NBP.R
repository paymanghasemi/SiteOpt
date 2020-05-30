
#'
#' Load and Optimize Nash Bargaining Problem (NBP)
#'
#' `Optimize_NBP` creates and optimizes the Nash Bargaining Problem (NBP) in Julia environment.
#' It requires two objective functions to be considered as two players.\cr
#' (For detailed information refer to the paper)
#'
#'
#'
#' To create  NBP, the \bold{nadir} values of both objectives are required, which entails 4 optimization problems.
#'
#' If `Optimize_NBP` terminates with a `Warning`, then there have been a problem in the process of finding the Nadir points, and
#' the function has terminated before even going to the NBP optimization. The corresponding found solution and status will be returned as a list.
#' Furhter, the `Warning` will report in what step the function was terminated.
#'
#' If `Optimize_NBP` finishes with no `Warning`, then the Nadir points were found successfully, and the NBP was created.
#' Then, in the list that `Optimize_NBP` returns, `[["Status"]]` defines the status of the returned NBP solution.\cr
#' If `[["Status"]]="OPTIMAL"`, the NBP was solved to optimality and the Parcels' optimal status are stored in `[["Result"]]`.
#' Also, the NBP optimal values of objectives will be stored in either `[["First_Objective"]]`, `[["Second_Objective"]]`, or `[["Risk_Objective"]]`
#' , based on the input objectives.\cr
#' If `[["Status"]]="TIME_LIMIT"`, the solver was terminated because the time limit was reached.
#' If any feasible solution was reported by the solver, it would be stored in `[["Result"]]`. Otherwise, `[["Result"]]` and objective values would be
#' all equal to "N/A".\cr
#' If `[["Status"]]="INFEASIBLE"`, then there has been a problem in the solver. In this stage, the feasibility and boundedness of the problem
#' is certain, and therefore, any status of this kind is showing an underlying problem. \bold{Contact the Developer.}\cr
#'
#' In any other case, the `[["Status"]]` will be the solution status that the solver has reported.
#'
#'
#' \strong{Note 1:} If only First Objective and Second Objective are considered, `Optimize_NBP` needs to solve a total of \bold{5} optimization problems to find the NBP solution:
#' Four Linear Problem (LP), and one Second Order Cone Program (SOCP).\cr
#'
#' \strong{Note 2:} If Risk objective is considered along with one of First objective or Second Objective, `Optimize_NBP` needs to solve \bold{3} optimization problems to find the NBP solution:
#' One Linear Problem (LP), one Quadratic Problem (QP), and one Second Order Cone Program (SOCP).
#'
#'
#' @usage Optimize_NBP(First_Objective=0, Second_Objective=0, Risk_Objective=0,
#'                     Time_limit=1e7, Solver="SCIP", Silence= FALSE, Env= .GlobalEnv)
#'
#' @param First_Objective A binary parameter: If \bold{1}, the first objective function (`First_Objective`) will be included in optimization.
#' If \bold{0}, it will \strong{not} be included in optimization.
#' @param Second_Objective A binary parameter: If \bold{1}, the second objective function (`Second_Objective`) will be included in optimization.
#' If \bold{0}, it will \strong{not} be included in optimization.
#' @param Risk_Objective A binary parameter: If \bold{1}, the Risk objective function (`Risk_Objective`) will be included in optimization.
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
#' ## to find NBP solution for First Objective and Second Objective: ##
#'
#' Optimize_NBP(First_Objective=1, Second_Objective=1, Time_limit=1e7, Solver="SCIP", Silence= FALSE)
#'
#' ## to find NBP solution for First Objective and Risk Objective: ##
#'
#' Optimize_NBP(First_Objective=1, Risk_Objective=1, Time_limit=1e7, Solver="SCIP", Silence= FALSE)
#'
#' ## to find NBP solution for Second Objective and Risk Objective: ##
#'
#' Optimize_NBP(Second_Objective=1, Risk_Objective=1, Time_limit=1e7, Solver="SCIP", Silence= FALSE)
#'}
#'
#'
#' @return A list with 8 members:\cr
#'  `[["Solution_time"]]`: a numeric value; defining the time spent in the solver in seconds.\cr
#'  `[["Status"]]`: a character; defining the status of the solution.\cr
#'  `[["Gap"]]`: a numeric value; defining the relative optimality gap of the solution.\cr
#'  `[["Result"]]`: a vector of binaries; its `i`'th element defines if the parcel named `Parcels[i]` is protected or not.\cr
#'  `[["Firs_Objective"]]`: a numveric value; (if applicable) returns the NBP value of first objective.\cr
#'  `[["Second_Objective"]]`: a numveric value; (if applicable) returns the NBP value of second objective.\cr
#'  `[["Risk_Objective"]]`: a numveric value; (if applicable) returns the NBP value of risk objective.\cr
#'  `[["Ideal"]]`: a list with two members returning the ideal values of the input objectives found in the process of creating the NBP.\cr
#'  `[["Nadir"]]`: a list with two members returning the nadir values of the input objectives found in the process of creating the NBP.\cr
#'
#' @family Optimizer Functions
#'
#' @export
Optimize_NBP<-function(First_Objective=0, Second_Objective=0, Risk_Objective=0, Time_limit=1e7, Solver="SCIP", Silence= FALSE, Env= .GlobalEnv){

  ################
  #    Errors    #
  ################

  if(First_Objective+ Second_Objective + Risk_Objective !=2){
    stop('Only two objectives can be input.\n')
  }

  ################
  #     Body     #
  ################
  Load_Problem(First_Objective, Second_Objective, Risk_Objective, Solver, Silence, Env = Env);
  if(Silence==TRUE){
    julia_command('set_silent(model);')
  }

  ################
  #     Body     #
  ################
  julia_assign("W",c(1,1));

  Optimization<- .NBP_delegate(First_Objective,Second_Objective,Risk_Objective,Time_limit, Silence, Env= Env)

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

.NBP_delegate<- function(First_power, Second_power, Risk_power, Time_limit, Silence, Env){
  if(Silence==FALSE){
    cat("Optimizing the Nash Bargaining Problem between: ");}

  if(First_power==Second_power){
    if(Silence==FALSE){
      cat("First Objective and Second Objective.\n")}
    return(.NBP_Optimizer(Name_1 = "First_Objective",Sense_1 = Env$First_Objective[["Sense"]],Name_2 = "Second_Objective", Sense_2 = Env$Second_Objective[["Sense"]], Time_limit))
  }else if(First_power==Risk_power){
    if(Silence==FALSE){
      cat("First Objective and Risk Objective.\n")}
    return(.NBP_Optimizer(Name_1 = "First_Objective",Sense_1 = Env$First_Objective[["Sense"]],Name_2 = "Risk_Objective", Sense_2 = "Min", Time_limit))
  }else if(Second_power==Risk_power){
    if(Silence==FALSE){
      cat("Second Objective and Risk Objective.\n")}
    return(.NBP_Optimizer(Name_1 = "Second_Objective",Sense_1 = Env$Second_Objective[["Sense"]],Name_2 = "Risk_Objective", Sense_2 = "Min", Time_limit))
  }
}

