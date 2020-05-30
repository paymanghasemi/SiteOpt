



#'
#' One-time installation function
#'
#' This function should be called only once when installing the package for the first time. This function should be called after when Julia 1.3 and SCIP 6.0.2 are installed and their global variables are defined.
#'
#' This function will initialize the installation of required packages and libraries in Julia. The packages are "JuMP.jl" and "SCIP.jl".
#' The installation may take several minutes to complete, and when completed, the message "Setup Completed!" will appear.
#'
#' @export
Setup <- function(){
  cat("Do you agree with Terms and Conditions of SiteOpt Provided in the link below? \n");
  cat("https://bit.ly/2TTjQzP");
  answer=readline(prompt = "Enter 1 if agree, enter 2 to disagree and exit: ")
  if(answer!=1){
    stop("You are required to accept the terms and conditions of SiteOpt in order to use the package.")
  }
  julia <- julia_setup()
  julia_command("import Pkg")
  julia_command('Pkg.add(Pkg.PackageSpec(;name="JuMP", version="0.21"))')
  julia_command('Pkg.add(Pkg.PackageSpec(;name="SCIP", version="0.9.3"))')
  julia_command('Pkg.build("SCIP");')
  julia_library("JuMP")
  julia_library("SCIP")
  cat("Setup Completed!")
}
