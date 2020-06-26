


.NBP_Optimizer<-function(Name_1,Sense_1,Name_2,Sense_2,Time_limit){



  Initial_time_limit=Time_limit;
  julia_assign('First_optimal',"N/A")
  julia_assign('Second_optimal',"N/A")
  julia_assign('First_nadir',"N/A")
  julia_assign('Second_nadir',"N/A")

  if(Sense_1=="Max"){
    julia_command(paste("@objective(model, Max,", Name_1,");",sep=''))
  }else{
    julia_command(paste("@objective(model, Min,", Name_1,");",sep=''))
  }


  Optimization<-.optimize(Time_limit= Time_limit,Skip_solution=1)
  Time_limit = Time_limit - Optimization[["Solution_time"]]
  if(Optimization[["Status"]]!="OPTIMAL"){
    warning('Error: Could not optimize ', Name_1,' to Optimality. \n')
    Optimization[[Name_1]]="N/A"
    Optimization[[Name_2]]="N/A"
    Optimization[["Solution_time"]]=Initial_time_limit - Time_limit
    Optimization[["Ideal"]]=list()
    Optimization[["Ideal"]][[Name_1]]=julia_eval('First_optimal')
    Optimization[["Ideal"]][[Name_2]]=julia_eval('Second_optimal')
    Optimization[["Nadir"]]=list()
    Optimization[["Nadir"]][[Name_1]]=julia_eval('First_nadir')
    Optimization[["Nadir"]][[Name_2]]=julia_eval('Second_nadir')
    return(Optimization);
  }else if(Time_limit<=0.0001){
    warning('Error: Time limit reached while optimizing ', Name_1,'. \n')
    julia_command("First_optimal = objective_value(model);")
    Optimization[["Status"]]="TIME_LIMIT"
    Optimization[[Name_1]]="N/A"
    Optimization[[Name_2]]="N/A"
    Optimization[["Solution_time"]]=Initial_time_limit - Time_limit
    Optimization[["Ideal"]]=list()
    Optimization[["Ideal"]][[Name_1]]=julia_eval('First_optimal')
    Optimization[["Ideal"]][[Name_2]]=julia_eval('Second_optimal')
    Optimization[["Nadir"]]=list()
    Optimization[["Nadir"]][[Name_1]]=julia_eval('First_nadir')
    Optimization[["Nadir"]][[Name_2]]=julia_eval('Second_nadir')
    return(Optimization);
  }

  julia_command("First_optimal = objective_value(model);")

  if(Sense_1=="Max"){
    julia_command(paste("@constraint(model,Temp_First_optimal,", Name_1," >= First_optimal);",sep=''))
  }else{
    julia_command(paste("@constraint(model,Temp_First_optimal,", Name_1," <= First_optimal);",sep=''))
  }

  if(Name_2=="Risk_Objective"){
    Optimization<-.Nash_for_Risk(Name_1 = Name_1,Sense_1 =  Sense_1, Name_2 = Name_2,Time_limit =  Time_limit,Initial_time_limit =  Initial_time_limit);
  }else{
    Optimization<-.Nash_for_LP(Name_1 = Name_1,Sense_1 =  Sense_1, Name_2 = Name_2,Sense_2 =  Sense_2, Time_limit =  Time_limit,Initial_time_limit =  Initial_time_limit);
  }
  if(length(Optimization)!=1){
    Optimization[["Ideal"]]=list()
    Optimization[["Ideal"]][[Name_1]]=julia_eval('First_optimal')
    Optimization[["Ideal"]][[Name_2]]=julia_eval('Second_optimal')
    Optimization[["Nadir"]]=list()
    Optimization[["Nadir"]][[Name_1]]=julia_eval('First_nadir')
    Optimization[["Nadir"]][[Name_2]]=julia_eval('Second_nadir')
    return(Optimization)
  }else{
    Time_limit=Optimization
  }

  julia_command("@variable(model,Small_Lambda>=0);")
  julia_command("@objective(model, Max, Small_Lambda);")
  julia_command("@variable(model,Large_Lambda>=0.0);")
  julia_command("@constraint(model,Small_Lambda-Large_Lambda<=0);")
  julia_command("Total_W=0;")
  julia_command("for i=1:N_Objectives
       global Total_W= Total_W + W[i]
    end;")
  julia_command("k=ceil(log(2,Total_W));")
  julia_command("pow_val=2^k;")
  julia_command("@variable(model,Tau_var[i=0:k-1,j=0:pow_val]>=0);")
  julia_command("@constraint(model,Large_Lambda*Large_Lambda - Tau_var[k-1,1]*Tau_var[k-1,2] <=0);")
  julia_command("if k >=2
                @constraint(model,SOCP_cont[i=1:k-1,j=1:(2^(k-i))],Tau_var[i,j] * Tau_var[i,j] - Tau_var[i - 1,(2 * j)-1] * Tau_var[i - 1,(2 * j) ] <= 0)
                end; ")
  julia_command("counter=1;")
  julia_command("Temp_sum_W=0;")
  julia_command("for j=1:pow_val
        if j<= Temp_sum_W
            @constraint(model,Tau_var[0,j]-z[counter-1]==0)
        elseif j<= Total_W

           global Temp_sum_W = Temp_sum_W +W[counter]
             @constraint(model,Tau_var[0,j]-z[counter]==0)
             global counter+=1

        else
            @constraint(model,Tau_var[0,j]-Large_Lambda==0)
        end
    end;")

  Optimization<-.optimize(Time_limit= Time_limit,Skip_solution=0)
  Time_limit = Time_limit - Optimization[["Solution_time"]]
  if(Optimization[["Gap"]]!=-1){
    Optimization[[Name_1]]=julia_eval(paste('value(',Name_1,')'))
    Optimization[[Name_2]]=julia_eval(paste('value(',Name_2,')'))
  }else{
    Optimization[[Name_1]]="N/A"
    Optimization[[Name_2]]="N/A"
  }
  Optimization[["Solution_time"]]=Initial_time_limit - Time_limit
  Optimization[["Ideal"]]=list()
  Optimization[["Ideal"]][[Name_1]]=julia_eval('First_optimal')
  Optimization[["Ideal"]][[Name_2]]=julia_eval('Second_optimal')
  Optimization[["Nadir"]]=list()
  Optimization[["Nadir"]][[Name_1]]=julia_eval('First_nadir')
  Optimization[["Nadir"]][[Name_2]]=julia_eval('Second_nadir')
  return(Optimization);
}



.Nash_for_Risk<-function(Name_1, Sense_1, Name_2, Time_limit, Initial_time_limit){
  julia_command("@variable(model, var_risk);")
  julia_command("Temp_Risk_variable=@constraint(model,var_risk - Risk_Objective >=0);")
  julia_command("@objective(model, Min, var_risk);")
  Optimization<-.optimize(Time_limit= Time_limit,Skip_solution=1)
  Time_limit = Time_limit - Optimization[["Solution_time"]]
  if(Optimization[["Status"]]!="OPTIMAL"){
    warning('Error: Could not optimize Risk_Objective subject to ', Name_1,' to Optimality. \n');
    Optimization[[Name_1]]="N/A"
    Optimization[[Name_2]]="N/A"
    Optimization[["Solution_time"]]=Initial_time_limit - Time_limit
    return(Optimization);
  }else if(Time_limit<=0.0001){
    warning('Error: Time limit reached while optimizing Risk_Objective subject to ',Name_1,'.\n')
    julia_command("Second_nadir = objective_value(model);")
    Optimization[["Status"]]="TIME_LIMIT"
    Optimization[[Name_1]]="N/A"
    Optimization[[Name_2]]="N/A"
    Optimization[["Solution_time"]]=Initial_time_limit - Time_limit
    julia_assign('Second_optimal',0)
    julia_assign('First_nadir',0)
    return(Optimization);
  }
  julia_command("Second_nadir = objective_value(model);")
  julia_command("delete(model,Temp_First_optimal);")
  julia_assign('Second_optimal',0)
  julia_assign('First_nadir',0)
  julia_command("@variable(model, z[i=1:N_Objectives]>= 0);")
  if(Sense_1=="Max"){
  julia_command(paste("@constraint(model, First_player, z[1] -", Name_1," <= 0 );",sep=''))
    }else{
    julia_command(paste("@constraint(model, First_player, z[1] +", Name_1," <= 0 );",sep=''))
  }
  julia_command("@constraint(model, Second_player, z[2] - Second_nadir + var_risk <= 0 );")
  return(Time_limit)
}





.Nash_for_LP<-function(Name_1, Sense_1, Name_2, Sense_2, Time_limit, Initial_time_limit){
  if(Sense_2=="Max"){
    julia_command("@objective(model, Max, Second_Objective);")
  }else{
  julia_command("@objective(model, Min, Second_Objective);")}
  Optimization<-.optimize(Time_limit=Time_limit,Skip_solution=1)
  Time_limit = Time_limit - Optimization[["Solution_time"]]
  if(Optimization[["Status"]]!="OPTIMAL"){
    warning('Error: Could not optimize Second_Objective subject to First_Objective to Optimality. \n')
    Optimization[[Name_1]]="N/A"
    Optimization[[Name_2]]="N/A"
    Optimization[["Solution_time"]]=Initial_time_limit - Time_limit
    return(Optimization);
  }else if(Time_limit<=0.0001){
    warning('Error: Time limit reached while optimizing Second_Objective subject to First_Objective. \n')
    Optimization[["Status"]]="TIME_LIMIT"
    Optimization[[Name_1]]="N/A"
    Optimization[[Name_2]]="N/A"
    Optimization[["Solution_time"]]=Initial_time_limit - Time_limit
    julia_command("Second_nadir = objective_value(model);")
    return(Optimization);
  }

  julia_command("Second_nadir = objective_value(model);")
  julia_command("delete(model,Temp_First_optimal);")

  #####
  if(Sense_2=="Max"){
    julia_command("@objective(model, Max, Second_Objective);")
  }else{
    julia_command("@objective(model, Min, Second_Objective);")}
  Optimization<-.optimize(Time_limit= Time_limit,Skip_solution=1)
  Time_limit = Time_limit - Optimization[["Solution_time"]]
  if(Optimization[["Status"]]!="OPTIMAL"){
    warning('Error: Could not optimize Second_Objective to Optimality. \n')
    Optimization[[Name_1]]="N/A"
    Optimization[[Name_2]]="N/A"
    Optimization[["Solution_time"]]=Initial_time_limit - Time_limit
    return(Optimization);
  }else if(Time_limit<=0.0001){
    warning('Error: Time limit reached while optimizing Second_Objective. \n')
    Optimization[["Status"]]="TIME_LIMIT"
    Optimization[[Name_1]]="N/A"
    Optimization[[Name_2]]="N/A"
    Optimization[["Solution_time"]]=Initial_time_limit - Time_limit
    julia_command("Second_optimal = objective_value(model);")
    return(Optimization);
  }

  julia_command("Second_optimal = objective_value(model);")
  if(Sense_2=="Max"){
    julia_command("@constraint(model,Temp_Second_optimal, Second_Objective >= Second_optimal);")
  }else{
    julia_command("@constraint(model,Temp_Second_optimal, Second_Objective <= Second_optimal);")
    }

  if(Sense_1=="Max"){
    julia_command("@objective(model, Max, First_Objective);")
  }else{
    julia_command("@objective(model, Min, First_Objective);")
  }


  Optimization<-.optimize(Time_limit= Time_limit,Skip_solution=1)
  Time_limit = Time_limit - Optimization[["Solution_time"]]
  if(Optimization[["Status"]]!="OPTIMAL"){
    warning('Error: Could not optimize First_Objective subject to Second_Objective to Optimality. \n')
    Optimization[[Name_1]]="N/A"
    Optimization[[Name_2]]="N/A"
    Optimization[["Solution_time"]]=Initial_time_limit - Time_limit
    return(Optimization);
  }else if(Time_limit<=0.0001){
    warning('Error: Time limit reached while optimizing First_Objective subject to Second_Objective \n')
    Optimization[["Status"]]="TIME_LIMIT"
    Optimization[[Name_1]]="N/A"
    Optimization[[Name_2]]="N/A"
    Optimization[["Solution_time"]]=Initial_time_limit - Time_limit
    julia_command("First_nadir = objective_value(model);")
    return(Optimization);
  }

  julia_command("First_nadir = objective_value(model);")
  julia_command("delete(model,Temp_Second_optimal);")


  julia_command("@variable(model, z[i=1:N_Objectives]>= 0);")
  if(Sense_1=="Max"){
    julia_command("@constraint(model, First_player, z[1] - (First_Objective - First_nadir)<= 0 );")
  }else{
    julia_command("@constraint(model, First_player, z[1] + (First_Objective - First_nadir)<= 0 );")
  }

  if(Sense_2=="Max"){
    julia_command("@constraint(model, Second_player, z[2] - (Second_Objective - Second_nadir)<= 0 );")
  }else{
    julia_command("@constraint(model, Second_player, z[2] + (Second_Objective - Second_nadir)<= 0 );")
  }
  return(Time_limit)
}
