


.WSO_Optimizer<-function(Name_1,Sense_1,Weight_1,Name_2,Sense_2,Weight_2,Time_limit){

  Initial_time_limit=Time_limit;
  julia_assign('First_optimal',"N/A")
  julia_assign('Second_optimal',"N/A")
  julia_assign("W",c(as.double(Weight_1),as.double(Weight_2)));

  if(Sense_1=="Max" && Sense_2=="Max"){
    julia_command(paste("@objective(model, Max,W[1]*", Name_1," + W[2]*",Name_2,");",sep=''))
  }else if(Sense_1=="Min" && Sense_2=="Min"){
    julia_command(paste("@objective(model, Min,W[1]*", Name_1," + W[2]*",Name_2,");",sep=''))
  }else if(Sense_1=="Min" && Sense_2=="Max"){
    julia_command(paste("@objective(model, Min,W[1]*", Name_1," - W[2]*",Name_2,");",sep=''))
  }else if(Sense_1=="Max" && Sense_2=="Min"){
    julia_command(paste("@objective(model, Min,-W[1]*", Name_1," + W[2]*",Name_2,");",sep=''))
  }


  Optimization<-.optimize(Time_limit,Skip_solution=0)

  if(Optimization[["Gap"]]!=-1){
    Optimization[[Name_1]]=julia_eval(paste('value(',Name_1,')'))
    Optimization[[Name_2]]=julia_eval(paste('value(',Name_2,')'))
  }else{
    Optimization[[Name_1]]="N/A"
    Optimization[[Name_2]]="N/A"
  }

  return(Optimization);
}

