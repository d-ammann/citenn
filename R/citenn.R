#' @export

citenn=function(x,t,y,arcitecture_treatment=c(128,64,32),arcitecture_propensity=c(128,64,32),random=FALSE,train_proportion=0.8,epochs_treatment=30,epochs_propensity=30){

  #check if keras is installed
  version_control=keras::is_keras_available(version=2)
  msg=paste("You do not have keras version 2.0 or higher installed.",
            "Please install the most current version of keras with a tensorflow backend",
            "For further information on how to install keras visit:",
            "https://tensorflow.rstudio.com/guide/keras/", sep="\n")
  if (version_control==FALSE){
    stop(msg)
  }

  #Checking inputs









  n_part=nrow(x)

  #train test split
  split=train_test_splits(x,y,t,train_proportion=train_proportion)
  x_train=split[[1]]
  x_valid=split[[2]]
  t_train=split[[3]]
  t_valid=split[[4]]
  y_train=split[[5]]
  y_valid=split[[6]]



  #First Model
  list1=first_NN(x_train,y_train,t_train,x,y,t,x_valid,y_valid,t_valid,arcitecture=arcitecture_treatment,epochs_t=epochs_treatment)
  mu0=list1[[1]]
  tau=list1[[2]]
  y_pred=list1[[3]]

  #Second Model
  if (random == FALSE){
    t_pred=second_NN(x_train,t_train,x,t,x_valid,t_valid,arcitect=arcitecture_propensity,epochs_p=epochs_propensity)
  } else{
    t_pred=mean(t)
  }

  #get influence functions
  list2=get_influence_functions(t,y,mu0,tau,t_pred,random=random)
  psi_0=list2[[1]]
  psi_1=list2[[2]]

  #get confidence interval
  list3=get_confidence_interval(psi_0,psi_1,n_part)
  lower_bound=list3[[1]]
  upper_bound=list3[[2]]

  cat(paste("Returning list with the following elements:",
            "confidence interval lower bound",
            "confidence interval upper bound",
            "Predictions of mu0",
            "Predictions of tau",
            "Predictions of y",
            "Predictions of T",sep="\n"))

  out=list(lower_bound,upper_bound,mu0,tau,y_pred,t_pred)


}
