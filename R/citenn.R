#'Creates a 95% confidence interval for treatment effects using neural networks
#' @param x A n x m matrix where n is the number of observations and m is the number of explanatory variables excluding the treatment status.
#' @param t A n x 1 matrix containing the treatment status(1=treated,0=non treated)
#' @param y A n x 1 matrix containing the target variable
#' @param arcitecture_treatment A vector where each entry stands for a hidden layer and the value represents the number of hidden nodes in that layer.
#' @param arcitecture_propensity  A vector where each entry stands for a hidden layer and the value represents the number of hidden nodes in that layer.
#' @param random A boolean indicating if treatment was randomised or not
#' @param train_proportion The proportion of data used for training
#' @param epochs_treatment The maximum number of epochs the treatment FFN is fitted for.
#' @param epochs_propensity The maximum number of epochs the propensity FFN is fitted for.
#' @return returns a list containing: 1.Confidence interval lower bound 2.Confidence interval upper bound 3.Predictions ofmu0
#' 4.Predictions of tau 5.Predictions of y 6.Predicted probability of treatment


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
  lower_bound_95=list3[[1]]
  upper_bound_95=list3[[2]]
  mean_psi=list3[[3]]
  lower_bound_90=list3[[4]]
  upper_bound_90=list3[[5]]
  lower_bound_99=list3[[6]]
  upper_bound_99=list3[[7]]

  lower_bound=cbind(lower_bound_90,lower_bound_95,lower_bound_99)
  upper_bound=cbind(upper_bound_90,upper_bound_95,upper_bound_99)
  colnames(lower_bound)=c("90","95","99")
  colnames(upper_bound)=c("90","95","99")

  cat(paste("Returning list with the following elements:",
            "confidence intervals lower bounds",
            "confidence intervals upper bounds",
            "Predictions of mu0",
            "Predictions of tau from the Neural Network",
            "Predictions of tau using the influence functions",
            "Predictions of y",
            "Predictions of T",sep="\n"))

  out=list(lower_bound,upper_bound,mu0,tau,mean_psi,y_pred,t_pred)


}
