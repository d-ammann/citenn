train_test_splits=function(x,y,t,train_proportion=0.8){

  train_inds=as.vector(matrix(0,length(t)))
  valid_inds=as.vector(matrix(1,length(t)))

  treated=which(t==1)
  non_treated=which(t==0)
  treated_sample=sample(treated,length(treated)*train_proportion)
  non_treated_sample=sample(non_treated,length(non_treated)*train_proportion)

  for (i in treated_sample){
    train_inds[i]=1
    valid_inds[i]=0
  }

  for (m in non_treated_sample){
    train_inds[m]=1
    valid_inds[m]=0
  }

  train_del=which(train_inds==0)
  valid_del=which(valid_inds==0)

  x_train=as.matrix(x[-train_del,])
  x_valid=as.matrix(x[-valid_del,])
  t_train=as.matrix(t[-train_del,])
  t_valid=as.matrix(t[-valid_del,])
  y_train=as.matrix(y[-train_del,])
  y_valid=as.matrix(y[-valid_del,])

  out=list(x_train,x_valid,t_train,t_valid,y_train,y_valid)
  return(out)
}
