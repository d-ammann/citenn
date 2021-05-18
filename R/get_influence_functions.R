get_influence_functions=function(T_t,Y_t,mu0,tau,pred_ps,random=FALSE){
  first_part=as.vector((1-T_t))*(Y_t-as.vector(mu0))
  second_part=as.vector(T_t)*(Y_t-as.vector(mu0)-as.vector(tau))
  if (random==FALSE){
    pred_psn=pred_ps
    psi_0=(first_part/(1-as.vector(pred_psn)))+as.vector(mu0)
    psi_1=(second_part/as.vector(pred_psn))+as.vector(mu0)+as.vector(tau)

  } else {
    psi_0=(first_part/(1-pred_ps))+as.vector(mu0)
    psi_1=(second_part/(pred_ps))+as.vector(mu0)+as.vector(tau)
  }
  outi=list(psi_0,psi_1)
  return(outi)
}
