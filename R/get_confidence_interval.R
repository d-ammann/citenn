get_confidence_interval=function(psi_0,psi_1,n_part){
  mean_d_psi=mean(psi_1-psi_0)
  sd_d_psi=sd(psi_1-psi_0)
  ci_upper=(mean_d_psi+1.96*sd_d_psi/sqrt(n_part))
  ci_lower=(mean_d_psi-1.96*sd_d_psi/sqrt(n_part))
  out=list(ci_lower,ci_upper)
  return(out)
}
