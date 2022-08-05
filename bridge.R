# MRCT validate (Hui Quan et al. 2010) sample 1 normal endpoint
# DOI: 10.1002/pst.380
# f1
alpha = 0.05
beta = 0.13
pi = 0.5
u = 1

fi <- function(fu,alpha,beta,pi,u,beta_){
  return(((qnorm(1-alpha/2,0,1)+qnorm(1-beta,0,1))*sqrt(fu)*(u-pi-pi*(u-1)*fu))/((1+(u-1)*fu)*(sqrt(1+(pi^2-2*pi)*fu)))-(qnorm(1-beta_,0,1)))
}

fu <- uniroot(f = fi, c(0, 1),alpha=alpha,beta=beta,pi=pi,u=u,beta_=0.2 ,tol = 1e-8)$root
cat("fu is: ",round(fu,digits = 3))
