#library(parallel)
Quan <- function(x)quantile(x,seq(0,.9999,length.out=10000))

wrap.nlminb <- function(LBDA=NULL,TETA=NULL,data=DATA,lower.bd=1){
  
  #data <- data[data[,"Area_ha"] >= lower.bd,]
  
  nll <- parse(text="-sum(mapply(as.list(data[,\"Area_ha\"]),lambda=mu,theta = theta,FUN=PtProcess::dtappareto, a=lower.bd,log=TRUE))") ## Function to minimize
  
  nLL <- function(p){
    p <- p %*% m
    
    mu <- eval(mu)
    
    theta <- eval(theta)
    
    if((any(mu <= 0) | any(is.na(mu))) | (any(theta <= 0) | any(is.na(theta)))){
      return(.Machine$double.xmax)
    } else {
      return(eval(nll))
    }
  }
  
  if(is.null(LBDA)){
    mu <- expression(p[1])
    nm.mu <- "Lambda_Intercept"
  } else {
    mu <- parse(text=paste0("p[1] + ",paste0("data[,\"",LBDA,"\"] * p[",seq(2,length(LBDA)+1),"]", collapse=" + ")))
    nm.mu <- c("Lambda_Intercept",paste("Lambda",LBDA,sep="_"))
  }
  
  if(is.null(TETA)){
    theta <- parse(text=paste0("p[",length(LBDA)+2,"]"))
    nm.theta <- "Theta_Intercept"
  } else {
    theta <- parse(text=paste0(paste0("p[",length(LBDA)+2,"] + "), paste0("data[,\"",TETA,"\"] * p[",seq(length(LBDA)+3,length(LBDA)+length(TETA)+2),"]", collapse=" + ")))
    nm.theta <- c("Theta_Intercept",paste("Theta",TETA,sep="_"))
  }
  
  ## Find out automatically what order of magnitude is needed for parameters scaling
  m <- matrix(0,nrow=length(nm.mu)+length(nm.theta),ncol=length(nm.mu)+length(nm.theta))
  diag(m) <- 1
  
  op <- DEoptim::DEoptim(nLL,lower=c(rep(-1,length(nm.mu)),rep(-1e6,length(nm.theta))),upper=abs(c(rep(-1,length(nm.mu)),rep(-1e6,length(nm.theta)))))
  
  oom <- function(x){
    10^(ceiling(log10(abs(x))))
  }
  
  diag(m) <- oom(op$optim$bestmem)
  nm <- c(nm.mu,nm.theta)
  
  ## Brute-force to make models converge & select the best fit (according to AICc criterion)
  st.l <- lapply(1:150,function(i)rnorm(length(op$optim$bestmem),0,2)+op$optim$bestmem/oom(op$optim$bestmem))
  
  out <- parLapplyLB(cl,st.l,function(st){
    
    op <- nlminb(start=st,nLL, control=list(iter.max = 1000, eval.max=1000, trace=TRUE))
    
    it.max <- 2
    it <- 0
    ## When there is no convergence and restart is possible, run nlminb() again
    while(as.integer(gsub("[\\(\\)]", "", regmatches(op$message, gregexpr("\\(.*?\\)", op$message))[[1]])) %in% 7:14 & it <= it.max){
      it <- it +1
      op <- nlminb(start=op$par,nLL, control=list(iter.max = 1000, eval.max=1000, trace=TRUE))
    }
    
    op$AICc <- 2 * length(op$par) + 2 * op$objective + (2 * length(op$par) * (length(op$par) + 1))/ (nrow(data) - length(op$par) - 1) ## AICc = 2 * K - 2 * LL + (2K * (K + 1)) / (nrow(data) - K - 1)
    
    return(op)
  })
  
  ## Select best MLE amongst all trials
  op <- out[[which.min(unlist(lapply(out,"[[","AICc")))]]
  
  ## Compute standard errors
  se <- try(sqrt(diag(solve(numDeriv::hessian(nLL,op$par)))))
  
  ## Parameters scaling: Revert back estimated coefficients to their original scale
  op$par <- setNames(drop(op$par %*% m), nm)
  se <- try(drop(se %*% m))
  
  p <- op$par
  
  mu <- eval(mu)
  theta <- eval(theta)
  
  ## Compute quantiles of predicted values
  QP <- Quan(rtappareto(10000000, lambda = mu, theta = theta, a=lower.bd))
  
  return(list(LAMBDA = LBDA, THETA = TETA, LL = -op$objective, AICc = op$AICc, par = op$par, se = se, convergence = op$convergence, message = op$message, mu = mu, theta = theta, QP = QP, lower.bd = lower.bd))
}

## Start a cluster on local machine
#cl <- makePSOCKcluster(rep("localhost", 7))
