

calcRickerSgen <- function(a,b){
# this is just a wrapper for functions from Holt & Ogden 2013

sgen.est <- Sgen.solver(a,b,sig=1)


return(sgen.est$SRfit)


}








# functions below are from Holt & Ogden 2013


Sgen.model<-function(S,a,b,sig){
	PR<-a*S*exp(-b*S)
	SMSY<-(log(a)/b)*(0.5-0.07*log(a))
	epsilon.wna=log(SMSY)-log(PR)	#residuals
	epsilon=as.numeric(na.omit(epsilon.wna))
	nloglike=sum(dnorm(epsilon,0,sig, log=T))
	if(is.na(sum(dnorm(epsilon,0,sig, log=T)))==TRUE) print(c(a,b,sig))
	return(list(PR=PR, epsilon=epsilon, nloglike=nloglike))#actually returns postive loglikelihood (CH note)
}

Sgen.fn <- function(S,a,b,sig) -1.0*Sgen.model(S,a,b,sig)$nloglike	#gives the min Ricker LL

Sgen.solver <- function(a,b,sig) {
	SMSY<-(log(a)/b)*(0.5-0.07*log(a))
	SRfit=optimize(f=Sgen.fn,interval=c(0, SMSY), a=a, b=b, sig=sig)	 # nb: not optim() !!
	return(list(SRfit=SRfit$minimum))  # returns the minimum S
}