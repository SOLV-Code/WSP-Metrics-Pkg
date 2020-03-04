# This function is part of the emerging "WSPMetrics" Package
# for the latest version and additional information, check
# https://github.com/SOLV-Code/WSP-Metrics-Code




##################################################
# Function to calculate percent change in spawners over 3 generations from time-series of logged abundances
# Input= vector of years covering 3 generations and corresponding vector of logged (natural log)  spawner abundances (CK: Used to use  logged& smoothed, but then changed to just logged)
# Output = % change in spawner abundanes over 3 generations
# This function originally contributed by Carrie Holt (DFO-Nanaimo)
# MODIFICATIONS: CHANGED TO SINGLE INPUT OBJECT (to use with apply, by) , ADDED NA HANDLING

per.change<-function(data.in,na.rm=TRUE){
		
	if(na.rm){data.in <- na.omit(data.in)}
	n<-dim(data.in)[[1]]
	lm<-lm(data.in[,"spn"]~data.in[,"yr"])
	y<-exp(predict(lm,as.data.frame(data.in[,"yr"])))
	pchange<-((y[n]-y[1])/y[1])*100
	return(pchange)
}

# modified version takes only single vector, because using in apply() on large array
per.change.mod<-function(vec.in,na.rm=TRUE){
	#print("starting per.change")
	if(na.rm){vec.in <- na.omit(vec.in)}
	n<-length(vec.in)
	yrs <- 1:n
	lm<-lm(vec.in~yrs)
	y<-exp(predict(lm,as.data.frame(yrs)))
	pchange<-((y[n]-y[1])/y[1])*100
	return(pchange)
}

# modified version above, further modified for speed 
# .lm.fit is almost 2 orders of magnitude faster than lm
# (i.e. 70 times faster)
# use index values instead of years to simplify pchange
# and use coefficients rather than fitted values
# using  fitted values output from lm is same as running via predict (for lm() anyway)


per.change.mod.fast<-function(vec.in,na.rm=TRUE){
	#print("starting per.change")
	if(na.rm){vec.in <- na.omit(vec.in)} # NEED TO DISCUSS THIS
	
	# if no NA  (left) in input
	if(sum(is.na(vec.in)) == 0 ){	
	n<-length(vec.in)
	yrs <- 1:n
	lm.coeff <- .lm.fit(cbind(1,yrs),vec.in)$coefficients # uses model matrix that is usually created inside lm()
	#print(lm.coeff)
	pchange <- (exp(lm.coeff[1]+lm.coeff[2]*n) -  exp(lm.coeff[1]+lm.coeff[2])) / exp(lm.coeff[1]+lm.coeff[2]) *100
	#print(pchange)	
	}
	
	if(sum(is.na(vec.in)) >0 ){	pchange <- NA }
	
	
	return(pchange)
	
	
}











