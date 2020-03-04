
status.assign <- function(X,bm.lower,bm.upper){
# X is a matrix of yr by stock with metric values 
#(i.e. output from one of the metric functions like calcPercChange)
# bm.lower and bm.upper are the bounds between Red/Amber and Amber/Green

X.status <- X

 X.status[X <= bm.lower] <- "Red"
 X.status[X > bm.upper] <- "Green"
 X.status[X > bm.lower & X <= bm.upper] <- "Amber"
 
return(X.status) 

}


status.crosstab <- function(x,y,labels=c("X","Y")){
			out.mat <- matrix(NA,ncol=4,nrow=4,dimnames=list(c("Red","Amber","Green","NA"),c("Red","Amber","Green","NA")))
			
			# couldn't get xtabs to work , table produces rearranged crosstab only of present factors
			# do with nested loops for now -  find better way later
			#table.tmp <- table(x,y)
			#print(table.tmp)

			for(x.status in c("Red","Amber","Green")){
				for(y.status in c("Red","Amber","Green")){
					out.mat[x.status,y.status] <- sum(x == x.status & y == y.status,na.rm=TRUE)

					out.mat[x.status,"NA"] <- sum(x == x.status & is.na(y), na.rm=TRUE)
					out.mat["NA",y.status] <- sum(is.na(x) & y == y.status, na.rm=TRUE)

				}} #end looping through statuses


			out.mat["NA","NA"] <- sum(is.na(x) & is.na(y))



			dimnames(out.mat) <- list(paste(labels[1],c("Red","Amber","Green","NA"),sep="_"),
					paste(labels[2],c("Red","Amber","Green","NA"),sep="_"))


			return(out.mat)
			}
			
			
			
# still need to fix this for uneven num of NA (e.g. when comparing alt time windows!)
status.mismatch <- function(x,y){
			#x and y are text vector of same length
			num.mismatch <- sum(x != y,na.rm=TRUE)
			prop.mismatch <- num.mismatch/sum(!is.na(x))
			return(list(prop=prop.mismatch,num=num.mismatch))
			}			
			