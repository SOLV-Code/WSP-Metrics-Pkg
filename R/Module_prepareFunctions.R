
prepareFunctions <- function(path.use=NULL){


 
	file.list <- list.files(path=path.use,pattern=".R") # get all .R files 
	module.list <- file.list[grepl("Module_",file.list)]
	for(module.source in module.list){
		print(paste("Sourcing: ",module.source))
		source(paste(path.use,module.source,sep="/"))}

		
	load_or_install(c("reshape"))	
	library(reshape)

}


load_or_install <- function(package_names){
	# function to load/install required packages
	for(package_name in package_names){
		if(!is_installed(package_name)){install.packages(package_name,repos="http://lib.stat.cmu.edu/R/CRAN")}
		library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)
	}
}#END load_or_install


is_installed <- function(mypkg){ is.element(mypkg, installed.packages()[,1])}

