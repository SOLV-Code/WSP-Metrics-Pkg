#' generateReportTemplate
#'
#' this function creates a basic markdown template after the other functions have been used to calculate metrics, apply the rapid status decision tree, and plot dashboards.
#' @param type one of "readme" or "quarto". "readme" creates a basic README.md file that will display formatted text including figures when viewed on Github or Gitlab. "quarto" creates a "*.qmd" file as a starting point for a more comprehensive document. 
#' @param file.label label to be used for markdown file (if type = "quarto")
#' @param files.path path to folder with input files and output files. The following files are required: "CU_Specs_LABEL.csv", and a sub folder "Dashboards" with the plots 
#' @param repo.path if type = "readme", need to provide a path for the repository that the folder with the files (i.e., the part before the files.path argument). For example, if files.path is "OUTPUT", and repo.path is "https://github.com/UserName/RepoName", then the Readmde.md file will look for plots in https://github.com/UserName/RepoName/OUTPUT/Dashboards".
#' @keywords markdown, report, readme
#' @export



generateReportTemplate <- function(type = "readme", file.label = "Report",
				files.path, repo.path=NULL){


library(tidyverse)
#library(plotrix) CHECK IF STILL NEEDED




if(type=="readme"){
report.path <- paste0(files.path,"/MarkdownReadme")
file.name <- paste0(report.path,"/README.md")
}


if(type=="quarto"){
report.path <- paste0(files.path,"/Quarto")
file.name <- paste0(report.path,"/",file.label,".qmd")
}


if(!dir.exists(report.path)){dir.create(report.path)}

if(type=="readme"){

cat("# Status Report \n",file = file.name)


}


if(type=="quarto"){

cat("
---	
title: \"Status Report\"
format: 
  docx:
    toc: true
    number-sections: true
    highlight-style: github
editor: visual
execute: 
  warning: false
  message: false
  echo: false
---	
	
	
	
	
	
	",file = file.name)

}

# Part 1 - Intro Materials

cat("
## Introduction

### Background

This report summarizes data, status metrics, and rapid status status results for INSERT LABEL.

### WSP Status Methods

put some brief text here with links to publications

## CU Status

Put some brief text that describes the dashboards


",file = file.name,append=TRUE)


# read in spec file

spec.file.path <- list.files(path=files.path,pattern = "CU_Specs_",full.names=TRUE)
cu.specs <- read_csv(spec.file.path)
cu.list <- cu.specs %>% select(CU_ID) %>% unlist()
print(cu.list)


fig.list <- list.files(path=paste0(files.path,"/","Dashboards"),pattern = ".png",full.names=FALSE)
print(fig.list)

# loop through CUs


for(cu.do in cu.list){


print(cu.do)

specs.sub <- cu.specs %>% dplyr::filter(CU_ID==cu.do)

cu.name <- specs.sub$CU_Name

print(cu.name)

if(type=="readme"){

cat("


",file = file.name,append=TRUE)

}


if(type=="quarto"){

cat("

{{< pagebreak >}}

",file = file.name,append=TRUE)

}


cat(paste0("### ",cu.name," (",specs.sub$CU_ID,", ",specs.sub$Group,")\n"),file = file.name,append=TRUE)

cat(specs.sub$Description,file = file.name,append=TRUE)

cat("


",file = file.name,append=TRUE)

fig.name <- grep(cu.do,fig.list,value=TRUE)
print(fig.name)

if(length(fig.name)>0){

if(type=="readme"){

cat(paste0('<img src=\"', repo.path,'/blob/main/',files.path,'/Dashboards/',fig.name,'\" width="600">'),file = file.name,append=TRUE)

}


if(type=="quarto"){
	cat(paste0('![Status Metrics and Rapid Status for ',cu.name,'](../Dashboards/',fig.name,')'),
			file = file.name,append=TRUE)
}

}






} # end looping through CUs


} # end report function
