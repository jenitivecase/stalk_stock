#Sourcing a standard set of options I maintain to make my life easier.
source("https://raw.githubusercontent.com/jenitivecase/Settings/master/options.R")

sapply(c("gsheet", "tidyverse", "DT", "knitr"), load_packages)

stalks <- as.data.frame(gsheet2tbl("docs.google.com/spreadsheets/d/1SVxFEGjCGhj3q6NQB1cJNf5CLeFTo9giG79HdOqJXgo/",
                                   sheetid = "selling"))
