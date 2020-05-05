#Sourcing a standard set of options I maintain to make my life easier.
source("https://raw.githubusercontent.com/jenitivecase/Settings/master/options.R")

sapply(c("gsheet", "tidyverse", "DT", "knitr"), load_packages)

stalks <- as.data.frame(gsheet2tbl("docs.google.com/spreadsheets/d/1SVxFEGjCGhj3q6NQB1cJNf5CLeFTo9giG79HdOqJXgo/",
                                   sheetid = "selling"))

##### GENERAL PROJECT STAGES ###################################################
#
# 1. MVP: hand-coded algorithm to identify pattern. >= 3 data points required.
# 2. loess regression lines?
# 3. time series analysis
# 4. ML time series analysis
#
##### QUEsTIONS ################################################################
#
# - how to handle missing data?
# - are there really four patterns?
# - are the four pattern labels put forth by the internet really real?
#
################################################################################


##### AN ATTEMPT AT WRITING DECISION RULES #####################################
