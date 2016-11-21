##############################
#   Clear Global Env         #
##############################
rm(list=ls())

##############################
#   Source exploration File  #
##############################
current_dir = dirname(sys.frame(1)$ofile)
source(paste(current_dir, "exploration.R", sep="/"))


