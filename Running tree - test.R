data <- read.csv('E:/_CSOC/2017 - 1ST/DATA MINING/Assignment 1/Test/data_x.txt')
label <- c(0,0,0,0,0,1,1,1,1,1)
nmin_obs <- 3 #the number of observations that a node must contain at least for it to be allowed to be split
minleaf_obs <- 1 #the minimum number of observations required for a leaf node
nfeat_obs <- 5
  
#test tree grow
tree <- tree.grow(data,label,nmin_obs,minleaf_obs,nfeat_obs)
tree

labelresult <- tree.classify(tree,data)
labelresult
