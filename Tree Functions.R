#calculate gini index, y is vector
impurity <- function (y){
  numberofelement <- length(y)
  if(numberofelement > 0){
    numberofone <- length(y[y == 1])
    numberofzero <- length(y[y == 0])
    result <- (numberofzero/numberofelement)*(numberofone/numberofelement)
  }
  else{
    result <- 0
  }
  
  return(result)
}

#define the best split in a node, x is data matrix and y is a vector, return a list (attribute, splitvalue, impurity reduction)
findbestsplit <- function (x, y){
  sampleindex <- sample(ncol(x),nfeat) #get (nfeat) random attributes
  x.selectedattr <- x[,sampleindex]
  
  bestsplitcandidates <- list()
  for(i in 1:ncol(x.selectedattr)){ #foreach attribute
    x.attr <- x.selectedattr[,i]
    x.sorted <- sort(unique(x.attr))
    if(length(x.sorted) == 1){  #ask this
      splitpoints <- x.sorted
    }
    else{
      splitpoints <- (x.sorted[1:(length(x.sorted)-1)]+x.sorted[2:length(x.sorted)])/2
    }
    
    i.t <- impurity(y)
    n <- length(y)
    tempresult <- list(NULL,0,0) # change to list(attribute, splitvalue, impurity reduction)
    for(j in 1:length(splitpoints)){
      x.l <- y[x.attr <= splitpoints[j]] #left
      pi.l <- length(x.l)/n
      i.l <- impurity(x.l)
      
      x.r <- y[x.attr > splitpoints[j]] #right
      pi.r <- length(x.r)/n
      i.r <- impurity(x.r)
      
      impurityreduction = i.t - (pi.l*i.l) - (pi.r*i.r)
      if(tempresult[3] < impurityreduction) { #tba: if they have same impurity reduction value
        tempresult <- list(colnames(x.selectedattr)[i], splitpoints[j], impurityreduction)
      }
    }
    bestsplitcandidates[[i]] <- tempresult
  }
  bestsplitcandidates.arr <- simplify2array(bestsplitcandidates)
  bestsplit <- bestsplitcandidates.arr[,which.max(bestsplitcandidates.arr[3,])]
  
  return (bestsplit)
}

#split the node (create 2 nodes), x is a data matrix (observations) contained in the parent node, y is the label vector
splitnode <- function(node_id.root, x, y){
  impurity.y <- impurity(y)
  if (impurity.y != 0 && nrow(x) >= nmin){ #stoping condition: minimum observation to be splitted (and impurity should be bigger than 0)
    bestsplit <- findbestsplit(x,y)
    print(paste('---- Split value:',bestsplit[1],bestsplit[2],sep=' '))
    
    index.left <- c(1:nrow(x))[x[,bestsplit[[1]][1]] <= bestsplit[[2]][1]]
    x.left <- x[index.left,]
    x.right <- x[-index.left,]
    y.left <- y[index.left]
    y.right <- y[-index.left]
      
    if(nrow(x.left) >= minleaf && nrow(x.right) >= minleaf){ #stoping condition: minimum observation in the leaf node (and should be bigger than 0)
      node_id.left <- node_id.current+1
      node_id.current <<- node_id.right <- node_id.current+2
      
      finaltree[[node_id.left]] <<- node.left <- list(node_id.left, node_id.root, bestsplit[[1]][1], "<=", bestsplit[[2]][1], classlabel(y.left))
      print(paste('Add node: [',node_id.left,']',nrow(x.left),'(',length(y.left[y.left == 0]),'/',length(y.left[y.left == 1]),')',sep=' '))
      
      finaltree[[node_id.right]] <<- node.right <- list(node_id.right, node_id.root, bestsplit[[1]][1], ">", bestsplit[[2]][1], classlabel(y.right))
      print(paste('Add node: [',node_id.right,']',nrow(x.right),'(',length(y.right[y.right == 0]),'/',length(y.right[y.right == 1]),')',sep=' '))
      
      #call splitnode function (recursively) for the left and right node
      print(paste('-- Split (left) node:',node_id.left,sep=' '))
      splitnode(node_id.left, x.left, y.left)
      print(paste('-- Split (right) node:',node_id.right,sep=' '))
      splitnode(node_id.right, x.right, y.right)
    }
  }
}

#get majority class of y
classlabel <- function(y){
  if(length(y[y == 0]) > length(y[y == 1])) return(0)
  else if (length(y[y == 1]) > length(y[y == 0])) return(1)
  else return(NULL)
}

#tree grow
tree.grow <- function(x,y,nmin,minleaf,nfeat){
  if(nmin == 0 || minleaf == 0 || nfeat == 0) stop('the nmin, minleaf, and nfeat should be bigger than 0')

  nmin <<- nmin
  minleaf <<- minleaf
  nfeat <<- nfeat
  
  finaltree <<- list()
  node_id.current <<- 1
  node.root <- list(node_id.current, NULL, NULL, NULL, NULL, classlabel(y)) #names(node) <- c("node", "parent", attribute", "L/R", splitvalue", "label")
  finaltree[[node_id.current]] <<- node.root
  print(paste('Create root node: [',node_id.current,']',nrow(x),'(',length(y[y == 0]),'/',length(y[y == 1]),')',sep=' '))
  
  splitnode(1, x, y)
  
  result <- aperm(simplify2array(finaltree))
  colnames(result) <-  c("node", "parent", "attribute", "L/R", "splitvalue", "label")
  
  return(result)
}


#get the label class for a row of x(row_x)
classify <- function(currentnode, row_x){
  indexchild <- c(1:nrow(tree))[tree[,"parent"] == currentnode] #get the rows in the tree whose parent is currentnode
  childnodes <- tree[indexchild,]
  
  if(nrow(childnodes) > 0){
    attrvalue <- row_x[toString(childnodes[1,'attribute'])]
    splitvalue <- as.double(childnodes[1,'splitvalue'])
    if (attrvalue <= splitvalue){
      currentnode <- toString(childnodes[1,"node"])
      #print(currentnode)
      classify(currentnode, row_x) #call the function recursively to go down through the tree
    }
    else{
      currentnode <- toString(childnodes[2,"node"])
      #print(currentnode)
      classify(currentnode, row_x)
    }
  }
  else{
    indexleafnode <- c(1:nrow(tree))[tree[,1] == currentnode]
    leafnode <- tree[indexleafnode,]
    label <- toString(leafnode["label"])
    return(label)
  }
}

#tree classify
tree.classify <- function(treepredictor, x){
  y <- vector(length=nrow(x))
  tree <<- treepredictor
  for(i in 1:nrow(x)){
    row_x <- x[i,]
    y[i] <- classify('1', row_x)
  }
  
  return(y)
}
