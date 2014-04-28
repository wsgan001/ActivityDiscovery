#============================= segmentation ========================================
isActivity = function(x){
  sumVar = 0;
  for(i in 1:K){sumVar = sumVar + var(x[,i]);}
  isAct = ifelse(sumVar<0.3,1,0);
  if(isAct == F){
    for(frameIndex in 1:length(x[,1])){
      sumVar = 0;
      for(i in 1:K){sumVar = sumVar + var(x[-frameIndex,i]);}
      isAct = ifelse(sumVar<0.25,1,isAct);
    }
  }
  return(isAct)
}


mergeNeighbourActivity = function(x){
  len = length(x[,1])
  actSize = 6
  findActivity = FALSE;
  startVec = vector()
  endVec = vector()
  for(i in 1:(len-actSize+1)){
    if(findActivity == FALSE){
      if(isActivity(x[i:(i+actSize-1),]) == TRUE){
        
        findActivity = TRUE;
        startVec = c(startVec, i + as.integer(actSize / 2));
      }
    }else{
      if(isActivity(x[i:(i+actSize-1),]) == FALSE){
        findActivity = FALSE;
        endVec = c(endVec, i+actSize-1 -1);
      }else{
        if(i == (len-actSize+1)){
          endVec = c(endVec, i+actSize-1 + as.integer(actSize / 2));
        }
      }
    }
  }
  return(rbind(startVec, endVec))
}
visualSegmentation = function(startEndList){
  pairCnt = length(startEndList[1,])
  for(i in 1:pairCnt){
    for(j in startEndList[1,i] : startEndList[2,i]){
      points(x = j, y = 0.7, col = colors[i], pch = 16)
    }
  }
}