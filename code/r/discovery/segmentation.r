#============================= segmentation ========================================
isActivity = function(x){
  sumVar = 0;
  frameCnt = length(x[,1]);
  for(i in 1:K){sumVar = sumVar + var(x[,i]);}
  isAct = ifelse(sumVar<0.25,1,0);
  #isAct = ifelse(sumVar<0.30,1,0);
  if(isAct == F){
    for(frameIndex in 1:length(x[,1])){
      sumVar = 0;
      for(i in 1:K){sumVar = sumVar + var(x[-frameIndex,i]);}
      #isAct = ifelse(sumVar<0.25,1,isAct);
      isAct = ifelse(sumVar<0.15,1,isAct);
    }
  }
  return(isAct)
}


mergeNeighbourActivity = function(x){
  len = length(x[,1])
  actSize = 10
  findActivity = FALSE;
  startVec = vector()
  endVec = vector()
  for(i in 1:(len-actSize+1)){
    if(findActivity == FALSE){
      if(isActivity(x[i:(i+actSize-1),]) == TRUE){
        
        if(i < len - actSize){
          findActivity = TRUE;
          startVec = c(startVec, i + as.integer(actSize / 3));
        }     
      }
    }else{
      if(isActivity(x[i:(i+actSize-1),]) == FALSE){
        findActivity = FALSE;
        endVec = c(endVec, i+as.integer(actSize*2/3)-1);
      }else{
        if(i == (len-actSize+1)){
          endVec = c(endVec, i+as.integer(actSize*2/3)-1);
        }
      }
    }
  }
  return(rbind(startVec, endVec))
}

mergeNeighbourSeg = function(segmentation){
  cur_start = segmentation[1,1];
  cur_end = segmentation[2,1];
  start_vec = c();
  end_vec = c();
  len = length(segmentation[1,]);
  for(i in 2:len){
    if(segmentation[1, i] < cur_end){
      cur_end = segmentation[2, i];
    }else{
      start_vec = c(start_vec, cur_start);
      end_vec = c(end_vec, cur_end);
      cur_start = segmentation[1, i];
      cur_end = segmentation[2, i];
    }
  }
  start_vec = c(start_vec, cur_start);
  end_vec = c(end_vec, segmentation[2, len]);
  return(rbind(start_vec, end_vec));
}

visualSegmentation = function(startEndList){
  pairCnt = length(startEndList[1,])
  for(i in 1:pairCnt){
    for(j in startEndList[1,i] : startEndList[2,i]){
      points(x = j, y = 1.2, col = ifelse(i%%2==0, 'gray', 'navy'), pch = 16)
    }
  }
}

visualTargetSegmentation = function(startEndList, viz_index){
  pairCnt = length(startEndList[1,])
  for(i in 1:pairCnt){
    for(j in startEndList[1,i] : startEndList[2,i]){
      if(j %in% viz_index){
        points(x = j - viz_index[1], y =1.2, col = ifelse(i%%2==0, 'gray', 'navy'), pch = 16)
      }
      
    }
  }
}