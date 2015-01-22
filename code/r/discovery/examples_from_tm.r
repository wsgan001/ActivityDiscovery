#====================== FUNCTION: GET THE LONGEST SEGMENTATION =======================

# get the index of the longest segment, where the segments in the exclusiveIndex list is excluded
get_long_segmentation = function(segmentation, exclusiveIndex){
  seg = segmentation
  seg[3, exclusiveIndex] = 0;
  return(which(seg[3,] == max(seg[3,])))
}

# decide whether a segment can be recognized by a classifier
# if over 70% data is recognized, then the segment is regared as known
is_class = function(classifier, data){
  data_length = length(data[,1])
  score = svm.predict(classifier, data);
  ifelse(length(score[score > 0]) > data_length*0.5, TRUE, FALSE);
}



# decide whether a segment can be recognized by a list of classifiers
is_known_segment = function(segmentation, segIndex, knownClassifiers, data){
  is_known = FALSE;
  segment_data = data[segmentation[1, segIndex] : segmentation[2, segIndex], ]
  
  is_model = lapply(knownClassifiers, is_class, data=segment_data);
  ifelse(length(is_model[is_model == TRUE]), TRUE, FALSE)
}

# decide each instance in the doc_list is a known instance
is_known_doclist = function(data, classifiers){
  is_class = lapply(classifiers, is_class_doclist, data=data);
  is_known_class = rep(FALSE, length(data[,1]));
  if(length(classifiers) > 0){
    for(i in 1:length(classifiers)){
      is_known_class = is_known_class | smooth_classify_tag(c(is_class[[i]]));
    }
  }
  return(is_known_class);
}
is_class_doclist = function(classifier, data){
  score = svm.predict(classifier, data);
  is_class = ifelse(score>0, TRUE, FALSE)
  return(is_class);
}

#======================== FUNCTION: POS/NEG EXAMPLE SAMPLING ========================
# get the mean distribution of each topic given a list of docs
get_period_topic_mean = function(startFrame, endFrame){
  topic_mean = 1:K;
  for(i in 1:K) topic_mean[i] = mean(pred[startFrame:endFrame, i]);
  return(topic_mean);
}
get_period_topic_differnce = function(topicMean1, topicMean2){sum(abs(topicMean1 - topicMean2));}

# sample positive examples for a garget segment
samplePosExamples = function(targetSegIndex, segmentation){
  start = segmentation[1, targetSegIndex]
  end =  segmentation[2, targetSegIndex]
  length = end - start;
  sampleCnt = as.integer(ifelse(length < 100, length * 0.7, length * 0.5));
  return(sample(start:end, sampleCnt));
}

# sample negative examples for a garget segment
sampleNegExamples = function(targetSegIndex, segmentation, diffThreshold){
  # noise --> ok            noise matrics = short or noisy
  # totally different --> sample it
  negFrameIndex = c();
  maxPosCnt = segmentation[2, targetSegIndex] - segmentation[1, targetSegIndex];
  #get the noise 
  shortSegIndex = which(segmentation[3,] < 10)
  #for(i in shortSegIndex){
  #  negFrameIndex = union(negFrameIndex, segmentation[1,i] : segmentation[2,i])
  #}
  # totally different segment
  allSegIndex = 1:length(segmentation[1,])
  targetTopicMean = get_period_topic_mean(segmentation[1, targetSegIndex], segmentation[2, targetSegIndex])
  for(i in allSegIndex[! allSegIndex %in% union(shortSegIndex, targetSegIndex)]){
    topicMean = get_period_topic_mean(segmentation[1, i], segmentation[2, i]);
    topicDistDiff = get_period_topic_differnce(targetTopicMean, topicMean);
    if(topicDistDiff > diffThreshold){ # a different model
      sampleMaxCnt = min(maxPosCnt * 0.3, (segmentation[2, i] - segmentation[1, i])*0.5);
      sampleCnt = as.integer(sampleMaxCnt * topicDistDiff*0.7);
      sampleIndex = sample(segmentation[1, i]:segmentation[2, i], sampleCnt);
      negFrameIndex = union(negFrameIndex, sampleIndex)
    }
  }
  return(negFrameIndex)
}

# visualizing the training samples
viz_train_sample = function(posTrainIndex, negTrainIndex, segmentation, doc_labels){
  plot(1, xlim=c(0,length(doc_labels)), ylim = c(0,1.5));
  for(i in 1:docCnt){   
    points(x=i, y=1.2, col=colors[which(doc_label_set==doc_labels[i])], pch=16)
  }
  visualSegmentation(segmentation);
  for(i in posTrainIndex){
    points(x=i, y=0.5, col='green');
  }
  for(i in negTrainIndex){
    points(x=i, y=0.5, col='red');
  }
}