
# ==============================================================================================
library(ISLR)
library(tree)
library(kernlab)


#============================ CO-TRAINING =========================================

vector.removenoise = function(vec, noiseRatio){
  validLength = as.integer(length(vec) * (1 - noiseRatio));
  svec = vec + rnorm(length(vec), 0, 0.0001);
  sub = vec[rank(svec) == validLength];
  vec[rank(vec) > validLength] = sub;
  return(vec)
}

dataframe.get_normalize_param = function(data){
  dim = length(data[1,])
  means = 1:dim;
  stdvar = 1:dim;
  for(i in 1:dim){
    means[i] = mean(data[,i]);
    stdvar[i] = sd(data[,i]);
  }
  return(rbind(means, stdvar));
}

vector.normalize = function(vec){(vec - mean(vec)) / ifelse(sd(vec)==0, 1, sd(vec));}
dataframe.normalize = function(data){
  dim = length(data[1,])
  for(i in 1:dim){data[,i] = vector.normalize(data[,i]);}
  return(data)
}


decision_tree.train = function(train_data){ # train_data = features + target
  tree_model = tree(target~., train_data)
  plot(tree_model)
  text(tree_model, pretty=0)
  return(tree_model)
}
decision_tree.predict = function(model, test_data){
  #test_target = dataset$High
  #tree_pred = predict(model, test_data, type='class')
  #misclass = mean(tree_pred != test_High)
  #print(misclass)
  return(predict(model, test_data, type='vector'))
}

decision_tree.find_confident_example = function(pred_score, num, test_index){
  all_cnt = length(pred_score)
  yes_prob = as.vector(pred_score)
  yes_prob = yes_prob + rnorm(all_cnt, 0, 0.001); # add noise for ranking
  yes_max_num = min(length(pred_score[pred_score > 0]) * 0.3, num/2);
  neg_max_num = min(length(pred_score[pred_score < 0]) * 0.3, num/2);
  exchange_num = min(yes_max_num, neg_max_num);
  
  pos_index = which(rank(yes_prob) > all_cnt - exchange_num)
  neg_index = which(rank(yes_prob) <= exchange_num)
  index = c(pos_index, neg_index);
  #return(test_index[index]);
  return(index);
}

carseats.text2num = function(dataset){
  dataset$ShelveLoc <- ifelse(dataset$ShelveLoc == 'Good', 1, ifelse(dataset$ShelveLoc == 'Medium', 0, -1));
  dataset$Urban <- ifelse(dataset$Urban == 'Yes', 1, 0)
  dataset$US <- ifelse(dataset$US == 'Yes', 1, 0)
  return(dataset)
}

svm.train = function(train_data){
  x_dimension = length(train_data[1,]) -1 
  x_train = train_data[, 1: x_dimension]
  y_train = train_data[, x_dimension+1]
  x_matrix = data.matrix(x_train);
  y_matrix = data.matrix(y_train);
  svp <- ksvm(x_matrix,y_matrix,type="C-svc",kernel='vanilladot',C=100,scaled=c())
  return(svp)
}

svm.predict = function(model, test_data){
 #y_data = dataset[index_test, x_dimension+1]
 #y_test = ifelse(y_data == 'Yes', 1, -1);
  x_matrix = data.matrix(test_data);
 # y_matrix = data.matrix(y_test);
  pred = predict(model, x_matrix)
  #misclass = mean(pred != y_test)
  #print(misclass)
  pred_score = predict(model, x_matrix, type='decision')
  return(pred_score)
}

svm.find_confident_example = function(pred_score, num, test_index){
  all_cnt = length(pred_score);
  yes_max_num = min(length(pred_score[pred_score > 0]) * 0.3, num/2);
  neg_max_num = min(length(pred_score[pred_score < 0]) * 0.3, num/2);
  exchange_num = min(yes_max_num, neg_max_num);
  pos = which(rank(pred_score) > (all_cnt - exchange_num))
  neg = which(rank(pred_score) <= exchange_num)
  index = c(pos, neg)
  #return(test_index[index] )
  return(index)
}

viz_pred_result = function(pred_score, test_index, doc_labels){
  #ground truth
  plot(1, type="o", col=colors[1],xlim=c(0, length(doc_labels)+1), ylim=c(0, 1.5), 
       xlab="activities", ylab="topic probability")
  for(i in 1:docCnt){
    points(x=i, y=0.9, col=colors[which(docLabelSet==doc_labels[i])], pch=16)
  }
  #prediction
  for(i in 1:length(test_index)){
    points(x=test_index[i], y = 0.5, col=ifelse(pred_score[i]>0, 'green', 'red'), pch=16)
  }
}