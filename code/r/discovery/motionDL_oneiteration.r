srcdir = "C:\\Users\\zetan\\workspace\\MotionDL\\code\\r\\discovery\\"
source(paste(srcdir, "util.R", sep=''))
source(paste(srcdir, "segmentation.R", sep=''))
source(paste(srcdir, "examples_from_tm.r", sep=''))
source(paste(srcdir, "self-training_co-training.R", sep=''))

dataset = "UBICOMP"

framesInDoc = 2 * 30; #30sec # 5*20

if(dataset == "PLCouple1"){
  base="D:\\lessons\\motion recognition\\dataset\\PLCouple1\\sensor\\2006-09-06";
  setwd(base);
  data_dir = "acc_data_fps2_matched"
  filesnames = paste(data_dir, list.files(data_dir), sep="\\")
  rawData = read.files(filenames)
  data = rawData[,5:20]
  label = rawData[, 21]
  featureCnt = 16;
  docCnt = as.integer(length(label) / framesInDoc)
}

if(dataset == "UBICOMP"){
  base = "D:\\lessons\\motion recognition\\dataset\\dataset_huynh_ubicomp08\\"
  setwd(base);
  filesnames = c("data\\day5-data.txt");
  rawData = read.files(filesnames)
  data = rawData[,1:12]
  featureCnt = 12; 
  #read label
  label = read.ubilabel("activities.txt", "label\\day5-activities.txt")
  docCnt = as.integer(length(label) / framesInDoc)
  label = label[1:(framesInDoc*docCnt)]
}


#=================== read file & generate feature ==============================

binned = matrix(0, length(data[,1]), featureCnt)

#-------------START: feature generation function region-------------------


bin.equalfreq_num = function(num, threshold){ max(which(threshold < num))}
bin.equalfreq_array = function(vec, threshold){ unlist(lapply(vec, bin.equalfreq_num, threshold=threshold));}

#equal-frequency binning
bin.equalfreq <- function(x,n){
  nx <- length(x)
  nrepl <- floor(nx/n)
  nplus <- sample(1:n,nx - nrepl*n)
  nrep <- rep(nrepl,n)
  nrep[nplus] <- nrepl+1
  x[order(x)] <- rep(seq.int(n),nrep)
  x
}
#-------------START: feature generation function region-------------------

if(dataset == "PLCouple1"){
  quantize_threshold = read.table("acc_data_fps2\\quantization.txt")
}

if(dataset == "UBICOMP"){
  quantize_threshold = read.table("quantization.txt")
}

for(i in 1:featureCnt){
  #binned[, i] = bin.equalfreq(data[,i],20)
  binned[, i] = bin.equalfreq_array(data[,i], quantize_threshold[,i])
}

if(dataset == "PLCouple1"){
  features = c('hip1_x', 'hip1_y', 'hip1_z', 'hip2_x', 'hip2_y', 'hip2_z', 'wrist_x','wrist_y','wrist_z',
               'thigh_x','thigh_y','thigh_z','hip1_var', 'hip2_var', 'wrist_var', 'thigh_var');
  
}

if(dataset == "UBICOMP"){
  features = c("pocket_x", "pocket_y", "pocket_z", "pocket_var_x", "pocket_var_y", "pocket_var_z",
               "wrist_x", "wrist_y", "wrist_z", "wrist_var_x", "wrist_var_y", "wrist_var_z");
}



#===================== generate doc =============================
docLabel = 1:docCnt
#file.remove(paste("docs\\", list.files("docs"), sep=''))
for(docIndex in 1:docCnt){
 # writeDoc(docIndex, "docs");
  docLabel[docIndex] = voteMajor(docIndex)
}


if(dataset == "PLCouple1"){
  for(i in 13:16){data[,i] = vector.removenoise(data[, i], 0.02);}
}
if(dataset == "UBICOMP"){
  for(i in c(4:6,10:12)){data[,i] = vector.removenoise(data[, i], 0.02);}
}


normalize_params = dataframe.get_normalize_param(data);
data = dataframe.normalize(data);

doc_data_m = matrix(0,nrow = docCnt, ncol = length(data[1,]))
for(row in 1:docCnt){
  for(col in 1:length(data[1,])){
    frameRows = ((row-1)*framesInDoc + 1) : (row * framesInDoc)
    doc_data_m[row, col] = mean(data[frameRows, col])
  }
}
doc_data = data.frame(doc_data_m)

#================= visualize topic distribution =============================

colors = c('gray','orange', 'red', 'blue',  'green',  'brown', 'cornflowerblue','pink', 'green4', 'lightcoral', 'mediumslateblue', 'navy','navajowhite', 'saddlebrown', 'gray20', 'darkgoldenrod3', 'dodgerblue', 'gold4', 'deeppink4')

# plot the distribution of topics
plot(pred[,1], type="o", col=colors[1],ylim=c(0, 1.5), 
     xlab="activities", ylab="topic probability")
for(i in 2:K){
  lines(pred[,i], type="o", pch=22, lty=2, col=colors[i])
}

docLabelSet = names(table(docLabel))
for(i in 1:docCnt){
  points(x=i, y=1.2, col=colors[which(docLabelSet==docLabel[i])], pch=16)
}

if(dataset == "PLCouple1"){
  labelConfig = load_labelIDConfig_plc("../../LABELID.config");
  for(i in 2:length(docLabelSet)){
    text(50 + i%%5 * 300, 1.2+(i / 5)*0.07, labelConfig[as.integer(docLabelSet[i])], adj = c(0,0), col=colors[i]);
  } 
}
if(dataset == "UBICOMP"){
  labelConfig = load_labelIDConfig_ubi("activities.txt");
  for(i in 2:length(docLabelSet)){
    text(50 + i%%5 * 300, 1.2+(i / 5)*0.07, labelConfig[as.integer(docLabelSet[i])+1], adj = c(0,0), col=colors[i]);
  }
}



# topic model


classifiers = c()
knownSegIndexes = c()


library(topicmodels)
library(tm)
ovid <- Corpus(DirSource(paste(base, "\\docs", sep="")),readerControl = list(language = "lat"))

is_known_doc = is_known_doclist(doc_data, classifiers);
known_docs_indexes = which(is_known_doc == T)
all_doc_indexes = 1:docCnt;
unknown_doc_indexes = all_doc_indexes[-known_docs_indexes];
  
  
ovid = 
dtm <- DocumentTermMatrix(ovid,control=list(wordLengths=c(1,Inf)))
K=10
train.lda <- LDA(dtm,K)   
get_terms(train.lda, 10)

post <- posterior(train.lda, newdata = dtm)
pred = round(post$topics, digits = 2)



#============================= segmentation ========================================

segmentation = mergeNeighbourActivity(pred[1:(length(docLabel) - 5),])
visualSegmentation(segmentation)


#===================== find the longest segmentation & pos/neg samples===============================
segLength = segmentation[2,] - segmentation[1, ]
segmentation = rbind(segmentation, segLength)


#=============================prepare training samples ==============================================




longSegIndex = get_long_segmentation(segmentation, knownSegIndexes)
while(is_known_segment(segmentation, longSegIndex, classifiers, doc_data) == T){
  knownSegIndexes = c(knownSegIndexes, longSegIndex)
  longSegIndex = get_long_segmentation(segmentation, knownSegIndexes)
}
knownSegIndexes = c(knownSegIndexes, longSegIndex)

posTrainIndex = samplePosExamples(longSegIndex, segmentation)
negTrainIndex = sampleNegExamples(longSegIndex, segmentation)
viz_train_sample(posTrainIndex, negTrainIndex, segmentation);


train_index = union(posTrainIndex, negTrainIndex)
index = 1:length(docLabel)
test_index <- index[-train_index]
train_x = doc_data[train_index, ]
train_y = c(rep(1, length(posTrainIndex)), rep(-1, length(negTrainIndex)));
test_x = doc_data[test_index, ]
train_data = data.frame(train_x, train_y);


colnames(train_data) = c(features, 'target')
colnames(test_x) = features;

# ===================================== co-training ===================================================

train_data1 = train_data;
train_data2 = train_data;
for(i in 1:4){
  exchangeNum = as.integer(length(test_index) * 0.2);
  model1 = decision_tree.train(train_data1);
  pred_score1 = decision_tree.predict(model1, test_x);
  confident_index1 = decision_tree.find_confident_example(pred_score1, exchangeNum, test_index)
  confident_x1 = doc_data[test_index[confident_index1], ]
  confident_y1 = ifelse(pred_score1[confident_index1] > 0, 1, -1);
  confident_data1 = data.frame(confident_x1, confident_y1)  
  colnames(confident_data1) = c(features, 'target')
  
  model2 = svm.train(train_data2);
  pred_score2 = svm.predict(model2, test_x)
  confident_index2 = svm.find_confident_example(pred_score2, exchangeNum, test_index)
  confident_x2 = doc_data[test_index[confident_index2], ]
  confident_y2 = ifelse(pred_score2[confident_index2] > 0, 1, -1);
  confident_data2 = data.frame(confident_x2, confident_y2)  
  colnames(confident_data2) = c(features, 'target')
  
  train_data1 = rbind(train_data1, confident_data2)
  train_data2 = rbind(train_data2, confident_data1)
}

viz_pred_result(pred_score2, test_index)
classifiers = c(classifiers, model2)


#======================== generate classfier and filter the knowing docs =============================================
is_known_doc = is_known_doclist(doc_data, classifiers);


