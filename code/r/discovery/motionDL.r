srcdir = "C:\\Users\\zetan\\workspace\\MotionDL\\code\\r\\discovery\\"
source(paste(srcdir, "util.R", sep=''))
source(paste(srcdir, "segmentation.R", sep=''))
source(paste(srcdir, "examples_from_tm.r", sep=''))
source(paste(srcdir, "self-training_co-training.R", sep=''))
source(paste(srcdir, "feature_factory.R", sep=''))

#configurable parameters
dataset = "UBICOMP";
extend = "fft_energy_entropy";
iteration_mode = TRUE;
FILTER_KNOWN_CLASSIFIER = FALSE;

framesInDoc = 2 * 30; #30sec PLCouple1 + Ubicomp
#framesInDoc = 2 * 10; #Opportunity
#framesInDoc = 20; #PAMAP

if(dataset == "PLCouple1"){
  base="D:\\lessons\\motion recognition\\dataset\\PLCouple1\\sensor\\2006-08-23";
  setwd(base);
  data_dir = "acc_data_fps2_matched"
  filenames = paste(data_dir, list.files(data_dir), sep="\\")
  rawData = read.files(filenames)
  data = rawData[,5:22]
  label = rawData[, 23]
  featureCnt = 18;
  docCnt = as.integer(length(label) / framesInDoc)
}

if(dataset == "UBICOMP"){
  base = "D:\\lessons\\motion recognition\\dataset\\dataset_huynh_ubicomp08\\"
  setwd(base);
  filesnames = c("data\\day1-data.txt");
  rawData = read.files(filesnames)
  data = rawData[,1:12]
  featureCnt = 12; 
  #read label
  label = read.ubilabel("activities.txt", "label\\day1-activities.txt")
  docCnt = as.integer(length(label) / framesInDoc)
  label = label[1:(framesInDoc*docCnt)]
}


if(dataset == "OPPOTUNITY"){
  base = "D:\\lessons\\motion recognition\\dataset\\OPPORTUNITY Activity Recognition Data Set\\OpportunityUCIDataset\\dataset";
  setwd(base);
  filenames = c("S1-ADL123.dat");
  data = read_downsample.oppotunity(filenames[1]);
  featureCnt = 12;
  label = read_label_downsample.oppotunity(filenames[1]);
  docCnt = as.integer(length(label) / framesInDoc);
  label = label[1:(framesInDoc*docCnt)];
}

if(dataset == "PAMAP"){
  base = "D:\\lessons\\motion recognition\\dataset\\PAMAP2_Dataset\\Protocol";
  setwd(base);
  filenames = c("subject102.dat");
  data = read.downsample.pamap(filenames[1], 2);
  featureCnt = 18;
  label = read.downsample.label.pamap(filenames[1], 2);
  docCnt = as.integer(length(label) / framesInDoc);
  label = label[1:(framesInDoc*docCnt)];
}

#=================== read file & generate feature ==============================

binned = matrix(0, length(data[,1]), featureCnt)

#-------------START: feature generation function region-------------------


bin.equalfreq_num = function(num, threshold){ ifelse(num<threshold[1], 0, max(which(threshold <= num)))}
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
  quantize_threshold = read.table("../quantize_data/quantization.txt")
}

if(dataset == "UBICOMP"){
  quantize_threshold = read.table("quantization.txt")
}

if(dataset == "OPPOTUNITY"){
  quantize_threshold = read.table("quantization.txt");
  fft_bin_threshold = read.table("fft_quantization.txt");
}

if(dataset == "PAMAP"){
  quantize_threshold = read.table("fps2_quantization.txt");
}

for(i in 1:featureCnt){
  #binned[, i] = bin.equalfreq(data[,i],20)
  binned[, i] = bin.equalfreq_array(data[,i], quantize_threshold[,i])
}

if(dataset == "PLCouple1"){
  features = c('hip_x', 'hip_y', 'hip_z', 'wrist_x','wrist_y','wrist_z','thigh_x','thigh_y','thigh_z', 
               'hip_var_x', 'hip_var_y', 'hip_var_z', 'wrist_var_x','wrist_var_y','wrist_var_z',
               'thigh_var_x','thigh_var_y','thigh_var_z');
  
}

if(dataset == "UBICOMP"){
  features = c("pocket_x", "pocket_y", "pocket_z", "pocket_var_x", "pocket_var_y", "pocket_var_z",
               "wrist_x", "wrist_y", "wrist_z", "wrist_var_x", "wrist_var_y", "wrist_var_z");
}

if(dataset == "OPPOTUNITY"){
  features = c('hip_x', 'hip_y', 'hip_z', 'wrist_x','wrist_y','wrist_z',
              'hip_var_x', 'hip_var_y', 'hip_var_z', 'wrist_var_x','wrist_var_y','wrist_var_z');
  
}

if(dataset == "PAMAP"){
  features = c('hip_x', 'hip_y', 'hip_z', 'hand_x', 'hand_y', 'hand_z', 'ankle_x', 'ankle_y', 'ankle_z',
               'hip_var_x', 'hip_var_y', 'hip_var_z', 'hand_var_x', 'hand_var_y', 'hand_var_z',
               'ankle_var_x', 'ankle_var_y', 'ankle_var_z');
  
}

doc_features = features;
if(dataset == "PLCouple1"){
  for(i in 10:18){data[,i] = vector.removenoise(data[, i], 0.02);}
  raw_sensor_dim = 1:12;
}
if(dataset == "UBICOMP"){
  for(i in c(4:6,10:12)){data[,i] = vector.removenoise(data[, i], 0.02);}
  raw_sensor_dim = c(1, 2, 3,7, 8, 9);
}

if(dataset == "OPPOTUNITY"){
  for(i in 1:12){data[,i] = vector.removenoise(data[, i], 0.02);}
  raw_sensor_dim = 1:6;
}
if(dataset == "PAMAP"){
  for(i in 10:18){data[,i] = vector.removenoise(data[, i], 0.02);}
  raw_sensor_dim = 1:9;
}

if(extend == "fft_coe"){
  fftnum = 5;
  extendedFeatureCnt = featureCnt + length(raw_sensor_dim) * fftnum * 2;
  features = c(features, paste("fft", c(1:(length(raw_sensor_dim) * fftnum * 2)), sep='_'))
}
if(extend == "fft_energy_entropy"){
  N = length(raw_sensor_dim);
  extendedFeatureCnt = featureCnt + N * 2;
  features = c(features, paste("energy",c(1:N), sep='_'), paste("entropy", c(1:N), sep='_'));
}

doc_data_m = matrix(0,nrow = docCnt, ncol = extendedFeatureCnt)
for(row in 1:docCnt){
  frameRows = ((row-1)*framesInDoc + 1) : (row * framesInDoc)
  for(col in 1:featureCnt){    
    doc_data_m[row, col] = mean(data[frameRows, col])
  }
  if(extend == "fft_coe"){
    doc_data_m[row, (featureCnt+1):extendedFeatureCnt] = data.fft(data.frame(data[frameRows, raw_sensor_dim]), fftnum);
  }
  if(extend == "fft_energy_entropy"){
    doc_data_m[row, (featureCnt+1):extendedFeatureCnt] = data.fft_energy_entropy(data.frame(data[frameRows, raw_sensor_dim]));
  }
}
doc_data = data.frame(doc_data_m)

normalize_params = dataframe.get_normalize_param(doc_data);
doc_data = dataframe.normalize(doc_data);


#================= mean and var in other length =================


#======================== fft word in doc =======================
fft_word_num = 10;
fft_params = matrix(0, nrow = docCnt, ncol = fft_word_num * length(raw_sensor_dim));
for(row in 1:docCnt){
  frameRows = ((row-1)*framesInDoc+1):(row*framesInDoc);
  fft_params[row, ] = data.fft(data.frame(data[frameRows, raw_sensor_dim]), fft_word_num / 2);    
}
binned_fft = matrix(0, nrow = docCnt, ncol = fft_word_num * length(raw_sensor_dim));
for(i in 1:length(fft_params[1,])){
  binned_fft[, i] = bin.equalfreq_array(fft_params[,i], fft_bin_threshold[,i])
}
get_fft_feature_names = function(colname, fft_word_num){
  return(paste(colname, 'fft', c(1:fft_word_num), sep='_'));
}
fft_doc_feature_names = unlist(lapply(c('hip_x', 'hip_y', 'hip_z', 'rwr_x', 'rwr_y', 'rwr_z'), get_fft_feature_names, fft_word_num= fft_word_num));


#===================== generate doc =============================
doc_labels = 1:docCnt
file.remove(paste("docs\\", list.files("docs"), sep=''))
for(docIndex in 1:docCnt){
  #writeDoc(docIndex, "docs");
  doc_labels[docIndex] = voteMajor(label[((docIndex-1)*framesInDoc+1):(docIndex*framesInDoc)])
  frame_words = data_toString(doc_features, binned[((docIndex-1)*framesInDoc+1):(docIndex*framesInDoc),] );
  #fft_words = data_toString(fft_doc_feature_names,matrix(binned_fft[docIndex,],nrow=1))
  #write_doc(paste(frame_words, fft_words,fft_words,fft_words,fft_words, sep=" "), 'docs', paste(docIndex, '_', doc_labels[docIndex], '.txt', sep=""));
  write_doc(frame_words, 'docs', paste(docIndex, '_', doc_labels[docIndex], '.txt', sep=""));
}
doc_label_set = names(table(doc_labels))

# topic model

#================= visualize topic distribution =============================

library(topicmodels)
library(tm)

ovid_all <- Corpus(DirSource(paste(base, "\\docs", sep="")),readerControl = list(language = "lat"))

#init all the values
classifiers = c()
knownSegIndexes = c()
all_doc_indexes = 1:docCnt;
iteration_doc_indexes = all_doc_indexes;
iteration_doc_labels = doc_labels;
iteration_doc_data = doc_data;
ovid = ovid_all
#K=10 # UBICOMP
K = 6
colors = c('gray','orange', 'red', 'blue',  'green',  'brown', 'cornflowerblue','pink', 'green4', 
           'lightcoral', 'mediumslateblue', 'navy','navajowhite', 'saddlebrown', 'gray20', 
           'darkgoldenrod3', 'dodgerblue', 'gold4', 'deeppink4', 'deeppink1', 'goldenrod2', 'gray26', 
           'greenyellow', 'lightgoldenrod4', 'mediumvioletred', 'salmon4')

get_topic_distribution = function(ovid, K){
  dtm <- DocumentTermMatrix(ovid,control=list(wordLengths=c(1,Inf)))
  train.lda <- LDA(dtm,K)   
  get_terms(train.lda, 10)
  post <- posterior(train.lda, newdata = dtm)
  pred = round(post$topics, digits = 2)
  return(pred);
}


if(FILTER_KNOWN_CLASSIFIER == TRUE){  #load classifiers and filter out known activities
  load("iterative_desk_lying_whiteboard_desk.RData");
  is_known_doc = is_known_doclist(iteration_doc_data, classifiers);
  known_docs_indexes = which(is_known_doc == T)
  iteration_doc_indexes = iteration_doc_indexes[-known_docs_indexes];
  ovid = ovid_all[iteration_doc_indexes];
  iteration_doc_labels = doc_labels[iteration_doc_indexes];
  iteration_doc_data = doc_data[iteration_doc_indexes, ]
}


# first time segmentation by topic model
pred = get_topic_distribution(ovid, K);
# plot the distribution of topics
viz_ground_truth(dataset, iteration_doc_labels);
viz_topic_distribution(pred, K);
#============================= segmentation ========================================
segmentation = mergeNeighbourActivity(pred[,])
#segmentation = mergeNeighbourSeg(segmentation)
visualSegmentation(segmentation)
segLength = segmentation[2,] - segmentation[1, ]
segmentation = rbind(segmentation, segLength)

# start iteration - build classifier

while(TRUE){
  # if needed, start another segmentation iteration
  if(iteration_mode == TRUE){
    is_known_doc = is_known_doclist(iteration_doc_data, classifiers);
    known_docs_indexes = which(is_known_doc == T)
    if(length(known_docs_indexes) > 0.5 * length(iteration_doc_labels)){  # another segmentation iteration
      iteration_doc_indexes = iteration_doc_indexes[-known_docs_indexes];
      ovid = ovid_all[iteration_doc_indexes];
      iteration_doc_labels = doc_labels[iteration_doc_indexes];
      iteration_doc_data = doc_data[iteration_doc_indexes, ]
      knownSegIndexes = c();
      
      pred = get_topic_distribution(ovid, K);
      # plot the distribution of topics
      viz_ground_truth(dataset, iteration_doc_labels);
      viz_topic_distribution(pred, K);
      #============================= segmentation ========================================
      segmentation = mergeNeighbourActivity(pred[1:(length(iteration_doc_indexes) - 5),])
      visualSegmentation(segmentation)
      segLength = segmentation[2,] - segmentation[1, ]
      segmentation = rbind(segmentation, segLength)
    } 
  }
  
  # build another classifier
  #=============================prepare training samples ==============================================
  
  longSegIndex = get_long_segmentation(segmentation, knownSegIndexes)
  while(is_known_segment(segmentation, longSegIndex, classifiers, iteration_doc_data) == T){
    knownSegIndexes = c(knownSegIndexes, longSegIndex)
    longSegIndex = get_long_segmentation(segmentation, knownSegIndexes)
  }
  knownSegIndexes = c(knownSegIndexes, longSegIndex)
  
  posTrainIndex = samplePosExamples(longSegIndex, segmentation)
  topicDiffThreshold = 1.2
  negTrainIndex = sampleNegExamples(longSegIndex, segmentation, topicDiffThreshold)
  while(length(negTrainIndex) < max(100,length(posTrainIndex))){
    topicDiffThreshold = topicDiffThreshold - 0.1;
    negTrainIndex = sampleNegExamples(longSegIndex, segmentation, topicDiffThreshold)
  }
  viz_train_sample(posTrainIndex, negTrainIndex, segmentation, iteration_doc_labels);
  
  
  train_index = union(posTrainIndex, negTrainIndex)
  index = 1:length(iteration_doc_labels)
  test_index <- index[-train_index]
  train_x = iteration_doc_data[train_index, ]
  train_y = c(rep(1, length(posTrainIndex)), rep(-1, length(negTrainIndex)));
  test_x = iteration_doc_data[test_index, ]
  
  if(iteration_mode == TRUE){ #add known doc as negtive examples
    neg_known_indexes = sample(all_doc_indexes[-iteration_doc_indexes], 0.1 * length(iteration_doc_indexes))
    neg_known_data = doc_data[neg_known_indexes, ]
    train_x = rbind(train_x, neg_known_data)
    train_y = c(train_y, rep(-1, length(neg_known_indexes)))
  }
  
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
  
  viz_pred_result(pred_score2, test_index, iteration_doc_labels)
  classifiers = c(classifiers, model2)
}






