
srcdir = "C:\\Users\\zetan\\workspace\\MotionDL\\code\\r\\discovery\\"
source(paste(srcdir, "util.R", sep=''))
source(paste(srcdir, "self-training_co-training.R", sep=''))

dataset = "UBICOMP"
if(dataset == "PLCouple1"){
  base="D:\\lessons\\motion recognition\\dataset\\PLCouple1\\sensor\\";
  setwd(base);
}
if(dataset== "UBICOMP"){
  base = "D:\\lessons\\motion recognition\\dataset\\dataset_huynh_ubicomp08\\"
  setwd(base);
}

#========================== LOAD CLASSIFIER =============================
load("desk.RData")  #classifiers

#load("2006-08-23\\sleep_model.RData")  #classifiers
#=========================== LOAD TEST DATA =======================================
framesInDoc = 2 * 30

if(dataset == "PLCouple1"){
  testdata_date = "2006-09-14"
  testdata_dir = "2006-09-14\\acc_data_fps2_matched"
  
  rawData = read.files(paste(testdata_dir, list.files(testdata_dir), sep="\\"))
  
  data = rawData[,5:20]
  label = rawData[, 21]
  featureCnt = 16;
  features = c('hip1_x', 'hip1_y', 'hip1_z', 'hip2_x', 'hip2_y', 'hip2_z', 'wrist_x','wrist_y','wrist_z',
               'thigh_x','thigh_y','thigh_z','hip1_var', 'hip2_var', 'wrist_var', 'thigh_var');
}

if(dataset == "UBICOMP"){
  filesnames = c("data\\day4-data.txt");
  rawData = read.files(filesnames)
  data = rawData[,1:12]
  featureCnt = 12; 
  #read label
  label = read.ubilabel("activities.txt", "label\\day4-activities.txt")
  
  features = c("pocket_x", "pocket_y", "pocket_z", "pocket_var_x", "pocket_var_y", "pocket_var_z",
               "wrist_x", "wrist_y", "wrist_z", "wrist_var_x", "wrist_var_y", "wrist_var_z");
 # docCnt = as.integer(length(label) / framesInDoc)
 # label = label[1:(framesInDoc*docCnt)]
  load("day1_desk_whiteboard_desk_lying_EE.RData"); # normalize_params
 
  for(i in c(4:6,10:12)){data[,i] = vector.removenoise(data[, i], 0.02);}
}
docCnt = as.integer(length(label) / framesInDoc)

extend = "fft_energy_entropy";
if(dataset == "UBICOMP"){
  raw_sensor_dim = c(1, 2, 3,7, 8, 9);
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
}

doc_data_m = matrix(0,nrow = docCnt, ncol = extendedFeatureCnt)
for(row in 1:docCnt){
  for(col in 1:length(data[1,])){
    frameRows = ((row-1)*framesInDoc + 1) : (row * framesInDoc)
    doc_data_m[row, col] = mean(data[frameRows, col])
  }
  if(extend == "fft_coe"){
    doc_data_m[row, (featureCnt+1):extendedFeatureCnt] = data.fft(data.frame(data[frameRows, raw_sensor_dim]), fftnum);
  }
  if(extend == "fft_energy_entropy"){
    doc_data_m[row, (featureCnt+1):extendedFeatureCnt] = data.fft_energy_entropy(data.frame(data[frameRows, raw_sensor_dim]));
  }
}


#normalized data with normalize_params
#for(i in 1:extendedFeatureCnt){
#  doc_data_m[, i] = (doc_data_m[, i] - normalize_params[1,i]) / ifelse(normalize_params[2,i] == 0, 1, normalize_params[2,i]);
#}

test_x = data.frame(doc_data_m)
for(i in 1:extendedFeatureCnt){
  test_x[, i] = (test_x[, i] - normalize_params[1,i]) / ifelse(normalize_params[2,i] == 0, 1, normalize_params[2,i]);
}

test_x = dataframe.normalize(test_x);
colnames(test_x) = features;


#===================== generate doc label =============================
docCnt = length(data[,1]) / framesInDoc
docLabel = 1:docCnt
for(docIndex in 1:docCnt){
  docLabel[docIndex] = voteMajor(docIndex)
}
docLabelSet = names(table(docLabel))

#================= visualize topic distribution =============================
colors = c('gray','orange', 'red', 'blue',  'green',  'brown', 'cornflowerblue','pink', 'green4', 'lightcoral', 'mediumslateblue', 'navy','navajowhite', 'saddlebrown', 'gray20', 'darkgoldenrod3', 'dodgerblue', 'gold4', 'deeppink4')

viz_ground_truth(dataset);

#==================== classify data with models ==================================


test_classifier_indexes = c(1, 2, 3, 4)
ground_truth_activities = c(14, 27, 14, 25)

classify_results = c();

sensitivity=rep("x", length(test_classifier_indexes));
specify=rep("x", length(test_classifier_indexes));
precision=rep("x", length(test_classifier_indexes));
accuracy=rep("x", length(test_classifier_indexes));

build_percent_str = function(numerator, denumerator){
  percent = format(round(numerator /denumerator, 2), nsmall = 2)
  return(ifelse(denumerator==0, "", paste(percent, " (", toString(numerator), "/", toString(denumerator), ")", sep='')));
}


for(i in 1:length(test_classifier_indexes)){
  classifier_index = test_classifier_indexes[i];
  activity = ground_truth_activities[i]
  ground_truth_act = ground_truth_activities[i]
  
  pred_score = svm.predict(classifiers[[classifier_index]], test_x)
  pred_class = ifelse(pred_score>0, T, F)
  
  classify_results = rbind(classify_results, pred_class);
  
  ground_pos = which(docLabel == activity)
  ground_neg = which(docLabel != activity)
  
  test_y_pos = which(pred_class == T)
  test_y_neg = which(pred_class == F)
  
  pos = length(ground_pos)
  neg = length(ground_neg)
  t_pos = length(intersect(ground_pos, test_y_pos))
  t_neg = length(intersect(ground_neg, test_y_neg))
  f_pos = length(intersect(ground_neg, test_y_pos))
  
  
  #sensitivity = t_pos / pos;
  #specify = t_neg / neg
  #precision = t_pos / (t_pos + f_pos)
  #accuracy = (t_pos + t_neg) / length(pred_class)
  sensitivity[i] = build_percent_str(t_pos, pos);
  specify[i] = build_percent_str(t_neg, neg)
  precision[i] = build_percent_str(t_pos, t_pos + f_pos);
  accuracy[i] = build_percent_str(t_pos + t_neg, length(pred_class)) 
  
  for(doc in 1:docCnt){
    points(x=doc, y=1 - 0.15*i, col=ifelse(pred_class[doc]==T, 'green', 'red'), pch=16);
  }
}

results = data.frame(sensitivity, specify, precision, accuracy)
print(results);


