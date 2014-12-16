#feature selection
srcdir = "C:\\Users\\zetan\\workspace\\MotionDL\\code\\r\\discovery\\"
source(paste(srcdir, "util.R", sep=''))
source(paste(srcdir, "feature_factory.R",sep=""))
library("FSelector")

#configurable parameters
dataset = "UBICOMP";

framesInDoc = 2 * 30; #30sec PLCouple1 + Ubicomp

if(dataset == "UBICOMP"){
  base = "D:\\lessons\\motion recognition\\dataset\\dataset_huynh_ubicomp08\\"
  setwd(base);
  datafiles = c("data\\day1-data.txt","data\\day2-data.txt", "data\\day4-data.txt");
  labelfiles = c("label\\day1-activities.txt", "label\\day2-activities.txt", "label\\day2-activities.txt");
  merged_data = matrix();
  merged_label= c();
  for(i in 1:length(datafiles)){
    rawData = read.files(datafiles[i])
    data = rawData[,1:12]
    label = read.ubilabel("activities.txt", labelfiles[i])
    example_cnt = as.integer(min(length(label), length(data[,1])) / framesInDoc)
    data = data[1:(example_cnt*framesInDoc), ];
    label = label[1:(example_cnt*framesInDoc)]
    if(i==1){
      merged_data = data;
      merged_label = label
    }else{
      merged_data = rbind(merged_data, data)
      merged_label = c(merged_label, label)
    }
   # merged_data = ifelse(i == 1, data, rbind(merged_data, data))
   # merged_label = ifelse(i == 1, label, c(merged_label, label))
  }
  
  doc_labels = rep(0, length(merged_label) / framesInDoc)
  for(i in 1:length(doc_labels)){
    start = (i-1) * framesInDoc + 1
    end = i * framesInDoc
    doc_labels[i] = voteMajor(merged_label[start:end])
  }
  doc_names = names(table(doc_labels))
  doc_freq = as.vector(table(doc_labels))
  doc_freq_sample = ifelse(doc_freq>200, 200, doc_freq)
  sampled_doc_index = c();
  for(i in 2:length(doc_freq_sample)){
    l = as.integer(doc_names[i]);
    freq = doc_freq_sample[i];
    sampled = sample(which(doc_labels == l), freq)
    sampled_doc_index = c(sampled_doc_index, sampled)
  }
  sampled_cnt = length(sampled_doc_index)
  feature_cnt = 43
  frame = matrix(0, nrow=sampled_cnt, ncol = feature_cnt*2+1);
  for(i in 1:sampled_cnt){
    start = (sampled_doc_index[i]-1) * framesInDoc + 1
    end = sampled_doc_index[i] * framesInDoc
    pocket = merged_data[start:end, 1:3]
    wrist = merged_data[start:end, 7:9]
    frame[i, 1:feature_cnt] = get_feature(pocket)
    frame[i, (feature_cnt+1):(2*feature_cnt)] = get_feature(wrist)
    frame[i, (2*feature_cnt+1)] = doc_labels[sampled_doc_index[i]]
  }

  write.table(frame, file=paste(base, "feature_select_data.txt", sep=''))
  frame = data.frame(frame)
}

weights <- information.gain(X87~., frame)
binding = t(rbind(paste("V",c(1:86),sep=''), weights$attr_importance))
result = binding[order(binding[,2], decreasing=T),]
print(result)
write.table(result, file=paste(base, "feature_weight_list.txt", sep=''))
pairs(~X2+X1+X5+X13+X4+X33+X3+X41+X43+X37+X6+X23+X36+X8+X42+X39,data=frame, main="Ubicomp08 Pocket Feature Scatterplot Matrix")
pairs(~X44+X48+X56+X76+X47+X84+X45+X46,data=frame, main="Ubicomp08 Wrist Feature Scatterplot Matrix")



get_train_data = function(raw_data, raw_label){
  example_cnt = as.integer(min(length(raw_label), length(raw_data[,1])) / framesInDoc)
  feature_cnt = 43;
  frame = matrix(0, nrow=example_cnt, ncol = feature_cnt*2+1);
  for(i in 1:example_cnt){
     start = (i-1) * framesInDoc + 1
     end = i * framesInDoc
     pocket = raw_data[start:end, 1:3]
     wrist = raw_data[start:end, 7:9]
     frame[i, 1:feature_cnt] = get_feature(pocket)
     frame[i, (feature_cnt+1):(2*feature_cnt)] = get_feature(wrist)
     frame[i, (2*feature_cnt+1)] = voteMajor(raw_label[start:end])
  }
  return(frame)
}