#feature selection
srcdir = "C:\\Users\\zetan\\workspace\\MotionDL\\code\\r\\discovery\\"
source(paste(srcdir, "util.R", sep=''))
source(paste(srcdir, "feature_factory.R",sep=""))
library("FSelector")

#configurable parameters
dataset = "UBICOMP";

framesInDoc = 2 * 30; #30sec PLCouple1 + Ubicomp

#====================== read data and labels ======================
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
    label = read.ubilabel(labelfiles[i], "activities.txt")
    example_cnt = as.integer(min(length(label), length(data[,1])) / framesInDoc)
    data = data[1:(example_cnt*framesInDoc), ];
    label = label[1:(example_cnt*framesInDoc)]
    merged_data = if(i==1) data else rbind(merged_data, data)
    merged_label = if(i==1) label else c(merged_label, label)
  }
  
  doc_labels = rep(0, length(merged_label) / framesInDoc)
  for(i in 1:length(doc_labels)){
    start = (i-1) * framesInDoc + 1
    end = i * framesInDoc
    doc_labels[i] = voteMajor(merged_label[start:end])
  }
  #================== sample data ==========================
  # making sure classes are balanced by sampling at most 200 instances from one class
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
  #================= computing features for these sampled data======================
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
  # ================= start feature selection ========================
  weights <- information.gain(X87~., frame) #compute information gain
  binding = t(rbind(paste("V",c(1:86),sep=''), weights$attr_importance))
  result = binding[order(binding[,2], decreasing=T),] #order by info-gain
  print(result)
  write.table(result, file=paste(base, "feature_weight_list.txt", sep='')) #write feature weight list to file
  # visulize the feature correlation by plotting the scatter plot
  pairs(~X2+X1+X5+X13+X4+X33+X3+X41+X43+X37+X6+X23+X36+X8+X42+X39,data=frame, main="Ubicomp08 Pocket Feature Scatterplot Matrix")
  pairs(~X44+X48+X56+X76+X47+X84+X45+X46,data=frame, main="Ubicomp08 Wrist Feature Scatterplot Matrix")
  
}

if(dataset == "PAMAP2"){
  base = "D:\\lessons\\motion recognition\\dataset\\PAMAP2_Dataset\\Protocol\\";
  setwd(base);
  filenames = c("subject102.dat", "subject103.dat", "subject104.dat", "subject105_all.dat");
  merged_data = matrix();
  merged_label= c();
  framesInDoc = 20
  #===================== read data =============================
  for(i in 1:length(filenames)){
    data = read.downsample.pamap(filenames[i], 2);
    label = read.downsample.label.pamap(filenames[i], 2);
    docCnt = as.integer(length(label) / framesInDoc);
    label = label[1:(framesInDoc*docCnt)]
    data = data[1:(framesInDoc*docCnt), 1:9]
  }
  merged_data = if(i==1) data else rbind(merged_data, data)
  merged_label = if(i==1) label else c(merged_label, label)
  
  write.table(merged_data, file=paste(base, "merged_data_subject102103014105.txt", sep=''))
  write.table(merged_label, file=paste(base, "merged_label_subject1021030140105.txt", sep=''))
  doc_labels = rep(0, length(merged_label) / framesInDoc)
  for(i in 1:length(doc_labels)){
    start = (i-1) * framesInDoc + 1
    end = i * framesInDoc
    doc_labels[i] = voteMajor(merged_label[start:end])
  }
  # ================ remove data without a label=========================
  # data in PAMAP2 dataset is quite balanced, no need to sample it again
  sampled_doc_index = which(doc_labels != 0)
  sampled_doc_cnt = length(sampled_doc_index)
  feature_cnt = 43
  #================= computing features ======================
  frame = matrix(0, ncol=feature_cnt*3+1, nrow=sampled_doc_cnt)
  for(i in 1:sampled_doc_cnt){
    start = (sampled_doc_index[i]-1) * framesInDoc + 1
    end = sampled_doc_index[i] * framesInDoc
    hip = merged_data[start:end, 1:3]
    hand = merged_data[start:end, 4:6]
    ankle = merged_data[start:end, 7:9]
    frame[i, 1:feature_cnt] = get_feature(hip)
    frame[i, (feature_cnt+1):(2*feature_cnt)] = get_feature(hand)
    frame[i, (2*feature_cnt+1):(3*feature_cnt)] = get_feature(ankle)
    frame[i, (3*feature_cnt+1)] = doc_labels[sampled_doc_index[i]]
  }
  write.table(frame, file=paste(base, "feature_select_data.txt", sep=''))
  frame = data.frame(frame)
  # ================= start feature selection ========================
  weights <- information.gain(X130~., frame)  #compute information gain
  binding = t(rbind(paste("V",c(1:129),sep=''), weights$attr_importance))
  result = binding[order(binding[,2], decreasing=T),]  #order by info-gain
  print(result)
  write.table(result, file=paste(base, "feature_weight_list.txt", sep=''))  #write feature weight list to file
  pairs(~X46+X86+X45+X51+X49+X66+X80+X50+X50+X78+X83+X47,data=frame, main="PAMAP2 Hand Feature Scatterplot Matrix")
  pairs(~X87+X127+X91+X99+X119+X88+X90+X93+X123+X121+X92+X109,data=frame, main="PAMAP2 Ankle Feature Scatterplot Matrix")
  
}


