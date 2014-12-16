#----------START: read file function region-------------------
read.rawfiles = function(x){read.table(x, sep=' ')}

removeTailLabel = function(labels){
  docCnt = as.integer(length(labels) / framesInDoc);
  x = labels[1:(docCnt * framesInDoc)];
  return(x);
}
removeTail = function(x){
  docCnt = as.integer(length(x[,1]) / framesInDoc)
  x = x[1:(docCnt*framesInDoc), ]
  return(x)
}

read.files = function(filenames){
  rawData = lapply(filenames, read.rawfiles)
  rawData = lapply(rawData, removeTail)
  merged = rawData[[1]]
  if(length(rawData) >=2){
    for(i in 2:length(rawData)) merged = rbind(merged, rawData[[i]])
  }
  
  return(merged)
}

read.data.oppotunity = function(filename){
  data = read.table(filename, sep=" ");
  hip_x = interporlate_nan(data$V5);
  hip_y = interporlate_nan(data$V6);
  hip_z = interporlate_nan(data$V7);
  
  rwr_x = interporlate_nan(data$V23);
  rwr_y = interporlate_nan(data$V24);
  rwr_z = interporlate_nan(data$V25);
  accel_data = data.frame(hip_x, hip_y, hip_z, rwr_x, rwr_y, rwr_z);
  
  return(accel_data);
}

interporlate_nan = function(vec){
  na_index = c(which(is.na(vec)), length(vec));
  is_start = FALSE;
  ptr = 0;
  start = -1;
  for(i in na_index){
    if(i != ptr + 1){
      end = ptr;
      if(start > 0){
        vec[start:end] = (vec[start - 1] + vec[end+1]) /2;
      }
      start = i;
    }
    ptr = i;
  }
  return(vec);
  
}

downsample = function(data, raw_fps, target_fps){
  compress_ratio = raw_fps / target_fps;
  col_cnt = length(data[1,])
  
  target_frame_cnt = as.integer(length(data[,1]) / compress_ratio);
  target_frame_data = matrix(0, nrow = target_frame_cnt, ncol = col_cnt * 2);
  
  for(i in 1:target_frame_cnt){
    for(col in 1:col_cnt){
      start = (i - 1) * compress_ratio + 1;
      end = i * compress_ratio;
      target_frame_data[i, col] = mean(data[start:end, col]);
      target_frame_data[i, col + col_cnt] = sd(data[start:end, col]);    
    }
  }
  return(target_frame_data);  
}

read_downsample.oppotunity = function(filename){
  raw_accel_data = read.data.oppotunity(filename);
  downsampled_data = downsample(raw_accel_data, 30, 2);
  return(downsampled_data);
}

read_label_downsample.oppotunity = function(filename){
  data = read.table(filename, sep=" ");
  hllabels = data$V245;
  raw_fps = 30;
  target_fps = 2;
  compress_ratio = raw_fps / target_fps;
  
  target_frame_cnt = as.integer(length(data[,1]) / compress_ratio);
  
  downsampled_labels = rep(0, target_frame_cnt);
  for(i in 1:target_frame_cnt){
    downsampled_labels[i] = voteMajor(hllabels[((i-1)*compress_ratio+1):(i*compress_ratio)]);
  }
  return(downsampled_labels);
}

read.data.pamap = function(filename){
  data = read.table(filename, sep=' ');
  hand_x = interporlate_nan(data$V5);
  hand_y = interporlate_nan(data$V6);
  hand_z = interporlate_nan(data$V7);
  chest_x = interporlate_nan(data$V22);
  chest_y = interporlate_nan(data$V23);
  chest_z = interporlate_nan(data$V24);
  ankle_x = interporlate_nan(data$V39);
  ankle_y = interporlate_nan(data$V40);
  ankle_z = interporlate_nan(data$V41);
  
  accel_data = data.frame(hand_x, hand_y, hand_z, chest_x, chest_y, chest_z,
                          ankle_x, ankle_y, ankle_z);
  return(accel_data);
}

read.downsample.pamap = function(file, target_fps){
  data = read.data.pamap(file);
  fps1 = downsample(data, 100, target_fps);
  return(fps1);
}

read.downsample.label.pamap = function(file, target_fps){
  data = read.table(file, sep=' ')
  hllabels = data$V2;
  raw_fps = 100;
#  target_fps = 1;
  compress_ratio = raw_fps / target_fps;
  
  target_frame_cnt = as.integer(length(data[,1]) / compress_ratio);
  
  downsampled_labels = rep(0, target_frame_cnt);
  for(i in 1:target_frame_cnt){
    downsampled_labels[i] = voteMajor(hllabels[((i-1)*compress_ratio+1):(i*compress_ratio)]);
  }
  return(downsampled_labels);
}

#-----------END: read file function region-------------------

#------------START: READ UBICOMP LABEL ------------------------
read.ubilabel = function(configfile, filename){
  ubiLabelStr = readLines(configfile)
  ubiLabelInt = 0:(length(ubiLabelStr) -1) 
  ubiLabelCombined = ubiLabelInt
  
  ubiLabels = rbind(ubiLabelStr, ubiLabelInt, ubiLabelCombined)
  walkingIndex = which(ubiLabels[1,] %in% c("walking","walking while carrying something", "walking freely"))
  eatingIndex = which(ubiLabels[1,] %in% c("having breakfast", "having lunch", "having dinner"))
  
  ubiLabels[3, walkingIndex] = ubiLabels[2,which(ubiLabels[1, ] == "walking")]
  ubiLabels[3, eatingIndex] = ubiLabels[2,which(ubiLabels[1, ] == "having breakfast")]
  
  rawLabels = readLines(filename);
  rawLabels = as.integer(rawLabels)
  combinedLabels = lapply(rawLabels, find.combined_label, ubiLabels = ubiLabels)
  return(as.integer(combinedLabels))
}

read.ubilabellist = function(configfile, filenames){
  ubiLabelStr = readLines(configfile)
  ubiLabelInt = 0:(length(ubiLabelStr) -1) 
  ubiLabelCombined = ubiLabelInt
  
  ubiLabels = rbind(ubiLabelStr, ubiLabelInt, ubiLabelCombined)
  walkingIndex = which(ubiLabels[1,] %in% c("walking","walking while carrying something", "walking freely"))
  eatingIndex = which(ubiLabels[1,] %in% c("having breakfast", "having lunch", "having dinner"))
  
  ubiLabels[3, walkingIndex] = ubiLabels[2,which(ubiLabels[1, ] == "walking")]
  ubiLabels[3, eatingIndex] = ubiLabels[2,which(ubiLabels[1, ] == "having breakfast")]
  
  rawLabelMat = lapply(filenames, readLines);
  mergedLabels = removeTailLabel(rawLabelMat[[1]]);
  if(length(filenames) > 1){
    for(i in 2:length(filenames)){
      mergedLabels = c(mergedLabels, removeTailLabel(rawLabelMat[[i]]));
    }
  }
  mergedLabels = as.integer(mergedLabels)
  combinedLabels = lapply(mergedLabels, find.combined_label, ubiLabels = ubiLabels)
  return(as.integer(combinedLabels))  
}

find.combined_label = function(rawLabel,ubiLabels){ubiLabels[3,which(as.integer(ubiLabels[2, ]) == rawLabel)]}

viz_xyz = function(data){
  data_min = min(min(data[,1]), min(data[,2]), min(data[,3]));
  data_max = max(max(data[,1]), max(data[,2]), max(data[,3]));
  plot(data[,1], ylim = c(data_min, data_max), type='n');
  lines(data[,1], col = 'red');
  lines(data[,2], col = 'green');
  lines(data[,3], col = 'blue');
  return();
}

#--------------END: READ UBICOMP LABEL------------------------

load_labelIDConfig_plc = function(configfile){
  labelIDConfig = readLines(configfile)
  split_label = function(x){unlist(strsplit(x, ":"))[1]}
  labelSet = unlist(lapply(labelIDConfig, split_label))
  return(labelSet);
}

load_labelIDConfig_ubi = function(configfile){
  labelIDConfig = readLines(configfile)
  return(labelIDConfig)
}

load_labelIDConfig_pamap = function(configfile){
  labelIDConfig = readLines(configfile)
  return(labelIDConfig)
}


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




#----------START: generate doc function region-------------------

voteMajor = function(frameLabels){
 # frameLabels = label[((docIndex-1)*framesInDoc+1):(docIndex*framesInDoc)]
  docLabel = names(which.max(table(frameLabels))) 
  return(as.numeric(docLabel))
}

writeDoc = function(docIndex, dir){
  docStr = ""
  for(i in 1:framesInDoc){
    frameStr = toString(paste(features, binned[((docIndex-1)*framesInDoc+i),], sep='_'), sep=' ');
    docStr = paste(docStr, paste(frameStr, ",", sep=""), sep="")
  }
  docLabel = voteMajor(docIndex);
  write(docStr, file=paste(dir, "\\doc_", docIndex, "_", docLabel, ".txt", sep=""));
}

data_toString = function(colnames, data){
  str = "";
  for(i in 1:length(data[,1])){
    linestr = toString(paste(colnames, binned[((docIndex-1)*framesInDoc+i),], sep='_'), sep=' ');
    str = paste(str, paste(linestr, ",", sep=""), sep="")
  }
  return(str);
}

write_doc = function(strdata, dir, filename){
  write(strdata, file = paste(dir, filename, sep="\\"));
}

#----------END: generate doc function region-------------------


#------------------------START: FFT feature--------------------------

vec.fft = function(vec, fnum){
  f = fft(vec);
  params = c(Re(f[1:fnum]), Im(f[1:fnum]));
  return(params);
}

data.fft = function(dataframe, fnum){
  vecCnt = length(dataframe[1, ])
  params = c();
  for(i in 1:vecCnt){
    params = c(params, vec.fft(dataframe[, i], fnum))
  }
  return(params);
}

vec.fft_energy = function(vec){
  N = length(vec);
  f = fft(vec);
  energy = sum(Re(f) * Re(f) + Im(f) * Im(f)) / N;
  return(energy)
}

vec.fft_entropy = function(vec){
  
  N = length(vec);
  vec = vec + rnorm(N, 0, 0.0001)
  f = fft(vec);
  f = f[1:(N/2)];
  f[1] = f[1] / N;
  f[2:(N/2)] = f[2:(N/2)] / (N/2);
  psd = sqrt(Re(f) * Re(f) + Im(f) * Im(f));
  entropy = sum(psd * log(psd)) * -1;
  return(entropy);
}

data.fft_energy_entropy = function(dataframe){
  vecCnt = length(dataframe[1, ])
  param = rep(0, vecCnt*2);
  for(i in 1:vecCnt){
    param[i] = vec.fft_energy(dataframe[, i]);
    param[i+vecCnt] = vec.fft_entropy(dataframe[, i]);
  }
  return(param);
}
#------------------------END: FFT feature--------------------------


#--------------------------------------------------------------------
viz_ground_truth = function(dataset, doc_labels){
  plot(1, type="o", col=colors[1],xlim=c(1, length(doc_labels) * 1.2), ylim=c(0, 2.3), 
       xlab="motion-documents", ylab="topic probability", cex=1.2)
  for(i in 1:length(doc_labels)){
    points(x=i, y=1.4, col=colors[which(doc_label_set==doc_labels[i])], pch=16)
  }  
  labelPadding = as.integer(length(doc_labels) / 4);
  if(dataset == "PLCouple1"){
    labelConfig = load_labelIDConfig_plc("../../LABELID.config");
    for(i in 2:length(doc_label_set)){
      text(labelPadding/2 + i%%5 * labelPadding, 1.4+(i / 5)*0.07, labelConfig[as.integer(doc_label_set[i])], adj = c(0,0), col=colors[i]);
    } 
  }
  if(dataset == "UBICOMP"){
    valid_label_set = doc_label_set[doc_label_set %in% doc_labels]
    labelConfig = load_labelIDConfig_ubi("activities.txt");
    labelConfig[10] = "having a meal";
    labelConfig[15] = "sitting desk working";
    for(i in 1:length(valid_label_set)){
      text(labelPadding/8 + (i-1)%%4 * labelPadding, 1.5+(as.integer((i-1) / 4))*0.13, labelConfig[as.integer(valid_label_set[i])+1], adj = c(0,0),
           col=colors[which(doc_label_set == valid_label_set[i])], cex=1.3);
    }
  }
  if(dataset == 'PAMAP'){
    valid_label_set = as.integer(names(table(doc_labels)));
    labelConfig = load_labelIDConfig_pamap("activities.txt");
    for(i in 1:length(valid_label_set)){
      text(labelPadding/8 + (i-1)%%4 * labelPadding, 1.6+(as.integer((i-1) / 4))*0.13, labelConfig[as.integer(valid_label_set[i])+1], adj = c(0,0),
           col=colors[which(doc_label_set == valid_label_set[i])], cex=1.3);
    }
  }
}

viz_topic_distribution = function(pred, K){
  for(i in 1:K){
    #lines(pred[,i], type="o", pch=22, lty=2, col=colors[i])
    lines(pred[,i], type="o", lty=2, col=colors[i])
  }
}

smooth_classify_tag = function(pred_class){
  smoothed = pred_class;
  window_size = 5;
  threshold = 0.6;
  for(i in 1: (length(pred_class) - window_size)){
    window = pred_class[i:(i+window_size-1)]
    smoothed[i + as.integer(window_size/2)] = ifelse(length(window[window==T]) >= (window_size*threshold), T, F);
  }
  return(smoothed);
}

plot_data = function(data){
  min = min(min(data[,1]), min(data[,2]), min(data[,3]));
  max = max(max(data[,1]), max(data[,2]), max(data[,3]));
  plot(data[,1], ylim = c(min, max), type='n', xlab="time", ylab="acceleration data")
  lines(data[,1], col='red');
  lines(data[,2], col='green');
  lines(data[,3], col='blue');
}
