#----------START: read file function region-------------------
read.rawfiles = function(x){read.table(x, sep=' ')}
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

find.combined_label = function(rawLabel,ubiLabels){ubiLabels[3,which(as.integer(ubiLabels[2, ]) == rawLabel)]}

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

voteMajor = function(docIndex){
  frameLabels = label[((docIndex-1)*framesInDoc+1):(docIndex*framesInDoc)]
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
  plot(1, type="o", col=colors[1],xlim=c(1, length(doc_labels)), ylim=c(0, 1.5), 
       xlab="activities", ylab="topic probability")
  for(i in 1:length(doc_labels)){
    points(x=i, y=1.2, col=colors[which(doc_label_set==doc_labels[i])], pch=16)
  }  
  if(dataset == "PLCouple1"){
    labelConfig = load_labelIDConfig_plc("../../LABELID.config");
    for(i in 2:length(doc_label_set)){
      text(50 + i%%5 * 300, 1.2+(i / 5)*0.07, labelConfig[as.integer(doc_label_set[i])], adj = c(0,0), col=colors[i]);
    } 
  }
  if(dataset == "UBICOMP"){
    labelConfig = load_labelIDConfig_ubi("activities.txt");
    for(i in 2:length(doc_label_set)){
      text(50 + i%%5 * 300, 1.2+(i / 5)*0.07, labelConfig[as.integer(doc_label_set[i])+1], adj = c(0,0), col=colors[i]);
    }
  }
}

viz_topic_distribution = function(pred, K){
  for(i in 1:K){
    lines(pred[,i], type="o", pch=22, lty=2, col=colors[i])
  }
}