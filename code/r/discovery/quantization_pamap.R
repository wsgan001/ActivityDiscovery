setwd("D:\\lessons\\motion recognition\\dataset\\PAMAP2_Dataset\\Protocol");


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
  col_num = length(data[1,]);
  
  compress_ratio = raw_fps / target_fps;
  
  target_frame_cnt = as.integer(length(data[,1]) / compress_ratio);
  target_frame_data = matrix(0, nrow = target_frame_cnt, ncol = col_num * 2);
  
  for(i in 1:target_frame_cnt){
    for(col in 1:col_num){
      start = (i - 1) * compress_ratio + 1;
      end = i * compress_ratio;
      target_frame_data[i, col] = mean(data[start:end, col]);
      target_frame_data[i, col + col_num] = sd(data[start:end, col]);    
    }
  }
  return(target_frame_data);  
}

read.downsample.pamap = function(file){
  raw_data = read.data.pamap(file);
  fps2 = downsample(raw_data, 100, 2);
  return(fps2);
}

files = dir();
files = files[substr(files, nchar(files)-3+1, nchar(files)) == "dat"]
data = lapply(files[1:3], read.downsample.pamap);
merged = data[[1]];
for(i in 2:length(data)){
  merged = rbind(merged, data[[i]]);
}


original_threshold_matrix = bin.data(merged, 20);
write.table(original_threshold_matrix, file="fps2_quantization.txt")
