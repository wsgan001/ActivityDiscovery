
source("C:\\Users\\zetan\\workspace\\MotionDL\\code\\r\\discovery\\quantization.R")
setwd("D:\\lessons\\motion recognition\\dataset\\OPPORTUNITY Activity Recognition Data Set\\OpportunityUCIDataset\\dataset");
read.data.oppotunity = function(filename){
  data = read.table(filename, sep=" ");
  hip_x = data$V5;
  hip_y = data$V6;
  hip_z = data$V7;
  
  rwr_x = data$V23;
  rwr_y = data$V24;
  rwr_z = data$V25;
  accel_data = data.frame(hip_x, hip_y, hip_z, rwr_x, rwr_y, rwr_z);
  for(i in 1:length(data[1,])){
    data[, i] = interporlate_nan(data[, i]);
  }
  
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
  
  target_frame_cnt = as.integer(length(data[,1]) / compress_ratio);
  target_frame_data = matrix(0, nrow = target_frame_cnt, ncol = 12);
  
  for(i in 1:target_frame_cnt){
    for(col in 1:6){
      start = (i - 1) * compress_ratio + 1;
      end = i * compress_ratio;
      target_frame_data[i, col] = mean(data[start:end, col]);
      target_frame_data[i, col + 6] = sd(data[start:end, col]);    
    }
  }
  return(target_frame_data);  
}

read_downsample.oppotunity = function(filename){
  raw_accel_data = read.data.oppotunity(filename);
  downsampled_data = downsample(raw_accel_data, 30, 2);
  return(downsampled_data);
}

files = dir();
files = files[substr(files, nchar(files)-3+1, nchar(files)) == "dat"]


data = lapply(files[c(1,8,15,22)], read_downsample.oppotunity);

merged = data[[1]];
for(i in 2:length(data)){
  merged = rbind(merged, data[[i]]);
}

#original data binning

bin.data = function(data, bin_size){
  col_cnt = length(data[1,]);
  threshold_matrix = matrix(0.0, nrow = bin_size, ncol=col_cnt)
  
  for(i in 1:col_cnt){
    threshold_matrix[,i] = get_quantize_threshold(data[,i], bin_size)
    threshold_matrix[,i] = as.numeric(format(round(threshold_matrix[,i], 2), nsmall = 2))
  }
  return(threshold_matrix);
}

original_threshold_matrix = bin.data(merged, 20);
write.table(original_threshold_matrix, file="quantization.txt")

#fft data binning
cal_fft_params = function(data, cols){
  framesInDoc = 20;
  fft_num = 5;
  docCnt = as.integer(length(data[,1]) / framesInDoc);
  fft_params = matrix(0, nrow = docCnt, ncol = fft_num * 2 * length(cols));
  
  for(row in 1:docCnt){
    frameRows = ((row-1)*framesInDoc+1):(row*framesInDoc);
    fft_params[row, ] = data.fft(data.frame(data[frameRows, cols]), fft_num);  
  }
  return(fft_params);
}

fft_data = cal_fft_params(merged, 1:6);
fft_threshold_matrix = bin.data(fft_data, 20);
write.table(fft_threshold_matrix, file="fft_quantization.txt");
