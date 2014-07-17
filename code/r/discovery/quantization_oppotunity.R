
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
  return(accel_data);
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


data = lapply(files[1:10], read_downsample.oppotunity);

merged = data[[1]];
for(i in 2:length(data)){
  merged = rbind(merged, data[[i]]);
}


bin_size = 20;
col_cnt = 12;
threshold_matrix = matrix(0.0, nrow = bin_size, ncol=col_cnt)

for(i in 1:col_cnt){
  threshold_matrix[,i] = get_quantize_threshold(merged[,i], bin_size)
  threshold_matrix[,i] = as.numeric(format(round(threshold_matrix[,i], 2), nsmall = 2))
}


write.table(threshold_matrix, file=paste(base, "quantization.txt", sep=''))
