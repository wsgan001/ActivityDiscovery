
#compute the threhsold list of equal-depth binning vector x into n bins
get_quantize_threshold = function(x, n){
   x = x[is.na(x) == F]
   len = length(x);
   x = x + rnorm(x, 0, 0.001);
   
   quantie =  (1:n)* as.integer(len / n) 
   thres_index = which(rank(x) %in% quantie)
   thres_num = x[thres_index]
   return(sort(thres_num))
}

get_quantized_num = function(num, threshold){ max(which(threshold < num))}
get_quantized_array = function(vec, threshold){ unlist(lapply(vec, get_quantized_num, threshold=threshold));}

# remove the NAN exceptions in raw data by interporlation
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

# read a table file
read.files = function(file){read.table(file, sep=' ');}

# merged a list of matrix data into one big matrix
merge.data = function(data){
  merged = data[[1]]
  for(i in 2:length(data)){merged = rbind(merged, data[[i]]);}
  return(merged)
}

dataset = "PLCouple1"

if(dataset == "PLCouple1"){
  base = "D:\\lessons\\motion recognition\\dataset\\PLCouple1\\sensor\\quantize_data\\"
  filenames = list.files(paste(base, "raw_data\\", sep=''))
  files = paste(base, "raw_data\\", filenames, sep='')
  raw_data = lapply(files, read.files);
  merged = merge.data(raw_data)
  data = merged[, 4:21]
  col_cnt = 18;
}

#============================ read data =============================
if(dataset == "UBICOMP"){
  base = "D:\\lessons\\motion recognition\\dataset\\dataset_huynh_ubicomp08\\"
  setwd(base);
  filenames = c("data\\day2-data.txt", "data\\day4-data.txt", "data\\day6-data.txt")
  files = paste(base, files, sep='')
  raw_data = lapply(files, read.files);
  merged = merge.data(raw_data)
  data = merged[,1:12]
  col_cnt = 12;
}

if(dataset == "PAMAP"){
  setwd("D:\\lessons\\motion recognition\\dataset\\PAMAP2_Dataset\\Protocol");
  files = dir();
  files = files[substr(files, nchar(files)-3+1, nchar(files)) == "dat"]
  raw_data = lapply(files[1:3], read.downsample.pamap);
  merged = merge.data(raw_data)
  data = merged
}

bin_size = 20;

#function: given a data matrix, computing the equal-depth binning threshold for each column vector
bin.data = function(data, bin_size){
  col_cnt = length(data[1,]);
  threshold_matrix = matrix(0.0, nrow = bin_size, ncol=col_cnt)
  
  for(i in 1:col_cnt){
    threshold_matrix[,i] = get_quantize_threshold(data[,i], bin_size)
    threshold_matrix[,i] = as.numeric(format(round(threshold_matrix[,i], 2), nsmall = 2))
  }
  return(threshold_matrix);
}

#===================== get equal-depth binning threshold =============================
threshold_matrix = bin.data(data, 20)

write.table(threshold_matrix, file=paste(base, "quantization.txt", sep=''))

