
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


dataset = "PLCouple1"

if(dataset == "PLCouple1"){
  base = "D:\\lessons\\motion recognition\\dataset\\PLCouple1\\sensor\\quantize_data\\"
  filenames = list.files(paste(base, "raw_data\\", sep=''))
  files = paste(base, "raw_data\\", filenames, sep='')
}

if(dataset == "UBICOMP"){
  base = "D:\\lessons\\motion recognition\\dataset\\dataset_huynh_ubicomp08\\"
  setwd(base);
  filenames = c("data\\day2-data.txt", "data\\day4-data.txt", "data\\day6-data.txt")
  paste(base, files, sep='')
}

read.files = function(file){read.table(file, sep=' ');}
raw_data = lapply(files, read.files);

merged = raw_data[[1]]
for(i in 2:length(data)){merged = rbind(merged, raw_data[[i]]);}

if(dataset == "PLCouple1"){
  data = merged[, 4:21]
  col_cnt = 18;
}

if(dataset == "UBICOMP"){
  data = merged[,1:12]
  col_cnt = 12;
}

bin_size = 20;
threshold_matrix = matrix(0, nrow = bin_size, ncol=col_cnt)



for(i in 1:col_cnt){
  threshold_matrix[,i] = get_quantize_threshold(data[,i], bin_size)
  threshold_matrix[,i] = as.numeric(format(round(threshold_matrix[,i], 2), nsmall = 2))
}

write.table(threshold_matrix, file=paste(base, "quantization.txt", sep=''))

