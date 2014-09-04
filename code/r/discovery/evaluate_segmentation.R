srcdir = "C:\\Users\\zetan\\workspace\\MotionDL\\code\\r\\discovery\\"
source(paste(srcdir, "util.R", sep=''))

dataset = "UBICOMP";
framesInDoc = 2 * 30;

if(dataset == "UBICOMP"){
  base = "D:\\lessons\\motion recognition\\dataset\\dataset_huynh_ubicomp08\\"
  setwd(base);
  label = read.ubilabel("activities.txt", "label\\day2-activities.txt")
}


label = c(100, 200, 300, 400, 500);
seg_points = c(50, 105, 190, 230, 250, 350, 430);

segmentation_ground_truth = function(labels){
  len = length(labels)
  cur_pre_pair = rbind(lables[2:len], labels[1:(len-1)]);
  return(which(cur_pre_pair != cur_pre_pair) + 1)
}

segmentation_cut_points = function(segmentation){
  return(segmentation[2, ])
}

compute_recall = function(cutpoints_truth, cutpoints_practise, margin){
  recalled = lapply(cutpoints_truth, is_recalled, cutpoint_vec = cutpoints_practise, margin = margin)
  return(length(recalled[recalled == TRUE]) / length(recalled))
}

is_recalled = function(target, cutpoint_vec, margin){
  recalled_points = cutpoint_vec[(cutpoint_vec >= target - margin) & (cutpoint_vec <= target + margin)]
  return(ifelse(length(recalled_points) > 0, TRUE,FALSE));
}

compute_precision = function(cutpoints_truth, cutpoints_practise, margin){
  precise = lapply(cutpoints_practise, is_recalled, cutpoint_vec = cutpoints_truth, margin = margin)
  return(length(precise[precise == TRUE]) / length(precise))
}