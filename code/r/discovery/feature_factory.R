

get_verticle = function(data_3d){
  mx = mean(data_3d[,1])
  my = mean(data_3d[,2])
  mz = mean(data_3d[,3])
  mag = sqrt(mx*mx + my*my + mz*mz)
  meanvec = c(mx, my, mz)
  len = length(data_3d[,1])
  verticle = rep(0, len)
  for(i in 1:len){
    verticle[i] = sum(meanvec * data_3d[i,]) / mag  # %*% is dot product, value is a matrix
  }
  return(verticle)
}

get_horizontal = function(data_3d, verticle){
  mx = mean(data_3d[,1])
  my = mean(data_3d[,2])
  mz = mean(data_3d[,3])
  mag = sqrt(mx*mx + my*my + mz*mz)
  meanvec = c(mx, my, mz)
  len = length(data_3d[,1])
  horizontal = rep(0, len)
  for(i in 1:len){
    horivec = data_3d[i,] - verticle[i]/mag * meanvec
    horizontal[i] = sqrt(sum(horivec * horivec))
  }
  return(horizontal)
}

# interface
# component_name = wrist | 
get_feature_words = function(data_3d, component_name, feature_name, window_size){
  
}

get_mean_words = function(data_3d, component, window_size){
  window_cnt = as.integer(length(data_3d[,1]) / window_size);
  mean_data =get_mean_nums(data_3d, window_size);
  quantize_threshold = read.table()
}

get_mean_nums =  function(data_3d, window_size){
  window_cnt = as.integer(length(data_3d[,1]) / window_size);
  mean_data = matrix(0, nrow=window_cnt, ncol=3);
  for(i in 1:window_cnt){
    range = ((i-1)*window_size + 1) : (i * window_size);
    mean_data[i, 1] = mean(data_3d[range, 1]);
    mean_data[i, 2] = mean(data_3d[range, 2]);
    mean_data[i, 3] = mean(data_3d[range, 3]);
  }
  return(mean_data)
}

get_peaknum_nums = function(data_3d, window_size){
  window_cnt = as.integer(length(data_3d[,1]) / window_size);
  peaknum_data = matrix(0, nrow=window_cnt, ncol=3);
  for(i in 1:window_cnt){
    range = ((i-1)*window_size + 1) : (i * window_size);
    peaknum_data[i, 1] = get_peaknum(data_3d[range, 1]);
    peaknum_data[i, 2] = get_peaknum(data_3d[range, 2]);
    peaknum_data[i, 3] = get_peaknum(data_3d[range, 3]);
  }
  return(peaknum_data)
}

get_peaknum = function(vec){
  halfwin = 2;
  len = length(vec);
  smoothed = vec
  for(i in (halfwin+1):(len-halfwin-1)){
    smoothed[i] = mean(vec[c( (i-halfwin):(i-1), (i+1):(i+halfwin) )]) * 0.6 + vec[i] * 0.4;
  }
  compare = rbind(smoothed[2:(len-1)], smoothed[1:(len-2)], smoothed[3:len]);
  peaks = which((compare[1, ] > compare[2, ] + 0.1) & (compare[1, ] > compare[3, ]+ 0.1))
  return(length(peaks))
}


get_mean_energy = function(data_3d){
  len = length(data_3d[,1])
  energy = rep(0, len)
  for(i in 1:len){
    energy[i] = sqrt(sum(data_3d[i,] * data_3d[i,]))
  }
  return(mean(energy))
}


get_skew = function(vec){
  mv = mean(vec);
  diff = vec - mv;
  numer = sum(diff^3)
  deno = (sum(diff^2))^1.5
  return(numer / deno)
}

get_kurtosis = function(vec){
  mv = mean(vec)
  diff = vec - mv
  numer = sum(diff^4)
  deno = (sum(diff^2))^2
  return(numer/deno)
}

get_vec_energy = function(vec){
  f = fft(vec)
  energy = sum(Re(f) * Re(f) + Im(f) * Im(f))
  return(energy)
}

get_fft_energy_low = function(vec){
  f = fft(vec)[1:2];
  energy = sum(Re(f) * Re(f) + Im(f) * Im(f));
  return(energy)
}

get_fft_energy_midlow = function(vec){
  f = fft(vec)[3:6];
  energy = sum(Re(f) * Re(f) + Im(f) * Im(f));
  return(energy)
}

get_fft_energy_midhigh = function(vec){
  f = fft(vec)[7:14];
  energy = sum(Re(f) * Re(f) + Im(f) * Im(f));
  return(energy)
}

get_fft_energy_high = function(vec){
  N = length(vec)
  f = fft(vec)[15:N];
  energy = sum(Re(f) * Re(f) + Im(f) * Im(f));
  return(energy)
}

get_fft_entropy = function(vec){
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

get_feature = function(data_3d){
  mx = mean(data_3d[,1])
  my = mean(data_3d[,2])
  mz = mean(data_3d[,3])
  mean_energy = get_mean_energy(data_3d)
  verticle = get_verticle(data_3d)
  horizontal = get_horizontal(data_3d, verticle)
  mean_vert = mean(verticle)
  mean_hori = mean(horizontal)
  sd_vert = sd(verticle)
  sd_hori = sd(horizontal)
  skew_vert = get_skew(verticle)
  skew_hori = get_skew(horizontal)
  kurto_vert = get_kurtosis(verticle)
  kurto_hori = get_kurtosis(horizontal)
  fft_vert = fft(verticle)
  fft_hori = fft(horizontal)
  energy_low_vert = get_fft_energy_low(verticle)
  energy_midlow_vert = get_fft_energy_midlow(verticle)
  energy_midhigh_vert = get_fft_energy_midhigh(verticle)
  energy_high_vert = get_fft_energy_high(verticle)
  energy_low_hori = get_fft_energy_low(horizontal)
  energy_midlow_hori = get_fft_energy_midlow(horizontal)
  energy_midhigh_hori = get_fft_energy_midhigh(horizontal)
  energy_high_hori = get_fft_energy_high(horizontal)
  fft_entropy_vert = get_fft_entropy(verticle)
  fft_entropy_hori = get_fft_entropy(horizontal)
  energy_ratio = get_vec_energy(horizontal) / get_vec_energy(verticle)
  
  features = c(mx, my, mz, mean_energy, mean_vert, mean_hori, sd_vert, sd_hori,
               skew_vert, skew_hori, kurto_vert, kurto_hori,
               Re(fft_vert)[1:5], Im(fft_vert)[1:5], Re(fft_hori)[1:5], Im(fft_hori)[1:5],
               energy_low_vert, energy_midlow_vert, energy_midhigh_vert, energy_high_vert,
               energy_low_hori, energy_midlow_hori, energy_midhigh_hori, energy_high_hori,
               fft_entropy_vert, fft_entropy_hori, energy_ratio
               )
  return(features)
}