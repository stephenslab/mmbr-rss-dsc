input = 'ukb_rss_small.2.rds'
pref = '~/GitHub/mvarbvs/dsc/mnm_prototype/output/ukb_rss_small/'

library(dplyr)
dat = readRDS(input)
output = paste0('ukb_rss_small_pip_extraction/ukb_rss_pip.rds')
res = list()
for (i in 1:nrow(dat)) {
  method = dat[i,]$method
  true_coef = readRDS(paste0(pref, dat[i,]$simulate.output.file, '.rds'))$meta$true_coef != 0
  true_coef = 1 * (rowSums(true_coef) != 0)
  if(method == 'PAINTOR+NA'){
    pip = readRDS(paste0(pref, dat[i,]$output_file, '.rds'))$pip
  }else{
    pip = readRDS(paste0(pref, dat[i,]$output_file, '.rds'))$result$pip
  }
  
  if (!(method %in% names(res))) {
    res[[method]] = list(pip = pip, truth = true_coef)
  } else {
    res[[method]]$pip = append(res[[method]]$pip, pip)
    res[[method]]$truth = append(res[[method]]$truth, true_coef)
  }
  if (i%%100==0) print(i)
}

for (method in unique(dat$method)) {
  res[[method]] = do.call(cbind, res[[method]])
}
saveRDS(res, output)

