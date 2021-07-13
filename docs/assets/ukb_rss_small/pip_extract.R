input = 'ukb_rss_small.2.rds'
pref = '~/GitHub/mvarbvs/dsc/mnm_prototype/output/ukb_rss_small/'

library(dplyr)
dat = readRDS(input)
simulate_method = c('artificial_mixture_2', 'artificial_mixture_2_indep', 'artificial_mixture_6')
all.comb = expand.grid(simulate_method)
colnames(all.comb) = c('simulate_method')

for(case in 1:nrow(all.comb)){
  simu = all.comb[case, 'simulate_method']
  output = paste0('ukb_rss_small_pip_extraction/ukb_rss_small_pip_simu', simu,'_glob.rds')
  dat_out = dat %>% filter(simulate_small.eff_mode == simu)
  
  res_glob = list()
  for (i in 1:nrow(dat_out)) {
    method = dat_out[i,]$method
    true_coef = readRDS(paste0(pref, dat_out[i,]$simulate.output.file, '.rds'))$meta$true_coef != 0
    true_coef_glob = 1 * (rowSums(true_coef) != 0)
    if(method == 'PAINTOR+NA'){
      pip_glob = readRDS(paste0(pref, dat_out[i,]$output_file, '.rds'))$pip
    }else{
      pip_glob = readRDS(paste0(pref, dat_out[i,]$output_file, '.rds'))$result$pip
    }
    
    if (!(method %in% names(res_glob))) {
      res_glob[[method]] = list(pip = pip_glob, truth = true_coef_glob)
    } else {
      res_glob[[method]]$pip = append(res_glob[[method]]$pip, pip_glob)
      res_glob[[method]]$truth = append(res_glob[[method]]$truth, true_coef_glob)
    }
    if (i%%100==0) print(i)
  }
  for (method in unique(dat_out$method)) {
    res_glob[[method]] = do.call(cbind, res_glob[[method]])
  }
  saveRDS(res_glob, output)
}

