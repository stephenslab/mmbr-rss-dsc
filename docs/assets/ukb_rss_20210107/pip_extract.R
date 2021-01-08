input = 'ukb_rss_20210107.2.rds'
pref = '~/GitHub/mvarbvs/dsc/mnm_prototype/output/ukb_rss/'

library(dplyr)
dat = readRDS(input)
# parameters
simulate_method = c('artificial_mixture_ukb', 'ukb_bloodcells_mixture')
all.comb = expand.grid(simulate_method)
colnames(all.comb) = c('simulate_method')

for(case in 1:nrow(all.comb)){
  simu = all.comb[case, 'simulate_method']
  output = paste0('ukb_rss_20210107_pip_extraction/ukb_rss_pip_simu', simu,'.rds')
  dat_out = dat %>% filter(simulate == simu)

  res = list()
  for (i in 1:nrow(dat_out)) {
    true_coef_global = 1 * (rowSums(readRDS(paste0(pref, dat_out[i,]$simulate.output.file, '.rds'))$meta$true_coef) != 0)
    method = dat_out[i,]$method
    if(grepl('susie', method, fixed = TRUE)){
      pip_global = apply(dat_out[i,]$susie.pip[[1]], 1, max)
    }else{
      tmp = readRDS(paste0(pref, dat_out[i,]$output_file, '.rds'))$result
      pip_global = tmp$pip
    }
    
    if (!(method %in% names(res))) {
      res[[method]] = list(pip = pip_global, truth = true_coef_global)
    } else {
      res[[method]]$pip = append(res[[method]]$pip, pip_global)
      res[[method]]$truth = append(res[[method]]$truth, true_coef_global)
    }
    if (i%%100==0) print(i)
  }
  for (method in unique(dat_out$method)) {
    res[[method]] = do.call(cbind, res[[method]])
  }
  saveRDS(res, output)
}
