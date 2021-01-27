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
  output_glob = paste0('ukb_rss_20210107_pip_extraction/ukb_rss_pip_simu', simu,'_glob.rds')
  output_cond = paste0('ukb_rss_20210107_pip_extraction/ukb_rss_pip_simu', simu,'_cond.rds')
  dat_out = dat %>% filter(simulate == simu)

  res_cond = list()
  res_glob = list()
  for (i in 1:nrow(dat_out)) {
    method = dat_out[i,]$method
    true_coef = readRDS(paste0(pref, dat_out[i,]$simulate.output.file, '.rds'))$meta$true_coef != 0
    true_coef_cond = c(true_coef)
    true_coef_glob = 1 * (rowSums(true_coef) != 0)
    if(grepl('susie', method, fixed = TRUE)){
      pip_cond = c(dat_out[i,]$susie_scores.pip[[1]])
      pip_glob = apply(dat_out[i,]$susie_scores.pip[[1]], 1, max)
    }else{
      tmp = readRDS(paste0(pref, dat_out[i,]$mnm_rss.output.file, '.rds'))$result
      pip_cond = c(1-tmp$lfsr)
      pip_glob = tmp$pip
    }
    
    if (!(method %in% names(res_glob))) {
      res_glob[[method]] = list(pip = pip_glob, truth = true_coef_glob)
      res_cond[[method]] = list(pip = pip_cond, truth = true_coef_cond)
    } else {
      res_glob[[method]]$pip = append(res_glob[[method]]$pip, pip_glob)
      res_glob[[method]]$truth = append(res_glob[[method]]$truth, true_coef_glob)
      res_cond[[method]]$pip = append(res_cond[[method]]$pip, pip_cond)
      res_cond[[method]]$truth = append(res_cond[[method]]$truth, true_coef_cond)
    }
    if (i%%100==0) print(i)
  }
  for (method in unique(dat_out$method)) {
    res_glob[[method]] = do.call(cbind, res_glob[[method]])
    res_cond[[method]] = do.call(cbind, res_cond[[method]])
  }
  saveRDS(res_glob, output_glob)
  saveRDS(res_cond, output_cond)
}
