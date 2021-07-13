input = 'ukb_rss_small.rds'

library(dplyr)
library(tibble)
dat = readRDS(input)
dat = as_tibble(dat)

dat$method = dat$mnm_method
dat$method[is.na(dat$mnm_method)] = 'PAINTOR'
dat$time = dat$mnm_method.DSC_TIME
dat$time[is.na(dat$time)] = dat$PAINTOR.DSC_TIME[!is.na(dat$PAINTOR.DSC_TIME)]
dat$resid_method = dat$mnm_method.resid_method
dat$output_file = dat$mnm_method.output.file
dat$output_file[is.na(dat$output_file)] = dat$PAINTOR.output.file[!is.na(dat$PAINTOR.output.file)]
dat$method = paste(dat$method, dat$resid_method,sep = '+') 

dat = dat %>% select(-c(mnm_method, mnm_method.resid_method, mnm_method.DSC_TIME, mnm_method.output.file,
                        PAINTOR.DSC_TIME, PAINTOR.output.file))

dat = dat %>% rename(simulate.output.file = simulate_small.output.file)

saveRDS(dat, 'ukb_rss_small.2.rds')
