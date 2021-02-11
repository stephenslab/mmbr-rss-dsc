input = 'result/ukb_rss_small/ukb_rss_small.rds'

library(dplyr)
library(tibble)
dat = readRDS(input)
dat = as_tibble(dat)
dat$model_method = NA
dat$model_method[!is.na(dat$mnm_suff_oracle.DSC_TIME)] = 'mnm_suff_oracle'
dat$model_method[!is.na(dat$mnm_rss_oracle.DSC_TIME)] = 'mnm_rss_oracle'
dat$model_method[!is.na(dat$mnm_rss_naive.DSC_TIME)] = 'mnm_rss_naive'
dat$model_method[!is.na(dat$PAINTOR.DSC_TIME)] = 'PAINTOR'
dat$time = dat$mnm_suff_oracle.DSC_TIME
dat$time[!is.na(dat$mnm_rss_oracle.DSC_TIME)] = dat$mnm_rss_oracle.DSC_TIME[!is.na(dat$mnm_rss_oracle.DSC_TIME)]
dat$time[!is.na(dat$mnm_rss_naive.DSC_TIME)] = dat$mnm_rss_naive.DSC_TIME[!is.na(dat$mnm_rss_naive.DSC_TIME)]
dat$time[!is.na(dat$PAINTOR.DSC_TIME)] = dat$PAINTOR.DSC_TIME[!is.na(dat$PAINTOR.DSC_TIME)]
dat$resid_method = dat$mnm_suff_oracle.resid_method
dat$resid_method[!is.na(dat$mnm_rss_oracle.DSC_TIME)] = dat$mnm_rss_oracle.resid_method[!is.na(dat$mnm_rss_oracle.resid_method)]
dat$resid_method[!is.na(dat$mnm_rss_naive.resid_method)] = dat$mnm_rss_naive.resid_method[!is.na(dat$mnm_rss_naive.resid_method)]
dat$method = paste(dat$model_method, dat$resid_method,sep = '+') 
dat$output_file = dat$PAINTOR.output.file
dat$output_file[!is.na(dat$mnm_suff_oracle.output.file)] = dat$mnm_suff_oracle.output.file[!is.na(dat$mnm_suff_oracle.output.file)]
dat$output_file[!is.na(dat$mnm_rss_oracle.output.file)] = dat$mnm_rss_oracle.output.file[!is.na(dat$mnm_rss_oracle.output.file)]
dat$output_file[!is.na(dat$mnm_rss_naive.output.file)] = dat$mnm_rss_naive.output.file[!is.na(dat$mnm_rss_naive.output.file)]

dat = dat %>% select(-c(mnm_suff_oracle.resid_method, mnm_suff_oracle.DSC_TIME, mnm_suff_oracle.output.file,
                        mnm_rss_oracle.resid_method, mnm_rss_oracle.DSC_TIME, mnm_rss_oracle.output.file,
                        mnm_rss_naive.resid_method, mnm_rss_naive.DSC_TIME, mnm_rss_naive.output.file,
                        PAINTOR.DSC_TIME, PAINTOR.output.file))
dat = dat %>% rename(simulate.output.file = artificial_mixture_small.output.file)
saveRDS(dat, 'result/ukb_rss_small/ukb_rss_small.2.rds')
