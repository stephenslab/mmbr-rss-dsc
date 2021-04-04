input = 'ukb_rss_20210313.rds'

library(dplyr)
library(tibble)
dat = readRDS(input)
dat = as_tibble(dat)

dat$mnm_rss[!is.na(dat$mnm_suff)] = dat$mnm_suff[!is.na(dat$mnm_suff)]
dat$mnm_rss[!is.na(dat$susie_uni)] = dat$susie_uni[!is.na(dat$susie_uni)]

dat$susie_uni.estimate_residual_variance = NA
dat$susie_uni.estimate_residual_variance[!is.na(dat$susie_uni.DSC_TIME)] = FALSE
dat$mnm_rss.resid_method[!is.na(dat$mnm_suff.resid_method)] = dat$mnm_suff.resid_method[!is.na(dat$mnm_suff.resid_method)]
dat$mnm_rss.resid_method[!is.na(dat$susie_uni.estimate_residual_variance)] = dat$susie_uni.estimate_residual_variance[!is.na(dat$susie_uni.estimate_residual_variance)]

dat$mnm_rss.DSC_TIME[!is.na(dat$mnm_suff.DSC_TIME)] = dat$mnm_suff.DSC_TIME[!is.na(dat$mnm_suff.DSC_TIME)]
dat$mnm_rss.DSC_TIME[!is.na(dat$susie_uni.DSC_TIME)] = dat$susie_uni.DSC_TIME[!is.na(dat$susie_uni.DSC_TIME)]

dat$mnm_rss.output.file[!is.na(dat$mnm_suff.output.file)] = dat$mnm_suff.output.file[!is.na(dat$mnm_suff.output.file)]
dat$mnm_rss.output.file[!is.na(dat$susie_uni.output.file)] = dat$susie_uni.output.file[!is.na(dat$susie_uni.output.file)]

dat$method = paste(dat$mnm_rss, dat$mnm_rss.resid_method,sep = '+') 
dat = dat %>% select(-c(mnm_suff.resid_method, mnm_suff.DSC_TIME, mnm_suff.output.file,
                        susie_uni.DSC_TIME, susie_uni.output.file, susie_uni.estimate_residual_variance))
saveRDS(dat, 'ukb_rss_20210313.2.rds')
