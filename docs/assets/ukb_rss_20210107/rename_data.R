input = 'result/ukb_rss_20210107/ukb_rss_20210107.rds'

library(dplyr)
library(tibble)
dat = readRDS(input)
dat = as_tibble(dat)
# rename_cols = function(dat) {
#   colnames.change = which(!grepl('output', colnames(dat)))
#   tmp = strsplit(colnames(dat)[colnames.change], "[.]")
#   colnames(dat)[colnames.change] = unlist(lapply(1:length(tmp), function(i) ifelse(length(tmp[[i]])>1, tmp[[i]][2], tmp[[i]][1])))
#   return(dat)
# }

dat$mnm_rss[!is.na(dat$mnm_suff_oracle.DSC_TIME)] = 'mnm_suff_oracle'
dat$mnm_rss[!is.na(dat$susie)] = dat$susie[!is.na(dat$susie)]
dat$mnm_rss.resid_method[!is.na(dat$mnm_suff_oracle.resid_method)] = dat$mnm_suff_oracle.resid_method[!is.na(dat$mnm_suff_oracle.resid_method)]
dat$mnm_rss.DSC_TIME[!is.na(dat$mnm_suff_oracle.DSC_TIME)] = dat$mnm_suff_oracle.DSC_TIME[!is.na(dat$mnm_suff_oracle.DSC_TIME)]
dat$mnm_rss.DSC_TIME[!is.na(dat$susie.DSC_TIME)] = dat$susie.DSC_TIME[!is.na(dat$susie.DSC_TIME)]
dat$mnm_rss.output.file[!is.na(dat$mnm_suff_oracle.output.file)] = dat$mnm_suff_oracle.output.file[!is.na(dat$mnm_suff_oracle.output.file)]
dat$mnm_rss.output.file[!is.na(dat$susie.output.file)] = dat$susie.output.file[!is.na(dat$susie.output.file)]
dat$mnm_rss.resid_method[!is.na(dat$susie.estimate_residual_variance)] = dat$susie.estimate_residual_variance[!is.na(dat$susie.estimate_residual_variance)]
dat$mnm_rss.resid_method[is.na(dat$mnm_rss.resid_method)] = TRUE
dat$method = paste(dat$mnm_rss, dat$mnm_rss.resid_method,sep = '+') 
dat = dat %>% select(-c(mnm_suff_oracle.resid_method, mnm_suff_oracle.DSC_TIME, mnm_suff_oracle.output.file,
                        susie.DSC_TIME, susie.output.file, susie.estimate_residual_variance))
saveRDS(dat, 'result/ukb_rss_20210107/ukb_rss_20210107.2.rds')
