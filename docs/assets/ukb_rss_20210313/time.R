dat = readRDS('ukb_rss_20210107.2.rds')

dat_out = dat %>% filter(simulate == 'artificial_mixture_ukb')
mmrss_out = dat_out %>% filter(method == 'mnm_rss_oracle+oracle')
mm_out = dat_out %>% filter(method == 'mnm_suff_oracle+oracle')
susie_out = dat_out %>% filter(method == 'susie_suff+TRUE')
susierss_out = dat_out %>% filter(method == 'susie_rss+TRUE')

summary(susie_out$mnm_rss.DSC_TIME)
summary(susierss_out$mnm_rss.DSC_TIME)
summary(mm_out$mnm_rss.DSC_TIME)
summary(mmrss_out$mnm_rss.DSC_TIME)


