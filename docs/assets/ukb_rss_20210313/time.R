dat = readRDS('ukb_rss_20210313.2.rds')

dat_out = dat %>% filter(simulate == 'artificial_mixture_ukb')
mmrss_out = dat_out %>% filter(method == 'mnm_rss_naive_corZ+nullz')
mm_out = dat_out %>% filter(method == 'mnm_suff_naive+covY')
susie_out = dat_out %>% filter(method == 'susie_suff+FALSE')
susierss_out = dat_out %>% filter(method == 'susie_rss+FALSE')

round(summary(susie_out$mnm_rss.DSC_TIME)[c(4,1,6)],2)
round(summary(susierss_out$mnm_rss.DSC_TIME)[c(4,1,6)],2)
round(summary(mm_out$mnm_rss.DSC_TIME)[c(4,1,6)],2)
round(summary(mmrss_out$mnm_rss.DSC_TIME)[c(4,1,6)],2)


