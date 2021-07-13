dat = readRDS('ukb_rss_small.2.rds')

paintor2indep_out = dat %>% filter(simulate_small.eff_mode == 'artificial_mixture_2_indep', 
                                   method == 'PAINTOR+NA')
mnmrss2indep_out = dat %>% filter(simulate_small.eff_mode == 'artificial_mixture_2_indep', 
                                   method == 'mnm_rss_naive_corZ+nullz')

summary(paintor2indep_out$time)
summary(mnmrss2indep_out$time)


