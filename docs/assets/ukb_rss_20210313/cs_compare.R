library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
## Functions
plot_panel = function(dat, quantity, legend = TRUE) {
  p = ggplot(dat, aes_string(x="method", y=quantity[1])) + 
    geom_point(position=position_dodge(.25), size=2.5)
  if (quantity[1] == 'power'){
    p = p + geom_errorbar(aes(ymin=power-power_se, ymax=power+power_se), 
                          width=.2, position=position_dodge(.25))
  }
  if (quantity[1] == 'coverage') {
    p = p + geom_errorbar(aes(ymin=coverage-coverage_se, ymax=coverage+coverage_se), 
                          width=.2, position=position_dodge(.25)) + 
      geom_hline(yintercept = 0.95, colour = 'gray') 
  }
  if (quantity[1] == 'fdr') {
    p = p + geom_errorbar(aes(ymin=fdr-fdr_se, ymax=fdr+fdr_se), 
                          width=.2, position=position_dodge(.25)) + 
      geom_hline(yintercept = 0.05, colour = 'gray') 
  }
  p = p + labs(x = "method", y = "") + theme_cowplot() + 
    background_grid(major = "x", minor = "none") + 
    ggtitle(quantity[2]) + theme(axis.title.x = element_text(size = 8),
                                 axis.text.x = element_text(angle = 60, size=6,hjust = 1),
                                 plot.title = element_text(size=10))
  if (!legend) p = p + theme(legend.position="none")
  return(p)
}
## parameters
input = 'ukb_rss_20210313.2.rds'
dat = readRDS(input)
# parameters
simulate_method = c('artificial_mixture_ukb', 'ukb_bloodcells_mixture')
level = c('glob', 'cond')
all.comb = expand.grid(simulate_method, level)
colnames(all.comb) = c('simulate_method', 'level')

rename = list('susie_suff+FALSE' = 'SuSiE',
              'susie_rss+FALSE' = 'SuSiE-RSS',
              'mnm_suff_oracle+oracle' = 'mvSuSiE Oracle prior Oracle residual',
              'mnm_suff_oracle+covY' = 'mvSuSiE Oracle prior Y residual',
              'mnm_suff_identity+oracle' = 'mvSuSiE Random effects prior Oracle residual',
              'mnm_suff_identity+covY' = 'mvSuSiE Random effects prior Y residual',
              'mnm_suff_naive+oracle' = 'mvSuSiE Default prior Oracle residual',
              'mnm_suff_naive+covY' = 'mvSuSiE Default prior Y residual',
              'mnm_suff_ed+oracle' = 'mvSuSiE ED prior Oracle residual',
              'mnm_suff_ed+covY' = 'mvSuSiE ED prior Y residual',
              'mnm_suff_ed_ddcan+oracle' = 'mvSuSiE ED+default prior Oracle residual',
              'mnm_suff_ed_ddcan+covY' = 'mvSuSiE ED+default prior Y residual',
              'mnm_rss_oracle+oracle' = 'mvSuSiE-RSS Oracle prior Oracle residual',
              'mnm_rss_oracle+identity' = 'mvSuSiE-RSS Oracle prior Identity residual',
              'mnm_rss_oracle+nullz' = 'mvSuSiE-RSS Oracle prior z residual', 
              'mnm_rss_oracle+corY' = 'mvSuSiE-RSS Oracle prior Y residual',
              'mnm_rss_identity+oracle' = 'mvSuSiE-RSS Random effects prior Oracle residual',
              'mnm_rss_shared+oracle' = 'mvSuSiE-RSS Fixed effects prior Oracle residual',
              'mnm_rss_naive+oracle' = 'mvSuSiE-RSS Default prior Oracle residual',
              'mnm_rss_ed+oracle' = 'mvSuSiE-RSS ED prior Oracle residual',
              'mnm_rss_ed_ddcan+oracle' = 'mvSuSiE-RSS ED+default prior Oracle residual',
              'mnm_rss_identity_corY+corY' = 'mvSuSiE-RSS Random effects prior Y residual',
              'mnm_rss_shared_corY+corY' = 'mvSuSiE-RSS Fixed effects prior Y residual',
              'mnm_rss_naive_corY+corY' = 'mvSuSiE-RSS Default prior Y residual',
              'mnm_rss_ed_corY+corY' = 'mvSuSiE-RSS ED prior Y residual',
              'mnm_rss_ed_ddcan_corY+corY' = 'mvSuSiE-RSS ED+default prior Y residual',
              'mnm_rss_identity_corZ+nullz' = 'mvSuSiE-RSS Random effects prior z residual',
              'mnm_rss_shared_corZ+nullz' = 'mvSuSiE-RSS Fixed effects prior z residual',
              'mnm_rss_naive_corZ+nullz' = 'mvSuSiE-RSS Default prior z residual',
              'mnm_rss_ed_corZ+nullz' = 'mvSuSiE-RSS ED prior z residual',
              'mnm_rss_ed_ddcan_corZ+nullz' = 'mvSuSiE-RSS ED+default prior z residual')

for(case in 1:nrow(all.comb)){
  simu = all.comb[case, 'simulate_method']
  level = all.comb[case, 'level']
  output = paste0('ukb_rss_20210313_cs/ukb_rss_cs_simu', simu, '_', level)
  dat_out = dat %>% filter(simulate == simu)
  res = list()
  
  if(level == 'glob'){
    methods = levels(factor(dat_out$method))[!grepl('susie', levels(factor(dat_out$method)), fixed=T)]
    methods_suff_oracle = c("mnm_suff_oracle+oracle","mnm_suff_identity+oracle","mnm_suff_naive+oracle",
                            "mnm_suff_ed+oracle","mnm_suff_ed_ddcan+oracle")
    methods_suff_y = c("mnm_suff_oracle+covY","mnm_suff_identity+covY","mnm_suff_naive+covY",
                       "mnm_suff_ed+covY","mnm_suff_ed_ddcan+covY")

    methods_resid = c('mnm_suff_oracle+oracle','mnm_rss_oracle+oracle', 
                      'mnm_rss_oracle+identity','mnm_rss_oracle+nullz','mnm_rss_oracle+corY')
    methods_prior_oracleresid = c('mnm_rss_oracle+oracle', 
                                  'mnm_rss_identity+oracle', 'mnm_rss_shared+oracle',
                                  'mnm_rss_naive+oracle', 'mnm_rss_ed+oracle','mnm_rss_ed_ddcan+oracle')
    methods_prior_y = c('mnm_rss_oracle+corY', 'mnm_rss_identity_corY+corY',
                        'mnm_rss_shared_corY+corY', 'mnm_rss_naive_corY+corY',
                        "mnm_rss_ed_corY+corY","mnm_rss_ed_ddcan_corY+corY")
    methods_prior = c('mnm_rss_oracle+nullz', 'mnm_rss_identity_corZ+nullz', 
                      'mnm_rss_shared_corZ+nullz', 'mnm_rss_naive_corZ+nullz',
                      'mnm_rss_ed_corZ+nullz', 'mnm_rss_ed_ddcan_corZ+nullz')
  }else{
    methods = levels(factor(dat_out$method))
    methods_suff_oracle = c('susie_suff+FALSE', 'susie_rss+FALSE', 
                            "mnm_suff_oracle+oracle","mnm_suff_identity+oracle","mnm_suff_naive+oracle",
                            "mnm_suff_ed+oracle","mnm_suff_ed_ddcan+oracle")
    methods_suff_y = c('susie_suff+FALSE', 'susie_rss+FALSE', 
                       "mnm_suff_oracle+covY","mnm_suff_identity+covY","mnm_suff_naive+covY",
                       "mnm_suff_ed+covY","mnm_suff_ed_ddcan+covY")
    
    methods_resid = c('susie_suff+FALSE', 'susie_rss+FALSE', 
                      'mnm_rss_oracle+oracle', 'mnm_rss_oracle+identity',
                      'mnm_rss_oracle+nullz','mnm_rss_oracle+corY')
    methods_prior_oracleresid = c('susie_suff+FALSE', 'susie_rss+FALSE', 
                                  'mnm_rss_oracle+oracle', 'mnm_rss_identity+oracle', 
                                  'mnm_rss_shared+oracle','mnm_rss_naive+oracle', 
                                  'mnm_rss_ed+oracle','mnm_rss_ed_ddcan+oracle')
    methods_prior_y = c('susie_suff+FALSE', 'susie_rss+FALSE', 
                        'mnm_rss_oracle+corY', 'mnm_rss_identity_corY+corY',
                        'mnm_rss_shared_corY+corY', 'mnm_rss_naive_corY+corY',
                        "mnm_rss_ed_corY+corY","mnm_rss_ed_ddcan_corY+corY")
    methods_prior = c('susie_suff+FALSE', 'susie_rss+FALSE', 
                      'mnm_rss_oracle+nullz', 'mnm_rss_identity_corZ+nullz', 
                      'mnm_rss_shared_corZ+nullz', 'mnm_rss_naive_corZ+nullz',
                      'mnm_rss_ed_corZ+nullz', 'mnm_rss_ed_ddcan_corZ+nullz')
  }
  
  for(met in methods){
    print(met)
    dat_sub = dat_out %>% filter(method == met)

    if(level == 'glob'){
      total = dat_sub$mvsusie_scores.total
      valid = dat_sub$mvsusie_scores.valid
      sizes = unlist(dat_sub$mvsusie_scores.size)
      purity = unlist(dat_sub$mvsusie_scores.purity)
      overlap = dat_sub$mvsusie_scores.overlap_cs
      expected = dat_sub$mvsusie_scores.n_causal
      cs_cor = dat_sub$mvsusie_scores.cs_correlation
    }else{
      if(grepl('susie', met)){
        total = unlist(dat_sub$susie_scores.total)
        valid = unlist(dat_sub$susie_scores.valid)
        sizes = unlist(dat_sub$susie_scores.size)
        purity = unlist(dat_sub$susie_scores.purity)
        overlap = NA
        expected = unlist(dat_sub$susie_scores.n_causal)
        cs_cor = dat_sub$susie_scores.cs_correlation
      }else{
        total = unlist(dat_sub$mvsusie_scores.true_cond_discoveries) +
          unlist(dat_sub$mvsusie_scores.false_pos_cond_discoveries)
        valid = unlist(dat_sub$mvsusie_scores.true_cond_discoveries)
        sizes = unlist(dat_sub$mvsusie_scores.size_cond_cs)
        purity = unlist(dat_sub$mvsusie_scores.purity_cond_cs)
        overlap = NA
        cs_cor = dat_sub$mvsusie_scores.cs_correlation
        tmp = dat_out %>% filter(method == 'susie_suff+FALSE')
        expected = unlist(tmp$susie_scores.n_causal)
      }
    }
    res[[met]] = list(total = sum(total), valid = sum(valid),
                      size = median(sizes, na.rm=T), purity = median(purity, na.rm=T),
                      overlap = sum(overlap), expected = sum(expected),
                      cs_cor = median(cs_cor, na.rm=T))
  }
  rates = matrix(unlist(res), length(res), byrow = T)
  rownames(rates) = names(res)
  colnames(rates) = c('discoveries', 'valid', 'size', 'purity', 'overlap', 'expected', 'cs_cor')
  rates = as.data.frame(rates)
  rates$power = rates$valid/rates$expected
  rates$fdr = (rates$discoveries - rates$valid)/rates$discoveries
  rates$coverage = rates$valid/rates$discoveries
  rates$power_se = sqrt(rates$power * (1-rates$power) / rates$expected)
  rates$power_se[is.nan(rates$power_se)] = 0
  rates$fdr_se = sqrt(rates$fdr * (1-rates$fdr) / rates$discoveries)
  rates$coverage_se = sqrt(rates$coverage * (1-rates$coverage) / rates$discoveries)
  saveRDS(rates, paste0(output, '.rds'))

  rates = readRDS(paste0(output, '.rds'))
  rates$method = rownames(rates)
  ## suff residuals
  ### oracle residual
  rates_suff_oracle = rates %>% filter(method %in% methods_suff_oracle)
  rates_suff_oracle$method = sapply(rates_suff_oracle$method, function(x) rename[[x]])
  rates_suff_oracle$method = gsub(' Oracle residual', '', rates_suff_oracle$method)
  rates_suff_oracle$method = factor(rates_suff_oracle$method, 
                                    levels=c('SuSiE','SuSiE-RSS','mvSuSiE Oracle prior',
                                             'mvSuSiE Random effects prior',
                                             'mvSuSiE Default prior','mvSuSiE ED prior',
                                             'mvSuSiE ED+default prior'))
  p1 = plot_panel(rates_suff_oracle, c('coverage', 'coverage'), legend=F)
  p2 = plot_panel(rates_suff_oracle, c('power', 'power'), legend=F)
  p3 = plot_panel(rates_suff_oracle, c('size', 'median number of variables'), legend=F)
  p4 = plot_panel(rates_suff_oracle, c('purity', 'median of purity'), legend=F)
  pdf(paste0(output, '_suff_oracleresid_plots.pdf'), width=12, height=3)
  grid.arrange(p1,p2,p3,p4, ncol=4, widths=c(3,3,3,3))
  dev.off()
  system(paste0("convert -flatten -density 120 ", paste0(output, '_suff_oracleresid_plots.pdf'), 
                " ", paste0(output, '_suff_oracleresid_plots.png')))
  
  ### oracle residual
  rates_suff_y = rates %>% filter(method %in% methods_suff_y)
  rates_suff_y$method = sapply(rates_suff_y$method, function(x) rename[[x]])
  rates_suff_y$method = gsub(' Y residual', '', rates_suff_y$method)
  rates_suff_y$method = factor(rates_suff_y$method, 
                                    levels=c('SuSiE','SuSiE-RSS','mvSuSiE Oracle prior',
                                             'mvSuSiE Random effects prior',
                                             'mvSuSiE Default prior','mvSuSiE ED prior',
                                             'mvSuSiE ED+default prior'))
  p1 = plot_panel(rates_suff_y, c('coverage', 'coverage'), legend=F)
  p2 = plot_panel(rates_suff_y, c('power', 'power'), legend=F)
  p3 = plot_panel(rates_suff_y, c('size', 'median number of variables'), legend=F)
  p4 = plot_panel(rates_suff_y, c('purity', 'median of purity'), legend=F)
  pdf(paste0(output, '_suff_yresid_plots.pdf'), width=12, height=3)
  grid.arrange(p1,p2,p3,p4, ncol=4, widths=c(3,3,3,3))
  dev.off()
  system(paste0("convert -flatten -density 120 ", paste0(output, '_suff_yresid_plots.pdf'), 
                " ", paste0(output, '_suff_yresid_plots.png')))
  
  # residuals
  rates_resid = rates %>% filter(method %in% methods_resid)
  rates_resid$method = sapply(rates_resid$method, function(x) rename[[x]])
  rates_resid$method = gsub(' Oracle prior', '', rates_resid$method)
  rates_resid$method = factor(rates_resid$method, levels=c('SuSiE','SuSiE-RSS',
                                                           'mvSuSiE Oracle residual',
                                                           'mvSuSiE-RSS Oracle residual',
                                                           'mvSuSiE-RSS Identity residual',
                                                           'mvSuSiE-RSS Y residual',
                                                           'mvSuSiE-RSS z residual'))
  p1 = plot_panel(rates_resid, c('coverage', 'coverage'), legend=F)
  p2 = plot_panel(rates_resid, c('power', 'power'), legend=F)
  p3 = plot_panel(rates_resid, c('size', 'median number of variables'), legend=F)
  p4 = plot_panel(rates_resid, c('purity', 'median of purity'), legend=F)
  pdf(paste0(output, '_resid_plots.pdf'), width=12, height=3)
  grid.arrange(p1,p2,p3,p4, ncol=4, widths=c(3,3,3,3))
  dev.off()
  system(paste0("convert -flatten -density 120 ", paste0(output, '_resid_plots.pdf'), " ", paste0(output, '_resid_plots.png')))
  
  # priors oracle residual
  rates_priors_oracleresid = rates %>% filter(method %in% methods_prior_oracleresid)
  rates_priors_oracleresid$method = sapply(rates_priors_oracleresid$method, function(x) rename[[x]])
  rates_priors_oracleresid$method = gsub(' Oracle residual', '', rates_priors_oracleresid$method)
  rates_priors_oracleresid$method = factor(rates_priors_oracleresid$method,
                                           levels=c('SuSiE','SuSiE-RSS','mvSuSiE Oracle prior',
                                                    'mvSuSiE-RSS Oracle prior',
                                                    'mvSuSiE-RSS Fixed effects prior',
                                                    'mvSuSiE-RSS Random effects prior',
                                                    'mvSuSiE-RSS Default prior',
                                                    'mvSuSiE-RSS ED prior',
                                                    'mvSuSiE-RSS ED+default prior'))
  p1 = plot_panel(rates_priors_oracleresid, c('coverage', 'coverage'), legend=F)
  p2 = plot_panel(rates_priors_oracleresid, c('power', 'power'), legend=F)
  p3 = plot_panel(rates_priors_oracleresid, c('size', 'median number of variables'), legend=F)
  p4 = plot_panel(rates_priors_oracleresid, c('purity', 'median of purity'), legend=F)
  pdf(paste0(output, '_priors_oracleresid_plots.pdf'), width=12, height=3)
  grid.arrange(p1,p2,p3,p4, ncol=4, widths=c(3,3,3,3))
  dev.off()
  system(paste0("convert -flatten -density 120 ", paste0(output, '_priors_oracleresid_plots.pdf'), " ", paste0(output, '_priors_oracleresid_plots.png')))
  
  # priors y residual
  rates_priors_yresid = rates %>% filter(method %in% methods_prior_y)
  rates_priors_yresid$method = sapply(rates_priors_yresid$method, function(x) rename[[x]])
  rates_priors_yresid$method = gsub(' Y residual', '', rates_priors_yresid$method)
  rates_priors_yresid$method = factor(rates_priors_yresid$method,
                                           levels=c('SuSiE','SuSiE-RSS','mvSuSiE Oracle prior',
                                                    'mvSuSiE-RSS Oracle prior',
                                                    'mvSuSiE-RSS Fixed effects prior',
                                                    'mvSuSiE-RSS Random effects prior',
                                                    'mvSuSiE-RSS Default prior',
                                                    'mvSuSiE-RSS ED prior',
                                                    'mvSuSiE-RSS ED+default prior'))
  p1 = plot_panel(rates_priors_yresid, c('coverage', 'coverage'), legend=F)
  p2 = plot_panel(rates_priors_yresid, c('power', 'power'), legend=F)
  p3 = plot_panel(rates_priors_yresid, c('size', 'median number of variables'), legend=F)
  p4 = plot_panel(rates_priors_yresid, c('purity', 'median of purity'), legend=F)
  pdf(paste0(output, '_priors_yresid_plots.pdf'), width=12, height=3)
  grid.arrange(p1,p2,p3,p4, ncol=4, widths=c(3,3,3,3))
  dev.off()
  system(paste0("convert -flatten -density 120 ", paste0(output, '_priors_yresid_plots.pdf'), 
                " ", paste0(output, '_priors_yresid_plots.png')))
  
  # priors
  rates_priors = rates %>% filter(method %in% methods_prior)
  rates_priors$method = sapply(rates_priors$method, function(x) rename[[x]])
  rates_priors$method = gsub(' Y residual', '', rates_priors$method)
  rates_priors$method = gsub(' z residual', '', rates_priors$method)
  rates_priors$method = factor(rates_priors$method,
                               levels=c('SuSiE','SuSiE-RSS',
                                        'mvSuSiE Oracle prior',
                                        'mvSuSiE-RSS Oracle prior', 
                                        'mvSuSiE-RSS Random effects prior',
                                        'mvSuSiE-RSS Fixed effects prior',
                                        'mvSuSiE-RSS Default prior',
                                        'mvSuSiE-RSS ED prior',
                                        'mvSuSiE-RSS ED+default prior'))
  p1 = plot_panel(rates_priors, c('coverage', 'coverage'), legend=F)
  p2 = plot_panel(rates_priors, c('power', 'power'), legend=F)
  p3 = plot_panel(rates_priors, c('size', 'median number of variables'), legend=F)
  p4 = plot_panel(rates_priors, c('purity', 'median of purity'), legend=F)
  pdf(paste0(output, '_priors_plots.pdf'), width=12, height=3)
  grid.arrange(p1,p2,p3,p4, ncol=4, widths=c(3,3,3,3))
  dev.off()
  system(paste0("convert -flatten -density 120 ", paste0(output, '_priors_plots.pdf'), 
                " ", paste0(output, '_priors_plots.png')))
  
}
