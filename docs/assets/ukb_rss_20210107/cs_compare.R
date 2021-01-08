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
  p = p + labs(x = "method", y = "") + theme_cowplot() + 
    background_grid(major = "x", minor = "none") + 
    ggtitle(quantity[2]) + theme(axis.title.x = element_text(size = 8),
                                 axis.text.x = element_text(angle = 45, size=7,hjust = 1),
                                 plot.title = element_text(size=10))
  if (!legend) p = p + theme(legend.position="none")
  return(p)
}
## parameters
input = 'ukb_rss_20210107.2.rds'
dat = readRDS(input)
# parameters
simulate_method = c('artificial_mixture_ukb', 'ukb_bloodcells_mixture')
all.comb = expand.grid(simulate_method)
colnames(all.comb) = c('simulate_method')

rename = list('mnm_rss_oracle+oracle' = 'RSS Oracle prior & residual',
              'mnm_rss_oracle+identity' = 'RSS Oracle prior Identity residual',
              'mnm_rss_oracle+nullz' = 'RSS Oracle prior, z cor residual', 
              'mnm_rss_oracle+corY' = 'RSS Oracle prior, Y cor residual', 
              'mnm_rss_naive+oracle' = 'RSS Default prior oracle residual',
              'mnm_rss_ed+oracle' = 'RSS ED prior oracle residual',
              'mnm_rss_identity+oracle' = 'RSS Random effects prior oracle residual',
              'mnm_rss_shared+oracle' = 'RSS Fixed effect prior oracle residual',
              'mnm_suff_oracle+oracle' = 'Oracle prior and residual',
              'mnm_suff_oracle+covY' = 'Oracle prior, Y cov residual')

for(case in 1:nrow(all.comb)){
  simu = all.comb[case, 'simulate_method']
  output = paste0('ukb_rss_20210107_cs/ukb_rss_cs_simu', simu)
  dat_out = dat %>% filter(simulate == simu)
  res = list()
  
  methods = levels(factor(dat_out$method))[!grepl('susie', levels(factor(dat_out$method)), fixed=T)]
  for(met in methods){
    dat_sub = dat_out %>% filter(method == met)
    total = dat_sub$total
    valid = dat_sub$valid
    sizes = dat_sub$size
    overlap = dat_sub$overlap_cs
    expected = dat_sub$n_causal
    res[[met]] = list(total = sum(total), valid = sum(valid), size = median(sizes), 
                      overlap = sum(overlap), expected = sum(expected), 
                      converged = mean(dat_out$converged))
  }
  rates = matrix(unlist(res), length(res), byrow = T)
  rownames(rates) = names(res)
  colnames(rates) = c('discoveries', 'valid', 'size', 'overlap', 'expected', 'converged')
  rates = as.data.frame(rates)
  rates$power = rates$valid/rates$expected
  rates$coverage = rates$valid/rates$discoveries
  rates$power_se = sqrt(rates$power * (1-rates$power) / rates$expected)
  rates$power_se[is.nan(rates$power_se)] = 0
  rates$coverage_se = sqrt(rates$coverage * (1-rates$coverage) / rates$discoveries)
  rates$method = sapply(rownames(rates), function(x) rename[[x]])
  saveRDS(rates, paste0(output, '.rds'))
  
  # residuals
  rates_resid = rates %>% filter(method %in% c('Oracle prior and residual', 'Oracle prior, Y cov residual', 
                                               'RSS Oracle prior & residual', 'RSS Oracle prior Identity residual',
                                               'RSS Oracle prior, Y cor residual', 'RSS Oracle prior, z cor residual'))
  p1 = plot_panel(rates_resid, c('coverage', 'coverage'), legend=F)
  p2 = plot_panel(rates_resid, c('power', 'power'), legend=F)
  p3 = plot_panel(rates_resid, c('size', 'median number of variables'), legend=F)
  pdf(paste0(output, '_resid_plots.pdf'), width=9, height=3)
  grid.arrange(p1,p2,p3, ncol=3, widths=c(3,3,3))
  dev.off()
  system(paste0("convert -flatten -density 120 ", paste0(output, '_resid_plots.pdf'), " ", paste0(output, '_resid_plots.png')))
  
  # priors
  rates_priors = rates %>% filter(method %in% c('Oracle prior and residual', 'RSS Oracle prior & residual', 
                                                'RSS Default prior oracle residual', 'RSS Random effects prior oracle residual',
                                                'RSS Fixed effect prior oracle residual', 'RSS ED prior oracle residual'))
  p1 = plot_panel(rates_priors, c('coverage', 'coverage'), legend=F)
  p2 = plot_panel(rates_priors, c('power', 'power'), legend=F)
  p3 = plot_panel(rates_priors, c('size', 'median number of variables'), legend=F)
  pdf(paste0(output, '_priors_plots.pdf'), width=9, height=3)
  grid.arrange(p1,p2,p3, ncol=3, widths=c(3,3,3))
  dev.off()
  system(paste0("convert -flatten -density 120 ", paste0(output, '_priors_plots.pdf'), " ", paste0(output, '_priors_plots.png')))
}
