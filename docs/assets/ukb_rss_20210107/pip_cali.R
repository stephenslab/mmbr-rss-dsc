library(ggplot2)
library(cowplot)
rename = list('mnm_rss_oracle+oracle' = 'RSS Oracle prior & residual', 
              'mnm_rss_oracle+identity' = 'RSS Oracle prior Identity residual', 
              'mnm_rss_oracle+nullz' = 'RSS Oracle prior, z cor residual', 
              'mnm_rss_oracle+corY' = 'RSS Oracle prior, Y cor residual', 
              'mnm_rss_naive+oracle' = 'RSS Default prior oracle residual', 
              'mnm_rss_ed+oracle' = 'RSS ED prior oracle residual', 
              'mnm_rss_identity+oracle' = 'RSS Random effects prior oracle residual',
              'mnm_rss_shared+oracle' = 'RSS Fixed effect prior oracle residual',
              'mnm_suff_oracle+oracle' = 'Oracle prior and residual',
              'mnm_suff_oracle+covY' = 'Oracle prior, Y cov residual',
              'susie_suff+TRUE' = 'SuSiE',
              'susie_rss+TRUE' = 'SuSiE-RSS',
              'susie_rss+FALSE' = 'SuSiE-RSS fixed residual variance')
dot_plot = function(dataframe) {
  ggplot(dataframe, aes(x=mean_pip, y=observed_freq)) + 
    geom_errorbar(aes(ymin=observed_freq-se, ymax=observed_freq+se), colour="gray", size = 0.2, width=.01) +
    geom_point(size=1.5, shape=21, fill="#002b36") + # 21 is filled circle
    xlab("Mean PIP") +
    ylab("Observed frequency") +
    coord_cartesian(ylim=c(0,1), xlim=c(0,1)) +
    geom_abline(slope=1,intercept=0,colour='red', size=0.2) +
    ggtitle(rename[[name]]) +
    expand_limits(y=0) +                        # Expand y range
    theme_cowplot() + theme(plot.title = element_text(size = 8))
}

# parameters
bin_size = 10
# parameters
simulate_method = c('artificial_mixture_ukb', 'ukb_bloodcells_mixture')
level = c('glob', 'cond')
all.comb = expand.grid(simulate_method, level)
colnames(all.comb) = c('simulate_method', 'level')

for(case in 1:nrow(all.comb)){
  simu = all.comb[case, 'simulate_method']
  level = all.comb[case, 'level']
  input = paste0('ukb_rss_20210107_pip_extraction/ukb_rss_pip_simu', simu, '_', level,'.rds')
  output = paste0('ukb_rss_20210107_pip_calibration/ukb_rss_pip_cali_simu', simu, '_', level)

  dat = readRDS(input)
  
  bins = cbind(seq(1:bin_size)/bin_size-1/bin_size, seq(1:bin_size)/bin_size)
  pip_cali = list()
  for (method in names(dat)) {
    pip_cali[[method]] = matrix(NA, nrow(bins), 3)
    for (i in 1:nrow(bins)) {
      data_in_bin = dat[[method]][which(dat[[method]][,1] > bins[i,1] & dat[[method]][,1] < bins[i,2]),]
      if(!is.null(dim(data_in_bin))) {
        pip_cali[[method]][i,1] = sum(data_in_bin[,1])
        pip_cali[[method]][i,2] = sum(data_in_bin[,2])
        pip_cali[[method]][i,3] = nrow(data_in_bin)
      } else {
        pip_cali[[method]][i,] = c(0,0,0) 
      }
    }
  }
  for (method in names(dat)) {
    pip_cali[[method]][,c(1,2)] = pip_cali[[method]][,c(1,2)] / pip_cali[[method]][,3]
  }
  saveRDS(pip_cali, paste0(output, '.rds'))
  
  idx = 0
  ex_idx = c()
  for (name in names(pip_cali)) {
    idx = idx + 1
    if(level == 'glob'){
      if(grepl('susie', name, fixed=TRUE)){
        ex_idx = c(ex_idx, idx)
      }
    }
    pip_cali[[name]][,3] = sqrt(pip_cali[[name]][,2] * (1 - pip_cali[[name]][,2]) / pip_cali[[name]][,3]) * 2
    pip_cali[[name]] = as.data.frame(pip_cali[[name]])
    colnames(pip_cali[[name]]) = c("mean_pip", "observed_freq", "se")
    pdf(paste0(output, '_' , idx, '.pdf'), width=3, height=3, pointsize=16)
    print(dot_plot(pip_cali[[name]]))
    dev.off()
    system(paste0("convert -flatten -density 120 ", output, '_' , idx, '.pdf', " ",output, '_' , idx, '.png'))
  }
  
  files = paste0(output, '_', setdiff(1:idx, ex_idx), '.png')
  cmd = paste('convert +append', paste(files, collapse=" "), paste0(output, '.png'))
  system(cmd)
  system(paste('rm -f', paste(files, collapse=" ")))
}

