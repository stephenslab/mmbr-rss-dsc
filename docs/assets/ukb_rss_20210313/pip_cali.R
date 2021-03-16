library(ggplot2)
library(cowplot)
rename_resid = list('susie_suff+TRUE' = 'SuSiE',
                    'susie_rss+TRUE' = 'SuSiE-RSS',
                    'mnm_suff_oracle+oracle' = 'mvSuSiE Oracle residual',
                    'mnm_suff_oracle+covY' = 'mvSuSiE Y cov residual',
                    'mnm_rss_oracle+oracle' = 'mvSuSiE-RSS Oracle residual',
                    'mnm_rss_oracle+identity' = 'mvSuSiE-RSS Identity residual',
                    'mnm_rss_oracle+nullz' = 'mvSuSiE-RSS z cor residual', 
                    'mnm_rss_oracle+corY' = 'mvSuSiE-RSS Y cor residual')

rename_priors_oracle_resid = list('susie_suff+TRUE' = 'SuSiE',
                                  'susie_rss+TRUE' = 'SuSiE-RSS',
                                  'mnm_suff_oracle+oracle' = 'mvSuSiE Oracle prior',
                                  'mnm_suff_ed+oracle' = 'mvSuSiE ED prior',
                                  'mnm_rss_oracle+oracle' = 'mvSuSiE-RSS Oracle prior',
                                  'mnm_rss_identity+oracle' = 'mvSuSiE-RSS Random effects prior',
                                  'mnm_rss_shared+oracle' = 'mvSuSiE-RSS Fixed effect prior',
                                  'mnm_rss_naive+oracle' = 'mvSuSiE-RSS Default prior',
                                  'mnm_rss_ed+oracle' = 'mvSuSiE-RSS ED prior'
                                  )

rename_priors = list('susie_suff+TRUE' = 'SuSiE','susie_rss+TRUE' = 'SuSiE-RSS',
                     'mnm_suff_oracle+covY' = 'mvSuSiE Oracle prior',
                     'mnm_suff_ed+covY' = 'mvSuSiE ED prior',
                     'mnm_rss_oracle+nullz' = 'mvSuSiE-RSS Oracle prior',
                     'mnm_rss_identity_corZ+nullz' = 'mvSuSiE-RSS Random effects prior',
                     'mnm_rss_shared_corZ+nullz' = 'mvSuSiE-RSS Fixed effect prior',
                     'mnm_rss_naive_corZ+nullz' = 'mvSuSiE-RSS Default prior',
                     'mnm_rss_ed_corZ+nullz' = 'mvSuSiE-RSS ED prior'
                     )

dot_plot = function(dataframe, rename_list) {
  ggplot(dataframe, aes(x=mean_pip, y=observed_freq)) + 
    geom_errorbar(aes(ymin=observed_freq-se, ymax=observed_freq+se), colour="gray", size = 0.2, width=.01) +
    geom_point(size=1.5, shape=21, fill="#002b36") + # 21 is filled circle
    xlab("Mean PIP") +
    ylab("Observed frequency") +
    coord_cartesian(ylim=c(0,1), xlim=c(0,1)) +
    geom_abline(slope=1,intercept=0,colour='red', size=0.2) +
    ggtitle(rename_list[[name]]) +
    expand_limits(y=0) +                        # Expand y range
    theme_cowplot() + theme(plot.title = element_text(size = 8))
}

# parameters
bin_size = 10
# parameters
simulate_method = c('artificial_mixture_ukb', 'ukb_bloodcells_mixture')
level = c('glob', 'cond')
# level = 'cond'
all.comb = expand.grid(simulate_method, level)
colnames(all.comb) = c('simulate_method', 'level')

for(case in 1:nrow(all.comb)){
  simu = all.comb[case, 'simulate_method']
  level = all.comb[case, 'level']
  input = paste0('ukb_rss_20210313_pip_extraction/ukb_rss_pip_simu', simu, '_', level,'.rds')
  output = paste0('ukb_rss_20210313_pip_calibration/ukb_rss_pip_cali_simu', simu, '_', level)

  # dat = readRDS(input)
  
  # s <- split(data_in_bin[,2], cumsum(c(TRUE, diff(data_in_bin[,2]) != 0)))
  # s[[which.max(lengths(s))]]
  
  # bins = cbind(seq(1:bin_size)/bin_size-1/bin_size, seq(1:bin_size)/bin_size)
  # pip_cali = list()
  # for (method in names(dat)) {
  #   pip_cali[[method]] = matrix(NA, nrow(bins), 3)
  #   for (i in 1:nrow(bins)) {
  #     data_in_bin = dat[[method]][which(dat[[method]][,1] > bins[i,1] & dat[[method]][,1] < bins[i,2]),]
  #     if(!is.null(dim(data_in_bin))) {
  #       pip_cali[[method]][i,1] = sum(data_in_bin[,1])
  #       pip_cali[[method]][i,2] = sum(data_in_bin[,2])
  #       pip_cali[[method]][i,3] = nrow(data_in_bin)
  #     } else {
  #       pip_cali[[method]][i,] = c(0,0,0) 
  #     }
  #   }
  # }
  # for (method in names(dat)) {
  #   pip_cali[[method]][,c(1,2)] = pip_cali[[method]][,c(1,2)] / pip_cali[[method]][,3]
  # }
  # saveRDS(pip_cali, paste0(output, '.rds'))

  pip_cali = readRDS(paste0(output, '.rds'))
  idx = 0
  for (name in names(rename_resid)) {
    if(level == 'glob'){
      if(grepl('susie', name, fixed=TRUE)){
        next
      }
    }
    idx = idx + 1
    pip_cali[[name]][,3] = sqrt(pip_cali[[name]][,2] * (1 - pip_cali[[name]][,2]) / pip_cali[[name]][,3]) * 2
    pip_cali[[name]] = as.data.frame(pip_cali[[name]])
    colnames(pip_cali[[name]]) = c("mean_pip", "observed_freq", "se")
    pdf(paste0(output, '_resid_' , idx, '.pdf'), width=3, height=3, pointsize=16)
    print(dot_plot(pip_cali[[name]], rename_resid))
    dev.off()
    system(paste0("convert -flatten -density 120 ", output, '_resid_' , idx, '.pdf', " ",output, '_resid_' , idx, '.png'))
  }

  files = paste0(output, '_resid_', 1:idx, '.png')
  # files = paste0(output, '_', c(1,2,4,6), '.png')
  # output2 = paste0('ukb_rss_20210107_pip_calibration_paper/ukb_rss_pip_cali_simu', simu, '_', level)
  cmd = paste('convert +append', paste(files, collapse=" "), paste0(output, '_resid.png'))
  system(cmd)
  system(paste('rm -f', paste(files, collapse=" ")))
  
  pip_cali = readRDS(paste0(output, '.rds'))
  idx = 0
  for (name in names(rename_priors_oracle_resid)) {
    if(level == 'glob'){
      if(grepl('susie', name, fixed=TRUE)){
        next
      }
    }
    idx = idx + 1
    pip_cali[[name]][,3] = sqrt(pip_cali[[name]][,2] * (1 - pip_cali[[name]][,2]) / pip_cali[[name]][,3]) * 2
    pip_cali[[name]] = as.data.frame(pip_cali[[name]])
    colnames(pip_cali[[name]]) = c("mean_pip", "observed_freq", "se")
    pdf(paste0(output, '_priors_oracleresid_' , idx, '.pdf'), width=3, height=3, pointsize=16)
    print(dot_plot(pip_cali[[name]], rename_priors_oracle_resid))
    dev.off()
    system(paste0("convert -flatten -density 120 ", output, '_priors_oracleresid_' , idx, '.pdf', " ",output, '_priors_oracleresid_' , idx, '.png'))
  }
  
  files = paste0(output, '_priors_oracleresid_', 1:idx, '.png')
  # files = paste0(output, '_', c(1,2,4,6), '.png')
  # output2 = paste0('ukb_rss_20210107_pip_calibration_paper/ukb_rss_pip_cali_simu', simu, '_', level)
  cmd = paste('convert +append', paste(files, collapse=" "), paste0(output, '_priors_oracleresid.png'))
  system(cmd)
  system(paste('rm -f', paste(files, collapse=" ")))
  
  pip_cali = readRDS(paste0(output, '.rds'))
  idx = 0
  for (name in names(rename_priors)) {
    if(level == 'glob'){
      if(grepl('susie', name, fixed=TRUE)){
        next
      }
    }
    idx = idx + 1
    pip_cali[[name]][,3] = sqrt(pip_cali[[name]][,2] * (1 - pip_cali[[name]][,2]) / pip_cali[[name]][,3]) * 2
    pip_cali[[name]] = as.data.frame(pip_cali[[name]])
    colnames(pip_cali[[name]]) = c("mean_pip", "observed_freq", "se")
    pdf(paste0(output, '_priors_' , idx, '.pdf'), width=3, height=3, pointsize=16)
    print(dot_plot(pip_cali[[name]], rename_priors))
    dev.off()
    system(paste0("convert -flatten -density 120 ", output, '_priors_' , idx, '.pdf', " ",output, '_priors_' , idx, '.png'))
  }
  files = paste0(output, '_priors_', 1:idx, '.png')
  # files = paste0(output, '_', c(1,2,4,6), '.png')
  # output2 = paste0('ukb_rss_20210107_pip_calibration_paper/ukb_rss_pip_cali_simu', simu, '_', level)
  cmd = paste('convert +append', paste(files, collapse=" "), paste0(output, '_priors.png'))
  system(cmd)
  system(paste('rm -f', paste(files, collapse=" ")))
  
}


