library(scam)
roc_data = function(d1, cutoff = c(pip_cutoff, 0.999), connect_org = F) {
  grid = 1000
  ttv = seq(1:grid)/grid
  ttv = ttv[which(ttv>=cutoff[1] & ttv<=cutoff[2])]
  rst1 = t(sapply(ttv, function(x) c(sum(d1[,2][d1[,1]>=x]), length(d1[,2][d1[,1]>=x]), sum(d1[,2][d1[,1]>=x]==0))))
  rst1 = cbind(rst1, sum(d1[,2]), sum(1-d1[,2]))
  rst1 = as.data.frame(rst1)
  colnames(rst1) = c('true_positive', 'total_positive', 'false_positive', 'total_signal', 'total_null')
  rst2 = as.data.frame(cbind(rst1$true_positive / rst1$total_positive, rst1$true_positive / rst1$total_signal,  ttv))
  rst3 = as.data.frame(cbind(1 - rst1$false_positive / rst1$total_null, rst1$true_positive / rst1$total_signal,  ttv))
  if (connect_org) {
    # make a stair to origin
    rst2 = rbind(rst2, c(max(0.995, rst2[nrow(rst2),1]), max(rst2[nrow(rst2),2]-0.01, 0), rst2[nrow(rst2),3]))
    rst2 = rbind(rst2, c(1, 0, 1))
    rst3 = rbind(rst3, c(1, 0, 1))
  }
  colnames(rst2) = c('Precision', 'Recall', 'Threshold')
  colnames(rst3) = c('TN', 'TP', 'Threshold')
  return(list(counts = rst1, pr = rst2, roc = rst3))
}
create_chunks = function(item, n) {
  splitted = suppressWarnings(split(item, 1:n))
  return(c(splitted[[1]], splitted[[length(splitted)]][length(splitted[[length(splitted)]])]))
}
make_smooth = function(x,y,subset=chunks, smooth = FALSE) {
  if (smooth) {
    if (subset < length(x) && subset > 0) {
      x = create_chunks(x, subset)
      y = create_chunks(y, subset)
    }
    dat = data.frame(cbind(x,y))
    colnames(dat) = c('x','y')
    y=predict(scam(y ~ s(x, bs = "mpi"), data = dat))
  }
  return(list(x=x,y=y))
}
add_text = function(thresholds, x, y, threshold, color, delta = -0.06) {
  idx = which(thresholds == threshold)
  # text(x[idx] - delta, y[idx], labels = threshold, col = color, cex=0.8)
  points(x[idx],y[idx], col = color)
}

colors = c('#A60628', '#7A68A6', '#348ABD', '#467821', '#FF00FF', '#E2A233', 
           '#00FFFF', '#A9A9A9', '#ADFF2F', '#188487',  '#FF0000', '#000000', 
           '#FFD700', '#00FF00', '#9400D3', '#7FFFD4', '#A52A2A', '#000080')

simulate_method = c('artificial_mixture_ukb', 'ukb_bloodcells_mixture')
level = c('glob', 'cond')
# level = 'cond'
all.comb = expand.grid(simulate_method, level)
colnames(all.comb) = c('simulate_method', 'level')

rename_resid = list('mnm_rss_oracle+oracle' = 'mvSuSiE-RSS Oracle residual',
                    'mnm_rss_oracle+identity' = 'mvSuSiE-RSS Identity residual',
                    'mnm_rss_oracle+nullz' = 'mvSuSiE-RSS z cor residual', 
                    'mnm_rss_oracle+corY' = 'mvSuSiE-RSS Y cor residual', 
                    'mnm_suff_oracle+oracle' = 'mvSuSiE Oracle residual',
                    'mnm_suff_oracle+covY' = 'mvSuSiE Y cov residual',
                    'susie_suff+TRUE' = 'SuSiE',
                    'susie_rss+TRUE' = 'SuSiE-RSS',
                    'susie_rss+FALSE' = 'SuSiE-RSS fixed residual variance')

rename_priors_oracle_resid = list('mnm_rss_naive+oracle' = 'mvSuSiE-RSS Default prior',
                                  'mnm_rss_ed+oracle' = 'mvSuSiE-RSS ED prior',
                                  'mnm_rss_identity+oracle' = 'mvSuSiE-RSS Random effects prior',
                                  'mnm_rss_shared+oracle' = 'mvSuSiE-RSS Fixed effect prior',
                                  'mnm_rss_oracle+oracle' = 'mvSuSiE-RSS Oracle prior',
                                  'mnm_suff_oracle+oracle' = 'mvSuSiE Oracle prior',
                                  'mnm_suff_ed+oracle' = 'mvSuSiE ED prior',
                                  'susie_suff+TRUE' = 'SuSiE',
                                  'susie_rss+TRUE' = 'SuSiE-RSS',
                                  'susie_rss+FALSE' = 'SuSiE-RSS fixed residual variance')

rename_priors = list('mnm_rss_shared_corZ+nullz' = 'mvSuSiE-RSS Fixed effect prior',
                     'mnm_rss_naive_corZ+nullz' = 'mvSuSiE-RSS Default prior',
                     'mnm_rss_ed_corZ+nullz' = 'mvSuSiE-RSS ED prior',
                     'mnm_rss_identity_corZ+nullz' = 'mvSuSiE-RSS Random effects prior',
                     'mnm_rss_oracle+nullz' = 'mvSuSiE-RSS Oracle prior', 
                     'mnm_suff_oracle+covY' = 'mvSuSiE Oracle prior',
                     'mnm_suff_ed+covY' = 'mvSuSiE ED prior',
                     'susie_suff+TRUE' = 'SuSiE','susie_rss+TRUE' = 'SuSiE-RSS',
                     'susie_rss+FALSE' = 'SuSiE-RSS fixed residual variance')

pip_cutoff = 0.05
chunks = 0
xlim = 0.8
ylim = 0.8

for(case in 1:nrow(all.comb)){
  simu = all.comb[case, 'simulate_method']
  level = all.comb[case, 'level']
  input = paste0('ukb_rss_20210313_pip_extraction/ukb_rss_pip_simu', simu,'_',level,'.rds')
  output = paste0('ukb_rss_20210313_roc/ukb_rss_roc_simu', simu, '_', level)
  
  # dat = readRDS(input)
  # 
  # print("Computing ROC data ...")
  # tb = list()
  # for (method in names(dat)) {
  #   print(method)
  #   tb[[method]] = roc_data(dat[[method]])
  # }
  # saveRDS(tb, paste0(output, '.rds'))
  
  tb = readRDS(paste0(output, '.rds'))
  for(type in c('pr', 'roc')){
    if(type == 'pr'){
      main = "FDR vs Power"
      ylab = "power"
      xlab = "FDR"
      xlim = 0.8
      ylim = 0.8
    }else{
      main = "ROC curve"
      ylab = "True Positive"
      xlab = "False Positive"
      xlim = 0.002
      ylim = 0.8
    }
    pdf(paste0(output,'.', type,'residual.pdf'), width=10, height=10, pointsize=15)
    i = 1
    labels = vector()
    for (method in c('mnm_suff_oracle+oracle', 'mnm_suff_oracle+covY',
                     'mnm_rss_oracle+oracle', 'mnm_rss_oracle+identity',
                     'mnm_rss_oracle+nullz', 'mnm_rss_oracle+corY',
                     'susie_suff+TRUE', 'susie_rss+TRUE')) {
      yy = make_smooth((1 - tb[[method]][[type]][,1]), tb[[method]][[type]][,2])
      if (i == 1) {
        plot(yy$x, yy$y, t="l", col=colors[i], ylab = ylab, xlab = xlab, main = main,
             bty='l',lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
      } else {
        lines(yy$x, yy$y, col=colors[i], lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
      }
      add_text(tb[[method]][[type]][,3], yy$x, yy$y, 0.95, colors[i])
      labels[i] = rename_resid[[method]]
      i = i + 1
    }
    legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.7)
    dev.off()
    system(paste0("convert -flatten -density 120 ", output, '.', type,'residual.pdf', " ", output, '.', type, 'residual.png'))

    pdf(paste0(output,'.', type,'prior_oracleresid.pdf'), width=10, height=10, pointsize=15)
    i = 1
    labels = vector()
    for (method in c('mnm_suff_oracle+oracle', 'mnm_rss_oracle+oracle',
                     'mnm_rss_naive+oracle', 'mnm_rss_ed+oracle',
                     'mnm_rss_identity+oracle', 'mnm_rss_shared+oracle',
                     'susie_suff+TRUE', 'susie_rss+TRUE')) {
      yy = make_smooth((1 - tb[[method]][[type]][,1]), tb[[method]][[type]][,2])
      if (i == 1) {
        plot(yy$x, yy$y, t="l", col=colors[i], ylab = ylab, xlab = xlab, main = paste0(main, ": using oracle residuals"), bty='l',
             lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
      } else {
        lines(yy$x, yy$y, col=colors[i], lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
      }
      add_text(tb[[method]][[type]][,3], yy$x, yy$y, 0.95, colors[i])
      labels[i] = rename_priors_oracle_resid[[method]]
      i = i + 1
    }
    legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.7)
    dev.off()
    system(paste0("convert -flatten -density 120 ", output, '.', type,'prior_oracleresid.pdf', 
                  " ", output, '.', type, 'prior_oracleresid.png'))
    
    pdf(paste0(output,'.', type,'prior.pdf'), width=10, height=10, pointsize=15)
    i = 1
    labels = vector()
    for (method in c('mnm_suff_oracle+covY', 'mnm_rss_oracle+nullz',
                     'mnm_rss_naive_corZ+nullz', 'mnm_rss_ed_corZ+nullz',
                     'mnm_rss_identity_corZ+nullz', 'mnm_rss_shared_corZ+nullz',
                     'susie_suff+TRUE', 'susie_rss+TRUE')) {
      yy = make_smooth((1 - tb[[method]][[type]][,1]), tb[[method]][[type]][,2])
      if (i == 1) {
        plot(yy$x, yy$y, t="l", col=colors[i], ylab = ylab, xlab = xlab, main = main, bty='l',
             lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
      } else {
        lines(yy$x, yy$y, col=colors[i], lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
      }
      add_text(tb[[method]][[type]][,3], yy$x, yy$y, 0.95, colors[i])
      labels[i] = rename_priors[[method]]
      i = i + 1
    }
    legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.7)
    dev.off()
    system(paste0("convert -flatten -density 120 ", output, '.', type,'prior.pdf', " ", output, '.', type, 'prior.png'))
  }
}

