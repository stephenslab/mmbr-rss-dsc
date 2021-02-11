library(dscrutils)
out = dscrutils::dscquery('output/ukb_rss_small', targets = c('artificial_mixture_small', 'artificial_mixture_small.eff_mode',
                                                      'mnm_suff_oracle','mnm_suff_oracle.resid_method', 'mnm_suff_oracle.DSC_TIME',
                                                      'mnm_rss_oracle', 'mnm_rss_oracle.resid_method','mnm_rss_oracle.DSC_TIME', 
                                                      'mnm_rss_naive', 'mnm_rss_naive.resid_method','mnm_rss_naive.DSC_TIME', 
                                                      'PAINTOR', 'PAINTOR.pip', 'PAINTOR.DSC_TIME',
                                                      'mvsusie_scores', 'mvsusie_scores.total', 'mvsusie_scores.valid', 'mvsusie_scores.size', 
                                                      'mvsusie_scores.purity', 'mvsusie_scores.top', 'mvsusie_scores.n_causal', 
                                                      'mvsusie_scores.included_causal', 'mvsusie_scores.overlap_var', 
                                                      'mvsusie_scores.overlap_cs',
                                                      'mvsusie_scores.total_cond_discoveries',
                                                      'mvsusie_scores.false_pos_cond_discoveries', 
                                                      'mvsusie_scores.false_neg_cond_discoveries', 'mvsusie_scores.true_cond_discoveries', 
                                                      'mvsusie_scores.size_cond_cs', 'mvsusie_scores.purity_cond_cs', 'mvsusie_scores.cs_correlation',
                                                      'mvsusie_scores.converged'),
                          module.output.files = c('artificial_mixture_small', 'PAINTOR','mnm_suff_oracle', 'mnm_rss_oracle', 'mnm_rss_naive'), 
                          ignore.missing.files = TRUE, 
                          groups = c('mnm:', 'mnm_suff:', 'mnm_rss:', 'susie:', 'simulate:'))

saveRDS(out, 'result/ukb_rss_small/ukb_rss_small.rds')
