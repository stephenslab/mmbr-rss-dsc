library(dscrutils)
out = dscrutils::dscquery('output/ukb_rss', targets = c('simulate', 'simulate.eff_mode',
                                                      'mnm_suff_oracle','mnm_suff_oracle.resid_method', 'mnm_suff_oracle.DSC_TIME',
                                                      'mnm_rss', 'mnm_rss.resid_method','mnm_rss.DSC_TIME', 
                                                      'susie', 'susie.estimate_residual_variance', 'susie.DSC_TIME',
                                                      'mvsusie_scores', 'mvsusie_scores.total', 'mvsusie_scores.valid', 'mvsusie_scores.size', 
                                                      'mvsusie_scores.purity', 'mvsusie_scores.top', 'mvsusie_scores.n_causal', 
                                                      'mvsusie_scores.included_causal', 'mvsusie_scores.overlap_var', 
                                                      'mvsusie_scores.overlap_cs',
                                                      'mvsusie_scores.total_cond_discoveries',
                                                      'mvsusie_scores.false_pos_cond_discoveries', 
                                                      'mvsusie_scores.false_neg_cond_discoveries', 'mvsusie_scores.true_cond_discoveries', 
                                                      'mvsusie_scores.size_cond_cs', 'mvsusie_scores.purity_cond_cs', 'mvsusie_scores.cs_correlation',
                                                      'mvsusie_scores.converged', 
                                                      'susie_scores', 'susie_scores.total', 'susie_scores.valid', 'susie_scores.size',
                                                      'susie_scores.purity', 'susie_scores.n_causal', 'susie_scores.converged', 
                                                      'susie_scores.cs_correlation', 'susie_scores.pip'),
                          module.output.files = c('simulate', 'mnm_suff_oracle','mnm_rss', 'susie'), 
                          ignore.missing.files = TRUE, 
                          groups = c('mnm:', 'mnm_suff:'))

saveRDS(out, 'result/ukb_rss_20210107/ukb_rss_20210107.rds')
