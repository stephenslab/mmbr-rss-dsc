library(dscrutils)
out = dscrutils::dscquery('output/ukb_rss_small', targets = c('simulate_small', 'simulate_small.eff_mode',
                                                              'mnm_method','mnm_method.resid_method', 'mnm_method.DSC_TIME',
                                                              'PAINTOR', 'PAINTOR.pip', 'PAINTOR.DSC_TIME',
                                                              'mvsusie_scores', 'mvsusie_scores.total', 'mvsusie_scores.valid', 'mvsusie_scores.size', 
                                                              'mvsusie_scores.purity', 'mvsusie_scores.top', 'mvsusie_scores.n_causal', 
                                                              'mvsusie_scores.included_causal', 'mvsusie_scores.overlap_var', 
                                                              'mvsusie_scores.overlap_cs',
                                                              'mvsusie_scores.total_cond_discoveries',
                                                              'mvsusie_scores.false_pos_cond_discoveries', 
                                                              'mvsusie_scores.false_neg_cond_discoveries', 'mvsusie_scores.true_cond_discoveries', 
                                                              'mvsusie_scores.size_cond_cs', 'mvsusie_scores.purity_cond_cs', 
                                                              'mvsusie_scores.cs_correlation','mvsusie_scores.converged'),
                          module.output.files = c('simulate_small', 'PAINTOR','mnm_method'), 
                          ignore.missing.files = TRUE, 
                          groups = c('mnm_method: mnm_suff_naive, mnm_rss_naive, mnm_rss_naive_corY, mnm_rss_naive_corZ',
                                     'mnm:','mnm_rss:','mnm_suff:'))

saveRDS(out, 'result/ukb_rss_small/ukb_rss_small.rds')
