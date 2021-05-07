plot_sharing = function(X, to_cor=FALSE, title="", remove_names=F) {
  clrs <- colorRampPalette(rev(c("#D73027","#FC8D59","#FEE090","#FFFFBF",
                                 "#E0F3F8","#91BFDB","#4575B4")))(128)
  if (to_cor) lat <- cov2cor(X)
  else lat = X/max(diag(X))
  lat[lower.tri(lat)] <- NA
  n <- nrow(lat)
  if (remove_names) {
    colnames(lat) = NULL
    rownames(lat) = NULL
  }
  return(lattice::levelplot(lat[n:1,],col.regions = clrs,
                            xlab = "",ylab = "", main=title,
                            colorkey = TRUE,at = seq(-1,1,length.out = 128),
                            scales = list(cex = 0.6,x = list(rot = 45))))
}

arg = commandArgs(trailingOnly = TRUE)
name = arg[1]
# number of components to show
max_comp = -1
# whether or not to convert to correlation
tocor = FALSE
tol = 1E-6
remove_label = T
ED = TRUE

dat = readRDS('~/GitHub/mvarbvs/dsc/ukb-bloodcells/ukb_prior_simulation.rds')


if (name != "") {
  if (is.null(dat[[name]])) stop(paste0("Cannot find data ", name))
  dat = dat[[name]]
}

if(ED){
 dat = dat$ED 
}
if (is.null(names(dat$U))) names(dat$U) = paste0("Comp_", 1:length(dat$U))
meta = data.frame(names(dat$U), dat$w, stringsAsFactors=F)
colnames(meta) = c("U", "w")
n_comp = length(meta$U[which(dat$w>tol)])
meta = head(meta[order(meta[,2], decreasing = T),], ifelse(max_comp>1, max_comp, nrow(meta)))
message(paste(n_comp, "components out of", length(dat$w), "total components have weight greater than", tol))
res = list()
for (i in 1:n_comp) {
  Uname = gsub('_default', '', meta$U[i])
  title = paste(Uname, "w =", round(meta$w[i], 6))
  res[[i]] = plot_sharing(dat$U[[meta$U[i]]], 
                          to_cor = ifelse(tocor, T, F), title=title, 
                          remove_names = ifelse(remove_label, T, F))
}
unit = 4
n_col = 4
n_row = ceiling(n_comp / n_col)

output = paste0('~/GitHub/mvarbvs/dsc/mnm_prototype/result/ukb_rss_20210313/ukb_prior_simulation_', name, '_ED.pdf')
pdf(output, width = unit * n_col, height = unit * n_row)
do.call(gridExtra::grid.arrange, c(res, list(ncol = n_col, nrow = n_row)))
dev.off()

