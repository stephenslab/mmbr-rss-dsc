# ---- mvsusie_missing_impute ----

## prior_variance is a matrix
## prior_variance and residual_variance are fixed in the algorithm.

## If compute_objective = TRUE, the stopping criteria is based on elbo;
## otherwise, it is based on changes in pip.
library(abind)
library(susieR)
mvsusie_missing_imp = function(X, Y, L=10, prior_variance, residual_variance, intercept = TRUE,
                               max_iter = 100, prior_weights=NULL, tol=1e-3, min_abs_corr=0.5, prior_tol=1E-9,
                               compute_objective = FALSE){
  if (is.null(dim(Y))) Y <- matrix(Y,length(Y),1)
  R <- ncol(Y)
  N <- nrow(Y)
  J <- ncol(X)
  Y_missing <- is.na(Y)
  Y_non_missing <- !Y_missing
  # store missing pattern, TRUE for observe, FALSE for missing
  missing_pattern <- unique(Y_non_missing)
  Y_missing_pattern_assign <- numeric(N)
  for(k in 1:nrow(missing_pattern)){
    idx = which(apply(Y_non_missing, 1, function(x) identical(x, missing_pattern[k,])))
    Y_missing_pattern_assign[idx] <- k
  }
  Y[Y_missing] <- 0

  ## compute inverse of residual variance for each missing pattern
  residual_variance_inv <- list()
  for(k in 1:nrow(missing_pattern)){
    if(R == 1){
      residual_variance_inv[[k]] <- missing_pattern[k,] / residual_variance
    }else{
      Vk = residual_variance[which(missing_pattern[k,]), which(missing_pattern[k,])]
      eigenVk <- eigen(Vk, symmetric = TRUE)
      dinv <- 1/(eigenVk$values)
      residual_variance_inv[[k]] <- eigenVk$vectors %*% (dinv * t(eigenVk$vectors))
    }
  }

  ## centering X
  if(intercept){
    cm_x <- colMeans(X)
    X <- t(t(X) - cm_x)
  }
  d <- colSums(X ^ 2)
  d[d == 0] <- 1E-6

  s = init(N, J, R, L, prior_variance, prior_weights)
  s$R = Y
  pip_history = list()
  if(compute_objective){
    elbo = c()
  }else{
    elbo = NA
  }
  for(iter in 1:max_iter){
    # impute missing Y
    mu = s$Xr
    for (i in 1:N){
      missing_pattern_i = !missing_pattern[Y_missing_pattern_assign[i],]
      residual_variance_mo = residual_variance[missing_pattern_i, !missing_pattern_i, drop=FALSE]
      if(any(missing_pattern_i)){
        imp_mean = mu[i, missing_pattern_i] + residual_variance_mo %*% residual_variance_inv[[Y_missing_pattern_assign[i]]] %*%
          (Y[i, !missing_pattern_i] - mu[i, !missing_pattern_i])
        Y[i,missing_pattern_i] = imp_mean

        y_var = matrix(0, R, R)
        y_var[missing_pattern_i, missing_pattern_i] = residual_variance[missing_pattern_i, missing_pattern_i] - residual_variance_mo %*%
          tcrossprod(residual_variance_inv[[Y_missing_pattern_assign[i]]], residual_variance_mo)
        s$extra_var[[i]] = y_var
      }else{
        s$extra_var[[i]] = matrix(0, R, R)
      }
      s$yologlik[i] = mvtnorm::dmvnorm(x = Y[i,!missing_pattern_i],mean=mu[i,!missing_pattern_i],
                           sigma = as.matrix(residual_variance[!missing_pattern_i, !missing_pattern_i]),
                           log = T)
    }
    v_inv = invert_via_chol(residual_variance)
    s$kly = sum(s$yologlik) + (N*R/2) * log(2*pi) + (N/2)*log(det(residual_variance)) +
      0.5 * tr(v_inv %*% crossprod(Y - mu)) - 0.5 * tr(v_inv %*% Reduce('+', s$extra_var))

    # centering Y
    if(intercept){
      cm_y <- colMeans(Y)
      Y <- t(t(Y) - cm_y)
    }

    s = update_each_effect(X, Y, s, residual_variance, d, compute_objective)

    pip_history[[iter]] = s$alpha
    if(compute_objective){
      elbo = c(elbo, get_elbo(s, X, Y, residual_variance))
    }
    if(iter > 1){
      if(compute_objective){
        if((elbo[iter]-elbo[iter-1])<tol) {
          s$niter = iter
          break;
        }
      }else{
        delta = max(abs(apply(1 - pip_history[[iter]], 2, prod) - apply(1 - pip_history[[iter-1]], 2, prod)))
        if(delta < tol){
          s$niter = iter
          s$delta = delta
          break
        }
      }
    }
  }
  s$elbo = elbo
  s$null_index = -9
  s$V = rep(1, L)
  s$sets = susie_get_cs(s, X=X, min_abs_corr=min_abs_corr)
  s$pip = susie_get_pip(s, prior_tol=prior_tol)
  return(s)
}

update_each_effect = function(X, Y, s, residual_variance, d, compute_objective) {
  # Repeat for each effect to update.
  L = nrow(s$alpha)
  if (L > 0)
    for (l in 1:L) {

      # Remove lth effect from fitted values.
      s$Xr = s$Xr - X %*% (s$alpha[l,] * s$mu[l,,])

      # Compute residuals.
      s$R = Y - s$Xr

      res = SER(X, s$R, residual_variance, d, s$prior, s$prior_weights)

      # Update the variational estimate of the posterior mean.
      s$alpha[l,]  = res$alpha
      s$mu[l,,]    = res$mu1
      s$mu2[l,,,]   = res$mu2
      s$lbf[l]     = res$lbf_model
      if(compute_objective){
        if (length(dim(res$mu2)) == 3) {
          pb2 = lapply(1:nrow(res$mu1), function(j) res$alpha[j] * res$mu2[j,,])
        } else {
          pb2 = lapply(1:nrow(res$mu1), function(j) res$alpha[j] * matrix(res$mu2[j,]))
        }
        v_inv = invert_via_chol(residual_variance)
        XtR = crossprod(X, s$R)
        B = s$alpha[l,] * s$mu[l,,]
        E1 = tr(v_inv %*% crossprod(B, XtR))
        E2 = tr(v_inv %*% crossprod(XtR, B))
        s$vbxxb[l] = sum(d * sapply(1:length(pb2), function(j) tr(v_inv %*% pb2[[j]])))
        s$kl[l] = (E1 + E2 - s$vbxxb[l]) / 2 - res$lbf_model
        # s$kl[l] = (E1 + E2) / 2 - res$lbf_model
      }
      s$Xr = s$Xr + X %*% (s$alpha[l,] * s$mu[l,,])
    }
  return(s)
}

SER = function(X, Y, residual_variance, d, prior, prior_weights = NULL){
  J = ncol(X)
  if (is.null(prior_weights))
    prior_weights = rep(1/J,J)
  XtY = crossprod(X,Y)
  post = multivariate_regression(XtY, residual_variance, d, prior)
  lbf = post$lbf
  maxlbf = max(lbf)
  # w is proportional to BF, but subtract max for numerical stability.
  w = exp(lbf - maxlbf)
  # Posterior prob for each SNP.
  w_weighted = w * prior_weights
  weighted_sum_w = sum(w_weighted)
  alpha = w_weighted / weighted_sum_w
  lbf_model = maxlbf + log(weighted_sum_w)
  return(list(alpha = alpha,mu1 = post$b1 , mu2 = post$b2 , lbf = lbf,
              lbf_model = lbf_model))
}

multivariate_regression = function(XtY, residual_variance, d, U) {
  v_inv = invert_via_chol(residual_variance)
  svs_inv = lapply(1:nrow(XtY), function(j) v_inv * d[j])
  post_cov = lapply(1:length(svs_inv), function(j) U %*% solve(diag(nrow(U)) + svs_inv[[j]] %*% U))
  lbf = sapply(1:length(svs_inv), function(j) -0.5 * log(det(diag(nrow(U)) + svs_inv[[j]] %*% U)) +
                 0.5*(t(XtY[j,]) %*% v_inv) %*%(post_cov[[j]]%*%(v_inv%*%XtY[j,])))
  lbf[which(is.nan(lbf))] = 0
  post_b1 = t(do.call(cbind, lapply(1:length(svs_inv), function(j) post_cov[[j]] %*% (v_inv %*% XtY[j,])))) # J by R
  post_b2 = lapply(1:length(post_cov), function(j) tcrossprod(post_b1[j,]) + post_cov[[j]]) # length J list with R by R matrix
  # deal with degerated case with 1 condition
  if (ncol(post_b1) == 1) {
    post_b2 = matrix(unlist(post_b2), length(post_b2), 1)
  } else {
    post_b2 = aperm(abind(post_b2, along = 3), c(3,1,2)) # J by R by R
  }
  return(list(b1 = post_b1, b2 = post_b2, lbf = lbf, cov = post_cov))
}

get_elbo = function(s, X, Y, residual_variance){
  N = nrow(Y)
  R = ncol(Y)

  expected_loglik = -(N * R / 2) * log(2*pi) - N / 2 * log(det(residual_variance))
  v_inv = invert_via_chol(residual_variance)
  XtX = crossprod(X)
  E1 = sapply(1:nrow(s$alpha), function(l){
    B = s$alpha[l,] * s$mu[l,,]
    tr(v_inv %*% crossprod(B, XtX %*% B))
  })
  E1 = tr(v_inv%*%crossprod(Y - s$Xr)) - sum(E1)

  E2 = tr(v_inv %*% Reduce('+', s$extra_var))
  essr = E1 + E2 + sum(s$vbxxb)

  expected_loglik - 0.5 * essr - sum(s$kl) - s$kly
}

## utils
invert_via_chol = function(x) {
  if (all(x==0)) return(x)
  return(chol2inv(muffled_chol(x)))
}

muffled_chol = function(x, ...){
  withCallingHandlers(chol(x, ...),
                      warning = function(w) {
                        if (grepl("the matrix is either rank-deficient or indefinite", w$message))
                          invokeRestart("muffleWarning")
                      })
}

init = function(N, J, R, L, U, prior_weights){
  if(is.null(prior_weights))
    prior_weights = rep(1/J,J)
  else
    prior_weights = prior_weights / sum(prior_weights)

  s = list(alpha  = matrix(1/J, L, J),
           mu     = array(0,dim = c(L, J, R)),
           mu2    = array(0,dim = c(L, J, R, R)),
           Xr     = matrix(0,N, R),
           prior      = U,
           prior_weights = prior_weights,
           kl = numeric(L),
           kly = 0,
           vbxxb = numeric(L),
           R = matrix(0,N, R),
           extra_var = list(),
           yologlik = numeric(N))
  class(s) = 'susie'
  return(s)
}

tr = function (m) {
  if (!is.matrix(m) | (dim(m)[1] != dim(m)[2]))
    stop("Input to tr() function must be a square matrix")
  return(sum(diag(m), na.rm = TRUE))
}

simulate_multivariate = function(n=100,p=100,r=2,center_scale=TRUE,y_missing=0) {
  set.seed(1)
  res = mmbr::mmbr_sim1(n,p,r,1,center_scale=center_scale,y_missing=y_missing)
  res$L = 10
  return(res)
}



