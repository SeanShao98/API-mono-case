# API, structural estimation based on NiN, functions
# Oct 28, 25
# wd: ~/Documents/project_main/API_drug
library(Matrix)

# 1. demand side -----------------------------------------------------------------------------------
fun_hdfe_demean <- function(dt_var, dt_g, tol = 1e-10, maxit = 1000L) {
  ### demean fixed effect dummy (high dimension)
  ### dt_var: N * K,  dt of variables
  ### dt_g  : N * gi, dt of group-indicators
  ### ==>   : N * K,  dt of demeaned variables
  
  cln_g <- names(dt_g); cln_v <- names(dt_var)
  res <- lapply(cln_v, function(v_name) {
    dt <- data.table(dt_g, v = dt_var[[v_name]])
    for (it in seq_len(maxit)) {
      # message(paste0("it = ", it))
      v0 <- dt$v
      for (g_name in cln_g) {
        dt[, v := v - mean(v), by = get(g_name)]
      }
      if (sqrt(mean((v0 - dt$v)^2)) < tol) break
    }
    dt$v
  })
  res <- as.data.table(res)
  setnames(res, cln_v)
  return(res)
}

fun_matrix_prep <- function(dt_share, cols_Z, cols_X, cols_fe) {
  ### prepare common matrix for 2SLS
  ### dt_share: N * full, raw data
  ### cols_** : chr vector of literal string for tidyselect::contains
  
  # extract Z and X from raw data, and remove FE by with-in group demeaning
  dt_g <- dt_share %>% select(contains(cols_fe))
  Z <- dt_share %>%
    select(contains(cols_Z)) %>%
    fun_hdfe_demean(dt_g = dt_g) %>%
    as.matrix()
  X <- dt_share %>%
    mutate(lns_mjg = log(s_mjg)) %>%
    select("p_jm", contains(cols_X), "lns_mjg") %>%
    fun_hdfe_demean(dt_g = dt_g) %>%
    as.matrix()
  
  # pre-calculation, fixed matrix
  ZtZ <- Matrix::crossprod(Z)     # Z'Z
  ZtX <- Matrix::crossprod(Z, X)  # Z'X
  # pre-calculation, operator     # (Z'Z)^(-1) B
  chol_ZtZ <- Cholesky(forceSymmetric(ZtZ), LDL = TRUE)
  solve_ZtZ <- function(B) solve(chol_ZtZ, B)
  # pre-calculation, fixed matrix
  ZtZ_inv_ZtX <- solve_ZtZ(ZtX)       # (Z'Z)^(-1) Z'X
  XtPX <- Matrix::crossprod(ZtX, ZtZ_inv_ZtX) # X'Z (Z'Z)^(-1) Z'X = X'PX
  
  # return
  list(
    dt_g = dt_g, X = X, Z = Z, ZtX = ZtX,
    solve_ZtZ = solve_ZtZ, # operator,     (Z'Z)^(-1) B
    XtPX = XtPX            # fixed matrix, X'P_ZX
    # not included
    # chol_ZtZ = chol_ZtZ, # Cholesky obj.
    # ZtZ = ZtZ,           # Z'Z, only use operator
  )
}

fun_GMM_value <- function(rho, rho_id, dt_share, prep) {
  ### update Xi given pre-calculated matrix
  ### rho     : sum n(rho_id) length non-linear parameter
  ### rho_id  : id of rho (aa, city, tt)
  ### dt_share: N * full, raw data
  ### prep    : prep. list
  
  # unpack rho
  rho_ls  <- lapply(rho_id, \(nm) unisrt(dt_share[[nm]]))
  rho_len <- vapply(rho_ls, length, integer(1))
  rho_idx <- cumsum(rho_len)
  rho_sum <- dt_share[, ..rho_id][
    , paste0("rho_", rho_id) :=
      Map(
        \(nm, len, end, vals) {
          v <- rho[(end - len + 1L):end]
          v <- v - mean(v)
          v[match(dt_share[[nm]], vals)]
        },
        nm = rho_id, len = rho_len, end = rho_idx, vals = rho_ls
      )
  ][, rowSums(.SD), .SDcols = paste0("rho_", rho_id)]
  
  # update Y using rho
  Y <- with(dt_share, {
    log(quantity / Q_m) - log(rho_ac_0 * (rho_ac_lb + log1p(exp(rho_sum))) * N_ct / Q_m - 1)
  }) %>% data.table(lhs = .) %>%
    fun_hdfe_demean(dt_g = prep$dt_g) %>%
    as.matrix()
  
  # update XtPY
  ZtY  <- Matrix::crossprod(prep$Z, Y)   # Z'Y
  XtPY <- Matrix::crossprod(             # X'PY
    prep$ZtX,           # Z'X
    prep$solve_ZtZ(ZtY) # (Z'Z)^(-1) Z'Y
  )
  
  # 2SLS: X'PX * theta = X'PY
  theta <- solve(prep$XtPX, XtPY)
  Xi <- Y - prep$X %*% theta
  
  # GMM object value
  ZtXi <- Matrix::crossprod(prep$Z, Xi)   # Z'Xi
  Matrix::crossprod(     # Xi'Z (Z'Z)^(-1) Z'Xi
    ZtXi,
    prep$solve_ZtZ(ZtXi) # (Z'Z)^(-1) Z'Xi
  ) %>%
    as.numeric() %>%
    return()
}
