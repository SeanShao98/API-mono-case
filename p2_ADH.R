# MAIN PROCESS: ADH
clust <- makeCluster(11)
clusterExport(
  clust,
  varlist = c(
    "ls_weightx", "TT", "folder.head",
    "dt_jz_info", "dt_prep_i", "vec_jz_all", "outTex_tbl",
    "i.weightx", "i.jz.name", "i.jz"
  ),
  envir = environment()
)
clusterEvalQ(clust, {
  library(dplyr); library(Synth); library(ggplot2); library(stringr)
})
parSapply(clust, c(i.jz, i.weightx), function(i.wx) {
  dt_prep_iwx <- dt_prep_i[N.t == unique(dt_prep_i[jz == i.wx, N.t]), ]
  train.span <- intersect(c(2:dt_jz_info[prod_jz == i.jz.name, start - 1]),
                          dt_prep_iwx[jz == i.wx, tt])
  case.span <- intersect(c(dt_jz_info[prod_jz == i.jz.name, start]:
                             dt_jz_info[prod_jz == i.jz.name, end]),
                         dt_prep_iwx[jz == i.wx, tt])
  treat.span <- intersect(c(dt_jz_info[prod_jz == i.jz.name, start]:TT),
                          dt_prep_iwx[jz == i.wx, tt])
  
  ## Continue foo dt
  pos.treat <- which(sort(unique(dt_prep_iwx$jz)) == i.wx)
  main.span <- c(train.span, treat.span)
  plot.span <- c("2013-2", "2013-3", "2013-4", "2014-1", "2014-2", "2014-3",
                 "2014-4", "2015-1", "2015-2", "2015-3", "2015-4", "2016-1", 
                 "2016-2", "2016-3", "2016-4", "2017-1", "2017-2", "2017-3", 
                 "2017-4", "2018-1", "2018-2", "2018-3", "2018-4", "2019-1",
                 "2019-2", "2019-3", "2019-4", "2020-1", "2020-2", "2020-3",
                 "2020-4", "2021-1", "2021-2")[main.span - 1]
  covriate.year.span <- train.span
  price.year.span <- train.span
  covariate.list <- sapply(c("N.firm","CR1","CR2","CR3","HHI"), function(x) {
    sapply(covriate.year.span, function(tt) {list(x, tt, "mean")}, simplify = F)
  }, simplify = F) %>% unlist(recursive = F)
  
  ## remove linear dependent predictors
  sapply(covariate.list, function(i.p) {
    dt_prep_iwx[tt == i.p[[2]] & jz != i.wx,
                i.p[[1]], with = FALSE] %>% unlist() %>% sd()
  }, simplify = T) %>% {
    covariate.list[. != 0]
  } -> covariate.list
  
  covariate.list.price <- append(
    covariate.list,
    sapply(price.year.span, function(tt) {list("p.tjz", tt, "mean")}, simplify = F)
  )
  
  ## Synth-prep and Synth-main
  Synth::dataprep(foo = dt_prep_iwx,
                  predictors = c("Q.tjz", "R.tjz"),
                  special.predictors = covariate.list.price,
                  predictors.op = "mean",
                  time.predictors.prior = train.span,
                  dependent = "p.tjz",
                  unit.variable = "jz",
                  time.variable = "tt",
                  treatment.identifier = i.wx,
                  controls.identifier = sort(unique(dt_prep_iwx$jz))[-pos.treat],
                  time.optimize.ssr = train.span,
                  time.plot = main.span) -> synDT_national
  tryCatch(
    expr = {
      synRS_national <- Synth::synth(synDT_national)
    },
    error = function(e) {
      message(paste0(e, "N-M opt failed, try different"))
      synRS_national <<- Synth::synth(synDT_national, optimxmethod = "All")
    }
  )
  
  ### Retrieve result DT
  synP_national <- dt_prep_iwx[tt %in% main.span,
                               .(tt, timeYQ, jz, jz_name, p.tjz)] %>%
    rbind(data.table(
      tt = main.span,
      timeYQ = sapply(main.span, function(tt) {
        paste((tt - 1) %/% 4 + 2013, (tt - 1) %% 4 + 1, sep = "-")
      }),
      product = 0, productName = "Synthetic Group",
      price = synDT_national$Y0plot %*% synRS_national$solution.w
    ), use.names = FALSE)
  
  ### Gap ratio
  unit.gap <- synP_national[jz == i.wx, p.tjz] - synP_national[jz == 0, p.tjz]
  data.table(jz = i.jz, jz.w = i.wx, tt = main.span, gaps = unit.gap) %>%
    return()
}, simplify = FALSE) -> ls_ADH_gaps
stopCluster(clust)





