# MAIN PROCESS: synth
## Continue foo dt
pos.treat <- which(sort(unique(dt_prep_i$jz)) == i.jz)
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
  dt_prep_i[tt == i.p[[2]] & jz != i.jz,
            i.p[[1]], with = FALSE] %>% unlist() %>% sd()
}, simplify = T) %>% {
  covariate.list[. != 0]
} -> covariate.list

covariate.list.price <- append(
  covariate.list,
  sapply(price.year.span, function(tt) {list("p.tjz", tt, "mean")}, simplify = F)
)

## Synth-prep and Synth-main
Synth::dataprep(foo = dt_prep_i,
                predictors = c("Q.tjz", "R.tjz"),
                special.predictors = covariate.list.price,
                predictors.op = "mean",
                time.predictors.prior = train.span,
                dependent = "p.tjz",
                unit.variable = "jz",
                time.variable = "tt",
                treatment.identifier = i.jz,
                controls.identifier = sort(unique(dt_prep_i$jz))[-pos.treat],
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

synP_national <- dt_prep_i[tt %in% main.span,
                           .(tt, timeYQ, jz, jz_name, p.tjz)] %>%
  rbind(data.table(
    tt = main.span,
    timeYQ = sapply(main.span, function(tt) {
      paste((tt - 1) %/% 4 + 2013, (tt - 1) %% 4 + 1, sep = "-")
    }),
    product = 0, productName = "Synthetic Group",
    price = synDT_national$Y0plot %*% synRS_national$solution.w
  ), use.names = FALSE)

## Synth-Plot
ggplot(data = synP_national, mapping = aes(x = tt, y = p.tjz)) +
  geom_line(data = synP_national[jz == i.jz],
            mapping = aes(linetype = "treat")) +
  geom_line(data = synP_national[jz == 0],
            mapping = aes(linetype = "syn")) +
  scale_linetype_manual(name = "",
                        values = c("treat" = "solid", "syn" = "dashed"),
                        labels = c("Synth", "Treat")) +
  geom_vline(xintercept = case.span[1], linetype = "dotted") +
  geom_vline(xintercept = case.span[length(case.span)],
             linetype = "dotted") +
  annotate("rect", xmin = case.span[1], xmax = case.span[length(case.span)],
           ymin = -Inf, ymax = Inf, alpha=0.3, fill="grey") +
  labs(x = "Time: Year-Quarter", y = "Price/CNY") +
  scale_x_continuous(breaks = main.span,
                     labels = ifelse(main.span %% 3 == 0, plot.span, "")) +
  theme_classic() -> p_syn_national_plain

## Damage Calculating
data.table(
  YearQuater  = synP_national[jz == i.jz & tt %in% treat.span, timeYQ],
  Price.Treat = synP_national[jz == i.jz & tt %in% treat.span, p.tjz],
  Price.Syn   = synP_national[jz == 0 & tt %in% treat.span, p.tjz],
  Q.sum       = dt_prep_i[jz == i.jz & tt %in% treat.span, Q.tjz]
) %>%
  `[`(, R.DELTA := Q.sum * (Price.Treat - Price.Syn)) %>%
  mutate_at(2:3, ~ round(., 1)) %>%
  rbind(data.table("Total", "", "",  sum(.$Q.sum), sum(.$R.DELTA)),
        use.names = FALSE) -> dt_dmg

## Damage I/O
dt_dmg[, Price.Treat := format(Price.Treat, digits = 2)] %>%
  `[`(, Price.Syn := format(Price.Syn, digits = 2)) %>%
  `[`(, Q.sum := format(Q.sum, big.mark = ",")) %>%
  `[`(, R.DELTA := format(round(R.DELTA), big.mark = ",")) %>%
  `names<-`(c("Time", "\\(P_{jt}\\)", "\\(\\hat{P}^N_{jt}\\)",
              "\\(Q_{jt}\\)", "Damage")) %>%
  outTex_tbl(file.name = paste0("output/", folder.head, "/",
                                str_replace(i.jz.name, "/", "-"),
                                "_dmg", ".tex"))

## Summary I/O
dt_prep_i[, c("N.firm","CR1","CR2","CR3","HHI","Q.tjz","R.tjz","p.tjz"),
          with = FALSE] %>%
  psych::describe() %>% `[`(, c("n", "mean", "sd", "min", "median", "max")) %>%
  round(2) %>% format(big.mark = ",") %>%
  as.data.table(keep.rownames = TRUE) %>%
  `names<-`(c("Stat.", "N", "Mean", "St. Dev.", "Min", "Median", "Max")) %>%
  `[`(, `Stat.` := c("\\(N^f_{jt}\\)", "\\(CR1_{jt}\\)", "\\(CR2_{jt}\\)",
                     "\\(CR3_{jt}\\)", "\\(HHI_{jt}\\)","\\(Q_{jt}\\)",
                     "\\(R_{jt}\\)", "\\(P_{jt}\\)")) %>%
  outTex_tbl(file.name = paste0("output/", folder.head, "/",
                                str_replace(i.jz.name, "/", "-"),
                                "_sum", ".tex"))

## Plot I/O
p_syn_national_plain +
  geom_text(data = synP_national[jz %in% c(0, i.jz) &
                                   tt %in% main.span[main.span %% 5 == 2],
                                 .(tt, p.tjz = round(p.tjz, 2))],
            mapping = aes(x = tt, y = p.tjz, label = p.tjz),
            vjust = 2, size = 2.7) +
  geom_point(data = synP_national[jz %in% c(0, i.jz) &
                                    tt %in% main.span[main.span %% 5 == 2],
                                  .(tt, p.tjz)],
             mapping = aes(x = tt, y = p.tjz), shape = 1) -> p_syn_national_plain

ggsave(filename = paste0("output/", folder.head, "/scp_",
                         str_replace(i.jz.name, "/", "-"),
                         ".pdf"),
       plot = p_syn_national_plain,
       width = 9, height = 6.5)

## final I/O
as.data.table(synRS_national$solution.w, keep.rownames = T) %>%
  `[`(order(w.weight, decreasing = T)) %>%
  mutate_all(~ as.numeric(.)) -> dt_weight.w

if (exists("FLAG.is_proc_robust") && FLAG.is_proc_robust == TRUE) {
  out_list <- list(
    i.jz_0 = list(
      "robust.jz" = i.jz,
      "robust.jz.name" = unique(dt_tjz[jz == i.jz, jz_name]),
      "DT" = synP_national,
      "weight.w" = dt_weight.w,
      "weight.v" = synRS_national$solution.v
    )
  ) %>% `names<-`(i.jz.name)
  
} else {
  out_list <- list(
    i.jz = list(
      "DT" = synP_national,
      "weight.w" = dt_weight.w,
      "weight.v" = synRS_national$solution.v
    )
  ) %>% `names<-`(i.jz.name)
}

save(synP_national, out_list,
     file = paste0("output/", folder.head, "/data_collect/sc_data_",
                   str_extract(i.jz.name, "[\u4e00-\u9fa5]+"),
                   ".RData"))
## Remark
print(p_syn_national_plain)
print(dt_dmg[.N, R.DELTA])






