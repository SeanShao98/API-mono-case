# API-mono-case
# Aug 14, 24
# Author: GitHub@SeanShao98
# 0. initializing ------------------------------------------------
setwd("~/***")
source("ini.R")
library(cowplot)
library(grid)
library(fixest)
load("MAIN.RData")

options(modelsummary_format_numeric_latex = "plain")
options(modelsummary_factory_latex = "kableExtra")
options(modelsummary_factory_html = "kableExtra")
f_gof_fmt <- function(x) format(round(x, 3), big.mark = ",")
f_gof_add <- function(model) data.frame(
  "dfr" = format(summary(model)$df[2], big.mark = ",", trim = T)
)
f_gof_add_2 <- function(model) data.frame(
  "dfr" = format(summary(model)$nobs - summary(model)$nparams,
                 big.mark = ",", trim = T)
)

font_add(family = "Hei", regular = "~/***")
showtext_auto()

# 1. AG robust in unit ------------------------------------------------
## 1.1. Full ------------------------------------------------
load("SCM_OUT_c1_top10.RData")

TT = 34; folder.head <- "scm_AG_out"; FLAG.is_proc_robust = T
clust <- makeCluster(11)
clusterExport(clust, varlist = c(
  "ls_out_full", "TT", "folder.head", "FLAG.is_proc_robust",
  "dt_jz_info", "dt_tjz", "vec_jz_all", "outTex_tbl"
))
clusterEvalQ(clust, {
  library(dplyr); library(Synth); library(ggplot2); library(stringr)
})
parSapply(clust, dt_jz_info$prod_jz, function(i.jz.name) {
  i.jz <- ls_out_full[[i.jz.name]][[i.jz.name]][["weight.w"]]$rn[1]
  i.jz_0 <- unique(dt_tjz[jz_name == i.jz.name, jz])
  i.jz.name_1 <- unique(dt_tjz[jz == i.jz, jz_name])
  message(paste0("Now SCM with ", i.jz.name_1))
  dt_prep_i <- dt_tjz[category2 == unique(dt_tjz[jz == i.jz, category2]) &
                        !(jz_name %in% vec_jz_all),] %>%
    setkey(jz, tt) %>%
    `[`(tt %in% .[jz_name == i.jz.name_1, tt]) %>%
    `[`(, N.t := .N, by = jz) %>%
    `[`(N.t == unique(.[jz == i.jz, N.t]))
  train.span <- intersect(c(2:dt_jz_info[prod_jz == i.jz.name, start - 1]),
                          dt_prep_i[jz == i.jz, tt])
  case.span <- intersect(c(dt_jz_info[prod_jz == i.jz.name, start]:
                             dt_jz_info[prod_jz == i.jz.name, end]),
                         dt_prep_i[jz == i.jz, tt])
  treat.span <- intersect(c(dt_jz_info[prod_jz == i.jz.name, start]:TT),
                          dt_prep_i[jz == i.jz, tt])
  source("p1_sc.R", local = T)
  return(out_list)
}, simplify = FALSE) -> ls_out_full
stopCluster(clust)

save(ls_out_full, file = "SCM_AG_OUT_c1_top10_full.RData")

## 1.2. Top 10/20 ------------------------------------------------
load("SCM_OUT_c2.RData")
ls_out_full_org <- ls_out_full
load("SCM_OUT_c2_top10.RData")

vec_top_notworking <- c("盐酸溴己新注射液-2ml:4mg",
                        "盐酸异丙肾上腺素注射液-2ml:1mg")
# vec_top_notworking <- c()
TT = 34; folder.head <- "scm_AG_out"; FLAG.is_proc_robust = T
clust <- makeCluster(11)
clusterExport(clust, varlist = c(
  "dt_jz_info", "dt_tjz",
  "TT", "folder.head", "FLAG.is_proc_robust",
  "vec_jz_all", "outTex_tbl",
  "ls_out_full_org", "ls_out_full", "vec_top_notworking"
))
clusterEvalQ(clust, {
  library(dplyr); library(Synth); library(ggplot2); library(stringr)
})
parSapply(clust, dt_jz_info$prod_jz, function(i.jz.name) {
  i.jz <- ls_out_full[[i.jz.name]][[i.jz.name]][["weight.w"]]$rn[1]
  i.jz_0 <- unique(dt_tjz[jz_name == i.jz.name, jz])
  i.jz.name_1 <- unique(dt_tjz[jz == i.jz, jz_name])
  message(paste0("Now SCM with ", i.jz.name_1))
  ## top w vec
  donor.top <- ls_out_full_org[[i.jz.name]][[1]][["weight.w"]] %>%
    as.data.table() %>% `[`(order(w.weight, decreasing = T)) %>%
    `[`(1:10, rn) %>% `[`(!is.na(.))
  ## same dt_prep
  dt_prep_i <- dt_tjz[category2 == unique(dt_tjz[jz == i.jz, category2]) &
                        !(jz_name %in% vec_jz_all),] %>%
    setkey(jz, tt) %>%
    `[`(tt %in% .[jz_name == i.jz.name_1, tt]) %>%
    `[`(, N.t := .N, by = jz) %>%
    `[`(N.t == unique(.[jz == i.jz, N.t]))
  ## keep only top w
  if (!(i.jz.name %in% vec_top_notworking)) {
    dt_prep_i <- dt_prep_i[jz %in% donor.top]
  }
  train.span <- intersect(c(2:dt_jz_info[prod_jz == i.jz.name, start - 1]),
                          dt_prep_i[jz == i.jz, tt])
  case.span <- intersect(c(dt_jz_info[prod_jz == i.jz.name, start]:
                             dt_jz_info[prod_jz == i.jz.name, end]),
                         dt_prep_i[jz == i.jz, tt])
  treat.span <- intersect(c(dt_jz_info[prod_jz == i.jz.name, start]:TT),
                          dt_prep_i[jz == i.jz, tt])
  source("p1_sc.R", local = T)
  return(out_list)
}, simplify = FALSE) -> ls_out_full
stopCluster(clust)
save(ls_out_full, file = "SCM_AG_OUT_c2_top10.RData")

## 1.3. all in one SCM plot, robust ---------------------------------------------
load("SCM_AG_OUT_c2_top20.RData")
sapply(1:length(ls_out_full), function(n.dt) {
  dt <- ls_out_full[[n.dt]][[1]]$DT
  jz.rohust <- ls_out_full[[n.dt]][[1]]$robust.jz.name
  dt <- dt[jz_name %in% c("Synthetic Group", jz.rohust)] %>%
    `[`(jz == 0, jz := unique(.[jz != 0, jz])) %>%
    `[`(, is.syc := jz_name == "Synthetic Group")
  dt[is.syc == FALSE, jz_name := names(ls_out_full)[n.dt]]
  dt[, jz := unique(dt_tjz[jz_name == names(ls_out_full)[n.dt], unique(jz)])]
  return(dt)
}, simplify = FALSE) %>% rbindlist() -> dt_syc_plot

unique(dt_inall[, .(jz_name = prod_jz, prod_jz_eng)])[dt_syc_plot, on = "jz_name"] %>%
  `[`(, prod_jz_eng := unique(prod_jz_eng[!is.na(prod_jz_eng)]),
      by = jz) -> dt_syc_plot

dt_syc_plot %<>%
  mutate(p.tjz = if_else(jz == 5121, log(2.5 + p.tjz), log(1 + p.tjz)),
         is.horizontal = vlookup(jz, dt_jz_info[, .(jz, horizontal)]),
         is.dominant = vlookup(jz, dt_jz_info[, .(jz, dominant)]),
         is.both = vlookup(jz, dt_jz_info[, .(jz, and(horizontal, dominant))]),
         m.class = ifelse(is.both, "Both",
                          ifelse(is.horizontal, "Cartel", "Abuse")))

ggplot(data = dt_syc_plot, mapping = aes(x = tt, y = p.tjz)) +
  geom_line(mapping = aes(linetype = is.syc)) +
  geom_vline(data = dt_jz_info,
             mapping = aes(xintercept = start),
             linetype = "dotted") +
  geom_vline(data = dt_jz_info,
             mapping = aes(xintercept = end),
             linetype = "dotted") +
  geom_rect(data = dt_jz_info,
            mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            alpha=0.3, fill="grey", inherit.aes = F) +
  facet_wrap( ~ prod_jz_eng, scales = "free") +
  scale_linetype_manual(values = c("TRUE" = "dashed",
                                   "FALSE" = "solid")) +
  scale_x_continuous(breaks = c(1:34)[1:34 %% 7 == 1],
                     labels = names(vec_yq)[1:34 %% 7 == 1]) +
  labs(x = "Year-Quarter", y = "log(1 + Price)") +
  guides(color = "none", linetype = "none") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 9)
  ) -> p_syn_inall_robust

ggsave(filename = "output/p_syn_inall_robust.pdf",
       plot = p_syn_inall_robust,
       width = 15, height = 10)


### grouped
ggplot(data = dt_syc_plot[m.class == "Abuse", ],
       mapping = aes(x = tt, y = p.tjz)) +
  facet_wrap(~ prod_jz_eng, scales = "free", ncol = 5) +
  geom_line(mapping = aes(linetype = is.syc)) +
  geom_vline(data = dt_jz_info[horizontal == F & dominant == T,],
             mapping = aes(xintercept = start),
             linetype = "dotted") +
  geom_vline(data = dt_jz_info[horizontal == F & dominant == T,],
             mapping = aes(xintercept = end),
             linetype = "dotted") +
  geom_rect(data = dt_jz_info[horizontal == F & dominant == T,],
            mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            alpha=0.3, fill="grey", inherit.aes = F) +
  scale_linetype_manual(values = c("TRUE" = "dashed",
                                   "FALSE" = "solid")) +
  scale_x_continuous(breaks = c(1:34)[1:34 %% 7 == 1],
                     labels = names(vec_yq)[1:34 %% 7 == 1]) +
  labs(x = "Abuse cases, Year-Quarter",
       y = " Abuse cases, log(1 + Price)") +
  guides(color = "none", linetype = "none") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 9)
  ) -> p_syn_abuse

ggplot(data = dt_syc_plot[m.class == "Cartel", ],
       mapping = aes(x = tt, y = p.tjz)) +
  facet_wrap(~ prod_jz_eng, scales = "free", ncol = 5) +
  geom_line(mapping = aes(linetype = is.syc)) +
  geom_vline(data = dt_jz_info[horizontal == T & dominant == F,],
             mapping = aes(xintercept = start),
             linetype = "dotted") +
  geom_vline(data = dt_jz_info[horizontal == T & dominant == F,],
             mapping = aes(xintercept = end),
             linetype = "dotted") +
  geom_rect(data = dt_jz_info[horizontal == T & dominant == F,],
            mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            alpha=0.3, fill="grey", inherit.aes = F) +
  scale_linetype_manual(values = c("TRUE" = "dashed",
                                   "FALSE" = "solid")) +
  scale_x_continuous(breaks = c(1:34)[1:34 %% 7 == 1],
                     labels = names(vec_yq)[1:34 %% 7 == 1]) +
  labs(x = "Cartel cases, Year-Quarter",
       y = " Cartel cases, log(1 + Price)") +
  guides(color = "none", linetype = "none") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 9)
  ) -> p_syn_cartel

ggplot(data = add_row(dt_syc_plot[m.class == "Both", ], prod_jz_eng = "ZZZ"),
       mapping = aes(x = tt, y = p.tjz)) +
  facet_wrap(~ prod_jz_eng, scales = "free", ncol = 5, drop = FALSE) +
  geom_line(mapping = aes(linetype = is.syc)) +
  geom_vline(data = dt_jz_info[horizontal == T & dominant == T,],
             mapping = aes(xintercept = start),
             linetype = "dotted") +
  geom_vline(data = dt_jz_info[horizontal == T & dominant == T,],
             mapping = aes(xintercept = end),
             linetype = "dotted") +
  geom_rect(data = dt_jz_info[horizontal == T & dominant == T,],
            mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            alpha=0.3, fill="grey", inherit.aes = F) +
  scale_linetype_manual(values = c("TRUE" = "dashed",
                                   "FALSE" = "solid")) +
  scale_x_continuous(breaks = c(1:34)[1:34 %% 7 == 1],
                     labels = names(vec_yq)[1:34 %% 7 == 1]) +
  labs(x = "Cartel and abuse, Year-Quarter",
       y = " Cartel and Abuse, log(1 + Price)") +
  guides(color = "none", linetype = "none") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 9)
  ) -> p_syn_both
p_syn_both_edited <- ggplotGrob(p_syn_both)
p_syn_both_edited$grobs[[31]]$grobs[[1]] <- nullGrob()

plot_grid(
  p_syn_abuse, p_syn_cartel, p_syn_both_edited,
  nrow = 3,
  rel_heights = c(2.7, 1, 1),
  align = "hv", axis = "tblr"
) -> p_syn_inall_robust_grouped

ggsave(filename = "output/p_syn_inall_robust_grouped.pdf",
       plot = p_syn_inall_robust_grouped,
       width = 15, height = 17)
## 1.4. AG DiD ---------------------------------------------
vec_lm_dt <- c("c1", "c1_top10", "c2_top10_full", "c2_top20", "c2_top20_full")

for (dt.i in vec_lm_dt) {
  load(paste0("SCM_AG_OUT_", dt.i, ".RData"))
  sapply(1:length(ls_out_full), function(n.dt) {
    dt <- ls_out_full[[n.dt]][[1]][["DT"]]
    dt[jz_name %in% c("Synthetic Group", ls_out_full[[n.dt]][[1]][["robust.jz.name"]])] %>%
      `[`(jz == 0, jz := unique(.[jz != 0, jz]) + 10000) %>%
      `[`(, jz_real := unique(.[jz < 10000, jz])) %>%
      `[`(jz > 10000, jz_name := paste0("Syc-", unique(.[jz < 10000, jz_name]))) %>%
      `[`(, after.shock := tt >= dt_jz_info[prod_jz == names(ls_out_full)[n.dt],
                                            start]) %>%
      `[`(, in.shock := tt %in%
            dt_jz_info[prod_jz == names(ls_out_full)[n.dt], start]:
            dt_jz_info[prod_jz == names(ls_out_full)[n.dt], end]) %>%
      `[`(, is.syc := jz > 10000) %>% return()
  }, simplify = FALSE) %>%
    rbindlist() -> dt_syc_all
  dt_tjz[
    , .(tt, jz_real = jz, product,
        Q.tjz, N.firm, HHI, CR1, CR2, CR3,
        generalName, category1, category2, category3)
  ][dt_syc_all, on = .(tt, jz_real)] -> dt_syc_all
  ### 3did only: append full data
  dt_tjz[!(jz %in% unique(dt_syc_all$jz_real)),
         .(tt, jz_real = jz, product, p.tjz,
           Q.tjz, N.firm, HHI, CR1, CR2, CR3,
           generalName, category1, category2, category3, timeYQ, jz, jz_name, 
           after.shock = F, in.shock = F, is.syc = F, is.vertical = F)] %>%
    rbind(dt_syc_all[after.shock == in.shock][, is.vertical := T]) -> dt_syc_3d
  
  ### reform part of variables
  dt_syc_3d %<>%
    mutate(
      CR1_cut = CR1 > 0.5,
      CR2_cut = CR2 > 0.5,
      CR3_cut = CR3 > 0.5,
      year = str_sub(timeYQ, 1, 4)
    )
  ### regression
  # feols(
  #   fml = log(p.tjz) ~ in.shock + is.syc + in.shock:is.syc +
  #     tt + HHI + N.firm |
  #     factor(jz_real) + factor(tt),
  #   data = dt_syc_all[after.shock == in.shock],
  #   nthreads = 10,
  #   cluster = c("category3", "year"),
  # ) -> lm_scdid_AG_log
  ### regression, 3did
  feols(
    fml = log(p.tjz) ~ in.shock:is.vertical + is.syc:is.vertical +
      in.shock:is.syc:is.vertical + is.vertical +
      tt + N.firm + log(Q.tjz) |
      factor(product) + factor(tt),
    data = dt_syc_3d,
    nthreads = 10,
    cluster = c("category3", "year"),
  ) -> lm_scdid_AG_log
  ### export
  assign(paste0("lm_scdid_AG_log_", dt.i), lm_scdid_AG_log)
}

modelsummary(list(lm_scdid_AG_log_c1,
                  lm_scdid_AG_log_c1_top10,
                  lm_scdid_AG_log_c2_top10_full,
                  lm_scdid_AG_log_c2_top20,
                  lm_scdid_AG_log_c2_top20_full),
             coef_omit = "^factor.*", stars = T,
             output = "output/reg_scdid_AG.html")

dt_FE <- matrix(c("Prod. FE", rep("Yes", 5), "Time FE", rep("Yes", 5)),
                nrow = 2, byrow = T) %>% as.data.frame() %>%
  `attr<-`("position", c(13, 14))
modelsummary(models = list("Full" = lm_scdid_AG_log_c1,
                           "Top 20" = lm_scdid_AG_log_c2_top20,
                           "Top 20, full" = lm_scdid_AG_log_c2_top20_full,
                           "Top 10" = lm_scdid_AG_log_c1_top10,
                           "Top 10, full" = lm_scdid_AG_log_c2_top10_full),
             coef_omit = "^factor.*",
             coef_map = c(
               "is.verticalTRUE" =
                 "\\(D^{up}_{j}\\)",
               "is.verticalTRUE:is.sycTRUE" =
                 "\\(D^{up}_{j} \\times D^{syc}_{j}\\)",
               "in.shockTRUE:is.verticalTRUE" =
                 "\\(D^{up}_{j} \\times D^{m}_{t}\\)",
               "in.shockTRUE:is.verticalTRUE:is.sycTRUE" =
                 "\\(D^{up}_{j} \\times D^{syc}_{j} \\times D^{m}_{t}\\)",
               # "HHI" = "\\(\\textrm{HHI}_{jt}\\)",
               "N.firm" = "\\(N^f_{jt}\\)",
               "log(Q.tjz)" = "\\(\\log Q_{jt}\\)",
               "(Intercept)" = "Constant"
             ),
             add_rows = dt_FE,
             estimate = "{estimate}{stars}",
             stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
             escape = FALSE,
             gof_function = f_gof_add_2,
             gof_map = list(
               list("raw" = "nobs",
                    "clean" = "\\#Obs",
                    "fmt" = f_gof_fmt),
               list("raw" = "dfr",
                    "clean" = "DF.Residual",
                    "fmt" = f_gof_fmt),
               list("raw" = "r.squared",
                    "clean" = "\\(R^2\\)",
                    "fmt" = f_gof_fmt),
               list("raw" = "adj.r.squared",
                    "clean" = "Adj. \\(R^2\\)",
                    "fmt" = f_gof_fmt)
             ),
             output = "latex") %>%
  add_header_above(c("", "$\\\\log P_{jt}$" = 5), escape = FALSE) %>%
  str_remove("\\\\begin\\{table\\}\\n\\\\centering\\n") %>%
  str_remove("\\n\\\\end\\{table\\}") %>%
  cat(sep = "\n", file = "output/reg_scdid_AG.tex")

# 2. ADH placebo ------------------------------------------------
load("SCM_OUT_c2_top20.RData")

lapply(ls_out_full, function(ls) {
  ls[[1]][["weight.w"]]$rn[1:10] %>% na.omit() %>% as.numeric()
}) -> ls_weightx

TT = 34; folder.head <- "scm_ADH_out"
## parallel
sapply(dt_jz_info$prod_jz, function(i.jz.name) {
  print(paste0("Now ADH with ", i.jz.name))
  i.weightx <- ls_weightx[[i.jz.name]]
  i.jz <- unique(dt_tjz[jz_name == i.jz.name, jz])
  dt_prep_i <- dt_tjz[category2 == unique(dt_tjz[jz == i.jz, category2]) &
                        !(jz_name %in% setdiff(vec_jz_all, i.jz.name)),] %>%
    setkey(jz, tt) %>%
    `[`(tt %in% dt_tjz[jz_name == i.jz.name, tt]) %>%
    `[`(, N.t := .N, by = jz)
  source("p2_ADH.R", local = TRUE) ### return ls_ADH_gaps list of ADH results
  
  train_span <- c(2:dt_jz_info[prod_jz == i.jz.name, start - 1])
  rbindlist(ls_ADH_gaps) %>%
    mutate(MSPE = mean(gaps[tt %in% train_span] ^ 2), .by = jz.w) %>%
    return()
}, simplify = FALSE) -> ls_out_full
save(ls_out_full, file = "ADH.RData")

## 2.1. ADH plot ------------------------------------------------
load("ADH.RData")
rbindlist(ls_out_full) %>%
  mutate(MSPE_cut = 20 * unique(MSPE[jz == jz.w]), .by = jz) %>%
  mutate(
    prod_jz_eng = vlookup(vlookup(jz, dt_tjz[, .(jz, jz_name)]),
                          dt_jz_info[, .(prod_jz, prod_jz_eng)]),
    is.horizontal = vlookup(jz, dt_jz_info[, .(jz, horizontal)]),
    is.dominant = vlookup(jz, dt_jz_info[, .(jz, dominant)]),
    is.both = vlookup(jz, dt_jz_info[, .(jz, and(horizontal, dominant))]),
    m.class = ifelse(is.both, "Both",
                     ifelse(is.horizontal, "Cartel", "Abuse"))
  ) %>%
  filter(MSPE <= MSPE_cut) -> dt_ADH

ggplot(mapping = aes(x = tt, y = gaps, group = jz.w)) +
  geom_line(data = filter(dt_ADH, jz == jz.w),
            mapping = aes(color = "treat")) +
  geom_line(data = filter(dt_ADH, jz != jz.w),
            mapping = aes(color = "placebo"),
            alpha = 0.7) +
  geom_vline(data = dt_jz_info,
             mapping = aes(xintercept = start),
             linetype = "dotted") +
  geom_vline(data = dt_jz_info,
             mapping = aes(xintercept = end),
             linetype = "dotted") +
  geom_rect(data = dt_jz_info,
            mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            alpha=0.3, fill="grey", inherit.aes = F) +
  facet_wrap(~ prod_jz_eng, scales = "free") +
  scale_color_manual(name = "",
                     values = c("treat" = "black", "placebo" = "grey"),
                     labels = c("Treat", "Placebo"),
                     limits = c("treat", "placebo")) +
  labs(x = "Year-Quarter", y = "Price gap") +
  scale_x_continuous(breaks = c(1:34)[1:34 %% 7 == 1],
                     labels = names(vec_yq)[1:34 %% 7 == 1]) +
  guides(color = "none", linetype = "none") +
  theme_classic()

## refine
vec_jz_i <- unique(dt_ADH$prod_jz_eng)
n.i <- 1
while (n.i <= length(vec_jz_i)) {
  jz.i <- vec_jz_i[n.i]
  ggplot(mapping = aes(x = tt, y = gaps, group = jz.w)) +
    geom_line(data = filter(dt_ADH, jz == jz.w, prod_jz_eng == jz.i),
              mapping = aes(color = "treat")) +
    geom_line(data = filter(dt_ADH, jz != jz.w, prod_jz_eng == jz.i,
                            !(jz.w %in% c())),
              mapping = aes(color = "placebo"),
              alpha = 0.7) +
    geom_vline(data = dt_jz_info[prod_jz_eng == jz.i,],
               mapping = aes(xintercept = start),
               linetype = "dotted") +
    geom_vline(data = dt_jz_info[prod_jz_eng == jz.i,],
               mapping = aes(xintercept = end),
               linetype = "dotted") +
    geom_rect(data = dt_jz_info[prod_jz_eng == jz.i,],
              mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
              alpha=0.3, fill="grey", inherit.aes = F)
  n.i <- n.i + 1
}

ls_ADH_rm <- list(
  "Isoniazid Injection-2ml:100mg" = c(747),
  "Isoniazid Tablets-100mg" = c(762, 747),
  "Chlorpheniramine Maleate Tablets-4mg" = c(1296),
  "Bromhexine Hydrochloride for Injection-4mg" = c(4031),
  "Bromhexine Hydrochloride Injection-2ml:4mg" = 1471,
  "Compound Fluocinolone Acetonide Tincture-20ml" = c(1233, 1224),
  "Compound Beclomethasone Camphor Cream-10g" = c(1233, 1224, 109),
  "Camphor Oxide Injection-1n" = c(4663),
  "Pralidoxime Chloride Injection-2ml:500mg" = 479,
  "Epinephrine Hydrochloride Injection-1ml:1mg" = 3900,
  "Norepinephrine Bitartrate Injection-1n" = 4125
)

for (jz.i in names(ls_ADH_rm)) {
  dt_ADH %<>%
    filter(!(prod_jz_eng == jz.i & jz.w %in% ls_ADH_rm[[jz.i]]))
}

## grouped
ggplot(mapping = aes(x = tt, y = gaps, group = jz.w)) +
  geom_line(data = filter(dt_ADH, jz == jz.w, m.class == "Abuse"),
            mapping = aes(color = "treat")) +
  geom_line(data = filter(dt_ADH, jz != jz.w, m.class == "Abuse"),
            mapping = aes(color = "placebo"),
            alpha = 0.7) +
  geom_vline(data = dt_jz_info[horizontal == F & dominant == T,],
             mapping = aes(xintercept = start),
             linetype = "dotted") +
  geom_vline(data = dt_jz_info[horizontal == F & dominant == T,],
             mapping = aes(xintercept = end),
             linetype = "dotted") +
  geom_rect(data = dt_jz_info[horizontal == F & dominant == T,],
            mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            alpha=0.3, fill="grey", inherit.aes = F) +
  facet_wrap(~ prod_jz_eng, scales = "free", ncol = 5) +
  scale_color_manual(name = "",
                     values = c("treat" = "black", "placebo" = "grey"),
                     labels = c("Treat", "Placebo"),
                     limits = c("treat", "placebo")) +
  labs(x = "Abuse cases, Year-Quarter",
       y = expression("Abuse cases, " ~ p^{syc} - p)) +
  scale_x_continuous(breaks = c(1:34)[1:34 %% 7 == 1],
                     labels = names(vec_yq)[1:34 %% 7 == 1]) +
  guides(color = "none", linetype = "none") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 9)
  ) -> p_ADH_abuse

ggplot(mapping = aes(x = tt, y = gaps, group = jz.w)) +
  geom_line(data = filter(dt_ADH, jz == jz.w, m.class == "Cartel"),
            mapping = aes(color = "treat")) +
  geom_line(data = filter(dt_ADH, jz != jz.w, m.class == "Cartel"),
            mapping = aes(color = "placebo"),
            alpha = 0.7) +
  geom_vline(data = dt_jz_info[horizontal == T & dominant == F,],
             mapping = aes(xintercept = start),
             linetype = "dotted") +
  geom_vline(data = dt_jz_info[horizontal == T & dominant == F,],
             mapping = aes(xintercept = end),
             linetype = "dotted") +
  geom_rect(data = dt_jz_info[horizontal == T & dominant == F,],
            mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            alpha=0.3, fill="grey", inherit.aes = F) +
  facet_wrap(~ prod_jz_eng, scales = "free", ncol = 5) +
  scale_color_manual(name = "",
                     values = c("treat" = "black", "placebo" = "grey"),
                     labels = c("Treat", "Placebo"),
                     limits = c("treat", "placebo")) +
  labs(x = "Cartel cases, Year-Quarter",
       y = expression("Cartel cases, " ~ p^{syc} - p)) +
  scale_x_continuous(breaks = c(1:34)[1:34 %% 7 == 1],
                     labels = names(vec_yq)[1:34 %% 7 == 1]) +
  guides(color = "none", linetype = "none") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 9)
  ) -> p_ADH_cartel

ggplot(mapping = aes(x = tt, y = gaps, group = jz.w)) +
  geom_line(data = add_row(filter(dt_ADH, jz == jz.w, m.class == "Both"),
                           prod_jz_eng = "ZZZ"),
            mapping = aes(color = "treat")) +
  geom_line(data = add_row(filter(dt_ADH, jz != jz.w, m.class == "Both"),
                           prod_jz_eng = "ZZZ"),
            mapping = aes(color = "placebo"),
            alpha = 0.7) +
  geom_vline(data = dt_jz_info[horizontal == T & dominant == T,],
             mapping = aes(xintercept = start),
             linetype = "dotted") +
  geom_vline(data = dt_jz_info[horizontal == T & dominant == T,],
             mapping = aes(xintercept = end),
             linetype = "dotted") +
  geom_rect(data = dt_jz_info[horizontal == T & dominant == T,],
            mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            alpha=0.3, fill="grey", inherit.aes = F) +
  facet_wrap(~ prod_jz_eng, scales = "free", ncol = 5) +
  scale_color_manual(name = "",
                     values = c("treat" = "black", "placebo" = "grey"),
                     labels = c("Treat", "Placebo"),
                     limits = c("treat", "placebo")) +
  labs(x = "Cartel and abuse, Year-Quarter",
       y = expression("Abuse and cartel cases, " ~ p^{syc} - p)) +
  scale_x_continuous(breaks = c(1:34)[1:34 %% 7 == 1],
                     labels = names(vec_yq)[1:34 %% 7 == 1]) +
  guides(color = "none", linetype = "none") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 9)
  ) -> p_ADH_both
p_ADH_both_edited <- ggplotGrob(p_ADH_both)
p_ADH_both_edited$grobs[[31]]$grobs[[1]] <- nullGrob()

plot_grid(
  p_ADH_abuse, p_ADH_cartel, p_ADH_both_edited,
  nrow = 3,
  rel_heights = c(2.7, 1, 1),
  align = "hv", axis = "tblr"
) -> p_ADH_grouped

ggsave(filename = "output/p_ADH_grouped.pdf", plot = p_ADH_grouped,
       width = 15, height = 17)

# past --------------------------------------------------------------
dt_firm %>%
  #filter(str_detect(productName, "巴曲")) %>%
  filter(productName == "巴曲酶注射液") %>%
  summarise(price = mean(price), .by = tt) %>% {
    ggplot(data = ., mapping = aes(x = tt, y = price)) +
      geom_line() +
      scale_x_continuous(breaks = 0:33)
  }












