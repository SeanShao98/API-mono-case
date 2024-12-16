# API-mono-case
# May 20, 24
# Author: GitHub@SeanShao98
# 0. initializing ------------------------------------------------
setwd("~/***")
library(cowplot)
library(grid)
library(fixest)
source("ini.R")
## load("chemical_cleaned.RData")
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

# 1. Descriptive plot ---------------------------------------------
## 1.1. case ---------------------------------------------
dt_case_tab <- read_excel("case_tab.xlsx") %>%
  as.data.table() %>%
  `names<-`(c("APIs", "Time of admin. penalty",
              "\\#. involved firms", "\\multicolumn{1}{c}{Duration}", "Types",
              "Behaviors", "Penalties amt. (million)")) %>%
  mutate(across(Behaviors, ~ str_replace_all(., "、|；", ", "))) %>%
  mutate(across(4, ~ str_replace_all(., "-", " - ")))
dt_case_tab$`Penalties amt. (million)` %<>% sapply(function (x) {
  if (!is.na(as.numeric(x))) {
    return(format(round(as.numeric(x), 3)))
  } else {
    return(x)
  }
})
outTex_tbl(dt_case_tab,
           file.name = "output/case_tab.tex")

## 1.2. firm-level data ---------------------------------------------
vec_yq <- c(1:34) %>% `names<-`(sort(unique(dt_firm[, timeYQ])))
dt_firm[productName == "复方倍氯米松樟脑乳膏" & size == "20g",
        c("size", "size.rescale") := .("10g", 10)]
dt_firm[productName == "氧化樟脑注射液",
        c("size", "size.rescale") := .("1n", 1)]
dt_firm[productName %in% c("左卡尼汀注射液", "注射用左卡尼汀"),
        c("quantity", "size", "size.rescale") :=
          .(quantity * size.rescale / min(size.rescale), "1n", 1), 
        by = .(product)]
dt_firm[productName %in% c("重酒石酸去甲肾上腺素注射液"),
        c("size", "size.rescale") := .("1n", 1)]
dt_tjfz <- dt_firm[, .(tt, timeYQ, product, productName, firm, city,
                       size, size.rescale, quantity, amount,
                       N.firm, HHI, CR1, CR2, CR3,
                       category1, category2, category3,
                       generalName, route, form)] %>%
  `[`(, n.size.min := size.rescale/min(size.rescale), by = product) %>%
  ## `[`(, c("Q.tjf", "R.tjf", "p.tjf") :=
  ##       .(sum(quantity * n.size.min), sum(amount),
  ##         sum(amount)/sum(quantity * n.size.min)),
  ##     by = c(tt, product, firm)) %>%
  `[`(, c("Q.tjfz", "R.tjfz", "p.tjfz") :=
        .(sum(quantity), sum(amount), sum(amount)/sum(quantity)),
      by = .(tt, product, firm, size.rescale)) %>%
  unique(by = c("tt", "product", "firm", "size.rescale")) %>%
  `[`(, .(tt, timeYQ, product, productName, firm, size, size.rescale,
          Q.tjfz, R.tjfz, p.tjfz, N.firm, HHI, CR1, CR2, CR3,
          category1, category2, category3, generalName, route, form))
## 1.3. plot ---------------------------------------------
### one-by-one
sapply(1:length(ls_api_drug), function(n.api) {
  if (length(ls_api_interest[[n.api]]) == 1) {
    dt_tjfz[productName %in% ls_api_interest[[n.api]][["prod"]]] -> dt_api
  } else {
    do.call(rbind, ls_api_interest[[n.api]]) %>%
      apply(MARGIN = 2, function(xx) {
        dt_tjfz[productName == xx[1] & size == xx[2]]
      }) %>% rbindlist() -> dt_api
  }
  if (dim(dt_api)[1] == 0) {
    print(paste0("Skip ", names(ls_api_interest)[n.api]))
  } else {
    api <- ls_api_drug[[n.api]]
    dt_api[, prod_jz := paste(productName, size, sep = "-")] %>%
      `[`(, p_avg_tjz := weighted.mean(p.tjfz, Q.tjfz),
          by = .(tt, product, size.rescale)) %>%
      `[`(, is.vi := sapply(firm, function(x) any(str_detect(x, api$firm)))) %>%
      `[`(, p_avg_f :=  weighted.mean(p.tjfz, Q.tjfz),
          by = .(tt, product, size.rescale, is.vi)) %>% {
            ggplot(data = ., mapping = aes(x = tt)) +
              geom_line(mapping = aes(group = firm, y = p.tjfz,
                                      color = is.vi, linetype = is.vi),
                        alpha = 0.5) +
              geom_line(mapping = aes(y = p_avg_tjz),
                        linetype = "solid") +
              geom_vline(xintercept = vec_yq[api$penal], linetype = "dashed") +
              annotate("rect",
                       xmin = vec_yq[api$start],
                       xmax = vec_yq[api$end],
                       ymin = -Inf, ymax = Inf, alpha=0.3, fill="grey") +
              facet_wrap( ~ prod_jz, scales = "free") +
              scale_x_continuous(breaks = c(1:34)[1:34 %% 7 == 1],
                                 labels = names(vec_yq)[1:34 %% 7 == 1]) +
              scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
              scale_linetype_manual(values = c("TRUE" = "solid",
                                               "FALSE" = "dotted")) +
              guides(color = "none") +
              labs(x = "Time: Year-Quarter", y = "Price/CNY") +
              theme(text = element_text(family = "Hei")) +
              theme_classic()
          } -> p_prod_ft
    ggsave(filename = paste0("output/plot_trend/",
                             names(ls_api_drug)[n.api], ".pdf"),
           plot = p_prod_ft,
           width = 10, height = 10)
    ### vertical integrated firm
    unique(dt_api, by = c("tt", "product", "size.rescale", "is.vi")) %>% {
      ggplot(data = ., mapping = aes(x = tt)) +
        geom_line(mapping = aes(group = is.vi, y = p_avg_f, color = is.vi),
                  linetype = "solid") +
        geom_vline(xintercept = vec_yq[api$penal], linetype = "dashed") +
        annotate("rect",
                 xmin = vec_yq[api$start],
                 xmax = vec_yq[api$end],
                 ymin = -Inf, ymax = Inf, alpha=0.3, fill="grey") +
        facet_wrap( ~ prod_jz, scales = "free") +
        scale_x_continuous(breaks = c(1:34)[1:34 %% 7 == 1],
                           labels = names(vec_yq)[1:34 %% 7 == 1]) +
        scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
        labs(x = "Time: Year-Quarter", y = "Price/CNY") +
        theme(text = element_text(family = "Hei")) +
        guides(color = "none") +
        theme_classic()
    } -> p_prod_ft_vi
    ggsave(filename = paste0("output/vertical integrated/",
                             names(ls_api_drug)[n.api], ".pdf"),
           plot = p_prod_ft_vi,
           width = 10, height = 10)
  }
})

### all-in-one
dt_inall <- NULL
for (n.api in 1:length(ls_api_drug)) {
  if (length(ls_api_interest[[n.api]]) == 1) {
    dt_tjfz[productName %in% ls_api_interest[[n.api]][["prod"]]] -> dt_api
  } else {
    do.call(rbind, ls_api_interest[[n.api]]) %>%
      apply(MARGIN = 2, function(xx) {
        dt_tjfz[productName == xx[1] & size == xx[2]]
      }) %>% rbindlist() -> dt_api
  }
  if (dim(dt_api)[1] == 0) {
    dt_api <- NULL
  } else {
    api <- ls_api_drug[[n.api]]
    dt_api <- dt_api[, prod_jz := paste(productName, size, sep = "-")] %>%
      `[`(, p_avg_tjz := weighted.mean(p.tjfz, Q.tjfz),
          by = .(tt, product, size.rescale)) %>%
      `[`(, is.vi := sapply(firm, function(x) any(str_detect(x, api$firm)))) %>%
      `[`(, p_avg_f :=  weighted.mean(p.tjfz, Q.tjfz),
          by = .(tt, product, size.rescale, is.vi))
  }
  dt_inall <- rbind(dt_inall, dt_api)
}

sapply(unique(dt_inall$prod_jz), function(jz) {
  rex <- sapply(names(ls_api_interest), function(short) {str_detect(jz, short)})
  data.table(names(ls_api_interest)[rex],
             vec_yq[sapply(ls_api_drug, `[[`, "penal")[rex]],
             vec_yq[sapply(ls_api_drug, `[[`, "start")[rex]],
             vec_yq[sapply(ls_api_drug, `[[`, "end")[rex]],
             sapply(ls_api_drug, `[[`, "horizontal")[rex],
             sapply(ls_api_drug, `[[`, "vertical")[rex],
             sapply(ls_api_drug, `[[`, "dominant")[rex])
}, simplify = F) %>% rbindlist(idcol = "prod") %>%
  `names<-`(c("prod_jz", "cate", "penal", "start", "end", "horizontal",
              "vertical", "dominant")) -> dt_jz_info

vec_prodname_eng <- c(
  "Allopurinol Tablets",
  "Compound Beclomethasone Camphor Cream",
  "Compound Fluocinolone Acetonide Tincture",
  "L-Carnitine Injection",
  "Isoniazid Injection",
  "Isoniazid Tablets",
  "Camphor Oxide Injection",
  "Pralidoxime Chloride Injection",
  "Salicylic Acid Phenol Plaster",
  "L-Carnitine for Injection",
  "Bromhexine Hydrochloride for Injection",
  "Metaraminol Bitartrate Injection",
  "Isoproterenol Hydrochloride Injection",
  "Bromhexine Hydrochloride Injection",
  "Bromhexine Hydrochloride Tablets",
  "Bromhexine Hydrochloride Glucose Injection",
  "Epinephrine Hydrochloride Injection",
  "Iodized Oil Injection",
  "Estazolam Tablets",
  "Calcium Gluconate Injection",
  "Fluocinolone Acetonide Cream",
  "Norepinephrine Bitartrate Injection",
  "Chlorpheniramine Maleate Injection",
  "Chlorpheniramine Maleate Tablets"
) %>% `names<-`(sort(unique(dt_inall$productName)))

dt_inall[, prod_jz_eng := paste0(vec_prodname_eng[productName], "-", size)]
dt_jz_info[, prod_jz_eng :=
             unique(vlookup(prod_jz, dt_inall[, .(prod_jz, prod_jz_eng)]))]

dt_inall %<>%
  mutate(lnp.tjfz = log(1 + p.tjfz),
         lnp_avg_tjz = log(1 + p_avg_tjz),
         is.horizontal = vlookup(prod_jz, dt_jz_info[, .(prod_jz, horizontal)]),
         is.dominant = vlookup(prod_jz, dt_jz_info[, .(prod_jz, dominant)]),
         is.both = vlookup(prod_jz, dt_jz_info[, .(prod_jz, and(horizontal, dominant))]),
         m.class = ifelse(is.both, "Both",
                          ifelse(is.horizontal, "Cartel", "Abuse")))

p_inall <- ggplot(data = dt_inall, mapping = aes(x = tt)) +
  geom_line(mapping = aes(group = firm, y = lnp.tjfz),
            alpha = 0.5, linetype = "dotted") +
  geom_line(mapping = aes(y = lnp_avg_tjz),
            linetype = "solid") +
  geom_vline(data = dt_jz_info,
             mapping = aes(xintercept = penal),
             linetype = "dashed") +
  geom_rect(data = dt_jz_info,
            mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            alpha=0.3, fill="grey", inherit.aes = F) +
  facet_wrap( ~ prod_jz_eng, scales = "free") +
  scale_x_continuous(breaks = c(1:34)[1:34 %% 7 == 1],
                     labels = names(vec_yq)[1:34 %% 7 == 1]) +
  labs(x = "Time: Year-Quarter", y = "log(1 + Price), CNY") +
  theme(text = element_text(family = "Hei")) +
  theme_classic()

ggsave(filename = "output/plot_trend/all.pdf",
       plot = p_inall,
       width = 15, height = 10)

### grouped
ggplot(data = dt_inall[m.class == "Abuse"], mapping = aes(x = tt)) +
  geom_line(mapping = aes(group = firm, y = lnp.tjfz),
            alpha = 0.5, linetype = "dotted") +
  geom_line(mapping = aes(y = lnp_avg_tjz),
            linetype = "solid") +
  geom_vline(data = dt_jz_info[horizontal == F & dominant == T,],
             mapping = aes(xintercept = penal),
             linetype = "dashed") +
  geom_rect(data = dt_jz_info[horizontal == F & dominant == T,],
            mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            alpha=0.3, fill="grey", inherit.aes = F) +
  facet_wrap( ~ prod_jz_eng, scales = "free", ncol = 5) +
  scale_x_continuous(breaks = c(1:34)[1:34 %% 7 == 1],
                     labels = names(vec_yq)[1:34 %% 7 == 1]) +
  labs(x = "Abuse cases, Year-Quarter",
       y = " Abuse cases, log(1 + Price)") +
  theme(text = element_text(family = "Hei")) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 9)
  ) -> p_inall_abuse

ggplot(data = dt_inall[m.class == "Cartel"], mapping = aes(x = tt)) +
  geom_line(mapping = aes(group = firm, y = lnp.tjfz),
            linetype = "dotted", alpha = 0.5) +
  geom_line(mapping = aes(y = lnp_avg_tjz),
            linetype = "solid") +
  geom_vline(data = dt_jz_info[horizontal == T & dominant == F,],
             mapping = aes(xintercept = penal),
             linetype = "dashed") +
  geom_rect(data = dt_jz_info[horizontal == T & dominant == F,],
            mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            alpha=0.3, fill="grey", inherit.aes = F) +
  facet_wrap( ~ prod_jz_eng, scales = "free", ncol = 5) +
  scale_x_continuous(breaks = c(1:34)[1:34 %% 7 == 1],
                     labels = names(vec_yq)[1:34 %% 7 == 1]) +
  labs(x = "Cartel cases, Year-Quarter", y = " Cartel cases, log(1 + Price)") +
  theme(text = element_text(family = "Hei")) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 9)
  ) -> p_inall_cartel

ggplot(data = add_row(dt_inall[m.class == "Both", ], prod_jz_eng = "ZZZ"),
       mapping = aes(x = tt)) +
  geom_line(mapping = aes(group = firm, y = lnp.tjfz),
            linetype = "dotted", alpha = 0.5) +
  geom_line(mapping = aes(y = lnp_avg_tjz),
            linetype = "solid") +
  geom_vline(data = dt_jz_info[horizontal == T & dominant == T,],
             mapping = aes(xintercept = penal),
             linetype = "dashed") +
  geom_rect(data = dt_jz_info[horizontal == T & dominant == T,],
            mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            alpha=0.3, fill="grey", inherit.aes = F) +
  facet_wrap( ~ prod_jz_eng, scales = "free", ncol = 5) +
  scale_x_continuous(breaks = c(1:34)[1:34 %% 7 == 1],
                     labels = names(vec_yq)[1:34 %% 7 == 1]) +
  labs(x = "Abuse and cartel, Year-Quarter",
       y = " Abuse and cartel, log(1 + Price)") +
  theme(text = element_text(family = "Hei")) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 9)
  ) -> p_inall_both
p_inall_both_edited <- ggplotGrob(p_inall_both)
p_inall_both_edited$grobs[[31]]$grobs[[1]] <- nullGrob()

plot_grid(
  p_inall_abuse, p_inall_cartel, p_inall_both_edited,
  nrow = 3,
  rel_heights = c(2.7, 1, 1),
  align = "hv", axis = "tblr"
) -> p_inall_grouped

ggsave(filename = "output/p_inall_grouped.pdf",
       plot = p_inall_grouped,
       width = 15, height = 17)


### slides ver
plot_grid(
  p_inall_cartel, p_inall_both_edited,
  nrow = 2,
  rel_heights = c(1, 1),
  align = "hv", axis = "tblr"
) -> p_inall_grouped_slides_2

ggsave(filename = "output/p_inall_grouped_slides_1.pdf",
       plot = p_inall_abuse,
       width = 15, height = 10)
ggsave(filename = "output/p_inall_grouped_slides_2.pdf",
       plot = p_inall_grouped_slides_2,
       width = 15, height = 7)


### national avg data.frame used in SCM
dt_tjz <- dt_tjfz[tt > 0] %>%
  `[`(, jz_name := paste(productName, size, sep = "-"))
vec_pjzName <- sort(unique(dt_tjz$jz_name))
vec_pjzName <- 1:length(vec_pjzName) %>% `names<-`(vec_pjzName)
dt_tjz[, jz := vec_pjzName[jz_name]]
dt_tjz <- dt_tjz[, c("Q.tjz", "R.tjz", "p.tjz") :=
                   .(sum(Q.tjfz), sum(R.tjfz), sum(R.tjfz)/sum(Q.tjfz)),
                 by = .(tt, product, size)] %>%
  unique(by = c("tt", "product", "size")) %>%
  `[`(, .(tt, timeYQ, product, jz, productName, jz_name, size, size.rescale,
          Q.tjz, R.tjz, p.tjz, N.firm, HHI, CR1, CR2, CR3,
          category1, category2, category3, generalName, route, form)) %>%
  setorder(tt, jz)
### full list of jz with monopoly case
vec_jz_all <- sapply(ls_api_drug, function(ls) {ls$prod}) %>% unlist()
dt_tjz[productName %in% vec_jz_all, .(jz, jz_name)] %>% unique() %>%
  `[`(, jz_name) -> vec_jz_all
dt_jz_info[, jz := vlookup(prod_jz, dt_tjz[, .(jz_name, jz)])]

save(api, dt_inall, dt_jz_info, dt_tjfz, dt_tjz,
     vec_jz_all, vec_pjzName, vec_prodName, vec_prodname_eng, vec_yq,
     ls_api_drug, ls_api_interest, file = "MAIN.RData")
# 2. SCM ---------------------------------------------
## 2.1 paralleling SCM for each product-size -----------------------------------
TT = 34; folder.head <- "scm_out"
clust <- makeCluster(11)
clusterExport(clust, varlist = c("dt_jz_info", "dt_tjz", "TT", "folder.head",
                                 "vec_jz_all", "outTex_tbl"))
clusterEvalQ(clust, {
  library(dplyr); library(Synth); library(ggplot2); library(stringr)
})
parSapply(clust, dt_jz_info$prod_jz, function(i.jz.name) {
  message(paste0("Now SCM with ", i.jz.name))
  i.jz <- unique(dt_tjz[jz_name == i.jz.name, jz])
  dt_prep_i <- dt_tjz[category2 == unique(dt_tjz[jz == i.jz, category2]) &
                        !(jz_name %in% vec_jz_all[vec_jz_all != i.jz.name]),] %>%
    setkey(jz, tt) %>%
    ## `[`(, delta := 
    ##       (log(Q.tjz) - log(dplyr::lag(Q.tjz)))/
    ##       (log(p.tjz) - log(dplyr::lag(p.tjz))),
    ##     by = .(jz)) %>%
    ## `[`(, delta := ifelse(is.infinite(delta), lag(delta), delta)) %>%
    ## `[`(!is.na(delta) & !is.infinite(delta)) %>%
    `[`(tt %in% .[jz_name == i.jz.name, tt]) %>%
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

save(ls_out_full, file = "SCM_OUT_c2.RData")

## 2.1.x in case of interruption -----------------------------------
vec_jz_finished <- list.files(path = "output/scm_out") %>%
  `[`(str_detect(., "scp")) %>%
  str_extract("[\u4e00-\u9fa5]+")
sapply(dt_jz_info$prod_jz, function(jzn) {
  any(str_detect(jzn, vec_jz_finished))
}) %>% {dt_jz_info$prod_jz[!.]} -> vec_jz_reinput

for (i.jz.name in vec_jz_reinput) {
  message(paste0("Now SCM with ", i.jz.name))
  i.jz <- unique(dt_tjz[jz_name == i.jz.name, jz])
  dt_prep_i <- dt_tjz[category1 == unique(dt_tjz[jz == i.jz, category1]) &
                        !(jz_name %in% vec_jz_all[vec_jz_all != i.jz.name]),] %>%
    setkey(jz, tt) %>%
    `[`(tt %in% .[jz_name == i.jz.name, tt]) %>%
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
}

## Receive data by step
ls_out_full_by_step <- list()
ls_out_DT_by_step <- list()
vec_outfile <- list.files(path = "output/scm_out/data_collect")
for (file in vec_outfile) {
  load(paste0("output/scm_out/data_collect/", file))
  ### DT
  list(i.jz = synP_national) %>%
    `names<-`(grep(str_extract(file, "[\u4e00-\u9fa5]+"),
                   dt_jz_info$prod_jz, value = T)) %>% {
                     append(ls_out_DT_by_step, .)
                   } -> ls_out_DT_by_step
  ### Full list
  ls_out_full_by_step %<>% append(out_list)
}
## save(ls_out_full, ls_out_DT_by_step, ls_out_full_by_step, file = "SCM_OUT_bystep.RData")

## 2.2 paralleling SCM top 20 w ---------------------------------------------
load("SCM_OUT_c2.RData")
vec_top20notworking <- c("盐酸溴己新注射液-2ml:4mg",
                         "盐酸异丙肾上腺素注射液-2ml:1mg")
TT = 34; folder.head <- "scm_out"
clust <- makeCluster(11)
clusterExport(clust, varlist = c("dt_jz_info", "dt_tjz", "TT", "folder.head",
                                 "vec_jz_all", "outTex_tbl",
                                 "ls_out_full", "vec_top20notworking"))
clusterEvalQ(clust, {
  library(dplyr); library(Synth); library(ggplot2); library(stringr)
})
parSapply(clust, dt_jz_info$prod_jz, function(i.jz.name) {
  message(paste0("Now SCM with ", i.jz.name))
  i.jz <- unique(dt_tjz[jz_name == i.jz.name, jz])
  donor.top20 <- ls_out_full[[i.jz.name]][[1]][["weight.w"]] %>%
    as.data.table() %>% `[`(order(w.weight, decreasing = T)) %>%
    `[`(1:10, rn) %>% `[`(!is.na(.))
  dt_prep_i <- dt_tjz[category2 == unique(dt_tjz[jz == i.jz, category2]) &
                        !(jz_name %in% vec_jz_all[vec_jz_all != i.jz.name]), ] %>%
    setkey(jz, tt) %>%
    ## `[`(, delta := 
    ##       (log(Q.tjz) - log(dplyr::lag(Q.tjz)))/
    ##       (log(p.tjz) - log(dplyr::lag(p.tjz))),
    ##     by = .(jz)) %>%
    ## `[`(, delta := ifelse(is.infinite(delta), lag(delta), delta)) %>%
    ## `[`(!is.na(delta) & !is.infinite(delta)) %>%
    `[`(tt %in% .[jz_name == i.jz.name, tt]) %>%
    `[`(, N.t := .N, by = jz) %>%
    `[`(N.t == unique(.[jz == i.jz, N.t]))
  if (!(i.jz.name %in% vec_top20notworking)) {
    dt_prep_i <- dt_prep_i[jz %in% c(donor.top20, i.jz)]
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
save(ls_out_full, file = "SCM_OUT_c2_top10.RData")

# 3. result wiz national avg ---------------------------------------------
## 3.1. Descriptive Statistics ---------------------------------------------
dt_summary <- dt_tjz[tt > 0, ] %>% `[`(, is.vertical := jz_name %in% vec_jz_all)
summarise(
  dt_summary[is.vertical == T],
  across(c("Q.tjz", "R.tjz", "p.tjz", "N.firm", "HHI", "CR1", "CR2", "CR3"),
         list(mean = mean, sd = sd, min = min, median = median, max = max),
         .names = "{col}_{fn}")
) %>% pivot_longer(cols = everything(),
                   names_to = c(".value", "Stat"),
                   names_sep = "_") %>%
  `names<-`(c("Stat", "Qty", "Amt", "Price", "#. firm", "HHI",
              "CR1", "CR2", "CR3")) %>%
  mutate_at(c(2:3), ~ format(round(., 0),, big.mark = ",")) %>%
  mutate_at(4:9, ~ round(., 3)) %>% {
    cbind(" " = c(
      "\\textbf{Treated}",
      "\\#. Obs", format(dt_summary[is.vertical == T, .N], big.mark = ","),
      "\\#. drug", format(dt_summary[is.vertical == T, unict(jz)], big.mark = ",")
    ), .)
  } %>%
  outTex_tbl(var.names = F,
             top.lines = "\\hline &&&&&&&&& \\\\[-1.8ex]",
             file.name = "output/summary_TRUE.tex")

summarise(
  dt_summary[is.vertical == F],
  across(c("Q.tjz", "R.tjz", "p.tjz", "N.firm", "HHI", "CR1", "CR2", "CR3"),
         list(mean = mean, sd = sd, min = min, median = median, max = max),
         .names = "{col}_{fn}")
) %>% pivot_longer(cols = everything(),
                   names_to = c(".value", "Stat"),
                   names_sep = "_") %>%
  `names<-`(c("Stat", "Qty", "Amt", "Price", "#. firm", "HHI",
              "CR1", "CR2", "CR3")) %>%
  mutate_at(c(2:3), ~ format(round(., 0),, big.mark = ",")) %>%
  mutate_at(4:9, ~ round(., 3)) %>% {
    cbind(" " = c(
      "\\textbf{Control}",
      "\\#. Obs", format(dt_summary[is.vertical == F, .N], big.mark = ","),
      "\\#. drug", format(dt_summary[is.vertical == F, unict(jz)], big.mark = ",")
    ), .)
  } %>%
  outTex_tbl(var.names = F,
             top.lines = "\\hline &&&&&&&&& \\\\[-1.8ex]",
             file.name = "output/summary_FALSE.tex")

summarise(
  dt_summary,
  across(c("Q.tjz", "R.tjz", "p.tjz", "N.firm", "HHI", "CR1", "CR2", "CR3"),
         list(mean = mean, sd = sd, min = min, median = median, max = max),
         .names = "{col}_{fn}")
) %>% pivot_longer(cols = everything(),
                   names_to = c(".value", "Stat"),
                   names_sep = "_") %>%
  `names<-`(c("Stat", "Qty", "Amt", "Price", "#. firm", "HHI",
              "CR1", "CR2", "CR3")) %>%
  mutate_at(c(2:3), ~ format(round(., 0),, big.mark = ",")) %>%
  mutate_at(4:9, ~ round(., 3)) %>% {
    cbind(" " = c(
      "\\textbf{Pool}",
      "\\#. Obs", format(dt_summary[, .N], big.mark = ","),
      "\\#. drug", format(dt_summary[, unict(jz)], big.mark = ",")
    ), .)
  } %>%
  outTex_tbl(var.names = F,
             top.lines = "\\hline &&&&&&&&& \\\\[-1.8ex]",
             file.name = "output/summary_FULL.tex")

## 3.2. all in one SCM plot ---------------------------------------------
load("SCM_OUT_c2.RData")
sapply(1:length(ls_out_full), function(n.dt) {
  dt <- ls_out_full[[n.dt]][[1]][[1]]
  dt[jz_name %in% c("Synthetic Group", names(ls_out_full)[n.dt])] %>%
    `[`(jz == 0, jz := unique(.[jz != 0, jz])) %>%
    `[`(, is.syc := jz_name == "Synthetic Group") %>%
    return()
}, simplify = FALSE) %>% rbindlist() -> dt_syc_plot

unique(dt_inall[, .(jz_name = prod_jz, prod_jz_eng)])[dt_syc_plot, on = "jz_name"] %>%
  `[`(, prod_jz_eng := unique(prod_jz_eng[!is.na(prod_jz_eng)]),
      by = jz) -> dt_syc_plot

dt_syc_plot %<>%
  mutate(p.tjz = log(1 + p.tjz),
         is.horizontal = vlookup(jz, dt_jz_info[, .(jz, horizontal)]),
         is.dominant = vlookup(jz, dt_jz_info[, .(jz, dominant)]),
         is.both = vlookup(jz, dt_jz_info[, .(jz, and(horizontal, dominant))]),
         m.class = ifelse(is.both, "Both",
                          ifelse(is.horizontal, "Cartel", "Abuse")))

## 3.2.1. sep ---------------------------------------------
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
    axis.text = element_text(size = 8),
    strip.text = element_text(size = 10)
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
    axis.text = element_text(size = 8),
    strip.text = element_text(size = 10)
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
    axis.text = element_text(size = 8),
    strip.text = element_text(size = 10)
  ) -> p_syn_both
p_syn_both <- ggplotGrob(p_syn_both)
p_syn_both$grobs[[31]]$grobs[[1]] <- nullGrob()

plot_grid(
  p_syn_cartel, p_syn_both,
  nrow = 2, rel_heights = c(1, 1),
  align = "hv", axis = "tblr"
) -> p_syn_both_cartel

ggsave(filename = "output/p_syn_inall_abuse.pdf",
       plot = p_syn_abuse,
       width = 15, height = 10)
ggsave(filename = "output/p_syn_inall_both_cartel.pdf",
       plot = p_syn_both_cartel,
       width = 15, height = 7)

## 3.2.2. group ---------------------------------------------
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
) -> p_syn_inall_grouped

ggsave(filename = "output/p_syn_inall_grouped.pdf",
       plot = p_syn_inall_grouped,
       width = 15, height = 17)


### slides ver
plot_grid(
  p_syn_cartel, p_syn_both_edited,
  nrow = 2,
  rel_heights = c(1, 1),
  align = "hv", axis = "tblr"
) -> p_syn_inall_grouped_slides_2

ggsave(filename = "output/p_syn_inall_grouped_slides_1.pdf",
       plot = p_syn_abuse,
       width = 15, height = 10)
ggsave(filename = "output/p_syn_inall_grouped_slides_2.pdf",
       plot = p_syn_inall_grouped_slides_2,
       width = 15, height = 7)

## 3.3. preliminary DID setting ---------------------------------------------
vec_lm_dt <- c("c1", "c1_top10", "c1_top20", "c2", "c2_top10", "c2_top20")

for (dt.i in vec_lm_dt) {
  load(paste0("SCM_OUT_", dt.i, ".RData"))
  sapply(1:length(ls_out_full), function(n.dt) {
    dt <- ls_out_full[[n.dt]][[1]][["DT"]]
    dt[jz_name %in% c("Synthetic Group", names(ls_out_full)[n.dt])] %>%
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
  ### reform part of variables
  dt_syc_all %<>%
    mutate(
      CR1_cut = CR1 > 0.5,
      CR2_cut = CR2 > 0.4,
      CR3_cut = CR3 > 0.3,
      year = str_sub(timeYQ, 1, 4)
    )
  ### regression
  feols(
    fml = log(p.tjz) ~ in.shock + is.syc + in.shock:is.syc +
      tt + HHI + N.firm |
      factor(jz_real) + factor(tt),
    data = dt_syc_all[after.shock == in.shock],
    nthreads = 10,
    cluster = c("product", "year"),
  ) -> lm_scdid_log
  ### export
  assign(paste0("lm_scdid_log_", dt.i), lm_scdid_log)
}

modelsummary(list(lm_scdid_log_c1, lm_scdid_log_c1_top10, lm_scdid_log_c1_top20,
                  lm_scdid_log_c2, lm_scdid_log_c2_top10, lm_scdid_log_c2_top20),
             stars = TRUE,
             coef_omit = "^factor.*",
             output = "output/reg_scdid.html")
## 3.4. DID results output ---------------------------------------------
dt_FE <- matrix(c("Prod. FE", rep("Yes", 3), "Time FE", rep("Yes", 3)),
                nrow = 2, byrow = T) %>% as.data.frame() %>%
  `attr<-`("position", c(11, 12))
modelsummary(models = list("Full" = lm_scdid_log_c1,
                           "Top 20" = lm_scdid_log_c1_top20,
                           "Top 10" = lm_scdid_log_c1_top10),
             coef_omit = "^factor.*",
             coef_map = c(
               "is.sycTRUE" = "\\(D^{syc}_{j}\\)",
               "in.shockTRUE" = "\\(D^{m}_{t}\\)",
               "in.shockTRUE:is.sycTRUE" = "\\(D^{syc}_j \\times D^{m}_{t}\\)",
               "HHI" = "\\(\\textrm{HHI}{jt}\\)",
               "N.firm" = "\\(N^f_{jt}\\)",
               "(Intercept)" = "Constant"
             ),
             add_rows = dt_FE,
             estimate = "{estimate}{stars}",
             stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
             notes = list("\\\\textit{Note}: *** $p<0.001$, ** $p<0.01$, *$p<0.05$"),
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
  add_header_above(c("", "$\\\\log P_{jt}$" = 3), escape = FALSE) %>%
  str_remove("\\\\begin\\{table\\}\\n\\\\centering\\n") %>%
  str_remove("\\n\\\\end\\{table\\}") %>%
  cat(sep = "\n", file = "output/reg_scdid.tex")

## 3.5. 3DiD for self-choice ---------------------------------------------
vec_lm_dt <- c("c1", "c1_top10", "c1_top20", "c2", "c2_top10", "c2_top20")

for (dt.i in vec_lm_dt) {
  load(paste0("SCM_OUT_", dt.i, ".RData"))
  sapply(1:length(ls_out_full), function(n.dt) {
    dt <- ls_out_full[[n.dt]][[1]][["DT"]]
    dt[jz_name %in% c("Synthetic Group", names(ls_out_full)[n.dt])] %>%
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
  feols(
    fml = log(p.tjz) ~ in.shock:is.vertical + is.syc:is.vertical +
      in.shock:is.syc:is.vertical + is.vertical +
      tt + N.firm + log(Q.tjz) |
      factor(product) + factor(tt),
    data = dt_syc_3d,
    nthreads = 10,
    cluster = c("product", "year"),
  ) -> lm_scdid_log
  ### export
  assign(paste0("lm_sc3d_log_", dt.i), lm_scdid_log)
}

modelsummary(list(lm_sc3d_log_c1, lm_sc3d_log_c1_top10, lm_sc3d_log_c1_top20,
                  lm_sc3d_log_c2, lm_sc3d_log_c2_top10, lm_sc3d_log_c2_top20),
             stars = TRUE,
             coef_omit = "^factor.*",
             output = "output/reg_3did.html")

dt_FE <- matrix(c("Prod. FE", rep("Yes", 3), "Time FE", rep("Yes", 3)),
                nrow = 2, byrow = T) %>% as.data.frame() %>%
  `attr<-`("position", c(13, 14))
modelsummary(models = list("Full" = lm_sc3d_log_c1,
                           "Top 20" = lm_sc3d_log_c1_top20,
                           "Top 10" = lm_sc3d_log_c1_top10),
             coef_omit = "^factor.*",
             coef_map = c(
               "is.verticalTRUE" =
                 "\\(D^{up}_{j}\\)",
               "is.verticalTRUE:is.sycTRUE" =
                 "\\(D^{up}_{j} \\times D^{syc}_{j}\\)",
               "in.shockTRUE:is.verticalTRUE" =
                 "\\(D^{up}_{j} \\times D^{m}_{jt}\\)",
               "in.shockTRUE:is.verticalTRUE:is.sycTRUE" =
                 "\\(D^{up}_{j} \\times D^{syc}_{j} \\times D^{m}_{jt}\\)",
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
  add_header_above(c("", "$\\\\log P_{jt}$" = 3), escape = FALSE) %>%
  str_remove("\\\\begin\\{table\\}\\n\\\\centering\\n") %>%
  str_remove("\\n\\\\end\\{table\\}") %>%
  cat(sep = "\n", file = "output/reg_scdid_3did.tex")

# 4. result wiz predictors ---------------------------------------------
## 4.1. dmg ---------------------------------------------
load("SCM_OUT_c2_top10.RData")
## note: C2-top10, w/o log(Q), tt
##       c1, w/o log(Q), tt
sapply(1:length(ls_out_full), function(n.dt) {
  dt <- ls_out_full[[n.dt]][[1]][[1]]
  dt[jz_name %in% c("Synthetic Group", names(ls_out_full)[n.dt])] %>%
    `[`(jz == 0, jz := unique(.[jz != 0, jz]) + 10000) %>%
    `[`(, jz_real := unique(.[jz < 10000, jz])) %>%
    `[`(jz > 10000, jz_name := paste0("Syc-", unique(.[jz < 10000, jz_name]))) %>%
    `[`(, after.shock := tt >= dt_jz_info[prod_jz == names(ls_out_full)[n.dt],
                                          start]) %>%
    `[`(, in.shock := tt %in%
          dt_jz_info[prod_jz == names(ls_out_full)[n.dt], start]:
          dt_jz_info[prod_jz == names(ls_out_full)[n.dt], end]) %>%
    `[`(, is.syc := jz > 10000) %>% return()
}, simplify = FALSE) %>% rbindlist() -> dt_syc_all

dt_tjz[, .(tt, jz_real = jz, size.rescale, Q.tjz,
           N.firm, HHI, CR1, CR2, CR3,
           category3, category2, category1, product)][
             dt_syc_all, on = .(tt, jz_real)] -> dt_syc_all

dt_jz_info[, .(prod_jz, col_dur = end - start, cate,
               horizontal, vertical, dominant)][
  dt_syc_all, on = .(prod_jz == jz_name)
] %>%
  `[`(, p.pre := .SD[after.shock == FALSE, mean(p.tjz)],
      by = prod_jz) -> dt_syc_all

dt_dmg_chr <- dt_syc_all[in.shock == T & is.syc == F, ] %>%
  `[`(, dmg := p.tjz / dt_syc_all[in.shock == T & is.syc == T, p.tjz] - 1)
dt_dmg_chr %<>%
  mutate(year = str_sub(timeYQ, 1, 4))


std_cluster <- NULL # c("prod_jz")
feols(dmg ~ N.firm + I(100 * CR1) + col_dur + p.pre + horizontal + dominant +
        factor(cate) + factor(tt),
      data = dt_dmg_chr,
      cluster = std_cluster) -> lm_dmg_CR1
feols(dmg ~ N.firm + I(100 * CR2) + col_dur + p.pre + horizontal + dominant +
        factor(cate) + factor(tt),
      data = dt_dmg_chr,
      cluster = std_cluster) -> lm_dmg_CR2
feols(dmg ~ N.firm + I(100 * CR3) + col_dur + p.pre + horizontal + dominant +
        factor(cate) + factor(tt),
      data = dt_dmg_chr,
      cluster = std_cluster) -> lm_dmg_CR3
feols(dmg ~ N.firm + I(100 * HHI) + col_dur + p.pre + horizontal + dominant +
        factor(cate) + factor(tt),
      data = dt_dmg_chr,
      cluster = std_cluster) -> lm_dmg_HHI

dt_dmg_chr %<>% mutate(CR1.crt = CR1 > .6)
feols(dmg ~ N.firm + CR1.crt + I(100 * HHI) + col_dur + p.pre +
        horizontal + dominant + factor(cate) + factor(tt),
      data = dt_dmg_chr,
      cluster = std_cluster) -> lm_dmg_2v


modelsummary(list("(1)" = lm_dmg_CR1, "(2)" = lm_dmg_CR2,
                  "(3)" = lm_dmg_CR3, "(4)" = lm_dmg_HHI,
                  "(5)" = lm_dmg_2v),
             stars = T, coef_omit = "^factor.*",
             output = "output/reg_dmg.html")

dt_FE <- matrix(c("G.N. FE", rep("Yes", 5), "Time FE", rep("Yes", 5)),
                nrow = 2, byrow = T) %>% as.data.frame() %>%
  `attr<-`("position", c(21, 22))
modelsummary(models = list("(1)" = lm_dmg_CR1,
                           "(2)" = lm_dmg_CR2,
                           "(3)" = lm_dmg_CR3,
                           "(4)" = lm_dmg_HHI,
                           "(5)" = lm_dmg_2v
                           ),
             coef_omit = c("^(\\(Intercept|factor).*"),
             coef_map = c(
               "N.firm" = "\\(N^f_{jt}\\)",
               "col_dur" = "\\(\\textrm{Duration}_j\\)",
               "p.pre" = "\\(\\bar{P}_{j}^{pre}\\)",
               "CR1.crtTRUE"  = "\\(\\textrm{CR1}_{jt} > 0.6\\)",
               "I(100 * CR1)" = "\\(100 \\times \\textrm{CR1}_{jt}\\)",
               "I(100 * CR2)" = "\\(100 \\times \\textrm{CR2}_{jt}\\)",
               "I(100 * CR3)" = "\\(100 \\times \\textrm{CR3}_{jt}\\)",
               "I(100 * HHI)" = "\\(100 \\times \\textrm{HHI}_{jt}\\)",
               "horizontalTRUE" = "Cartel case",
               "dominantTRUE" = "Abuse case"
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
  add_header_above(c("", "$P_{jt}/P^{syc}_{jt} - 1$" = 5), escape = FALSE) %>%
  str_remove("\\\\begin\\{table\\}\\n\\\\centering\\n") %>%
  str_remove("\\n\\\\end\\{table\\}") %>%
  cat(sep = "\n", file = "output/reg_dmg.tex")

## 4.2. without damage ---------------------------------------------
dt_tjz[!(jz %in% unique(dt_syc_all$jz_real)),
       .(tt, jz_real = jz, prod_jz = jz_name,
         Q.tjz, N.firm, HHI, CR1, CR2, CR3, category3, p.tjz,
         in.shock = F, is.vertical = F,
         horizontal = F, vertical = F, dominant = F)] %>%
  rbind(
    dt_syc_all[is.syc == F & after.shock == in.shock,
               .(tt, jz_real, prod_jz, Q.tjz, p.tjz, N.firm, HHI, CR1, CR2, CR3,
                 category3, in.shock, is.vertical = T,
                 horizontal, vertical, dominant)]
  ) -> dt_prob

dt_prob %<>%
  group_by(jz_real, in.shock) %>%
  mutate(across(c(Q.tjz, N.firm, HHI, CR1, CR2, CR3, p.tjz),
                mean, .names = "mean_{col}")) %>%
  select(jz_real, prod_jz, category3,
         is.vertical, in.shock, horizontal, vertical, dominant,
         starts_with("mean")) %>%
  unique() %>% as.data.table()

lm(log(1 + mean_HHI) ~ in.shock + is.vertical +
     horizontal + vertical + dominant + mean_N.firm + log(mean_Q.tjz) +
     factor(category3), data = dt_prob) -> lm_chr_shock_HHI
lm(log(1 + mean_CR1) ~ in.shock + is.vertical +
     horizontal + vertical + dominant + mean_N.firm + log(mean_Q.tjz) +
     factor(category3), data = dt_prob) -> lm_chr_shock_CR1
lm(log(1 + mean_CR2) ~ in.shock + is.vertical +
     horizontal + vertical + dominant + mean_N.firm + log(mean_Q.tjz) +
     factor(category3), data = dt_prob) -> lm_chr_shock_CR2
lm(log(1 + mean_CR3) ~ in.shock + is.vertical +
     horizontal + vertical + dominant + mean_N.firm + log(mean_Q.tjz) +
     factor(category3), data = dt_prob) -> lm_chr_shock_CR3

stargazer(lm_chr_shock_HHI,lm_chr_shock_CR1, lm_chr_shock_CR2, lm_chr_shock_CR3,
          type = "html",
          out = paste0("output/reg_chr_shock.html"),
          omit = "^factor.*")

dt_FE <- matrix(c("Cate. FE", rep("Yes", 4)), nrow = 1, byrow = T) %>%
  as.data.frame() %>% `attr<-`("position", c(15))
modelsummary(models = list("log(1 + CR1)" = lm_chr_shock_CR1,
                           "log(1 + CR2)" = lm_chr_shock_CR2,
                           "log(1 + CR3)" = lm_chr_shock_CR3,
                           "log(1 + HHI)" = lm_chr_shock_HHI
                           ),
             coef_omit = "^factor.*",
             coef_map = c(
               "in.shockTRUE" = "\\(D^{m}_{t}\\)",
               "is.verticalTRUE" = "\\(D^{up}_{j}\\)",
               "horizontalTRUE" = "Hrzt case",
               "dominantTRUE" = "Dmnt case",
               "mean_N.firm" = "\\(\\bar{N}^f_{j}\\)",
               "log(mean_Q.tjz)" = "\\(\\log(\\bar{Q}_{j})\\)",
               "(Intercept)" = "Constant"
             ),
             add_rows = dt_FE,
             estimate = "{estimate}{stars}",
             stars = c('*' = 0.1, '**' = 0.01, '***' = 0.001),
             notes = list("\\\\textit{Note}: *** $p<0.001$, ** $p<0.01$, *$p<0.1$"),
             escape = FALSE, 
             gof_function = f_gof_add,
             gof_map = list(
               list("raw" = "nobs",
                    "clean" = "\\#Obs",
                    "fmt" = f_gof_fmt),
               list("raw" = "dfr",
                    "clean" = "df",
                    "fmt" = f_gof_fmt),
               list("raw" = "r.squared",
                    "clean" = "\\(R^2\\)",
                    "fmt" = f_gof_fmt),
               list("raw" = "adj.r.squared",
                    "clean" = "Adj. \\(R^2\\)",
                    "fmt" = f_gof_fmt),
               list("raw" = "F",
                    "clean" = "F Statistic",
                    "fmt" = f_gof_fmt)
             ),
             output = "latex") %>%
  add_header_above(c("", "\\\\textit{Dependent variables:}" = 4), escape = FALSE) %>%
  str_remove("\\\\begin\\{table\\}\\n\\\\centering\\n") %>%
  str_remove("\\n\\\\end\\{table\\}") %>%
  cat(sep = "\n", file = "output/reg_chr_shock.tex")


## glm(is.vertical ~ log(1 + mean_CR1) + mean_N.firm + mean_Q.tjz +
##       in.shock + factor(category3), family = binomial,
##     data = dt_prob) -> glm_prob_CR1
# 5. logit ---------------------------------------------
## 5.1. data for logit ---------------------------------------------
dt_jz_info_all <- sapply(ls_api_drug, function(d.i) {
  data.table(
    productName = d.i$prod,
    start = vlookup(d.i$start, dt_tjz[, .(timeYQ, tt)]),
    end = vlookup(d.i$end, dt_tjz[, .(timeYQ, tt)])
  ) %>% return()
}, simplify = F) %>% rbindlist() %>%
  unique(by = "productName") %>%
  `[`(productName == "樟脑苯酚溶液", c("start", "end") := .(5, 28))

dt_prob <- dt_tjz %>%
  select(tt, productName, jz, jz_name,
         p.tjz, Q.tjz, N.firm, HHI, CR1, CR2, CR3,
         category1, category2, category3, product, generalName, timeYQ) %>%
  left_join(dt_jz_info_all, by = "productName") %>%
  mutate(is.up = ifelse(is.na(start), FALSE, start <= tt & tt <= end),
         is.up_j = jz_name %in% vec_jz_all) %>%
  select(!all_of(c("start", "end", "productName"))) %>%
  mutate(CR1_cut  = CR1 > 0.6,
         CR2_cut1 = CR2 > 0.6 & CR1 < 0.4,
         CR2_cut2 = CR2 > 0.6 & CR1 < 0.6,
         CR3_cut  = CR3 > 0.6 & CR2 < 0.65,
         year = str_sub(timeYQ, 1, 4))
## 5.2. CR classifier ---------------------------------------------
dt_prob$CR1 %>% quantile(probs = seq(0, 1, 0.05)) # 0.55 ~ 20%
dt_prob$CR2 %>% quantile(probs = seq(0, 1, 0.05)) # 0.84 ~ 20%
dt_prob$CR3 %>% quantile(probs = seq(0, 1, 0.05)) # 0.95 ~ 20%

dt_prob[CR1 < 0.6, CR2] %>% length()
dt_prob[CR1 < 0.6, CR2] %>% quantile(probs = seq(0, 1, 0.05)) # 0.6 ~ 20%

dt_prob[CR2 < 0.6, CR2] %>% length()
dt_prob[CR2 < 0.6, CR3] %>% quantile(probs = seq(0, 1, 0.05)) # 0.6 ~ 40%
### principally, make CR2_cut not collinear with CR1_cut,
### each time include at least another 20% sample

### iterated reg: CR2
dt_reg_cr2 <- data.table("CR1" = integer(0), "CR2" = integer(0),
                         "coef" = integer(0), "tv" = integer(0),
                         "5%" = character(0))
for (CR1.i in seq(0.3, 0.9, 0.05)) {
  for (CR2.i in seq(0.3, 0.9, 0.05)) {
    if (CR2.i < CR1.i) {
      ret.i <- rep(NA, 5)
    } else {
      dt_i <- dt_prob %>% mutate(idc = CR2 > CR2.i & CR1 < CR1.i)
      feglm(
        fml = is.up ~ idc + log(p.tjz) + log(Q.tjz) + N.firm | category1 + tt,
        family = binomial(link = "logit"),
        data = dt_i,
        cluster = c("product", "year"),
        nthreads = 10
      ) -> glm.i
      tv <- glm.i$coefficients["idcTRUE"]/glm.i$se["idcTRUE"]
      if (is.na(tv) | tv < 0) {
        ret.i <- rep(NA, 5)
      } else {
        ret.i <- c("CR1" = CR1.i,
                   "CR2" = CR2.i,
                   "coef" = glm.i$coefficients["idcTRUE"],
                   "tv" = tv,
                   "5%" = ifelse(tv > 1.96, "*", ""))
      }
    }
    dt_reg_cr2 %<>% rbind(t(ret.i), use.names=FALSE)
  }
}

### iterated reg: CR3
dt_reg_cr3 <- data.table("CR2" = integer(0), "CR3" = integer(0),
                         "coef" = integer(0), "tv" = integer(0),
                         "5%" = character(0))
for (CR2.i in seq(0.3, 0.95, 0.05)) {
  for (CR3.i in seq(0.3, 0.95, 0.05)) {
    if (CR2.i < CR3.i) {
      ret.i <- rep(NA, 5)
    } else {
      dt_i <- dt_prob %>% mutate(idc = CR3 > CR3.i & CR2 < CR2.i)
      feglm(
        fml = is.up ~ idc + p.tjz + log(Q.tjz) + N.firm | category1 + tt,
        family = binomial(link = "logit"),
        data = dt_i,
        cluster = c("product", "year"),
        nthreads = 10
      ) -> glm.i
      tv <- glm.i$coefficients["idcTRUE"]/glm.i$se["idcTRUE"]
      if (is.na(tv) | tv < 0) {
        ret.i <- rep(NA, 5)
      } else {
        ret.i <- c("CR2" = CR2.i,
                   "CR3" = CR3.i,
                   "coef" = glm.i$coefficients["idcTRUE"],
                   "tv" = tv,
                   "5%" = ifelse(tv > 1.96, "*", ""))
      }
    }
    dt_reg_cr3 %<>% rbind(t(ret.i), use.names=FALSE)
  }
}

dt_reg_cr3 %>% filter(`5%` == "*")

## 5.3. logit reg ---------------------------------------------
### is.up_j,  0.5-0.4-0.3, fe = category1, cluster = product * year
### is.up_jt, 0.6-0.5-0.4, fe = category1, cluster_1 = jz * year
feglm(
  fml = is.up ~ CR1_cut + log(p.tjz) + log(Q.tjz) + N.firm |
    category1 + tt,
  family = binomial(link = "logit"),
  data = dt_prob, 
  cluster = c("jz", "year"),
  nthreads = 10
) -> glm_logit_CR1

feglm(
  fml = is.up ~ CR2_cut1 + log(p.tjz) + log(Q.tjz) + N.firm |
    category1 + tt,
  family = binomial(link = "logit"),
  data = dt_prob,
  cluster = c("product", "year"),
  nthreads = 10
) -> glm_logit_CR2_1

feglm(
  fml = is.up ~ CR2_cut2 + log(p.tjz) + log(Q.tjz) + N.firm |
    category1 + tt,
  family = binomial(link = "logit"),
  data = dt_prob,
  cluster = c("product", "year"),
  nthreads = 10
) -> glm_logit_CR2_2

feglm(
  fml = is.up ~ CR3_cut + log(p.tjz) + log(Q.tjz) + N.firm |
    category1 + tt,
  family = binomial(link = "logit"),
  data = dt_prob,
  cluster = c("product", "year"),
  nthreads = 10
) -> glm_logit_CR3


modelsummary(list(glm_logit_CR1, glm_logit_CR3),
             stars = TRUE, output = "output/glm_logit.html")

dt_FE <- matrix(c("Cate. FE", rep("Yes", 4), "Time FE", rep("Yes", 4)),
                nrow = 2, byrow = T) %>% as.data.frame() %>%
  `attr<-`("position", c(15, 16))
modelsummary(models = list("(1)" = glm_logit_CR1,
                           "(2)" = glm_logit_CR2_2,
                           "(3)" = glm_logit_CR2_1,
                           "(4)" = glm_logit_CR3
                           ),
             coef_omit = "^factor.*",
             coef_map = c(
               "CR1_cutTRUE" = "\\(I(\\textrm{CR1}_{jt} > 0.6)\\)",
               "CR2_cut2TRUE" = "\\(I(\\textrm{CR2}_{jt} > 0.6,\\textrm{CR1}_{jt} < 0.6)\\)",
               "CR2_cut1TRUE" = "\\(I(\\textrm{CR2}_{jt} > 0.6,\\textrm{CR1}_{jt} < 0.4)\\)",
               "CR3_cutTRUE" = "\\(I(\\textrm{CR3}_{jt} > 0.6,\\textrm{CR2}_{jt} < 0.6)\\)",
               "log(p.tjz)"    = "\\(\\log P_{jt}\\)",
               "log(Q.tjz)"  = "\\(\\log Q_{jt}\\)",
               "N.firm"      = "\\(N^f_{jt}\\)"
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
                    "fmt" = f_gof_fmt),
               list("raw" = "aic",
                    "clean" = "AIC",
                    "fmt" = f_gof_fmt)
             ),
             output = "latex") %>%
  add_header_above(c("", "$\\\\Pr(D^{m}_{jt} = 1)$" = 4), escape = FALSE) %>%
  str_remove("\\\\begin\\{table\\}\\n\\\\centering\\n") %>%
  str_remove("\\n\\\\end\\{table\\}") %>%
  cat(sep = "\n", file = "output/reg_logit.tex")

### pct y=1
dt_prob$is.up[glm_logit_CR1$obs_selection$obsRemoved] %>% table()
dt_prob %>%
  summarise(m.t = sum(is.up)/length(is.up), .by = tt) %>%
  mutate(m = 100 * mean(m.t[m.t > 0])) %>%
  pull(m) %>% mean()

