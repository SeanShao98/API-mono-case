# project = /Users/seanshao/Documents/project_main/API drug
# May 20, 24
# initializing ------------------------------------------------
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(stargazer)
library(readxl)
library(tibble)
library(partools)
library(showtext)
library(Synth)
library(parallel)
library(doParallel)
library(stringdist)
library(modelsummary)
library(kableExtra)

rm(list = ls())
options(scipen = 10)
# 0. API-Prod ------------------------------------------------
ls_api_drug <- list(
  "别嘌醇" = list(
    prod = c("别嘌醇片", "别嘌醇缓释片", "别嘌醇缓释胶囊", "复方别嘌醇片"),
    firm = c("青阳药业", "湘百合"),
    penal = "2015-4",
    start = "2013-4",
    end = "2014-1",
    cate = "拒绝交易",
    horizontal = F,
    vertical = F,
    dominant = T
  ),
  "艾司唑仑" = list(
    prod = c("艾司唑仑注射液", "艾司唑仑片"),
    firm = c("华中药业", "信谊", "常州四药"),
    penal = "2016-2",
    start = "2015-1",
    end = "2016-2",
    cate = "联合抵制交易",
    horizontal = T,
    vertical = F,
    dominant = F
  ),
  ## "苯酚" = list(
  ##   prod = c("氨酚伪麻美芬片(Ⅱ)与苯酚伪麻片", "水杨酸苯酚贴膏", "樟脑苯酚溶液"),
  ##   firm = c("先锋"),
  ##   penal = "2021-2",
  ##   start = "2014-1",
  ##   end = "2015-4",
  ##   cate = "不公平高价",
  ##   horizontal = F,
  ##   vertical = F,
  ##   dominant = T
  ## ),
  "苯酚-2" = list(
    prod = c("氨酚伪麻美芬片(Ⅱ)与苯酚伪麻片", "水杨酸苯酚贴膏", "樟脑苯酚溶液"),
    firm = c("西南制药", "先锋"),
    penal = "2021-4",
    start = "2014-1",
    end = "2017-1",
    cate = "拒绝交易",
    horizontal = F,
    vertical = F,
    dominant = T
  ),
  "水杨酸甲酯" = list(
    prod = c("复方水杨酸甲酯巴布膏", "复方水杨酸甲酯苯海拉明喷雾剂",
             "复方水杨酸甲酯薄荷醇贴剂", "复方水杨酸甲酯乳膏"),
    firm = "武汉新兴精英",
    penal = "2017-1",
    start = "2015-1",
    end = "2015-4",
    cate = "附加不合理交易条件",
    horizontal = F,
    vertical = F,
    dominant = T
  ),
  "异烟肼" = list(
    prod = c("异烟肼注射液", "异烟肼片", "利福平异烟肼片",
             "对氨基水杨酸异烟肼片"),
    firm = c("新赛科", "汉德威"),
    penal = "2017-3",
    start = "2014-4",
    end = "2017-3",
    cate = c("不公平高价", "拒绝交易"),
    horizontal = F,
    vertical = F,
    dominant = T
  ),
  "扑尔敏" = list(
    prod = c("马来酸氯苯那敏注射液", "马来酸氯苯那敏片"),
    firm = c("九势", "尔康"),
    penal = "2018-4",
    start = "2018-1",
    end = "2018-4",
    cate = c("不公平高价", "拒绝交易", "搭售"),
    horizontal = F,
    vertical = F,
    dominant = T
  ),
  "注射用葡萄糖酸钙原料药" = list(
    prod = c("葡萄糖酸钙注射液", "葡萄糖酸钙氯化钠注射液"),
    firm = c("康惠", "普云惠", "太阳神"),
    penal = "2020-2",
    start = "2015-3",
    end = "2017-4",
    cate = c("不公平高价", "附加不合理交易条件"),
    horizontal = F,
    vertical = F,
    dominant = T
  ),
  "盐酸溴己新" = list(
    prod = c("注射用盐酸溴己新", "盐酸溴己新注射液", "盐酸溴己新片",
             "盐酸溴己新葡萄糖注射液"),
    firm = c("万邦德"),
    penal = "2020-4",
    start = "2015-3",
    end = "2016-4",
    cate = c("附加不合理交易条件"),
    horizontal = F,
    vertical = F,
    dominant = T
  ),
  "巴曲酶" = list(
    prod = c("巴曲酶注射液"),
    firm = c("先声"),
    penal = "2021-1",
    start = "2019-4",
    end = "2020-2",
    cate = c("拒绝交易"),
    horizontal = F,
    vertical = F,
    dominant = T
  ),
  "醋酸氟轻松" = list(
    prod = c("复方醋酸氟轻松酊", "醋酸氟轻松乳膏", "醋酸氟轻松冰片乳膏"),
    firm = c("天药", "太平洋", "富海通"),
    penal = "2021-2",
    start = "2017-2",
    end   = "2019-4",
    cate = c("固定或变更商品价", "分割市场"),
    horizontal = T,
    vertical = F,
    dominant = F
  ),
  "樟脑" = list(
    prod = c("复方倍氯米松樟脑乳膏", "复方倍氯米松樟脑乳膏(曾用名:无极膏)",
             "樟脑薄荷柳酯乳膏", "氧化樟脑注射液", "樟脑苯酚溶液", "樟脑水合氯醛酊"),
    firm = c("黄埔", "优合", "嘉福"),
    penal = "2021-2",
    start = "2017-2",
    end   = "2019-4",
    cate = c("固定或变更商品价", "分割市场"),
    horizontal = T,
    vertical = F,
    dominant = F
  ),
  "氯解磷定" = list(
    prod = c("氯解磷定注射液"),
    firm = c("宁卫"),
    penal = "2021-4",
    start = "2018-1",
    end   = "2019-4",
    cate = c("不公平高价", "附加不合理交易条件"),
    horizontal = F,
    vertical = F,
    dominant = T
  ),
  "左卡尼汀" = list(
    prod = c("左卡尼汀口服溶液", "左卡尼汀注射液", "注射用左卡尼汀",
             "盐酸乙酰左卡尼汀片"),
    firm = c("东北"),
    penal = "2023-1",
    start = "2018-4",
    end   = "2019-2",
    cate = c("不公平高价"),
    horizontal = F,
    vertical = F,
    dominant = T
  ),
  "肾上腺素" = list(
    prod = c("去氧肾上腺素溴苯那敏胶囊", "普鲁卡因肾上腺素注射液",
             "盐酸去氧肾上腺素注射液", "盐酸异丙肾上腺素注射液",
             "盐酸甲哌卡因肾上腺素注射液", "盐酸肾上腺素注射液",
             "重酒石酸去甲肾上腺素注射液", "阿替卡因肾上腺素注射液"),
    firm = c("远大", "汇海"),
    penal = "2023-2",
    start = "2016-2",
    end   = "2019-3",
    cate = c("限制商品的生产数量或者销售数量", "附加不合理交易条件", "搭售"),
    horizontal = T,
    vertical = F,
    dominant = T
  ),
  "碘化油" = list(
    prod = c("碘化油注射液"),
    firm = c("祥宇"),
    penal = "2023-4",
    start = "2016-2",
    end   = "2020-1",
    cate = c("不公平高价"),
    horizontal = F,
    vertical = F,
    dominant = T
  )
)

ls_api_interest <- list(
  "别嘌醇" = list(prod = c("别嘌醇片")),
  "艾司唑仑" = list(prod = c("艾司唑仑片")),
  "苯酚" = list(
    prod = c("水杨酸苯酚贴膏"),
    size = c("200mg")
  ),
  "水杨酸甲酯" = list(prod = c()),
  "异烟肼" = list(
    prod = c("异烟肼注射液", "异烟肼片"),
    size = c("2ml:100mg", "100mg")
  ),
  "氯苯那敏" = list(prod = c("马来酸氯苯那敏注射液", "马来酸氯苯那敏片")),
  "葡萄糖酸钙" = list(
    prod = c("葡萄糖酸钙注射液"),
    size = c("10ml:1g")
  ),
  "盐酸溴己新" = list(prod = c("注射用盐酸溴己新", "盐酸溴己新片",
                          "盐酸溴己新注射液",
                          "盐酸溴己新葡萄糖注射液")),
  "巴曲酶" = list(prod = c()),
  "醋酸氟轻松" = list(
    prod = c("复方醋酸氟轻松酊", "醋酸氟轻松乳膏"),
    size = c("20ml", "10g:2.5mg")
  ),
  "樟脑" = list(
    prod = c("复方倍氯米松樟脑乳膏", "氧化樟脑注射液"),
    size = c("10g", "1n")
  ),
  "氯解磷定" = list(
    prod = c("氯解磷定注射液"),
    size = c("2ml:500mg")
  ),
  "左卡尼汀" = list(prod = c("左卡尼汀注射液", "注射用左卡尼汀")),
  "肾上腺素" = list(
    prod = c("盐酸去氧肾上腺素注射液", "盐酸异丙肾上腺素注射液",
             # "盐酸甲哌卡因肾上腺素注射液", "阿替卡因肾上腺素注射液",
             "盐酸肾上腺素注射液", "重酒石酸去甲肾上腺素注射液")
  ),
  "碘化油" = list(prod = c("碘化油注射液"))
)
# function ---------------------------------------------
## 1. time count ---------------------------------------------
timeCount <- function(x, measure, start) {
  divisor <- c("w" = 7, "m" = 30, "d" = 1)
  x %>%
    as.Date() %>%
    difftime(as.Date(start), units = "d") %>%
    as.numeric() %>%
    `/`(divisor[measure]) %>%
    floor() %>%
    return()
}

## 2. vlookup ---------------------------------------------
vlookup <- function(dt_index, dt_lookup_area, pointer = NULL, no.match = NA) {
  # {vec, df} :-> vector
  dt_lookup_area %<>% unique()
  if (is.null(pointer)) {
    area <- dt_lookup_area
  } else {
    area <- subset(dt_lookup_area, select = c(1, pointer))
  }
  area %<>%
    as.data.table() %>%
    `names<-`(c("index", "value"))
  index <- dt_index %>% as.data.table() %>% `names<-`("index")
  area[index, on = "index"][[2]] %>%
    replace(is.na(.), no.match) %>%
    return()
}

## 3. de-NA ---------------------------------------------
deNA <- function(dt, value = 0) {
  # replace NAs in data.table with 0; for vectors, using replace
  dt %>%
    setDT() %>%
    mutate_all( ~ replace(., is.na(.), value)) %>%
    setDT() %>%
    return()
}

## 4. matrix flip paste ---------------------------------------------
mat_flip <- function(mat) {
  mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
  return(mat)
}

## 5. cbind tables without recycle ---------------------------------------------
cbind.fill <- function(tbl1, tbl2, fill.with = NA) {
  # extension of cbind: if tbl1.nrow != tbl2.nrow, fill rows with "fill.with"
  # instead of recycle
  max_len <- max(dim(tbl1)[1], dim(tbl2)[1])
  ext_tbl1 <-
    rbind(tbl1, matrix(rep(fill.with, (max_len - dim(tbl1)[1]) * dim(tbl1)[2]),
                       nrow = max_len - dim(tbl1)[1]),
          use.names = F)
  ext_tbl2 <-
    rbind(tbl2, matrix(rep(fill.with, (max_len - dim(tbl2)[1]) * dim(tbl2)[2]),
                       nrow = max_len - dim(tbl2)[1]),
          use.names = F)
  return(cbind(ext_tbl1, ext_tbl2))
}

## 6. manually output table content as latex -----------------------------------
outTex_tbl <- function(tbl,
                       var.names = TRUE,
                       top.lines = "\\\\[-1.8ex]\\hline \\hline\\\\[-1.8ex]",
                       title.lines = "\\hline \\\\[-1.8ex]",
                       bottom.lines = "[1ex]\\hline \\\\[-2.5ex]",
                       column.add = NULL,
                       file.name) {
  # Tex tabular output, without environment, only inner content
  # including lines, by default
  # var.names: if TRUE, use names(tbl) as title. Or no title.
  n.pos <- 0
  for (pos in column.add) {
    tbl <- add_column(tbl, new = rep("", dim(tbl)[1]), .after = pos + n.pos)
    n.pos <- n.pos + 1
  }
  tbl_tex <- apply(tbl, 1, function(row) {
    paste0(paste(row, collapse = " & "), " \\\\")
  })
  if (var.names == TRUE) {
    tbl_tex <- c(paste0(paste(names(tbl), collapse = " & "), " \\\\"),
                 title.lines, tbl_tex)
  }
  tbl_tex <- c(top.lines, tbl_tex, bottom.lines)
  cat(tbl_tex, sep = "\n", file = file.name)
}

## 7. separate a long table ---------------------------------------------
## into pieces by row, and cbind them
cut_tbl <- function(tbl, nrow, N, fill.with = "") {
  # if tbl is 17*2, cut it into N pieces, each piece has nrow rows
  # if out-bounded, fill with "" by default
  new_tbl <- tbl[1:nrow, ]
  for (n in c(2:N)) {
    new_tbl <- cbind(new_tbl, tbl[((n - 1) * nrow + 1):(n * nrow), ])
  }
  names_new_tbl <- names(new_tbl)
  new_tbl %>%
    `names<-`(paste0("V", c(1:dim(new_tbl)[2]))) %>%
    mutate_all(~replace(., is.na(.), fill.with)) %>%
    `names<-`(names_new_tbl) %>%
    return()
}

## 8. full-return ifelse ---------------------------------------------
## E.g., x <- c(1, 2, -1, 0, -3, 4)
##       ifelse(x > 0, x, 9999)
## base::ifelse is scalar-width. This fun is vector-width
ifelse_full <- function(condition, yes, no) {
  if (condition) {
    return(yes)
  } else {
    return(no)
  }
}

## 9. unique count ---------------------------------------------
unicnt <- function(vv) {
  return(length(unique(vv)))
}
