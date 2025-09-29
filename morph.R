rm(list = ls()) # initialize env
setwd('/Users/han/data/SCU/lab/kuang_project/co_evolution/morph')
library(dplyr)
library(ggplot2)
library(Momocs)
library(ggpubr)

#### tail length function description
distcal <- function(x){
  a <- dist(x)
  v <- vector()
  v[1] = 1
  for (i in 2:(nrow(x)-1)) {
    v[i] <- v[i-1]+nrow(x)-i+1
  }
  return(sum(a[v]))
}
### Readin data
setwd('/Users/han/data/SCU/lab/kuang_project/co_evolution/morph/photos/good')
file_list <- list.files(recursive = T)
t <- list()
s <- list()
ta <- list()
for (i in 1:length(file_list)) {
  if (grepl('t.txt', file_list[i]) == T) {
    t[[i]] <- c(read.delim(file_list[i], header = F),strsplit(file_list[i],'/')[[1]][1])
  } else if (grepl('s.txt', file_list[i]) == T) {
    s[[i]] <- c(read.delim(file_list[i], header = F),strsplit(file_list[i],'/')[[1]][1])
  } else if (grepl('tail.txt', file_list[i]) == T) {
    ta[[i]] <- c(distcal(read.delim(file_list[i], header = F)),strsplit(file_list[i],'/')[[1]][1])
  }
}
s=s[!sapply(s,is.null)]
t=t[!sapply(t,is.null)]
ta=ta[!sapply(ta,is.null)]

r_s <- list()
r_t <- list()
for (i in 1:length(s)) {
  r_s[[i]] <- data.frame(x = s[[i]][[1]], y = s[[i]][[2]]) %>% as.matrix()
  r_t[[i]] <- data.frame(x = t[[i]][[1]], y = t[[i]][[2]]) %>% as.matrix()
}

r_s <- Out(r_s)
r_t <- Out(r_t)
stack(r_s)
stack(r_t)

vol <- scale(coo_area(r_s)*coo_area(r_t)^(3/4), scale = T, center = F)
name_list <- vector()
for (i in 1:length(file_list)) {
  if (grepl('t.txt', file_list[i]) == T) {
    name_list[i] <- strsplit(file_list[i],'/')[[1]][1]}}
name_list = name_list[!is.na(name_list)]

meta <- data.frame(name = name_list, volume = vol, tail = NA)
for (i in 1:length(ta)) {
  meta[which(grepl(ta[[i]][2], meta$name)),3] <- as.numeric(ta[[i]][1])
}
write.csv(meta, 'meta.csv')


# manually added pop info
setwd('/Users/han/data/SCU/lab/kuang_project/co_evolution/morph/')
meta <- read.csv('meta.csv')
ggboxplot(meta[which(meta$pop != 'KG'),], x = 'pop', y = 'volume',
          add = 'jitter') + 
  stat_compare_means()
ggboxplot(meta, x = 'NS', y = 'volume',
          add = 'jitter') + 
  stat_compare_means()
ggboxplot(meta, x = 'gender', y = 'volume',
          add = 'jitter') + 
  stat_compare_means()
meta$rat <- meta$tail/meta$volume
ggboxplot(meta, x = 'pop', y = 'rat',
          add = 'jitter') + 
  stat_compare_means()
ggboxplot(meta, x = 'pop', y = 'tail',
          add = 'jitter') + 
  stat_compare_means()
wall <- t(wall)
wall <- as.data.frame(wall)
wall$pop <- as.vector(c(rep('WS',5),rep('LJ',5),rep('HZ',5),rep('DXS',5),rep('DQ',5),rep('DS',5)))
wall$avg <- mean(wall[,1:10])
for (i in 1:30) {
  wall[i,12] <- mean(unlist(wall[i,1:10]))
}
ggboxplot(wall, x = 'pop', y = 'avg',
          add = 'jitter') + 
  stat_compare_means()


tmp <- meta[which(!is.na(meta$tail)),]
tmp <- tmp[which(tmp$pop != 'KG'),]
tail <- tmp[which(tmp$pop != 'KS'),]
rm(tmp)

ggboxplot(tail, x = 'NS', y = 'tail',
          add = 'jitter') + 
  stat_compare_means()
ggboxplot(tail, x = 'pop', y = 'tail',
          add = 'jitter') + 
  stat_compare_means()

ggboxplot(meta[which(meta$pop != 'KG'),], x = 'NS', y = 'volume',
          add = 'jitter') + 
  stat_compare_means()


boot_vol <- data.frame(vol = c(replicate(100, expr = {
  u <- meta[which(meta$pop == 'DQ'),][sample(nrow(meta[which(meta$pop == 'DQ'),]), 3),]
  median(u$volume)}),
replicate(100, expr = {
  u <- meta[which(meta$pop == 'DS'),][sample(nrow(meta[which(meta$pop == 'DS'),]), 3),]
  median(u$volume)}),
replicate(100, expr = {
  u <- meta[which(meta$pop == 'DXS'),][sample(nrow(meta[which(meta$pop == 'DXS'),]), 3),]
  median(u$volume)}),
replicate(100, expr = {
  u <- meta[which(meta$pop == 'HZ'),][sample(nrow(meta[which(meta$pop == 'HZ'),]), 3),]
  median(u$volume)}),
replicate(100, expr = {
  u <- meta[which(meta$pop == 'KG'),][sample(nrow(meta[which(meta$pop == 'KG'),]), 3),]
  median(u$volume)}),
replicate(100, expr = {
  u <- meta[which(meta$pop == 'KS'),][sample(nrow(meta[which(meta$pop == 'KS'),]), 3),]
  median(u$volume)}),
replicate(100, expr = {
  u <- meta[which(meta$pop == 'LJ'),][sample(nrow(meta[which(meta$pop == 'LJ'),]), 3),]
  median(u$volume)}),
replicate(100, expr = {
  u <- meta[which(meta$pop == 'WS'),][sample(nrow(meta[which(meta$pop == 'WS'),]), 3),]
  median(u$volume)}),
replicate(100, expr = {
  u <- meta[which(meta$pop == 'XC'),][sample(nrow(meta[which(meta$pop == 'XC'),]), 3),]
  median(u$volume)})), 
pop = rep(unique(meta$pop), rep(100,9)),
NS = rep(c('N','S','N','S','N','N','S','S','S'), rep(100,9)))

ggboxplot(boot_vol, x = 'NS', y = 'vol',
          add = 'jitter', color = 'NS', palette = 'jco') + 
  stat_compare_means(method = 't.test')
## error test
           