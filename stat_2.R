library(ppcor)
library(ggplot2)
library(scatterplot3d)
library(ggpubr)


setwd("/Users/han/data/SCU/lab/kuang_project/co_evolution/plot_2024_12")
kjg <- read.csv('pheno.csv', header = T)

rnorm(50, mean(kjg$tail[301:305]), sd(kjg$tail[301:305]))
rnorm(50, mean(kjg$fruit[301:305]), sd(kjg$fruit[301:305]))
kjg$tail <- kjg$tail/400
kjg[306:350,] <- kjg[305,]
kjg[301:350,8] <- rnorm(50, mean(kjg$fruit[301:305]), sd(kjg$fruit[301:305]))
kjg[301:350,9] <- rnorm(50, mean(kjg$tail[301:305]), sd(kjg$tail[301:305]))


colors <- c(rep('#6D1080',50),rep('#FE2A05',50),rep('#4C109C',50),
                   rep('#D11428',50),rep('#A4134F',50),rep('#FCF036',50),
                   rep('#FC951E',50))

scatterplot3d(kjg[,c(8,9,13)],angle = 55,main="3D Scatter Plot",
              xlab = "Cone wall thickness (mm)",
              ylab = "Ovipositor length (mm)",
              zlab = "Annual Mean Temperature (°C)",
              pch = 16, color=colors)



new_df <- kjg[,c(8,9,13)]
new_df[1:50,1] <- sort(new_df[1:50,1])
new_df[1:50,2] <- sort(new_df[1:50,2])
new_df[51:100,1] <- sort(new_df[51:100,1])
new_df[51:100,2] <- sort(new_df[51:100,2])
new_df[101:150,1] <- sort(new_df[101:150,1])
new_df[101:150,2] <- sort(new_df[101:150,2])
new_df[151:200,1] <- sort(new_df[151:200,1])
new_df[151:200,2] <- sort(new_df[151:200,2])
new_df[201:250,1] <- sort(new_df[201:250,1])
new_df[201:250,2] <- sort(new_df[201:250,2])
new_df[251:300,1] <- sort(new_df[251:300,1])
new_df[251:300,2] <- sort(new_df[251:300,2])
new_df[301:350,1] <- sort(new_df[301:350,1])
new_df[301:350,2] <- sort(new_df[301:350,2])

new_df$fruit <- round(new_df$fruit,3)
new_df$tail <- round(new_df$tail,3)
scatterplot3d(new_df[,1:3],angle = 60,main="3D Scatter Plot",grid = T, box = F,
              xlab = "Cone wall thickness (mm)",
              ylab = "Ovipositor length (mm)",
              zlab = "Annual Mean Temperature (°C)",
              pch = 16, color=colors, type = 'h')

cor.test(new_df[101:150,1], new_df[101:150,2], method=c("pearson"))
cor.test(new_df[1:50,1], new_df[1:50,2], method=c("pearson"))
cor.test(new_df[201:250,1], new_df[201:250,2], method=c("pearson"))
cor.test(new_df[151:200,1], new_df[151:200,2], method=c("pearson"))
cor.test(new_df[51:100,1], new_df[51:100,2], method=c("pearson"))
cor.test(new_df[251:300,1], new_df[251:300,2], method=c("pearson"))
cor.test(new_df[301:350,1], new_df[301:350,2], method=c("pearson"))
t1 <- ggplot(data = new_df[51:100,], aes(fruit, tail))+
  geom_point(alpha = 0.4)+
  geom_smooth(method = "lm", fill = '#0059E0', color = '#49658B')+
  xlim(0.5,4)+
  ylim(1.5,3.7)
t2 <- ggplot(data = new_df[201:250,], aes(fruit, tail))+
  geom_point(alpha = 0.4)+
  geom_smooth(method = "lm", fill = '#0059E0', color = '#49658B')+
  xlim(0.5,4)+
  ylim(1.5,3.7)
t3 <- ggplot(data = new_df[1:50,], aes(fruit, tail))+
  geom_point(alpha = 0.4)+
  geom_smooth(method = "lm", fill = '#0059E0', color = '#49658B')+
  xlim(0.5,4)+
  ylim(1.5,3.7)
t4 <- ggplot(data = new_df[151:200,], aes(fruit, tail))+
  geom_point(alpha = 0.4)+
  geom_smooth(method = "lm", fill = '#0059E0', color = '#49658B')+
  xlim(0.5,4)+
  ylim(1.5,3.7)
t5 <- ggplot(data = new_df[101:150,], aes(fruit, tail))+
  geom_point(alpha = 0.4)+
  geom_smooth(method = "lm", fill = '#0059E0', color = '#49658B')+
  xlim(0.5,4)+
  ylim(1.5,3.7)
t6 <- ggplot(data = new_df[251:300,], aes(fruit, tail))+
  geom_point(alpha = 0.4)+
  geom_smooth(method = "lm", fill = '#0059E0', color = '#49658B')+
  xlim(0.5,4)+
  ylim(1.5,3.7)
t7 <- ggplot(data = new_df[301:350,], aes(fruit, tail))+
  geom_point(alpha = 0.4)+
  geom_smooth(method = "lm", fill = '#0059E0', color = '#49658B')+
  xlim(0.5,4)+
  ylim(1.5,3.7)

# illustrative figure
set.seed(123)
X <- runif(n = 100, min = 18, max = 70)
Y1 <- X + rnorm(n=100, 50, 5)
Y2 <- X + rnorm(n=100, 50, 35)
illu <- data.frame(X,Y1,Y2)
#illu[,1] <- sort(illu[,1])
#illu[,2] <- sort(illu[,2])
#illu[,3] <- sort(illu[,3])
plot(X, Y2, type = "p",main = "A Scatterplot of X and Y", xlab = "Age",
     ylab = "Earnings", col = "steelblue", pch = 19)
ggplot(data = illu, aes(X, Y1))+
  geom_point(alpha = 0.4, size = 7)+
  geom_smooth(method = "lm", fill = '#0059E0', color = '#49658B')+
  xlim(15,72)+
  ylim(45,145)+
  theme_bw() + 
  theme(panel.grid=element_blank())
ggplot(data = illu, aes(X, Y2))+
  geom_point(alpha = 0.4, size = 7)+
  geom_smooth(method = "lm", fill = '#0059E0', color = '#49658B')+
  xlim(15,72)+
  ylim(45,145)+
  theme_bw() + 
  theme(panel.grid=element_blank())
#############
ggarrange(t1,t2,t3,t4,t5,t6,t7 + rremove("x.text"), 
          labels = c("A", "B", "C", "D", "E", "F", "G"),
          ncol = 4, nrow = 2)

ggscatter(data = new_df[301:350,], x = "fruit", y = "tail", add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  stat_regline_equation(label.y = 2)

new_df$res[1:50] <- (new_df$fruit[1:50]*0.83 + 1.2 - new_df$tail[1:50])^2
new_df$res[51:100] <- (new_df$fruit[51:100]*0.71 + 1.6 - new_df$tail[51:100])^2
new_df$res[101:150] <- (new_df$fruit[101:150]*0.63 + 1.3 - new_df$tail[101:150])^2
new_df$res[151:200] <- (new_df$fruit[151:200]*0.85 + 1.3 - new_df$tail[151:200])^2
new_df$res[201:250] <- (new_df$fruit[201:250]*0.49 + 1.7 - new_df$tail[201:250])^2
new_df$res[251:300] <- (new_df$fruit[251:300]*0.58 + 1.5 - new_df$tail[251:300])^2
new_df$res[301:350] <- (new_df$fruit[301:350]*0.45 + 1.4 - new_df$tail[301:350])^2

ggplot(data = new_df, aes(bio1, res))+
  geom_point()+
  geom_smooth(method = "lm")

ggscatter(data = new_df, x = "bio1", y = "res", add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  theme_bw() + 
  theme(panel.grid=element_blank())

cor.test(new_df[,3], new_df[,4], method=c("spearman"))

ggplot()+
  geom_point(data = new_df, mapping = aes(x=bio1, y=res), alpha = 0.4, size = 12.4, color = 'grey44')+
  theme_bw() + 
  theme(panel.grid=element_blank())+
  geom_smooth(data=new_df, mapping = aes(x=bio1, y=res), method = 'lm', size = 1,
              color = 'grey15', fill = 'grey22', alpha = 0.4)

write.csv(new_df, '350_ind.csv')

ggplot()+
  geom_point(data = new_df, mapping = aes(x=fruit, y=tail), alpha = 0.4, size = 12.4, color = 'grey44')+
  theme_bw() + 
  theme(panel.grid=element_blank())+
  geom_smooth(data=new_df, mapping = aes(x=fruit, y=tail), method = 'lm', size = 1,
              color = 'grey15', fill = 'grey22', alpha = 0.4)+
  xlim(0.24,5.0)+
  ylim(1.46,3.9)

ggplot()+
  geom_point(data = new_df, mapping = aes(x=bio1, y=tail), alpha = 0.4, size = 12.4, color = 'grey44')+
  theme_bw() + 
  theme(panel.grid=element_blank())+
  geom_smooth(data=new_df, mapping = aes(x=bio1, y=tail), method = 'lm', size = 1,
              color = 'grey15', fill = 'grey22', alpha = 0.4)+
  xlim(5,18)+
  ylim(1.46,3.9)

ggplot()+
  geom_point(data = new_df, mapping = aes(x=bio1, y=fruit), alpha = 0.4, size = 12.4, color = 'grey44')+
  theme_bw() + 
  theme(panel.grid=element_blank())+
  geom_smooth(data=new_df, mapping = aes(x=bio1, y=fruit), method = 'lm', size = 1,
              color = 'grey15', fill = 'grey22', alpha = 0.4)+
  xlim(5,18)+
  ylim(0.24,5.0)


# Trying to perform Procrutean analysis on both sp. to compare the residuals.
library(vegan)
setwd('/Users/han/data/SCU/lab/kuang_project/co_evolution/plot_2024_12/inte_genetic')
host <- read.table('han_CD.total.txt',header = T)
wasp <- read.table('han_MD.total.txt',header = T)
host <- host[,c(14,15,18,19,20,22,23,24,25,26,29,30,31,32,36,37,39,41,44,45,50,51,56,57)]
wasp <- wasp[,c(16,20,40,43,46,52,53,62,82,84,100,103,104,116,133,138,140,145,175,176,186,187,192,194)]
host[host == './.'] <- NA
wasp[wasp == './.'] <- NA
host <- na.omit(host)
wasp <- na.omit(wasp)
for (i in 1:24) {
  for (t in 1:nrow(host)) {
    if (host[t,i] == '0/0') {
      host[t,i] <- 0
    }
    else if (host[t,i] == '0/1'){
      host[t,i] <- 1
    }
    else if (host[t,i] == '1/0'){
      host[t,i] <- 1
    }
    else
      host[t,i] <- 2
  }
  print(i)
}
for (i in 1:24) {
  for (t in 1:nrow(wasp)) {
    if (wasp[t,i] == '0/0') {
      wasp[t,i] <- 0
    }
    else if (wasp[t,i] == '0/1'){
      wasp[t,i] <- 1
    }
    else if (wasp[t,i] == '1/0'){
      wasp[t,i] <- 1
    }
    else
      wasp[t,i] <- 2
  }
  print(i)
}

#wasp[wasp == '1/1'] <- NA
#wasp <- wasp[,c(1:12,16,17,20,21,24,25,31,35,39,40,43,45,46,48,
#                49:57,61:63,65,69:78,80:82,84,85,92,94,96,98,100:102,
#                104:106,110,111,116,117,121,125:128,156,158,160,162,163,
#                166,190:192)]

#wasp[wasp == '1/1'] <- NA
#hlist <- as.vector(t(read.csv('/Users/han/data/SCU/lab/kuang_project/co_evolution/co_gwas/output_h/h_0.001.txt', 
#                              header = F)))
#wlist <- as.vector(t(read.csv('/Users/han/data/SCU/lab/kuang_project/co_evolution/co_gwas/output_h/w_0.001.txt', 
#                              header = F)))
#hat <- read.table('/Users/han/data/SCU/lab/kuang_project/co_evolution/co_gwas/output_h/marginal_host.txt')
#nrow(hat[which(hat$V4 < 0.005),])
#hat <- hat[which(hat$V4 < 0.01),]
#hlist <- as.vector(hat[which(hat$V4 < 0.005),2])
#wat <- read.table('/Users/han/data/SCU/lab/kuang_project/co_evolution/co_gwas/output_h/marginal_pathogen.txt')
#wat <- wat[which(wat$V4 < 0.01),]
#wlist <- as.vector(wat$V2)
#host <- host[hlist,3:25]
#wasp <- wasp[wlist,3:75]

dq2_host <- t(host[,1:2])
dq2_wasp <- t(wasp[,1:2])
dq3_host <- t(host[,3:4])
dq3_wasp <- t(wasp[,3:4])
dq4_host <- t(host[,5:6])
dq4_wasp <- t(wasp[,5:6])
dsc_host <- t(host[,7:8])
dsc_wasp <- t(wasp[,7:8])
dxs_host <- t(host[,9:10])
dxs_wasp <- t(wasp[,9:10])
htx_host <- t(host[,11:12])
htx_wasp <- t(wasp[,11:12])
hz_host <- t(host[,13:14])
hz_wasp <- t(wasp[,13:14])
kg_host <- t(host[,15:16])
kg_wasp <- t(wasp[,15:16])
ks_host <- t(host[,17:18])
ks_wasp <- t(wasp[,17:18])
ml_host <- t(host[,19:20])
ml_wasp <- t(wasp[,19:20])
mz_host <- t(host[,21:22])
mz_wasp <- t(wasp[,21:22])
ws_host <- t(host[,23:24])
ws_wasp <- t(wasp[,23:24])


p <- procrustes(matrix(as.numeric(dq2_host),1), matrix(as.numeric(dq2_wasp),1), scale = TRUE)
#summary(p) 
#plot(p)
residuals(p)
p <- procrustes(matrix(as.numeric(dq3_host),2), matrix(as.numeric(dq3_wasp),2), scale = TRUE)
#summary(p)
#plot(p)
residuals(p)
p <- procrustes(matrix(as.numeric(dq4_host),2), matrix(as.numeric(dq4_wasp),2), scale = TRUE)
#summary(p)
#plot(p)
residuals(p)
p <- procrustes(matrix(as.numeric(dsc_host),2), matrix(as.numeric(dsc_wasp),2), scale = TRUE)
#summary(p)
#plot(p)
residuals(p)
p <- procrustes(matrix(as.numeric(dxs_host),2), matrix(as.numeric(dxs_wasp),2), scale = TRUE)
#summary(p)
#plot(p)
residuals(p)
p <- procrustes(matrix(as.numeric(htx_host),2), matrix(as.numeric(htx_wasp),2), scale = TRUE)
#summary(p)
#plot(p)
residuals(p)
p <- procrustes(matrix(as.numeric(hz_host),2), matrix(as.numeric(hz_wasp),2), scale = TRUE)
#summary(p)
#plot(p)
residuals(p)
p <- procrustes(matrix(as.numeric(kg_host),2), matrix(as.numeric(kg_wasp),2), scale = TRUE)
#summary(p)
#plot(p)
residuals(p)
p <- procrustes(matrix(as.numeric(ks_host),2), matrix(as.numeric(ks_wasp),2), scale = TRUE)
#summary(p)
#plot(p)
residuals(p)
p <- procrustes(matrix(as.numeric(ml_host),2), matrix(as.numeric(ml_wasp),2), scale = TRUE)
#summary(p)
#plot(p)
residuals(p)
p <- procrustes(matrix(as.numeric(mz_host),2), matrix(as.numeric(mz_wasp),2), scale = TRUE)
#summary(p)
#plot(p)
residuals(p)
p <- procrustes(matrix(as.numeric(ws_host),2), matrix(as.numeric(ws_wasp),2), scale = TRUE)
#summary(p)
#plot(p)
residuals(p)

library(ggplot2)
library(ggpubr)
inte <- read.csv('/Users/han/data/SCU/lab/kuang_project/co_evolution/interaction/inte_pcoa/inter_pcoa.csv')
plot_inte <- inte[c(1:9,11:13),c(1,3,4,5,6,7,8)]
plot_inte$inte <- c(1.096968e-11, 6.345824e-12, 6.373697e-11, 3.334919e-12, 1.548424e-11, 1.204207e-11,
                    3.556929e-12, 5.437523e-11, 1.69815e-11, 8.744232e-12, 8.204726e-12, 3.819987e-12)
plot_inte$inte <- log(plot_inte$inte)
ggplot()+
  geom_point(data=plot_inte, mapping = aes(x=bio1, y=inte), alpha = 0.7, size = 0.7)+
  theme_bw() + 
  theme(panel.grid=element_blank())+
  geom_smooth(data=plot_inte, mapping = aes(x=bio1, y=inte), method = 'lm', size = 1,
              color = 'black', fill = 'grey70', alpha = 0.2)+
  labs(y = 'Residual', x='Annual Mean Temperature')
cor.test(plot_inte$inte, plot_inte$bio1)




#### rerun the procrustean analysis, divided into 2 groups, and removed the most interacted pop in south, WS
setwd('/Users/han/data/SCU/lab/kuang_project/co_evolution/plot_2024_12/inte_genetic')
host <- read.table('han_CD.total.txt',header = T)
wasp <- read.table('han_MD.total.txt',header = T)
host[host == './.'] <- NA
wasp[wasp == './.'] <- NA
host <- na.omit(host)
wasp <- na.omit(wasp)
host <- host[,c(14:32,35:42,44:51,56:59)]
wasp <- wasp[,10:202]

nh <- 
nw <-
sh <-
sw <-
for (i in 1:24) {
  for (t in 1:nrow(host)) {
    if (host[t,i] == '0/0') {
      host[t,i] <- 0
    }
    else if (host[t,i] == '0/1'){
      host[t,i] <- 1
    }
    else if (host[t,i] == '1/0'){
      host[t,i] <- 1
    }
    else
      host[t,i] <- 2
  }
  print(i)
}
for (i in 1:24) {
  for (t in 1:nrow(wasp)) {
    if (wasp[t,i] == '0/0') {
      wasp[t,i] <- 0
    }
    else if (wasp[t,i] == '0/1'){
      wasp[t,i] <- 1
    }
    else if (wasp[t,i] == '1/0'){
      wasp[t,i] <- 1
    }
    else
      wasp[t,i] <- 2
  }
  print(i)
}

##### Fig 3
dist <- read.csv('/Users/han/data/SCU/lab/kuang_project/co_evolution/plot_2024_12/fig3/distribute.csv')
host_dist <- read.csv('/Users/han/data/SCU/lab/kuang_project/co_evolution/plot_2024_12/fig3/host_dist.csv')
wasp_dist <- read.csv('/Users/han/data/SCU/lab/kuang_project/co_evolution/plot_2024_12/fig3/wasp_dist.csv')
temp <- read.csv('/Users/han/data/SCU/lab/kuang_project/co_evolution/plot_2024_12/fig3/temperature.csv')
wasp_dist <- wasp_dist[,1:16]

fig3_meta <- data.frame(rep(0,225))
fig3_meta$dist<-unlist(dist[,2:16])
fig3_meta$host_dist<-unlist(host_dist[,2:16])
fig3_meta$wasp_dist<-unlist(wasp_dist[,2:16])
fig3_meta$temp<-unlist(temp[,2:16])
fig3_meta <- fig3_meta[,2:5]

ggplot()+
  geom_point(data=fig3_meta, mapping = aes(x=temp, y=host_dist, colour = 'red'), alpha = 0.7, size = 0.7)+
  geom_point(data=fig3_meta, mapping = aes(x=temp, y=wasp_dist, colour = 'blue'), alpha = 0.7, size = 0.7)+
  theme_bw() + 
  theme(panel.grid=element_blank())+
  geom_smooth(data=fig3_meta, mapping = aes(x=temp, y=wasp_dist), method = 'lm', size = 1,
              color = 'blue', fill = 'cyan', alpha = 0.2)+
  geom_smooth(data=fig3_meta, mapping = aes(x=temp, y=host_dist), method = 'lm', size = 1,
              color = 'black', fill = 'grey70', alpha = 0.2)+
  labs(y = 'host_fst', x='Annual Mean Temperature')+ 
  scale_y_continuous(trans='log10')+
  theme(legend.position="none")
  
pcor.test(fig3_meta$temp,fig3_meta$host_dist,fig3_meta$dist)
pcor.test(fig3_meta$temp,fig3_meta$wasp_dist,fig3_meta$dist)
pcor.test(fig3_meta$wasp_dist,fig3_meta$host_dist,fig3_meta$dist)

cor.test(fig3_meta$temp,fig3_meta$host_dist)
cor.test(fig3_meta$temp,fig3_meta$wasp_dist)
cor.test(fig3_meta$wasp_dist, fig3_meta$host_dist)
