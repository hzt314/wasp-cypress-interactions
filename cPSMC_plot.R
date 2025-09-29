setwd('/Users/han/data/SCU/lab/kuang_project/co_evolution/C_PSMC/Co_PSMC/')
wasp <- read.csv('MD_LJ_plot.csv')
host <- read.csv('CD_LJ_plot.csv')
#wasp$x <- wasp$x*100
wasp <- wasp[c(1,9,16,22,29,31,34,37,40,44,54),]
host <- host[c(1,5,8,11,14,17,20,23,26,29,32,35,37,39,41,43,45,47,51),]

#plot(wasp[,2],wasp[,3])
#lines(wasp[,2],wasp[,3])
#lines(host[,2],host[,3])
for (i in 2:nrow(wasp)) {
  wasp[i,5] <- wasp[i-1,3] / wasp[i,3]
}
wasp[,5] <- wasp[,5] - 1
plot(wasp[2:11,2],wasp[2:11,5])

for (i in 2:nrow(host)) {
  host[i,5] <- host[i-1,3] / host[i,3]
}
host[,5] <- host[,5] - 1
host$x <- host$x/100

f.w <- lm(wasp[2:11,5] ~ poly(wasp[2:11,2],6))
plot(wasp[2:11,2],wasp[2:11,5])
lines(wasp[2:11,2],wasp[2:11,5])
lines(host[2:19,2],host[2:19,5])

library(ggplot2)
df_p <- rbind(wasp[2:11,c(2,5)],host[2:19,c(2,5)])
df_p$group <- c(rep('wasp',10),rep('host',18))
colnames(df_p) <- c('YBP', 'Ne', 'group')
p1 <- ggplot(df_p,aes(x = YBP, y = Ne, group = group))+
  geom_line(aes(color=group), size = 0.7, linetype='solid')+
  geom_point(alpha = 0.3, aes(color = group))+
  theme_bw() + 
  theme(panel.grid=element_blank()) + 
  labs(y = 'Increasing ratio of Ne', x = 'Year Before Present') +
  scale_color_manual(values=c('#235494','orange')) +
  scale_x_continuous(breaks = seq(0,20000,1000), limits = c(0,16000), expand = c(0,100))+
  theme(legend.position="none")
  

cli <- read.csv('cli1.csv')[,c(1,2)]
cli <- cli[8:574,]
cli[,1] <- as.numeric(cli[,1])
cli[,2] <- as.numeric(cli[,2])
p2 <- ggplot(cli,aes(x = Age, y = Temperature),xlim = c(0,16000))+
  geom_line()+
  theme_bw() + 
  theme(panel.grid=element_blank()) + 
  labs(y = 'Temperature', x = 'Year Before Present') +
  scale_x_continuous(breaks = seq(0,16000,1000), limits = c(0,16000), expand = c(0,100))


library(ggpubr)
ggarrange(p1, p2, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)





lines(wasp[2:11,2], fitted(f.w))


ht <- vector()
wt <- vector()

for (i in 1:nrow(host)-1) {
  ht[i] <- (host[i,2] + host[i+1,2])/2
}
for (i in 1:nrow(wasp)-1) {
  wt[i] <- (wasp[i,2] + wasp[i+1,2])/2
}

plot(ht, host[2:19,5])
lines(ht, host[2:19,5])
lines(wt,wasp[2:11,5])

f.h <- lm(host[2:19,5] ~ poly(host[2:19,2],6))
plot(host[2:19,2],host[2:19,5])
lines(host[2:19,2], fitted(f.h))

hw <- function(x) (f.h$coefficients - f.w$coefficients)[2]*x + (f.h$coefficients - f.w$coefficients)[3]*x^2 +
  (f.h$coefficients - f.w$coefficients)[4]*x^3 + (f.h$coefficients - f.w$coefficients)[5]*x^4 +
  (f.h$coefficients - f.w$coefficients)[6]*x^5 + (f.h$coefficients - f.w$coefficients)[7]*x^6 +
  (f.h$coefficients - f.w$coefficients)[1]
curve(hw,0,20000)
