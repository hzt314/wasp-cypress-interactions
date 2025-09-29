inte <- read.csv('/Users/han/data/SCU/lab/kuang_project/co_evolution/interaction/inte_pcoa/inter_pcoa.csv')
p <- 1:19
r <- 1:19
for (i in 8:26) {
  p[i-7] <- cor.test(inte$interaction, inte[,i])[3]
  r[i-7] <- cor.test(inte$interaction, inte[,i])[4]
}
cor <- data.frame(unlist(p), unlist(r))
cor.test(inte$interaction, inte$bio1)
plot(x = inte$bio1, y = inte$interaction)
pcor.test(inte$interaction, inte$bio1, inte[,3:4])
library(ggplot2)
library(ggpubr)
ggplot()+
  geom_point(data=inte, mapping = aes(x=bio1, y=interaction), alpha = 0.7, size = 0.7)+
  theme_bw() + 
  theme(panel.grid=element_blank())+
  geom_smooth(data=inte, mapping = aes(x=bio1, y=interaction), method = 'lm', size = 1,
              color = 'black', fill = 'grey70', alpha = 0.2)+
  labs(y = 'Residual', x='Annual Mean Temperature')
cor.test(inte$interaction, inte$bio1)




library(lme4)
library(lmerTest)
trait <- read.csv('/Users/han/data/SCU/lab/kuang_project/co_evolution/interaction/inte_trait/inte.csv')
trait$tail <- trait$tail*0.0025
ggplot()+
  geom_point(data = trait, mapping = aes(x=fruit, y=tail, color=Pop), alpha = 0.7, size = 0.7)+
  theme_bw() + 
  theme(panel.grid=element_blank())+
  geom_smooth(data=trait, mapping = aes(x=fruit, y=tail), method = 'lm', size = 1,
              color = 'black', fill = 'grey70', alpha = 0.2)

cor.test(trait$fruit, trait$tail)
ggscatter(trait[6:305,], x = "fruit", y = "tail", add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  stat_regline_equation(label.y = 2)

trait_id <- unique(trait$Pop)

trait$res <- (trait$fruit*0.32 + 2.1 - trait$tail)^2
trait[1:4,5:23] <- inte[1,8:26]

res_mean <- data.frame(trait_id,c(mean(trait[which(trait$Pop == trait_id[1]),27]),
  mean(trait[which(trait$Pop == trait_id[2]),27]),
  mean(trait[which(trait$Pop == trait_id[3]),27]),
  mean(trait[which(trait$Pop == trait_id[4]),27]),
  mean(trait[which(trait$Pop == trait_id[5]),27]),
  mean(trait[which(trait$Pop == trait_id[6]),27]),
  mean(trait[which(trait$Pop == trait_id[7]),27]),
  mean(trait[which(trait$Pop == trait_id[8]),27])))
colnames(res_mean) <- c('pop','res')
res_mean$bio1 <- unique(trait$bio1)




new_trait <- trait[c(1:8,51:54,101:104,151:154,201:204,251:254,301:304),]
new_trait[5,3:4] <- c(mean(trait[5:16,3]),mean(trait[5:16,4]))
new_trait[6,3:4] <- c(mean(trait[17:28,3]),mean(trait[17:28,4]))
new_trait[7,3:4] <- c(mean(trait[29:40,3]),mean(trait[29:40,4]))
new_trait[8,3:4] <- c(mean(trait[41:50,3]),mean(trait[41:50,4]))
new_trait[9,3:4] <- c(mean(trait[51:64,3]),mean(trait[51:64,4]))
new_trait[10,3:4] <- c(mean(trait[65:77,3]),mean(trait[65:77,4]))
new_trait[11,3:4] <- c(mean(trait[78:89,3]),mean(trait[78:89,4]))
new_trait[12,3:4] <- c(mean(trait[90:100,3]),mean(trait[90:100,4]))
new_trait[13,3:4] <- c(mean(trait[101:112,3]),mean(trait[101:112,4]))
new_trait[14,3:4] <- c(mean(trait[113:125,3]),mean(trait[113:125,4]))
new_trait[15,3:4] <- c(mean(trait[126:139,3]),mean(trait[126:139,4]))
new_trait[16,3:4] <- c(mean(trait[140:150,3]),mean(trait[140:150,4]))
new_trait[17,3:4] <- c(mean(trait[151:164,3]),mean(trait[151:164,4]))
new_trait[18,3:4] <- c(mean(trait[165:177,3]),mean(trait[165:177,4]))
new_trait[19,3:4] <- c(mean(trait[178:190,3]),mean(trait[178:190,4]))
new_trait[20,3:4] <- c(mean(trait[191:200,3]),mean(trait[191:200,4]))
new_trait[21,3:4] <- c(mean(trait[201:212,3]),mean(trait[201:212,4]))
new_trait[22,3:4] <- c(mean(trait[213:225,3]),mean(trait[213:225,4]))
new_trait[23,3:4] <- c(mean(trait[226:238,3]),mean(trait[226:238,4]))
new_trait[24,3:4] <- c(mean(trait[239:250,3]),mean(trait[239:250,4]))
new_trait[25,3:4] <- c(mean(trait[251:263,3]),mean(trait[251:263,4]))
new_trait[26,3:4] <- c(mean(trait[264:277,3]),mean(trait[264:277,4]))
new_trait[27,3:4] <- c(mean(trait[278:290,3]),mean(trait[278:290,4]))
new_trait[28,3:4] <- c(mean(trait[291:300,3]),mean(trait[291:300,4]))
new_trait[29,3:4] <- c(mean(trait[c(301,303),3]),mean(trait[c(301,303),4]))

new_trait$res <- (new_trait$fruit*140 + 780 - new_trait$tail)^2

new_trait <- new_trait[1:28,]

plot_tail <- data.frame(rep(0,7))
plot_tail$mean <- c(mean(trait[which(trait$Pop == 'WS'),4]),
               mean(trait[which(trait$Pop == 'DS'),4]),mean(trait[which(trait$Pop == 'HZ'),4]),
               mean(trait[which(trait$Pop == 'LJ'),4]),mean(trait[which(trait$Pop == 'DQ3'),4]),
               mean(trait[which(trait$Pop == 'DQ2'),4]),mean(trait[which(trait$Pop == 'DXS'),4]))
plot_tail$sd <- c(sd(trait[which(trait$Pop == 'WS'),4]),
                    sd(trait[which(trait$Pop == 'DS'),4]),sd(trait[which(trait$Pop == 'HZ'),4]),
                    sd(trait[which(trait$Pop == 'LJ'),4]),sd(trait[which(trait$Pop == 'DQ3'),4]),
                    sd(trait[which(trait$Pop == 'DQ2'),4]),sd(trait[which(trait$Pop == 'DXS'),4]))
plot_tail$bio1 <- unique(trait$bio1)[2:8]

plot_fruit <- data.frame(rep(0,7))
plot_fruit$mean <- c(mean(trait[which(trait$Pop == 'WS'),3]),
                    mean(trait[which(trait$Pop == 'DS'),3]),mean(trait[which(trait$Pop == 'HZ'),3]),
                    mean(trait[which(trait$Pop == 'LJ'),3]),mean(trait[which(trait$Pop == 'DQ3'),3]),
                    mean(trait[which(trait$Pop == 'DQ2'),3]),mean(trait[which(trait$Pop == 'DXS'),3]))
plot_fruit$sd <- c(sd(trait[which(trait$Pop == 'WS'),3]),
                  sd(trait[which(trait$Pop == 'DS'),3]),sd(trait[which(trait$Pop == 'HZ'),3]),
                  sd(trait[which(trait$Pop == 'LJ'),3]),sd(trait[which(trait$Pop == 'DQ3'),3]),
                  sd(trait[which(trait$Pop == 'DQ2'),3]),sd(trait[which(trait$Pop == 'DXS'),3]))
plot_fruit$bio1 <- unique(trait$bio1)[2:8]

plot_res <- data.frame(rep(0,7))
plot_res$mean <- c(mean(trait[which(trait$Pop == 'WS'),27]),
                     mean(trait[which(trait$Pop == 'DS'),27]),mean(trait[which(trait$Pop == 'HZ'),27]),
                     mean(trait[which(trait$Pop == 'LJ'),27]),mean(trait[which(trait$Pop == 'DQ3'),27]),
                     mean(trait[which(trait$Pop == 'DQ2'),27]),mean(trait[which(trait$Pop == 'DXS'),27]))
plot_res$sd <- c(sd(trait[which(trait$Pop == 'WS'),27]),
                   sd(trait[which(trait$Pop == 'DS'),27]),sd(trait[which(trait$Pop == 'HZ'),27]),
                   sd(trait[which(trait$Pop == 'LJ'),27]),sd(trait[which(trait$Pop == 'DQ3'),27]),
                   sd(trait[which(trait$Pop == 'DQ2'),27]),sd(trait[which(trait$Pop == 'DXS'),27]))
plot_res$bio1 <- unique(trait$bio1)[2:8]


ggplot(plot_tail, aes(x=bio1, y=mean)) + 
  geom_point()+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(0.05))+
  theme_bw() + 
  theme(panel.grid=element_blank())+
  geom_smooth(data=trait, mapping = aes(x=bio1, y=tail), method = 'lm', size = 1,
              color = 'black', fill = 'grey70', alpha = 0.2)

ggplot(plot_fruit, aes(x=bio1, y=mean)) + 
  geom_point()+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(0.05))+
  theme_bw() + 
  theme(panel.grid=element_blank())+
  geom_smooth(data=trait, mapping = aes(x=bio1, y=fruit), method = 'lm', size = 1,
              color = 'black', fill = 'grey70', alpha = 0.2)

ggplot(plot_res, aes(x=bio1, y=mean)) + 
  geom_point()+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(0.05))+
  theme_bw() + 
  theme(panel.grid=element_blank())+
  geom_smooth(data=plot_res, mapping = aes(x=bio1, y=mean), method = 'lm', size = 1,
              color = 'black', fill = 'grey70', alpha = 0.2)





ggplot()+
  geom_point(data = new_trait, mapping = aes(x=bio1, y=res), alpha = 0.7, size = 0.7)+
  theme_bw() + 
  theme(panel.grid=element_blank())+
  geom_smooth(data=new_trait, mapping = aes(x=bio1, y=res), method = 'lm', size = 1,
              color = 'black', fill = 'grey70', alpha = 0.2)




ggplot()+
  geom_point(data = trait, mapping = aes(x=bio1, y=fruit), alpha = 0.7, size = 1)+
  theme_bw() + 
  theme(panel.grid=element_blank())+
  geom_smooth(data=trait, mapping = aes(x=bio1, y=fruit), method = 'lm', size = 1,
              color = 'black', fill = 'grey30', alpha = 0.2)+
  labs(y = 'fruit', x='Annual Mean Temperature')


ggplot()+
  geom_point(data = trait, mapping = aes(x=bio1, y=tail), alpha = 0.7, size = 1)+
  theme_bw() + 
  theme(panel.grid=element_blank())+
  geom_smooth(data=trait, mapping = aes(x=bio1, y=tail), method = 'lm', size = 1,
              color = 'black', fill = 'grey30', alpha = 0.2)+
  labs(y = 'tail', x='Annual Mean Temperature')

ggplot()+
  geom_point(data = trait, mapping = aes(x=bio1, y=res), alpha = 0.7, size = 0.7)+
  theme_bw() + 
  theme(panel.grid=element_blank())+
  geom_smooth(data=trait, mapping = aes(x=bio1, y=res), method = 'lm', size = 1,
              color = 'black', fill = 'grey70', alpha = 0.2)+
  labs(y = 'Residual', x='Annual Mean Temperature')


cor.test(trait$fruit, trait$bio1)
cor.test(trait$tail, trait$bio1)
cor.test(trait$res, trait$bio1)

ggscatter(trait, x = "bio1", y = "res", add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  stat_regline_equation(label.y = 1)

cor.test(trait[,27], trait[,5])

trait <- trait[order(trait$bio1, decreasing = TRUE),]
trait$x <- factor(trait$Pop, levels=c("XC","WS","DS","HZ","LJ","DQ3","DQ2","DXS"))

cbPalette <- c('#5D269D',"#009E73", "#A52494", "#BBA13A", "#999999","#0072B2","#D55E00")
ggplot(data=trait[6:305,],aes(x=factor(Pop,levels = c("WS","DS","HZ","LJ","DQ3","DQ2","DXS")),y=res,color=Pop))+
  geom_jitter(alpha=0.2,
              position=position_jitterdodge(jitter.width = 0.35, 
                                            jitter.height = 0, 
                                            dodge.width = 0.8))+
  geom_boxplot(alpha=0.2,width=0.45,
               position=position_dodge(width=0.8),
               size=0.75,outlier.colour = NA)+
  geom_violin(alpha=0.2,width=0.9,
              position=position_dodge(width=0.8),
              size=0.75)+
  scale_color_manual(values = cbPalette)+
  theme_classic() +
  theme(legend.position="none") + 
  theme(text = element_text(size=16)) + 
  #ylim(0.0,1.3)+
  ylab("Squared residuals")+
  xlab('Populations')
  


tail <- na.omit(tail)
tail$pop
id <- intersect(sub("\\..*", "",colnames(wall)),tail$pop)
sub("\\..*", "",colnames(wall))
tail <- tail[which(tail$pop %in% id),]
tail$pop

for (i in 1:length(id)) {
  lmer(unlist(tail[which(tail$pop == id[i]),4])~unlist(wall[,which(sub("\\..*", "",colnames(wall)) == id[i])]))
}

model <- lmer()

summary(model)
