diff <- readxl::read_xlsx('/Users/han/data/SCU/lab/kuang_project/co_evolution/trans_meta/01.Data/quantitation/metabolome.abundance.xlsx')
diff <- diff[which(diff$`Class I` == 'Terpenoids'),]
diff$WSC_mean <- (diff$`WS-C-1` + diff$`WS-C-2` + diff$`WS-C-3`)/3
diff$WSG_mean <- (diff$`WS-G-1` + diff$`WS-G-2` + diff$`WS-G-3`)/3
diff$DQG_mean <- (diff$`DQ-G-1` + diff$`DQ-G-2` + diff$`DQ-G-3`)/3
#diff<-diff[which(diff$KEGG_map != '--'),]
diff_new <- data.frame(rep(0,1000))
diff_new[1:250,1] <- diff[,11]
diff_new[251:500,1] <- diff[,17]
diff_new[501:750,1] <- diff[,18]
diff_new[751:1000,1] <- diff[,19]
diff_new$group <- c(rep('DQC',250),rep('WSC',250),rep('WSG',250),rep('DQG',250))
colnames(diff_new) <- c('count', 'group')


diff_2 <- data.frame(rep(0,1500))
diff_2[1:250,1] <- diff[,5]
diff_2[251:500,1] <- diff[,6]
diff_2[501:750,1] <- diff[,7]
diff_2[751:1000,1] <- diff[,8]
diff_2[1001:1250,1] <- diff[,9]
diff_2[1251:1500,1] <- diff[,10]
diff_2$group <- c(rep('WS',750),rep('DQ',750))
colnames(diff_2) <- c('count', 'group')

library(ggplot2)
library(ggpubr)
ggplot(diff_new[c(501:1000),], aes(x=group, y=count)) + 
  geom_boxplot()
ggboxplot(diff_new[c(501:1000),], x = "group", y = "count")+ 
  stat_compare_means(method = 't.test')+
  geom_jitter(color="black", size=0.4, alpha=0.9)

ggboxplot(diff_2, x = "group", y = "count")+ 
  stat_compare_means(method = 't.test')
