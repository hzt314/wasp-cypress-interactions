host_lf <- read.csv('/Users/han/data/SCU/lab/kuang_project/co_evolution/fig4/host_new/host_plot.csv')
wasp_lf <- read.csv('/Users/han/data/SCU/lab/kuang_project/co_evolution/fig4/wasp_new/lfmm/wasp_plot.csv')
host_co <- read.csv('/Users/han/data/SCU/lab/kuang_project/co_evolution/fig4/host_new/cogwas_den.csv')
wasp_co <- read.csv('/Users/han/data/SCU/lab/kuang_project/co_evolution/fig4/wasp_new/cogwas_den.csv')

host_lf <- host_lf[which(host_lf[,5] < 0.00001),]
wasp_lf <- wasp_lf[which(wasp_lf[,5] < 0.000000000000001),]

#chr <- 1:565
#pos <- 1:565
#for (i in 1:nrow(host_co)) {
#  chr[i] <- as.numeric(strsplit(strsplit(host_co$V3[i], ':')[[1]][1], 'Chr')[[1]][2])
#  pos[i] <- as.numeric(strsplit(host_co$V3[i], ':')[[1]][2])
#}
host_co$chr <- host_co$V1
host_co$pos <- host_co$V2
host_co <- host_co[,5:6]

#chr <- 1:419
#pos <- 1:419
#for (i in 1:nrow(wasp_co)) {
#  chr[i] <- as.numeric(strsplit(strsplit(wasp_co$ID[i], ':')[[1]][1], 'chr')[[1]][2])
#  pos[i] <- as.numeric(strsplit(wasp_co$ID[i], ':')[[1]][2])
#}
wasp_co$chr <- wasp_co$CHR
wasp_co$pos <- wasp_co$POS
wasp_co <- wasp_co[,5:6]
host_lf <- host_lf[,3:4]
wasp_lf <- wasp_lf[,3:4]
#host_co[50:130,1] <- 1
#host_co[218:293,1] <- 2
#host_co[347:403,1] <- 3
#host_co[435:452,1] <- 4
#host_co[524:578,1] <- 5
#host_co[635:667,1] <- 6
#host_co[711:743,1] <- 7
#host_co[792:838,1] <- 8
#host_co[906:925,1] <- 9
#host_co[966:977,1] <- 10
#host_co[1005:1027,1] <- 11

wasp_anno <- read.table('/Users/han/data/SCU/lab/kuang_project/genome/MD_out.gff3')
host_anno <- read.table('/Users/han/data/SCU/lab/kuang_project/genome/Cugi.all.chr.gff')

host_co$gene <- 0
host_lf$gene <- 0
wasp_co$gene <- 0
wasp_lf$gene <- 0

host_co$chr <- paste0('Chr',host_co$chr)
host_lf$CHR <- paste0('Chr',host_lf$CHR)

for (i in 1:nrow(host_co)) {
  if (length(unique(host_anno[which(host_anno$V1 == host_co$chr[i]), 4] < host_co$pos[i] & host_co$pos[i] < host_anno[which(host_anno$V1 == host_co$chr[i]), 5])) == 2) {
    host_co$gene[i] <- host_anno$V9[which(host_anno[which(host_anno$V1 == host_co$chr[i]), 4] < host_co$pos[i] & host_co$pos[i] < host_anno[which(host_anno$V1 == host_co$chr[i]), 5])]
    print(i)
  }
}
for (i in 1:nrow(host_lf)) {
  if (length(unique(host_anno[which(host_anno$V1 == host_lf$CHR[i]), 4] < host_lf$POS[i] & host_lf$POS[i] < host_anno[which(host_anno$V1 == host_lf$CHR[i]), 5])) == 2) {
    host_lf$gene[i] <- host_anno$V9[which(host_anno[which(host_anno$V1 == host_lf$CHR[i]), 4] < host_lf$POS[i] & host_lf$POS[i] < host_anno[which(host_anno$V1 == host_lf$CHR[i]), 5])]
    print(i)
  }
}

wasp_anno$V1[wasp_anno$V1 == 'Hic_asm_0'] <- 1
wasp_anno$V1[wasp_anno$V1 == 'Hic_asm_1'] <- 2
wasp_anno$V1[wasp_anno$V1 == 'Hic_asm_2'] <- 3
wasp_anno$V1[wasp_anno$V1 == 'Hic_asm_3'] <- 4
wasp_anno$V1[wasp_anno$V1 == 'Hic_asm_4'] <- 5

for (i in 1:nrow(wasp_co)) {
  if (length(unique(wasp_anno[which(wasp_anno$V1 == wasp_co$chr[i]), 4] < wasp_co$pos[i] & wasp_co$pos[i] < wasp_anno[which(wasp_anno$V1 == wasp_co$chr[i]), 5])) == 2) {
    wasp_co$gene[i] <- wasp_anno$V9[which(wasp_anno[which(wasp_anno$V1 == wasp_co$chr[i]), 4] < wasp_co$pos[i] & wasp_co$pos[i] < wasp_anno[which(wasp_anno$V1 == wasp_co$chr[i]), 5])]
    print(i)
  }
}
colnames(wasp_lf) <- c('CHR','POS','gene')
for (i in 1:nrow(wasp_lf)) {
  if (length(unique(wasp_anno[which(wasp_anno$V1 == wasp_lf$CHR[i]), 4] < wasp_lf$POS[i] & wasp_lf$POS[i] < wasp_anno[which(wasp_anno$V1 == wasp_lf$CHR[i]), 5])) == 2) {
    wasp_lf$gene[i] <- wasp_anno$V9[which(wasp_anno[which(wasp_anno$V1 == wasp_lf$CHR[i]), 4] < wasp_lf$POS[i] & wasp_lf$POS[i] < wasp_anno[which(wasp_anno$V1 == wasp_lf$CHR[i]), 5])]
    print(i)
  }
}

intersect(host_co$gene, host_lf$gene)
intersect(wasp_co$gene, wasp_lf$gene)

write.csv(host_co, '/Users/han/data/SCU/lab/kuang_project/co_evolution/fig4/host_co.csv')
write.csv(host_lf, '/Users/han/data/SCU/lab/kuang_project/co_evolution/fig4/host_lf.csv')
write.csv(wasp_co, '/Users/han/data/SCU/lab/kuang_project/co_evolution/fig4/wasp_co.csv')
write.csv(wasp_lf, '/Users/han/data/SCU/lab/kuang_project/co_evolution/fig4/wasp_lf.csv')
