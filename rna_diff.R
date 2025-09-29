library(DESeq2)

options(stringsAsFactors = FALSE)
dc1 <-read.table("/Users/han/data/SCU/lab/kuang_project/count/DSC-C-1_FRAS210052148-1a_matrix.count",
                sep = "\t",col.names = c("gene_id","dc1"))
dc2 <-read.table("/Users/han/data/SCU/lab/kuang_project/count/DSC-C-2_FRAS210052149-1a_matrix.count",
                sep = "\t",col.names = c("gene_id","dc2"))
dc3 <-read.table("/Users/han/data/SCU/lab/kuang_project/count/DSC-C-3_FRAS210052150-1a_matrix.count",
                sep = "\t",col.names = c("gene_id","dc3"))
dc4 <-read.table("/Users/han/data/SCU/lab/kuang_project/count/DSC-C-4_FRAS210052151-1a_matrix.count",
                sep = "\t",col.names = c("gene_id","dc4"))
dc5 <-read.table("/Users/han/data/SCU/lab/kuang_project/count/DSC-C-5_FRAS210052152-1a_matrix.count",
                sep = "\t",col.names = c("gene_id","dc5"))
dc6 <-read.table("/Users/han/data/SCU/lab/kuang_project/count/DSC-C-6_FRAS210052153-1a_matrix.count",
                sep = "\t",col.names = c("gene_id","dc6"))
dc7 <-read.table("/Users/han/data/SCU/lab/kuang_project/count/XLHS-C_FRAS210052146-1a_matrix.count",
                 sep = "\t",col.names = c("gene_id","dc7"))
dx1 <-read.table("/Users/han/data/SCU/lab/kuang_project/count/DSC-X-1_FRAS210052147-1a_matrix.count",
                sep = "\t",col.names = c("gene_id","dx1"))
dx2 <-read.table("/Users/han/data/SCU/lab/kuang_project/count/MZ-X-1_FRAS210052154-1a_matrix.count",
                 sep = "\t",col.names = c("gene_id","dx2"))
dx3 <-read.table("/Users/han/data/SCU/lab/kuang_project/count/MZ-X-2_FRAS210052155-1a_matrix.count",
                 sep = "\t",col.names = c("gene_id","dx3"))
dx4 <-read.table("/Users/han/data/SCU/lab/kuang_project/count/MZ-X-3_FRAS210052156-1a_matrix.count",
                 sep = "\t",col.names = c("gene_id","dx4"))
dx5 <-read.table("/Users/han/data/SCU/lab/kuang_project/count/MZ-X-4_FRAS210052157-1a_matrix.count",
                 sep = "\t",col.names = c("gene_id","dx5"))
dx6 <-read.table("/Users/han/data/SCU/lab/kuang_project/count/MZ-X-5_FRAS210052158-1a_matrix.count",
                 sep = "\t",col.names = c("gene_id","dx6"))
sc1 <-read.table("/Users/han/data/SCU/lab/kuang_project/count/DX-C-1_FRAS210052161-1a_matrix.count",
                sep = "\t",col.names = c("gene_id","sc1"))
sc2 <-read.table("/Users/han/data/SCU/lab/kuang_project/count/DX-C-2_FRAS210052162-1a_matrix.count",
                sep = "\t",col.names = c("gene_id","sc2"))
sx1 <-read.table("/Users/han/data/SCU/lab/kuang_project/count/DX-X-1_FRAS210052159-1a_matrix.count",
                sep = "\t",col.names = c("gene_id","sx1"))
sx2 <-read.table("/Users/han/data/SCU/lab/kuang_project/count/DX-X-2_FRAS210052160-1a_matrix.count",
                sep = "\t",col.names = c("gene_id","sx2"))

raw_count_kuang <- merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(
  sx1, sx2, by = "gene_id"), sc2, by = "gene_id") , sc1, by = "gene_id"), dx6, by = "gene_id"),dx5, by = "gene_id"),dx4, by = "gene_id"),dx3, by = "gene_id"),dx2, by = "gene_id"),dx1, by = "gene_id"), 
  dc7, by = "gene_id"),dc6, by = "gene_id"),dc5, by = "gene_id"),dc4, by = "gene_id"),dc3, by = "gene_id"),dc2, by = "gene_id"), dc1, by = "gene_id")
#colnames(raw_count_kuang) <- c('gene_id', '7s', '9s', '7m', '2m', 'c2', 'c1', '10b', '9b', '8b', '7b', '6b', '5b', '4b', 
#                         '3b', '2b', '1b', '10a', '9a', '8a', '7a', '6a', '5a', '4a', '3a', '2a', '1a')

count_data_kuang <- raw_count_kuang[,2:18]
row.names(count_data_kuang) <- raw_count_kuang[,1]
condition_kuang <- factor(c('sx', 'sx', 'sc', 'sc', rep('dx', 6), rep('dc', 7)))
#batch <- factor(c(rep('a', 9), rep('b', 26)))
col_data_kuang <- data.frame(row.names = colnames(count_data_kuang), condition_kuang)
dds_kuang <- DESeqDataSetFromMatrix(countData = count_data_kuang, colData = col_data_kuang, design = ~ condition_kuang)
dds_kuang$condition_kuang <- relevel(dds_kuang$condition_kuang, ref = 'dc')
nrow(dds_kuang)
dds_filter_kuang <- dds_kuang[rowSums(counts(dds_kuang) >= 10) >= 3,] #筛除在少于3个样品中表达量少于10 reads的基因以防假阳性
nrow(dds_filter_kuang)


dds_out_kuang <- DESeq(dds_filter_kuang)
res_kuang <- results(dds_out_kuang)
summary(res_kuang)


table(res_kuang$padj < 0.05)
res_deseq_kuang <- res_kuang[order(res_kuang$padj),]
diff_gene_deseq2_kuang <- subset(res_deseq_kuang, padj < 0.05 & (log2FoldChange > 1 | log2FoldChange < -1))
diff_gene_deseq2_kuang <- row.names(diff_gene_deseq2_kuang)
res_diff_data_kuang <- merge(as.data.frame(res_kuang), as.data.frame(counts(dds_out_kuang, normalize = T)), by = "row.names", sort = F)
write.csv(res_diff_data_kuang,file = "/Users/han/data/SCU/lab/kuang_project/res_diff_data.csv",row.names = F)

res_kuang
plotMA(res_kuang,ylim=c(-15, 15))
#topGene <- rownames(res)[which.min(res$padj)]
#with(res[topGene, ], {
#  points(baseMean, log2FoldChange, col="dodgerblue", cex=2, lwd=2)
#  text(baseMean, log2FoldChange, topGene, pos=2, col="dodgerblue")
#})
resLFC_kuang <- lfcShrink(dds_out_kuang, coef = 4, res=res_kuang)
write.table(resLFC_kuang,"/Users/han/data/SCU/lab/kuang_project/mm.deseq2.resLFC.csv",quote = F,sep = "\t")
plotMA(resLFC_kuang, ylim=c(-15,15))
hist(res_kuang$pvalue[res_kuang$baseMean > 1], breaks = 0:20/20, col = 'grey', border = 'white')
vsd_kuang <- vst(dds_kuang, blind=FALSE)
#rlog_kuang <- rlog(dds_kuang, blind = F)
plotPCA(vsd_kuang, intgroup=c("condition_kuang"))
#plotPCA(rlog_kuang, intgroup=c("condition_kuang"))






library(genefilter)
rld_kuang <- rlogTransformation(dds_out_kuang, blind = F)
write.csv(assay(rld_kuang) ,file = '/Users/han/data/SCU/lab/kuang_project/mm.DESeq2.pseudo.counts.csv')
topVarGenes_kuang <- head(order(rowVars(assay(rld_kuang)), decreasing = T), 80)
mat_kuang <- assay(rld_kuang)[ topVarGenes_kuang[1:40], ]
#mat_kuang_1 <- assay(rld_kuang)[ topVarGenes_kuang[3:53], ] # 尝试去除outlier
topVarGenes_kuang_1 <- head(order(rowVars(assay(rld_kuang)), decreasing = T), 30)
anno_kuang <- as.data.frame(colData(rld_kuang)[, c('condition_kuang', 'sizeFactor')])
library(pheatmap)
pheatmap(mat_kuang, annotation_col = anno_kuang, fontsize = 6)
mat_kuang <- mat_kuang - rowMeans(mat_kuang)
pheatmap(mat_kuang, annotation_col = anno_kuang, fontsize = 6)


ass_rld <- assay(rld_kuang)
ass_rld <- ass_rld[-c(1:4),]

# output diff genes in .csv format
# sx & sc
VarGenes_kuang_sx_sc <- head(order(rowVars(ass_rld[, 1:4]), decreasing = T), 3976)
all_mat_kuang_sx_sc <- ass_rld[, 1:4][ VarGenes_kuang_sx_sc, ]
write.csv(mat_kuang_sx_sc ,file = '/Users/han/data/SCU/lab/kuang_project/mat_sx_sc.csv')
 # dx & dc
VarGenes_kuang_dx_dc <- head(order(rowVars(ass_rld[, 5:17]), decreasing = T), 3976)
all_mat_kuang_dx_dc <- ass_rld[, 5:17][ VarGenes_kuang_sx_sc, ]
write.csv(mat_kuang_dx_dc ,file = '/Users/han/data/SCU/lab/kuang_project/mat_dx_dc.csv')
# sx & dx
VarGenes_kuang_sx_dx <- head(order(rowVars(ass_rld[, c(1:2,5:10)]), decreasing = T), 3976)
all_mat_kuang_sx_dx <- ass_rld[, c(1:2,5:10)][ VarGenes_kuang_sx_sc, ]
write.csv(mat_kuang_sx_dx ,file = '/Users/han/data/SCU/lab/kuang_project/mat_sx_dx.csv')
# sc & dc
VarGenes_kuang_sc_dc <- head(order(rowVars(ass_rld[, c(3:4,11:17)]), decreasing = T), 3976)
all_mat_kuang_sc_dc <- ass_rld[, c(3:4,11:17)][ VarGenes_kuang_sx_sc, ]
write.csv(mat_kuang_sc_dc ,file = '/Users/han/data/SCU/lab/kuang_project/mat_sc_dc.csv')
# all
VarGenes_kuang_all <- head(order(rowVars(ass_rld[, c(1:17)]), decreasing = T), 3976)
all_mat_kuang_all <- ass_rld[, c(1:17)][ VarGenes_kuang_all, ]
write.csv(all_mat_kuang_all ,file = '/Users/han/data/SCU/lab/kuang_project/mat_all.csv')


# plot heatmaps
# sx & sc
ass_rld[, 1:4] 
topVarGenes_kuang_sx_sc <- head(order(rowVars(ass_rld[, 1:4]), decreasing = T), 30)
mat_kuang_sx_sc <- ass_rld[, 1:4][ topVarGenes_kuang_sx_sc[1:30], ]
pheatmap(mat_kuang_sx_sc, annotation_col = anno_kuang)
# dx & dc
ass_rld[, 5:17]
topVarGenes_kuang_dx_dc <- head(order(rowVars(ass_rld[, 5:17]), decreasing = T), 38)
mat_kuang_dx_dc <- ass_rld[, 5:17][ topVarGenes_kuang_dx_dc[1:30], ]
pheatmap(mat_kuang_dx_dc, annotation_col = anno_kuang)
# sx & dx
ass_rld[, c(1:2,5:10)]
topVarGenes_kuang_sx_dx <- head(order(rowVars(ass_rld[, c(1:2,5:10)]), decreasing = T), 30)
mat_kuang_sx_dx <- ass_rld[, c(1:2,5:10)][ topVarGenes_kuang_sx_dx[1:30], ]
pheatmap(mat_kuang_sx_dx, annotation_col = anno_kuang)
# sc & dc
ass_rld[, c(3:4,11:17)]
topVarGenes_kuang_sc_dc <- head(order(rowVars(ass_rld[, c(3:4,11:17)]), decreasing = T), 30)
mat_kuang_sc_dc <- ass_rld[, c(3:4,11:17)][ topVarGenes_kuang_sc_dc[1:30], ]
pheatmap(mat_kuang_sc_dc, annotation_col = anno_kuang)



vol_data_kuang <-read.table(file="/Users/han/data/SCU/lab/kuang_project/volcano.txt",header = TRUE, row.names =1,sep = "\t")
volcano <- ggplot(data = vol_data_kuang, aes(x=log2FoldChange,y= -1*log10(padj)))+geom_point(aes(color=significant))+scale_color_manual(values = c("red","grey","blue")) + labs(title="Volcano_Plot",x=expression((log[2](FC)), y=expression(-log[10](padj)) ))+geom_hline(yintercept=1.3,linetype=4)+geom_vline(xintercept=c(-1,1),linetype=4)
volcano
