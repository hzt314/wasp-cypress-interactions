library(WGCNA)
gene <- read.csv('/Users/han/data/SCU/lab/Finished/kuang_project/res_diff_data.csv', header = T)
dim(exprMat)

gene <- gene[5:3980,c(1, 8:24)]
rownames(gene) <- gene[,1]
gene <- gene[,-1]
#gene <- read.delim('/Users/han/data/SCU/lab/Finished/kuang_project/test_data.txt', row.names = 1, check.names = FALSE)
gene <- subset(gene, rowSums(gene)/ncol(gene) >= 1)
gene <- t(gene)

powers <- 1:20
sft <- pickSoftThreshold(gene, powerVector = powers, verbose = 5)
sft$powerEstimate

par(mfrow = c(1, 2))
plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2], type = 'n', 
     xlab = 'Soft Threshold (power)', ylab = 'Scale Free Topology Model Fit,signed R^2', 
     main = paste('Scale independence'))
text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     labels = powers, col = 'red');
abline(h = 0.90, col = 'red')


plot(sft$fitIndices[,1], sft$fitIndices[,5],
     xlab = 'Soft Threshold (power)', ylab = 'Mean Connectivity', type = 'n',
     main = paste('Mean connectivity'))
text(sft$fitIndices[,1], sft$fitIndices[,5], labels = powers, col = 'red')

powers <- sft$powerEstimate
adjacency <- adjacency(gene, power = powers)
tom_sim <- TOMsimilarity(adjacency)
rownames(tom_sim) <- rownames(adjacency)
colnames(tom_sim) <- colnames(adjacency)
tom_sim[1:6,1:6]

#write.table(tom_sim, 'TOMsimilarity.txt', sep = '\t', col.names = NA, quote = FALSE)

tom_dis  <- 1 - tom_sim
geneTree <- hclust(as.dist(tom_dis), method = 'average')
plot(geneTree, xlab = '', sub = '', main = 'Gene clustering on TOM-based dissimilarity',
     labels = FALSE, hang = 0.04)

minModuleSize <- 0
dynamicMods <- cutreeDynamic(dendro = geneTree, distM = tom_dis,
                             deepSplit = 2, pamRespectsDendro = FALSE, minClusterSize = minModuleSize)

table(dynamicMods)


dynamicColors <- labels2colors(dynamicMods)
table(dynamicColors)

plotDendroAndColors(geneTree, dynamicColors, 'Dynamic Tree Cut',
                    dendroLabels = FALSE, addGuide = TRUE, hang = 0.03, guideHang = 0.05,
                    main = 'Gene dendrogram and module colors')
