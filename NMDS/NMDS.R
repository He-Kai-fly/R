library(vegan)	#排序分析
rm(list = ls())
library(ggplot2)	#作图
library(ape)	#可选，用于读取进化树文件，见最后
setwd('E:/Study/Bioinformation/Script/NMDS')
##############################
##NMDS 排序（基于 OTU 丰度表）
#读入 OTU 丰度表
otu <- read.delim('otu_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))

#排序，预设 4 个排序轴
nmds1 <- metaMDS(otu, distance = 'bray', k = 2)
?metaMDS()
#可简要查看结果
nmds1
#或
summary(nmds1)

#提取应力函数值（stress）
nmds1.stress <- nmds1$stress
#提取样本排序坐标
nmds1.point <- data.frame(nmds1$point)
#提取物种（OTU）排序坐标
nmds1.species <- data.frame(nmds1$species)

#简要绘图展示
nmds_plot <- nmds1
nmds_plot$species <- {nmds_plot$species}[1:10, ]
plot(nmds_plot, type = 't', main = paste('Stress =', round(nmds1$stress, 4)))

##############################
##NMDS 排序（基于现有的距离矩阵）
#读入现有的距离矩阵
dis <- read.delim('bray.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

#排序，预设 4 个排序轴 dis为读入的距离矩阵
nmds2 <- metaMDS(as.dist(dis), k = 2)

#可简要查看结果
summary(nmds2)
#可发现“species”信息中为“1”，打印出来为“NA”
nmds2$species

#简要绘图展示，可发现与基于 OTU 丰度表的 NMDS 排序结果不同
plot(nmds2, type = 't', main = paste('Stress =', round(nmds2$stress, 4)))

#可使用 wascores() 将 OTU “被动地”加入 NMDS 排序图
species <- wascores(nmds2$points, otu)
text(species[1:10, ], rownames(species)[1:10], col = 'red')

##############################
##NMDS 评估
stressplot(nmds_plot, main = 'Shepard 图')
gof <- goodness(nmds_plot)
plot(nmds_plot,type = 't', main = '拟合度')
points(nmds_plot, display = 'sites', cex = gof * 200, col = 'red')

##############################
##ggplot2 作图（使用基于 OTU 丰度表的 NMDS 排序结果，预设 2 个排序轴）
#读入 OTU 丰度表
otu <- read.delim('otu_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))

#读入样本分组文件
group <- read.delim('group.txt', sep = '\t', stringsAsFactors = FALSE)

#排序，预设 2 个排序轴
nmds1 <- metaMDS(otu, distance = 'bray', k = 2)

#提取样本点坐标（前两轴）
sample_site <- nmds1.point[1:2]
sample_site$names <- rownames(sample_site)
names(sample_site)[1:2] <- c('NMDS1', 'NMDS2')

#为样本点坐标添加分组信息
sample_site <- merge(sample_site, group, by = 'names', all.x = TRUE)

#提取相对丰度 top20 的 OTU 坐标（前两轴）
otu <- read.delim('otu_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu$sum <- rowSums(otu)
otu <- otu[order(otu$sum, decreasing = TRUE), ]
species_site <-{nmds1.species[rownames(otu[1:10, ]), ]}[1:2]

#整理为与 sample_site 相同的样式，方便被 ggplot2 识别
species_site$group <- rownames(species_site)
names(species_site)[1:2] <- c('NMDS1', 'NMDS2')

#使用 ggplot2 绘制 NMDS 排序图
nmds_plot <- ggplot(sample_site, aes(NMDS1, NMDS2, group = group)) +
geom_point(aes(color = group, shape = group), size = 1.5, alpha = 0.8) + #可在这里修改点的透明度、大小
scale_shape_manual(values = c(17, 16)) + #可在这里修改点的形状
scale_color_manual(values = c('red', 'blue')) + #可在这里修改点的颜色
theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) + #去掉背景框
theme(legend.key = element_rect(fill = 'transparent'), legend.title = element_blank()) + #去掉图例标题及标签背景
labs(x = 'NMDS axis1', y = 'NMDS axis2', title = paste('Stress =', round(nmds1$stress, 4))) +
theme(plot.title = element_text(hjust = 0.5)) + #标题居中
geom_text(aes(label = group), data = species_site, color = 'green4', size = 2) #添加物种排序（top10 OTU，展示为标签）

#ggsave('NMDS.pdf', nmds_plot, width = 6, height = 5)
ggsave('NMDS.png', nmds_plot, width = 6, height = 5)

##############################
##基于 Weighted UniFrac 距离的 NMDS 排序（测试）

#加载函数
source('unifrac_nmds.r')

#OTU 丰度表
otu <- read.delim('test_otu.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))

#进化树（使用 ape 包中的 read.tree() 读取，此处进化树必须为有根树）
otu_tree <- read.tree('test_tree.tre')

#距离矩阵（weighted unifrac 距离矩阵，使用 GUniFrac 包中的命令计算）
unifrac <- GUniFrac(otu, otu_tree)
unifrac <- unifrac$unifracs
dis <- as.dist(unifrac[, , 'd_1'])		# Weighted UniFrac

#分组文件
group <- read.delim('test_group.txt', sep = '\t', stringsAsFactors = FALSE)

#NMDS 排序（基于 OTU 丰度表和进化树文件，使用 weighted unifrac 距离）
nmds_un1 <- metaMDS2(otu, distance = 'dw', tree = otu_tree)

#NMDS 排序（直接基于 weighted unifrac 距离矩阵）
nmds_un2 <- metaMDS(as.dist(dis))

#分别作图展示
sample_site1 <- data.frame(nmds_un1$point); sample_site2 <- data.frame(nmds_un2$point)
sample_site1$names <- rownames(sample_site1); sample_site2$names <- rownames(sample_site2)
names(sample_site1)[1:2] <- c('NMDS1', 'NMDS2'); names(sample_site2)[1:2] <- c('NMDS1', 'NMDS2')
sample_site1 <- merge(sample_site1, group, by = 'names', all.x = TRUE); sample_site2 <- merge(sample_site2, group, by = 'names', all.x = TRUE)

ggplot(sample_site1, aes(NMDS1, NMDS2, color = group)) + geom_point()
ggplot(sample_site2, aes(NMDS1, NMDS2, color = group)) + geom_point()
