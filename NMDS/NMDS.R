library(vegan)	#�������
rm(list = ls())
library(ggplot2)	#��ͼ
library(ape)	#��ѡ�����ڶ�ȡ�������ļ��������
setwd('E:/Study/Bioinformation/Script/NMDS')
##############################
##NMDS ���򣨻��� OTU ��ȱ���
#���� OTU ��ȱ�
otu <- read.delim('otu_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))

#����Ԥ�� 4 ��������
nmds1 <- metaMDS(otu, distance = 'bray', k = 2)
?metaMDS()
#�ɼ�Ҫ�鿴���
nmds1
#��
summary(nmds1)

#��ȡӦ������ֵ��stress��
nmds1.stress <- nmds1$stress
#��ȡ������������
nmds1.point <- data.frame(nmds1$point)
#��ȡ���֣�OTU����������
nmds1.species <- data.frame(nmds1$species)

#��Ҫ��ͼչʾ
nmds_plot <- nmds1
nmds_plot$species <- {nmds_plot$species}[1:10, ]
plot(nmds_plot, type = 't', main = paste('Stress =', round(nmds1$stress, 4)))

##############################
##NMDS ���򣨻������еľ������
#�������еľ������
dis <- read.delim('bray.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

#����Ԥ�� 4 �������� disΪ����ľ������
nmds2 <- metaMDS(as.dist(dis), k = 2)

#�ɼ�Ҫ�鿴���
summary(nmds2)
#�ɷ��֡�species����Ϣ��Ϊ��1������ӡ����Ϊ��NA��
nmds2$species

#��Ҫ��ͼչʾ���ɷ�������� OTU ��ȱ��� NMDS ��������ͬ
plot(nmds2, type = 't', main = paste('Stress =', round(nmds2$stress, 4)))

#��ʹ�� wascores() �� OTU �������ء����� NMDS ����ͼ
species <- wascores(nmds2$points, otu)
text(species[1:10, ], rownames(species)[1:10], col = 'red')

##############################
##NMDS ����
stressplot(nmds_plot, main = 'Shepard ͼ')
gof <- goodness(nmds_plot)
plot(nmds_plot,type = 't', main = '��϶�')
points(nmds_plot, display = 'sites', cex = gof * 200, col = 'red')

##############################
##ggplot2 ��ͼ��ʹ�û��� OTU ��ȱ��� NMDS ��������Ԥ�� 2 �������ᣩ
#���� OTU ��ȱ�
otu <- read.delim('otu_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))

#�������������ļ�
group <- read.delim('group.txt', sep = '\t', stringsAsFactors = FALSE)

#����Ԥ�� 2 ��������
nmds1 <- metaMDS(otu, distance = 'bray', k = 2)

#��ȡ���������꣨ǰ���ᣩ
sample_site <- nmds1.point[1:2]
sample_site$names <- rownames(sample_site)
names(sample_site)[1:2] <- c('NMDS1', 'NMDS2')

#Ϊ�������������ӷ�����Ϣ
sample_site <- merge(sample_site, group, by = 'names', all.x = TRUE)

#��ȡ��Է�� top20 �� OTU ���꣨ǰ���ᣩ
otu <- read.delim('otu_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu$sum <- rowSums(otu)
otu <- otu[order(otu$sum, decreasing = TRUE), ]
species_site <-{nmds1.species[rownames(otu[1:10, ]), ]}[1:2]

#����Ϊ�� sample_site ��ͬ����ʽ�����㱻 ggplot2 ʶ��
species_site$group <- rownames(species_site)
names(species_site)[1:2] <- c('NMDS1', 'NMDS2')

#ʹ�� ggplot2 ���� NMDS ����ͼ
nmds_plot <- ggplot(sample_site, aes(NMDS1, NMDS2, group = group)) +
geom_point(aes(color = group, shape = group), size = 1.5, alpha = 0.8) + #���������޸ĵ��͸���ȡ���С
scale_shape_manual(values = c(17, 16)) + #���������޸ĵ����״
scale_color_manual(values = c('red', 'blue')) + #���������޸ĵ����ɫ
theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) + #ȥ��������
theme(legend.key = element_rect(fill = 'transparent'), legend.title = element_blank()) + #ȥ��ͼ�����⼰��ǩ����
labs(x = 'NMDS axis1', y = 'NMDS axis2', title = paste('Stress =', round(nmds1$stress, 4))) +
theme(plot.title = element_text(hjust = 0.5)) + #�������
geom_text(aes(label = group), data = species_site, color = 'green4', size = 2) #������������top10 OTU��չʾΪ��ǩ��

#ggsave('NMDS.pdf', nmds_plot, width = 6, height = 5)
ggsave('NMDS.png', nmds_plot, width = 6, height = 5)

##############################
##���� Weighted UniFrac ����� NMDS ���򣨲��ԣ�

#���غ���
source('unifrac_nmds.r')

#OTU ��ȱ�
otu <- read.delim('test_otu.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))

#��������ʹ�� ape ���е� read.tree() ��ȡ���˴�����������Ϊ�и�����
otu_tree <- read.tree('test_tree.tre')

#�������weighted unifrac �������ʹ�� GUniFrac ���е�������㣩
unifrac <- GUniFrac(otu, otu_tree)
unifrac <- unifrac$unifracs
dis <- as.dist(unifrac[, , 'd_1'])		# Weighted UniFrac

#�����ļ�
group <- read.delim('test_group.txt', sep = '\t', stringsAsFactors = FALSE)

#NMDS ���򣨻��� OTU ��ȱ��ͽ������ļ���ʹ�� weighted unifrac ���룩
nmds_un1 <- metaMDS2(otu, distance = 'dw', tree = otu_tree)

#NMDS ����ֱ�ӻ��� weighted unifrac �������
nmds_un2 <- metaMDS(as.dist(dis))

#�ֱ���ͼչʾ
sample_site1 <- data.frame(nmds_un1$point); sample_site2 <- data.frame(nmds_un2$point)
sample_site1$names <- rownames(sample_site1); sample_site2$names <- rownames(sample_site2)
names(sample_site1)[1:2] <- c('NMDS1', 'NMDS2'); names(sample_site2)[1:2] <- c('NMDS1', 'NMDS2')
sample_site1 <- merge(sample_site1, group, by = 'names', all.x = TRUE); sample_site2 <- merge(sample_site2, group, by = 'names', all.x = TRUE)

ggplot(sample_site1, aes(NMDS1, NMDS2, color = group)) + geom_point()
ggplot(sample_site2, aes(NMDS1, NMDS2, color = group)) + geom_point()