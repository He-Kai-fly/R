#读取数据
library(reshape2)
setwd('E:\\Study\\Bioinformation\\Script\\box-volin')
alpha <- read.csv('alpha.csv')
alpha$group2 <- factor(alpha$group2)

alpha1 <- melt(alpha, id = c('samples', 'group1', 'group2'))
alpha2 <- subset(alpha1, variable == 'chao1')
alpha3 <- subset(alpha2, group1 == 'c')

##boxplot()，箱线图

#pdf('boxplot.pdf', width = 13, height = 5)
png('boxplot.png', width = 4000, height = 1500, res = 300, units = 'px')
par(mfrow = c(1, 2))
boxplot(value~group2, data = alpha3, col = '#f8766d', ylab = 'Chao1 (group c)')
boxplot(value~group2, data = alpha3, col = '#f8766d', notch = TRUE, varwidth = TRUE, ylab = 'Chao1 (group c)')
dev.off()

##vioplot()，提琴图
library(vioplot)

c1 <- subset(alpha3, group2 == '1')$value
c2 <- subset(alpha3, group2 == '2')$value
c3 <- subset(alpha3, group2 == '3')$value
c4 <- subset(alpha3, group2 == '4')$value
c5 <- subset(alpha3, group2 == '5')$value

#pdf('vioplot.pdf', width = 6.5, height = 5)
png('vioplot.png', width = 2000, height = 1500, res = 300, units = 'px')
vioplot(c1, c2, c3, c4, c5, col = '#f8766d')
title(ylab = 'Chao1 (group c)')
dev.off()

##ggplot2
library(ggplot2)

#箱线图
p <- ggplot(alpha2, aes(x = group2, y = value, fill = group1)) + 
geom_boxplot(outlier.size = 0.7) +
labs(x = '', y = 'Chao1') +
theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black'), legend.title = element_blank(), legend.key = element_blank())

#ggsave('ggplot2.box.pdf', p, width = 6, height = 4)
ggsave('ggplot2.box.png', p, width = 6, height = 4)

#带散点及凹槽的箱线图
p <- ggplot(alpha3, aes(x = group2, y = value, fill = group1)) + 
geom_boxplot(fill = '#f8766d', notch = TRUE) +
geom_jitter(color = 'red') +
labs(x = '', y = 'Chao1 (group c)') +
theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black'), legend.position = 'none')

#ggsave('ggplot2.box_point.pdf', p, width = 6, height = 4)
ggsave('ggplot2.box_point.png', p, width = 6, height = 4)

#内置箱线图的提琴图
p <- ggplot(alpha2, aes(x = group2, y = value, fill = group1)) + 
geom_violin(position = position_dodge(width = 1), scale = 'width') +
geom_boxplot(position = position_dodge(width = 1), outlier.size = 0.7, width = 0.2, show.legend = FALSE) +
labs(x = '', y = 'Chao1') +
theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black'), legend.title = element_blank(), legend.key = element_blank())

#ggsave('ggplot2.box_violin.pdf', p, width = 6, height = 4)
ggsave('ggplot2.box_violin.png', p, width = 6, height = 4)

#带分面的箱线图
p <- ggplot(alpha1, aes(x = group2, y = value, fill = group1)) + 
geom_boxplot(outlier.size = 0.3, size = 0.25) +
facet_wrap(~variable, 2, scales = 'free') +
labs(x = '', y = '') +
theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black'), legend.title = element_blank(), legend.key = element_blank())

#ggsave('ggplot2.box_facet.pdf', p, width = 7, height = 5)
ggsave('ggplot2.box_facet.png', p, width = 7, height = 5)
