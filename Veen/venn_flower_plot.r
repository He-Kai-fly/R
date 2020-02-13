####Τ��ͼ��VennDiagram �������������� 2-5��
library(VennDiagram)

#������ͼ�ļ���Ԥ����
venn_dat <- read.delim('venn.txt', header = T, sep = '\t', stringsAsFactors = F, check.names = F)
venn_list <- list(venn_dat[,1], venn_dat[,2])
names(venn_list) <- colnames(venn_dat)

#��ͼ
venn.diagram(venn_list, filename = 'venn.png', fill = c('red', 'blue'), alpha = 0.50, col = 'black', cex = 1, fontfamily = 'serif', cat.col = c('black', 'black'), cat.cex = 1, cat.fontfamily = 'serif', margin = 0.2)

####Τ��ͼ��venn �������������� 2-7��
library(venn)

#������ͼ�ļ���Ԥ����
venn_dat <- read.delim('flower.txt', header = T, sep = '\t', stringsAsFactors = F, check.names = F)[1:7]
venn_list <- list(venn_dat[,1], venn_dat[,2], venn_dat[,3], venn_dat[,4], venn_dat[,5], venn_dat[,6], venn_dat[,7])
names(venn_list) <- colnames(venn_dat)

#��ͼ
png('venn_7.png', width = 1500, height = 1500, res = 200, units = 'px')
venn(venn_list,zcolor='style')
dev.off()

####Τ��ͼ��UpSetR �����������������ƣ�
library(UpSetR)

#������ͼ�ļ���Ԥ����
otu <- read.delim('otu_table.txt', header = T, row.names = 1, sep = '\t', stringsAsFactors = F, check.names = F)[1:10]
otu[otu > 0] <- 1

#��ͼ
png('venn_10.png', width = 1500, height = 1500, res = 200, units = 'px')
upset(otu, nsets = 10, order.by = "freq")
dev.off()

####����ͼ
library(plotrix)

#������ͼ�ļ���Ԥ����
flower_dat <- read.delim('flower.txt', header = T, sep = '\t', stringsAsFactors = F, check.names = F)
sample_id <- colnames(flower_dat)
otu_id <- unique(flower_dat[,1])
otu_id <- otu_id[otu_id != '']
core_otu_id <- otu_id
otu_num <- length(otu_id)
ellipse_col <- c('#6181BD4E','#F348004E','#64A10E4E','#9300264E','#464E044E','#049a0b4E','#4E0C664E','#D000004E','#FF6C004E','#FF00FF4E','#c7475b4E','#00F5FF4E','#BDA5004E','#A5CFED4E','#f0301c4E','#2B8BC34E','#FDA1004E','#54adf54E','#CDD7E24E','#9295C14E')

for (i in 2:ncol(flower_dat)) {
	otu_id <- unique(flower_dat[,i])
	otu_id <- otu_id[otu_id != '']
	core_otu_id <- intersect(core_otu_id, otu_id)
	otu_num <- c(otu_num, length(otu_id))
}
core_num <- length(core_otu_id)

#������ͼ�������ο��� https://www.cnblogs.com/xudongliang/p/7884667.html��
flower_plot <- function(sample, otu_num, core_otu, start, a, b, r, ellipse_col, circle_col) {
	par( bty = 'n', ann = F, xaxt = 'n', yaxt = 'n', mar = c(1,1,1,1))
	plot(c(0,10),c(0,10),type='n')
	n   <- length(sample)
	deg <- 360 / n
	res <- lapply(1:n, function(t){
		draw.ellipse(x = 5 + cos((start + deg * (t - 1)) * pi / 180), 
			y = 5 + sin((start + deg * (t - 1)) * pi / 180), 
			col = ellipse_col[t],
			border = ellipse_col[t],
			a = a, b = b, angle = deg * (t - 1))
		text(x = 5 + 2.5 * cos((start + deg * (t - 1)) * pi / 180),
			y = 5 + 2.5 * sin((start + deg * (t - 1)) * pi / 180),
			otu_num[t])
		
		if (deg * (t - 1) < 180 && deg * (t - 1) > 0 ) {
			text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
				y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
				sample[t],
				srt = deg * (t - 1) - start,
				adj = 1,
				cex = 1
				)
		} else {
			text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
				y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
				sample[t],
				srt = deg * (t - 1) + start,
				adj = 0,
				cex = 1
				)
		}			
	})
	draw.circle(x = 5, y = 5, r = r, col = circle_col, border = NA)
	text(x = 5, y = 5, paste('Core:', core_otu))
}

#��ͼ
png('flower.png', width = 1500, height = 1500, res = 200, units = 'px')
flower_plot(sample = sample_id, otu_num = otu_num, core_otu = core_num, 
	start = 90, a = 0.5, b = 2, r = 1, ellipse_col = ellipse_col, circle_col = 'white')
dev.off()
#ע������a��b�������û�����Բ�ĳߴ磬ellipse_col�������û�����Բ����ɫ������r������������ԲȦ�ߴ磬circle_col������������ԲȦ����ɫ
