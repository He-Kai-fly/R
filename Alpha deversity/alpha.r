library(vegan)
setwd('E:\\Users\\Lenovo\\Desktop\\Genomics\\16s rRNA amplicon\\Table')
#������������
otu <- read.delim('OTU_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- t(otu)


##���ַḻ�� Richness ָ��
richness <- rowSums(otu > 0)
#��
richness <- estimateR(otu)[1, ]

##Shannon������ Shannon ��ʽ�Ķ�����������ʹ�� e���� R �м���ʾΪ exp(1)��
#Shannon ָ��
shannon_index <- diversity(otu, index = 'shannon', base = exp(1))

#Shannon ������
shannon_diversity <- exp(1)^shannon_index

#Shannon ���ȶȣ�Pielou ���ȶȣ�
pielou <- shannon_index / log(richness, exp(1))

##Simpson
#Gini-Simpson ָ��������ƽʱ���õ� Simpson ָ����Ϊ Gini-Simpson ָ����
gini_simpson_index <- diversity(otu, index = 'simpson')

#���� Simpson ָ����ʹ��Ƶ�ʱȽϵͣ�
simpson_index <- 1 - gini_simpson_index


#Invsimpson ָ����Gini-Simpson �ĵ�����
invsimpson_index <- 1 / gini_simpson_index
#��
invsimpson_index <- diversity(otu, index = 'invsimpson')

#Simpson ������
simpson_diversity <- 1 / (1 - gini_simpson_index)

#Simpson ���ȶȣ�equitability ���ȶȣ�
equitability <- 1 / (richness * (1 - gini_simpson_index))
str(ace)
##Chao1 & ACE
#Chao1 ָ��
chao1 <- estimateR(otu)[2, ]

#ACE ָ��
ace <- estimateR(otu)[4, ]

##goods_coverage ָ��
goods_coverage <- 1 - rowSums(otu == 1) / rowSums(otu)

##��ϵ�����ԣ���������ȣ�����ָ���������ļ���
#PD_whole_tree���ο� https://daijiang.name/en/2014/05/04/notes-func-phylo-book-1/
library(picante)
tree <- read.tree('otu_phylo.tre')
pd_whole_tree <- pd(otu, tree, include.root = FALSE)	#����ʱ�����и������޸����� PD_whole_tree ��������һ���ģ������޸����ļ�������

##��Ϻ���
#���庯��������ḻ�ȡ�Shannon ��ָ����Simpson ָ����Gini-Simpson ָ������Pielou ���ȶȡ�Chao1 ָ����ACE ָ����goods_coverage��PD_whole_tree
library(picante)	#picante ������ʱĬ��ͬʱ���� vegan

alpha <- function(x, tree = NULL, base = exp(1)) {
	est <- estimateR(x)
	Richness <- est[1, ]
	Chao1 <- est[2, ]
	ACE <- est[4, ]
	Shannon <- diversity(x, index = 'shannon', base = base)
	Simpson <- diversity(x, index = 'simpson')	#Gini-Simpson ָ��
	Pielou <- Shannon / log(Richness, base)
	goods_coverage <- 1 - rowSums(x == 1) / rowSums(x)
	
	result <- data.frame(Richness, Shannon, Simpson, Pielou, Chao1, ACE, goods_coverage)
	if (!is.null(tree)) {
		PD_whole_tree <- pd(x, tree, include.root = FALSE)[1]
		names(PD_whole_tree) <- 'PD_whole_tree'
		result <- cbind(result, PD_whole_tree)
	}
	result
}

#���� OTU ��ȱ�ͽ������ļ�
otu <- read.delim('otu_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- t(otu)
tree <- read.tree('otu_tree.tre')

#��������ϵ�����ԣ�����ָ����������Shannon ��ʽ�� log ��������ʹ�� 2
alpha_all <- alpha(otu, base = 2)
#������ϵ������ʱ��ָ���������ļ���Shannon ��ʽ�� log ��������ʹ�� 2
alpha_all <- alpha(otu, tree, base = 2)

#��������� csv ��ʽ
write.csv(alpha_all, 'alpha.csv', quote = FALSE)

