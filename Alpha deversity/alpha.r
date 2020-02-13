library(vegan)
setwd('E:\\Users\\Lenovo\\Desktop\\Genomics\\16s rRNA amplicon\\Table')
#读入物种数据
otu <- read.delim('OTU_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- t(otu)


##物种丰富度 Richness 指数
richness <- rowSums(otu > 0)
#或
richness <- estimateR(otu)[1, ]

##Shannon（以下 Shannon 公式的对数底数均设使用 e，在 R 中即表示为 exp(1)）
#Shannon 指数
shannon_index <- diversity(otu, index = 'shannon', base = exp(1))

#Shannon 多样性
shannon_diversity <- exp(1)^shannon_index

#Shannon 均匀度（Pielou 均匀度）
pielou <- shannon_index / log(richness, exp(1))

##Simpson
#Gini-Simpson 指数（我们平时常用的 Simpson 指数即为 Gini-Simpson 指数）
gini_simpson_index <- diversity(otu, index = 'simpson')

#经典 Simpson 指数（使用频率比较低）
simpson_index <- 1 - gini_simpson_index


#Invsimpson 指数（Gini-Simpson 的倒数）
invsimpson_index <- 1 / gini_simpson_index
#或
invsimpson_index <- diversity(otu, index = 'invsimpson')

#Simpson 多样性
simpson_diversity <- 1 / (1 - gini_simpson_index)

#Simpson 均匀度（equitability 均匀度）
equitability <- 1 / (richness * (1 - gini_simpson_index))
str(ace)
##Chao1 & ACE
#Chao1 指数
chao1 <- estimateR(otu)[2, ]

#ACE 指数
ace <- estimateR(otu)[4, ]

##goods_coverage 指数
goods_coverage <- 1 - rowSums(otu == 1) / rowSums(otu)

##谱系多样性（与上述相比，还需指定进化树文件）
#PD_whole_tree，参考 https://daijiang.name/en/2014/05/04/notes-func-phylo-book-1/
library(picante)
tree <- read.tree('otu_phylo.tre')
pd_whole_tree <- pd(otu, tree, include.root = FALSE)	#测试时发现有根树和无根树的 PD_whole_tree 计算结果是一样的，但是无根树的计算会更快

##组合函数
#定义函数，计算丰富度、Shannon 熵指数、Simpson 指数（Gini-Simpson 指数）、Pielou 均匀度、Chao1 指数、ACE 指数、goods_coverage、PD_whole_tree
library(picante)	#picante 包加载时默认同时加载 vegan

alpha <- function(x, tree = NULL, base = exp(1)) {
	est <- estimateR(x)
	Richness <- est[1, ]
	Chao1 <- est[2, ]
	ACE <- est[4, ]
	Shannon <- diversity(x, index = 'shannon', base = base)
	Simpson <- diversity(x, index = 'simpson')	#Gini-Simpson 指数
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

#加载 OTU 丰度表和进化树文件
otu <- read.delim('otu_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- t(otu)
tree <- read.tree('otu_tree.tre')

#不包含谱系多样性，无需指定进化树；Shannon 公式的 log 底数我们使用 2
alpha_all <- alpha(otu, base = 2)
#包含谱系多样性时，指定进化树文件；Shannon 公式的 log 底数我们使用 2
alpha_all <- alpha(otu, tree, base = 2)

#输出，例如 csv 格式
write.csv(alpha_all, 'alpha.csv', quote = FALSE)

