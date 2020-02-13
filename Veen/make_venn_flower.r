#!/usr/bin/env Rscript

#用法参考
#Rscript make_venn_flower.r otu_table.txt group1.txt venn.txt
#Rscript make_venn_flower.r otu_table.txt group2.txt flower.txt

args <- commandArgs(T)

otu <- read.delim(args[1], header = T, row.names = 1, sep = '\t', stringsAsFactors = F, check.names = F)
group <- read.delim(args[2], header = F, sep = '\t', stringsAsFactors = F)
names(group) <- c('sample', 'group')

group_id <- unique(group[,2])
result <- NULL
len = 0
for (i in group_id) {
	group_i <- subset(group, group == i)
	otu_i <- otu[group_i$sample]
	otu_i <- subset(otu_i, rowSums(otu_i) > 0)
	otu_select <- c(i, rownames(otu_i))
	if (length(otu_select) > len) len <- length(otu_select)
	length(otu_select) = nrow(otu)
	result <- cbind(result, otu_select)
}

result <- result[1:len, ]
write.table(result, args[3], sep = '\t', col.names = F,row.names = F, na = '', quote = F)
