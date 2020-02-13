#qiime 计算 alpha 多样性指数，shell 命令行操作

cp otu_table.txt otu_table.tsv

#在 otu_table.tsv 开头添加一行“# Constructed from biom file”，以便将 otu_table.tsv 转为 qiime 可识别样式
sed -i '1i\# Constructed from biom file' otu_table.tsv

#otu_table.tsv 转换为 otu_table.biom
biom convert -i otu_table.tsv -o otu_table.biom --table-type="OTU table" --to-json

#alpha 多样性指数计算，包含 7 种指数
alpha_diversity.py -i otu_table.biom -o alpha.txt -t otu_tree.tre -m observed_otus,shannon,simpson,chao1,ace,goods_coverage,PD_whole_tree

#如果不计算 PD_whole_tree 指数，就无需指定进化树文件 otu_tree.tre
#得到的结果文件 alpha.txt 就是了