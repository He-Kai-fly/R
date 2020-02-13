#qiime ���� alpha ������ָ����shell �����в���

cp otu_table.txt otu_table.tsv

#�� otu_table.tsv ��ͷ���һ�С�# Constructed from biom file�����Ա㽫 otu_table.tsv תΪ qiime ��ʶ����ʽ
sed -i '1i\# Constructed from biom file' otu_table.tsv

#otu_table.tsv ת��Ϊ otu_table.biom
biom convert -i otu_table.tsv -o otu_table.biom --table-type="OTU table" --to-json

#alpha ������ָ�����㣬���� 7 ��ָ��
alpha_diversity.py -i otu_table.biom -o alpha.txt -t otu_tree.tre -m observed_otus,shannon,simpson,chao1,ace,goods_coverage,PD_whole_tree

#��������� PD_whole_tree ָ����������ָ���������ļ� otu_tree.tre
#�õ��Ľ���ļ� alpha.txt ������