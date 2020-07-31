[TOC]



## 可视化

### **两组显著性差异图**

```R
##显著性差异图
rm(list = ls())
Normal <- c(0.83, 0.79, 0.99, 0.69)
Cancer <- c(0.56, 0.56, 0.64, 0.52) 
m <- c(mean(Normal), mean(Cancer))
s <- c(sd(Normal), sd(Cancer)) 
d <- data.frame(V=c("Normal", "Cancer"), mean=m, sd=s) 
d$V <- factor(d$V, levels=c("Normal", "Cancer")) 
p <- ggplot(d, aes(V, mean, fill=V, width=.5))+ 
   geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, width=.2),position=position_dodge(width=.8)) +
   geom_bar(stat="identity", position=position_dodge(width=.8), colour="black")+
 scale_fill_manual(values = c('grey80','white'))+theme_bw() +theme(legend.position="none") + xlab("") + ylab("")+
 theme(axis.text.x = element_text(face="bold", size=12),axis.text.y = element_text(face="bold", size=12))+
 scale_y_continuous(expand=c(0,0), limits=c(0, 1.2), breaks=seq(0, 1.2, by=.2))
p <- p+geom_segment(aes(x=1, y=.98, xend=1, yend=1.1))+geom_segment(aes(x=2, y=.65, xend=2, yend=1.1))+
 geom_segment(aes(x=1, y=1.1, xend=2, yend=1.1))+annotate("text", x=1.5, y=1.08, label="*") 
p     
#带误差棒的线图
geom+line()+geom+point()+geom_errorbar
#也可使用函数 geom_pointrange() 或 geom_linerange() 替换 geom_errorbar()
```

![image-20200221114214336](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221114214336.png)

**barplot和ggplot2绘制柱状图**

```R

data <- data.frame(
  specie=c(rep("sorgho" , 10) , rep("poacee" , 10) ),
  cond_A=rnorm(20,10,4),
  cond_B=rnorm(20,8,3),
  cond_C=rnorm(20,5,4)
)
rm(list = ls())

#Let's calculate the average value for each condition and each specie with the *aggregate* function
bilan <- aggregate(cbind(cond_A,cond_B,cond_C)~specie , data=data , mean)
rownames(bilan) <- bilan[,1]
bilan <- as.matrix(bilan[,-1])

#Plot boundaries
lim <- 1.2*max(bilan)

#A function to add arrows on the chart
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

#Then I calculate the standard deviation for each specie and condition :
stdev <- aggregate(cbind(cond_A,cond_B,cond_C)~specie , data=data , sd)
rownames(stdev) <- stdev[,1]
stdev <- as.matrix(stdev[,-1]) * 1.96 / 10

#I am ready to add the error bar on the plot using my "error bar" function !
ze_barplot <- barplot(bilan , beside=T , legend.text=T,col=c("blue" , "skyblue") , ylim=c(0,lim) , ylab="height",las =2)
error.bar(ze_barplot,bilan, stdev)

##用ggplot作图
library(doBy)
data1 <- melt(data,id.vars = 'specie')

data_stat <- summaryBy(value~specie+variable, data1, FUN = c(mean, sd))

#同样我们也设置一下均值和方差的阈值
height <- max(data_stat$value.mean)*1.2
data_stat$value.sd <- data_stat$value.sd*1.96/10
#作图
ggplot(data_stat, aes(variable, value.mean, fill = specie)) +
  geom_col(position = position_dodge(width = 0.5), width = 0.5, size = 0.3, colour = 'black') +	#柱形图绘制
  geom_errorbar(aes(ymin = value.mean - value.sd, ymax = value.mean + value.sd), width = 0.3, size = 0.3, position = position_dodge(0.5)) +	#误差线绘制
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.title = element_blank()) + 	#调整背景
  scale_y_continuous(limits = c(0,height),expand = c(0,0))
                      
```



### **华夫pie图**

```R
library(dplyr)
rm(list = ls())
d <- data_frame(
  date = as.Date(1:813, origin = "2018-01-01"),
  year = format(date, "%Y"),
  week = as.integer(format(date, "%W")) + 1,  # Week starts at 1
  day = factor(weekdays(date, TRUE),
               levels = rev(c("周一","周二","周三","周四","周五","周六","周日"))),
  hours = 0)
d
set.seed(1)
# Simulate weekends
weekends <- dplyr::filter(d, grepl("S(at|un)", day))
# Hours worked are (might be) poisson distributed
weekends$hours <- rpois(nrow(weekends), lambda = 4)
# Simulate missing days with probability .7
weekends$na <- rbinom(nrow(weekends), 1, 0.7)
weekends$hours <- ifelse(weekends$na, NA, weekends$hours)

# Simulate weekdays
weekdays <- filter(d, !grepl("S(at|un)", day))
weekdays$hours <- rpois(nrow(weekdays), lambda = 8)  # Greater lambda
weekdays$na <- rbinom(nrow(weekdays), 1, 0.1)  # Smaller p(missing)
weekdays$hours <- ifelse(weekdays$na, NA, weekdays$hours)

# Concatenate weekends and weekdays and arrange by date
d <- bind_rows(weekends, weekdays) %>%
  arrange(date) %>%  # Arrange by date
  select(-na)  # Remove na column
d

library(ggplot2)
library(viridis)  # Color palette
library(ggthemes)
library(ggcor)
gh_waffle <- function(data, pal = "D", dir = -1,type = 21){
  
  p <- ggplot(data, aes(x = week, y = day, fill = hours)) +
    scale_fill_viridis(name="Hours",
                       option = pal,  # Variable color palette
                       direction = dir,  # Variable color direction
                       na.value = "grey93",
                       limits = c(0, max(data$hours))) +
    # geom_tile(color = "white", size = 0.4) +
    facet_wrap("year", ncol = 1) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = seq(1, 52, length = 12),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    theme_tufte(base_family = "Helvetica") +
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom",
          legend.key.width = unit(1, "cm"),
          strip.text = element_text(hjust = 0.01, face = "bold", size = 12))
  
  #圆形
  if(type == 21){p = p +geom_point(color = "white", size = 4,pch = 21) }
  #方形
  if(type == 22){p = p +geom_point(color = "white", size = 3,pch = 22)}
  #菱方形
  if(type == 23){p = p +geom_point(color = "white", size = 3,pch = 23) }
  #上三角
  if(type == 24){p = p +geom_point(color = "white", size = 3,pch = 24) }
  #下三角
  if(type == 25){p = p +geom_point(color = "white", size = 3,pch = 25) }
  
  #星形状图
  if(type == "star"){p = p +geom_star() }
  #饼图
  if(type == "pie2"){p = p + geom_pie2(size = 0.5) }
  
  
  print(p)
}
gh_waffle(d,pal = "D", dir = -1,type = 21)
gh_waffle(d,pal = "D", dir = -1,type = "pie2")
for (pal in c("A", "B", "C")) {
  gh_waffle(d,pal, dir = -1,type = "star")
}

```

![image-20200221114744185](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221114744185.png)

### **折线图**

```R
rm(list = ls())
library(ggplot2)
#绘制单线条
df <- data.frame(dose =c('A','B','C'),len=c(5.16,10.10,30))
head(df)
#因为横坐标的属性为因子（离散型的字符转换为因子），所以需要添加‘group = 1’的设置。
ggplot(data=df,aes(x=dose,y=len,group=1))+geom_line(linetype = "dashed",color="red")+geom_point()
#添加箭头
library(grid)
ggplot(data=df, aes(x=dose, y=len, group=1))+geom_line(arrow = arrow())+geom_point()
#自定义箭头类型
myarrow=arrow(angle = 15, ends = "both", type = "closed")
ggplot(data=df, aes(x=dose, y=len, group=1)) +geom_line(arrow=myarrow)+geom_point()
#附赠
ggplot(data=df, aes(x=dose, y=len, group=1)) + geom_step()+ geom_point()

#绘制多线条
df2 <- data.frame(supp=rep(c("Case", "Control"),each=3), dose=rep(c("A", "B", "C"),2),len=c(6.8, 15, 38, 5.16, 10.10, 30))
head(df2)

ggplot(data=df2, aes(x=dose, y=len, group=supp))+
  geom_line(linetype="dashed", color="blue", size=1.2)+
  geom_point(color="red", size=3)
#改变线形
ggplot(df2, aes(x=dose, y=len, group=supp)) +
  geom_line(aes(linetype=supp))+
  geom_point(aes(shape=supp))
#自定义线性
ggplot(df2, aes(x=dose, y=len, group=supp)) +
  geom_line(aes(linetype=supp))+
  geom_point()+
  scale_linetype_manual(values=c("twodash", "dotted"))
#更改颜色
p <- ggplot(df2, aes(x=dose, y=len, group=supp)) +
  geom_line(aes(color=supp))+
  geom_point(aes(color=supp))
p
# Use custom color palettes
p+scale_color_manual(values=c("#E69F00", "#56B4E9"))
# Use brewer color palettes
p+scale_color_brewer(palette="Dark2")
# Use grey scale
p +scale_color_grey() + theme_classic()

#绘制添加误差棒的折线图 可以使用position_dodge 参数，防止errorbars重叠
ggplot(df3, aes(x=dose, y=len, group = supp, color=supp))+ 
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.1, 
                position=position_dodge(0.05)) +
  geom_line(aes(linetype=supp)) + 
  geom_point(aes(shape=supp))+
  labs(title="Plot of lengthby dose",x="Dose (mg)", y = "Length")+
  theme_classic()+
  scale_color_manual(values=c('#999999','#E69F00'))

# 我们的另一个备选方案，拆线图也很好看
library(ggpubr)
p=ggline(all, x="group", y="distance", add = "mean_se",  color = "type",
       palette = "jco",legend = "right") +main_theme
p
ggsave(paste("e.line",".pdf", sep=""), p, width = 5, height = 3)
```

![image-20200221115058802](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221115058802.png)

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200421110311297.png" alt="image-20200421110311297" style="zoom:50%;" />



### **雷达图**

```R
setwd('./Script/雷达图/')
#ggradar包需要通过 github 来安装，安装方法如下：
devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
library(ggradar)

# 或者手动下载下来
#install.packages("E:/R/R-3.5.2/library/ggradar-master/", repos = NULL, type = "source") 

set.seed(123)
mydata<-matrix(runif(24,0,1),4,6)
rownames(mydata) <- LETTERS[1:4]
colnames(mydata) <- c("Apple","Google","Amozon","Tencent","Alibaba","Baidu")
#使用以上文本向量为矩阵列命名，方便展示
mynewdata<-data.frame(mydata)
Name<-c("USA","CHN","UK","RUS")
mynewdata<-data.frame(Name,mynewdata)
mynewdata

#绘制所有变量的雷达图
ggradar(mynewdata)

library(ggradar)
library(scales)
library(tibble)
library(dplyr)
#利用内置car数据集进行雷达图绘制，主要是对数据进行scale，然后绘制
mtcars %>%
  rownames_to_column( var = "group" ) %>% #保留行名称
  mutate_at(vars(-group),funs(rescale)) %>%
  tail(4) %>% select(1:6) -> mtcars_radar
ggradar(mtcars_radar, grid.line.width = 0.5,axis.label.size= 5,group.line.width = 1,group.point.size = 2)


#简单例子
set.seed(1234)
dat <- data.frame(
  obj = c('obj1', 'obj2', 'obj3'),
  factor1 = runif(3, 0, 1),
  factor2 = runif(3, 0, 1),
  factor3 = runif(3, 0, 1),
  factor4 = runif(3, 0, 1),
  factor5 = runif(3, 0, 1))
#查看数据集结构
dat
ggradar(dat, background.circle.transparency = 0, group.colours = c('blue', 'red', 'green3'))
```

![image-20200221115409118](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221115409118.png)

### **发散性正负图**

```R
setwd('../发散性正负图/')
rm(list = ls())
library(ggplot2)
# 使用mtcars数据集
data("mtcars")  
# 保留car name ，新建一列
mtcars$car_name <- rownames(mtcars) 
# 对mpg进行标准化处理 
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  
# 按照0未阈值 ，分上 下
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  

mtcars <- mtcars[order(mtcars$mpg_z), ]  # 为展示美观，数据排序
# 改为因子，能够保持原顺序 改为因子使图形按照原顺序输出，很常用
mtcars$car_name <- factor(mtcars$car_name, levels = mtcars$car_name)  

#Diverging bars是一种可以同时处理负值和正值的条形图。注意为了使柱状图创建柱形图而不是直方图，需要确保：
#（1）设置stat=identity

#（2）在aes()中同时提供x和y，其中x是字符或因子，y是数值。
ggplot(mtcars, aes(x=car_name, y=mpg_z, label=mpg_z)) +   geom_bar(stat='identity', aes(fill=mpg_type), width=.5)  + 
  scale_fill_manual(name="Mileage", labels = c("Above Average", "Below Average"), values = c("above"="#00ba38", "below"="#f8766d")) +  
  labs(subtitle="Normalised mileage from 'mtcars'", title= "Diverging Bars") + coord_flip() + theme_bw()
#Diverging Lollipop Chart
ggplot(mtcars, aes(x=car_name, y=mpg_z, label=mpg_z)) +   geom_point(stat='identity', color="orange",size=4)  +  geom_segment(aes(y = 0, 
                                                                                                                                  x =car_name, 
                                                                                                                                  yend = mpg_z, 
                                                                                                                                  xend =car_name), 
                                                                                                                              color = "grey")  +  labs(title="Diverging Lollipop Chart") +   ylim(-2.5, 2.5) +  coord_flip() + theme_bw()
#点图
ggplot(mtcars, aes(x=car_name, y=mpg_z, label=mpg_z)) +   geom_point(stat='identity', aes(col=mpg_type), size=6)  +  scale_color_manual(name="Mileage",
                                                                                                                                        labels = c("Above Average", "Below Average"),
                                                                                                                                        values = c("above"="#00ba38", "below"="#f8766d")) +   geom_text(color="white", size=2) +  labs(title="Diverging Dot Plot",
                                                                                                                                                                                                                                       subtitle="Normalized mileage from 'mtcars': Dotplot") +   ylim(-2.5, 2.5) +  coord_flip() + theme_bw()

```

![image-20200221115730316](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221115730316.png)

![image-20200221115756264](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221115756264.png)

****

### **bee蜜蜂图**

```R
setwd('./蜜蜂图/')
rm(list = ls())
#install.packages("beeswarm")
library('beeswarm')
'''
其中主要的参数：
Method 主要是指的散点图的样式。Center对称群；另外还有swarm（成群的），hex，square。
spacing 各点之间距离。
Cex 各点的大小。
corral 控制点不超过坐标点的区域，可以结合corralWidth设置坐标位置宽度。超出的部分如果设置omit则会忽略；设置random会随机显示在覆盖的区域。
Priority 点的排布，但是只在method=swarm时可用。其可选的参数包括：ascending（升序），descending（降序），density， random（随机显示） ，none。
Add 是否加在其它绘图之上，比如箱线图上加这个图则设置为TRUE。
Pwcol 设置分组，可以给与不同的颜色显示不同的点。当然，可以支持list设置多个组的不同样本颜色。
Pch 点的形状，可以参考plot对点的设置。'''
#单组数据
distributions <- data.frame(runif = runif(200,min = -3, max = 3), rnorm = rnorm(200),rlnorm = rlnorm(200, sdlog = 0.5))
beeswarm(distributions, col = 2:4, pch=16)
###多组数据

data(breast)
beeswarm(time_survival ~ ER, data = breast, pch = 16, pwcol = 1 +as.numeric(event_survival),xlab = "", ylab = "Follow-up time(months)",labels = c("ER neg", "ER pos")) 
legend("topright", legend = c("Yes","No"), title = "Censored", pch = 16, col = 1:2)

##每个组独立设置颜色
Dd=1 + as.numeric(breast $event_survival)
mycol=list() 
neg=which(Dd==1)
pos=which(Dd==2)
mycol$neg=Dd[1:length(neg)]
mycol$pos=Dd[1:length(pos)]+1
data=list() 
data$neg=breast$time_survival[neg]
data$pos=breast$time_survival[pos]
beeswarm(data, pch = 16, pwcol = mycol,xlab= "", ylab = "Follow-up time (months)",labels = c("ERneg", "ER pos"))
legend("topright", legend = 1:3,title = "Censored", pch = 16, col = 1:3)

#另外此包还内置了自己的一个箱线图的绘制函数，和R语言基础的函数名称是一样的boxplot：

##基础箱线图 
beeswarm(len ~ dose, data = ToothGrowth) 
bxplot(len ~ dose, data = ToothGrowth, add = TRUE)

###负责箱线图绘制
data(breast) 
bxplot(time_survival ~ event_survival, data = breast, probs = seq(0, 1,by = 0.1), col = rainbow(10))
beeswarm(time_survival ~ event_survival, data = breast, pch = 21, bg ="green", add = TRUE)

```

![image-20200221120253010](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221120253010.png)

![image-20200221120323606](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221120323606.png)

### BeanPlot(豆荚图)

```R
library(beanplot)
par(mfrow = c(1,2))
boxplot(len ~ dose, data = ToothGrowth,
        boxwex = 0.25, at = 1:3 - 0.2,
        subset = supp == "VC", col = "yellow",
        main = "Guinea Pigs' Tooth Growth",
        xlab = "Vitamin C dose mg",
        ylab = "tooth length", ylim = c(-1, 40), yaxs = "i")
boxplot(len ~ dose, data = ToothGrowth, add= TRUE,
        boxwex = 0.25, at = 1:3 + 0.2,
        subset = supp == "OJ", col = "orange")

legend("bottomright",bty="n",c("Ascorbic acid", "Orange juice"),
       fill = c("yellow", "orange"))

beanplot(len ~ reorder(supp, len, mean) *dose, ToothGrowth,
         side = "b", col = list("yellow","orange"), border = c("yellow2",
                                                               "darkorange"), main = "Guinea Pigs' Tooth Growth",
         xlab = "Vitamin C dose mg", ylab = "tooth length",ylim = c(-1,
                                                                    40), yaxs = "i")
legend("bottomright",bty="n",c("Ascorbic acid", "Orange juice"),
       fill = c("yellow", "orange"))
```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200404100132164.png" alt="image-20200404100132164" style="zoom:67%;" />

### ggalluvial冲击图

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200301152013590.png" alt="image-20200301152013590" style="zoom:25%;" />

```R
library(ggplot2)
library(ggsci)
library(ggalluvial)
rm(list = ls())
pdf('./phylum.pdf')
phylum <- read.delim("1.txt",check.names = FALSE,stringsAsFactors = FALSE)
p <- ggplot(data = phylum,aes(x=variable,y=value,alluvium=Phylum,stratum=Phylum,fill=Phylum))+
  geom_alluvium(aes(fill=Phylum,colour = Phylum))+geom_stratum()
p+scale_color_npg()+scale_fill_npg()+theme(axis.text.x=element_text(size=12),axis.text.y=element_text(size=12),
                                           legend.text=element_text(size=10),legend.title=element_blank(),
                                           legend.position = 'bottom')+xlab('')+ylab('')

dev.off()

```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200301160340060.png" alt="image-20200301160340060" style="zoom:33%;" />

饼图

[【R与Excel】满足需求：多层饼图 ](https://mp.weixin.qq.com/s?__biz=MzA3MzcwNDMyNA==&mid=2247485098&idx=2&sn=4adcf3c4aca5d9c03208aec5e4c2f518&chksm=9f0bb041a87c39571e5e0a40af63163d0e1d50519de3ccf0a4eb2a39085d058fd53cb770a233&mpshare=1&scene=1&srcid=&sharer_sharetime=1592181290604&sharer_shareid=a9c678b9d5b98f3f89a861807cb45f79&key=15fd7ea6eecdea226a0c8d700a877f32f4cc6f881ba1899577fd51be3fd12a1b6786d85702fda9dd1b3ae38d5e9191d24d627e02150f230a186dbf45b6747e6be38f080f5b8f93564eb463ac6a0911c3&ascene=1&uin=Mjc2NjUwNjgzOA%3D%3D&devicetype=Windows+10+x64&version=62090070&lang=zh_CN&exportkey=AzadIfn6%2BwTEjW2SziDxCMM%3D&pass_ticket=u7NR5bYzf02sDg1rcV%2FEKVKBwY5earOnuUBXjo84nhp6rauQdjFylEmD7mJC851l)

```python
import matplotlib.pyplot as plt
#The slices will be ordered and plotted counter-clockwise.
labels ='Frogs','Hogs','Dogs','Logs'#定义标签
sizes=[15,30,45,10]#每一块的比例
colors=['yellowgreen','gold','lightskyblue','lightcoral']#每一块的颜色
explode=(0, 0.1, 0.1, 0)#突出显示，这里仅仅突出显示第二块（即Hogs'）
plt.pie(sizes,explode=explode,labels=labels,colors=colors,autopct='%1.1f%%',shadow=True,startangle=90)
plt.axis ('equal')#显示为圆（避免比例压缩为椭圆）
plt.show ()
plt.savefig('piechart.png' , dpi=300)

***************R中方法******************
a <- c("A","B","C","D","E","F","G")
b <- c(17,13,8,5,10,2,9)
pct <- round(b/sum(b)*100)
lab <- paste(a,' ',pct,"%",sep = '')
d <- data.frame(a,b,lab)
d <- d[order(d[,"b"]),]
library(RColorBrewer)
pie(d[,"b"],labels=NA,clockwise=T,col=brewer.pal(7,"Set1"),
    border="white",radius=0.9,cex=0.5,main="Pie chart")
#添加图例
legend("right",legend = lab,bty = "n",inset = c(-0.7,0),
       xpd = T,fill = brewer.pal(7,"Set1"))
#还有一种方式是在扇形的附近进行文字标注
pie(d$b,labels=d$lab,col=rainbow(length(d$lab)),
    main="Pie Chart with Percentage")
#3D饼图
library(plotrix)
pie3D(d$b, labels=d$lab, explode=0.1, height = 0.2,
      radius = 0.8, main="3D Pie Chart",col =  brewer.pal(7,"Set1"))

#ggplot2画法
color <- c('#FF7F00', '#FFFF33', '#984EA3', '#4DAF4A', '#377EB8', '#E41A1C',
           '#FFED6F', '#CCEBC5', '#BC80BD', '#FCCDE5', '#B3DE69', '#FDB462')
names(color) <- names(phylum)
library(ggplot2)

dir.create('plot', recursive = TRUE)  #创建目录“plot”用于存放图片

for (module in unique(node$modularity_class)) {
  node_module <- subset(node, modularity_class == module)
  node_phylum <- data.frame(table(node_module$phylum))
  
  p <- ggplot(node_phylum, aes(x = '', y = Freq, fill = Var1)) + 
    geom_bar(stat = 'identity', width = 1) +
    coord_polar(theta = 'y') +
    scale_fill_manual(limits = names(color), values = color) + 
    theme(panel.grid = element_blank(), panel.background = element_blank(), 
          axis.text.x = element_blank()) +
    labs(x = '', y = '')
  ggsave(paste('plot/', module,'.pdf', sep = ''), p)
}
```

### **直方图**

```
barplot(data3, xlab="x-axis", ylab="y-axis", main="bar chart 1",
col=c("grey", "blue", "yellow"), beside=TRUE);#不加beside就位填充图
```

**base包画直方图**

```
> set.seed(123);
> data1 <- rnorm(100, mean=5, sd=3);
> hist(data1, main="histogram", xlab="x-axis", col="green",
border="blue", breaks=10, freq=FALSE);
> lines(density(data1), col="red");

hist(x,breaks=20,ylim=c(0,220))
text(res$mids,res$counts+10,labels=res$counts)
#ggplot格式
ggplot(data,aes(x=mids,y=counts)) + geom_bar(stat="identity") + geom_text(aes(label=counts),vjust=-1) + ylim(0,220)
```

![image-20200618164956571](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200618164956571.png)



**填充形状不同的直方图**

```R
# create dummy data
data <- data.frame(
  name=letters[1:5],
  value=sample(seq(4,15),5)
)

# barplot
barplot( height=data$value, names=data$name , density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) , col=c("red","grey","black","#EA1D4B","yellow"))
#density可以通过调控线的密度，angle通过调控线的方向来做出不同样式的填充线。这两种参数一起调控，可实现相当多的类型。
```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200610090406478.png" alt="image-20200610090406478" style="zoom:33%;" />



```R
setwd('../直方图/')
rm(list = ls())
library(ggplot2)
set.seed(1234)
df <-  data.frame(sex = factor(rep(c("F","M"),each = 200)),weight = round(c(rnorm(200,mean = 55,sd = 5),rnorm(200,mean = 65,sd= 5))))
ggplot(df,aes(x=weight))+
  geom_histogram(binwidth = 1,color = " black",fill = " white")

ggplot(df, aes(x=weight)) +
  geom_histogram(binwidth=1,color="black", fill="lightblue",linetype="dashed")+ #设置框线类型，颜色和fill的颜色
  geom_vline(aes(xintercept=mean(weight)), color="blue", linetype="dashed", size=1) #添加均值线，设置线型，颜色等

ggplot(df, aes(x=weight)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+ # 需要密度形式 
  geom_density(alpha=.2, fill="#FF6666")

ggplot(df, aes(x=weight, color=sex)) +
  geom_histogram(fill="white", alpha=0.5, position="identity") #分组设置 其中position可选 “identity”, “stack”, “dodge”. 默认值是 “stack”.

#分组添加均值线
library(plyr)
mu <- ddply(df, "sex", summarise, grp.mean=mean(weight))
p<-ggplot(df, aes(x=weight, color=sex)) +
  geom_histogram(fill="white", position="dodge")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=sex),linetype="dashed")+
  theme(legend.position="top")
p

# Use custom color palettes  自定义颜色
p+scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
# Use brewer color palettes
p+scale_color_brewer(palette="Dark2")
# Use grey scale
p + scale_color_grey() + theme_classic() + theme(legend.position="top")

#分组更改fill设置
ggplot(df, aes(x=weight, fill=sex, color=sex)) +  geom_histogram(binwidth=1,position="identity", alpha=0.5)+ geom_vline(data=mu, aes(xintercept=grp.mean),linetype="dashed")

```

![image-20200221120541471](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221120541471.png)

```R
#depth 深度、GC 含量散点密度图
depth_GC <- ggplot(depth_base, aes(GC, depth)) +
	geom_point(color = 'gray', alpha = 0.6, pch = 19, size = 0.5) +
	geom_vline(xintercept = GC_mean, color = 'red', lty = 2, lwd = 0.5) + 
	geom_hline(yintercept = depth_mean, color = 'red', lty = 2, lwd = 0.5) +
	stat_density2d(aes(fill = ..density.., alpha = ..density..), geom = 'tile', contour = FALSE, n = 500) +
	scale_fill_gradientn(colors = c('transparent', 'gray', 'yellow', 'red')) +
	theme(panel.grid.major = element_line(color = 'gray', linetype = 2, size = 0.25), panel.background = element_rect(color = 'black', fill = 'transparent')) +
	labs(x = paste('GC % (Average :', GC_mean, '%)'), y = paste('Depth (Average :', depth_mean, 'X)')) +
	theme(axis.text = element_text(size = 10)) +
	theme(axis.title = element_text(size = 12)) +
	theme(legend.position = 'none')

#depth 深度直方密度图
depth_hist <- ggplot(depth_base, aes(depth)) +
	geom_histogram(binwidth = (max(depth_base$depth) - min(depth_base$depth))/100, fill = 'gray', color = 'gray40', size = 0.1) +
	geom_rug(color = 'gray', alpha = 0.6) +
	theme(panel.grid.major = element_line(color = 'gray', linetype = 2, size = 0.25), panel.background = element_rect(color = 'black', fill = 'transparent')) +
	theme(axis.line = element_line(color = 'black', size = 0.3), axis.text = element_text(size = 10), axis.title = element_text(size = 12)) +
	labs(x = '', y = 'Numbers') +
	coord_flip() +
	geom_vline(xintercept = depth_mean, color = 'red', lty = 2, lwd = 0.5)

#GC 含量直方密度图
GC_hist <- ggplot(depth_base, aes(GC)) +
	geom_histogram(binwidth = (max(depth_base$GC) - min(depth_base$GC))/100, fill = 'gray', color = 'gray40', size = 0.1) +
	geom_rug(color = 'gray', alpha = 0.6) +
	theme(panel.grid.major = element_line(color = 'gray', linetype = 2, size = 0.25), panel.background = element_rect(color = 'black', fill = 'transparent')) +
	theme(axis.line = element_line(color = 'black', size = 0.3), axis.text = element_text(size = 10), axis.title = element_text(size = 12)) +
	labs(x = '', y = 'Numbers') +
	geom_vline(xintercept = GC_mean, color = 'red', lty = 2, lwd = 0.5)

#组合图片并输出
#pdf(paste(opt$output, '.pdf', sep = '.'), width = 8, height = 8)
#	grid.newpage()
#	pushViewport(viewport(layout = grid.layout(3, 3)))
#	print(depth_GC, vp = viewport(layout.pos.row = 2:3, layout.pos.col = 1:2))
#	print(GC_hist, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
#	print(depth_hist, vp = viewport(layout.pos.row = 2:3, layout.pos.col = 3))
#dev.off()
```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200430112318784.png" alt="image-20200430112318784" style="zoom:50%;" />





### 热图

#### 对角线热图

[ggplot绘制热图](https://blog.csdn.net/zhouhucheng00/article/details/86143828)

```R
rm(list = ls())
library('ggmatrix')
# install.packages("devtools")
devtools::install_github("houyunhuang/ggmatrix")
library(ggmatrix) ## 0.1.2版本
library(RColorBrewer)
df <- data.frame(x = rep(1:11, 14),
                 y = rep(1:14, each = 11),
                 group = sample(LETTERS[1:5], 154, replace = TRUE),
                 values1 = rnorm(154, mean = 10, sd = 5),
                 values2 = rnorm(154, mean = -10, sd = 2),
                 stringsAsFactors = FALSE)
ggplot(df, aes(x = x, y = y)) + geom_triangle() #默认填充色（fill）为空，只画出上下三角形
ggplot(df, aes(x = x, y = y)) + geom_triangle(fill = "lightblue") #当设置fill参数时，表明把上下三角形当成一个整体来处理，映射颜色也完全相同

ggplot(df, aes(x = x, y = y)) + geom_triangle(aes(fill = values1))
## 设置上三角颜色为浅蓝色
ggplot(df, aes(x = x, y = y)) + geom_triangle(fill.upper = "lightblue")
## 设置上三角颜色为浅蓝色，且设置下三角颜色为橙色
ggplot(df, aes(x = x, y = y)) +
  geom_triangle(fill.upper = "lightblue", fill.lower = "orange")
#采用映射
ggplot(df, aes(x = x, y = y, fill.upper = values1)) +
  geom_triangle() +
  scale_fill_upper_gradientn(colours = c("red", "white", "blue"))
#同时映射上下三角，是不是感觉不用ggnewscale或者relayer包的黑科技了，会爽很多。
ggplot(df, aes(x = x, y = y, fill.upper = values1, fill.lower = values2)) +
  geom_triangle(mode = "lb-rt") +
  scale_fill_upper_gradientn(colours = brewer.pal(5, "Greys")) +
  scale_fill_lower_gradientn(colours = brewer.pal(5, "YlGnBu"))
#也可以映射离散变量。
ggplot(df, aes(x = x, y = y, fill.upper = group, fill.lower = values2)) +
  geom_triangle() +
  scale_fill_upper_manual(values = brewer.pal(5, "Set1")) +
  scale_fill_lower_gradientn(colours = c("#E9A3C9", "#F7F7F7", "#A1D76A"),
                             guide = guide_legend())
#ggmatrix提供了五种颜色映射函数，每个函数的用法均和对应的原ggplot2中的函数相同。
#scale_fill_lower/upper_gradient()——ggplot2中scale_fill_gradient()。
#scale_fill_lower/upper_gradient2()——ggplot2中scale_fill_gradient2()。
#scale_fill_lower/upper_gradientn()——ggplot2中scale_fill_gradientn()。
#scale_fill_lower/upper_identity()——ggplot2中scale_fill_identity()。
#scale_fill_lower/upper_manual()——ggplot2中scale_fill_manual()
```

#### 环状热图

```R
rm(list = ls())
library(ggforce)
#加上force=TRUE升级
devtools::install_github(("houyunhuang/ggcor"),force = TRUE)
library(ggcor)
correlate(mtcars,method = 'spearman') %>% trans_polar()
#trans_polar()是在ggcor的总体框架下设计的，所以对于输入数据也要求是cor_tbl对象，
#当输入不是cor_tbl对象时，调用fortify_cor()函数转化。
correlate(mtcars) %>%polarcor()
#对称矩阵
correlate(mtcars) %>%
  polarcor() + geom_arc_colour(aes(fill = r))
## 非对称的情况
library(ggplot2)
data("varespec", package = "vegan")
data("varechem", package = "vegan")
correlate(varechem, varespec) %>%
  polarcor() +
  geom_arc_colour(aes(fill = r), size = 0.25) +
  coord_fixed(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))

#通过`set_arc_axis_x()`、`set_arc_axis_y()`函数来设置尺寸
correlate(varechem, varespec) %>%
  polarcor(no.axis = TRUE) +
  geom_arc_colour(aes(fill = r), size = 0.25) +
  set_arc_axis_x() +
  set_arc_axis_y(size = 2.8) +
  coord_fixed(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))

#分组的极坐标
set.seed(20200328)
grp <- sample(LETTERS[1:3], 24, replace = TRUE)
grp
fortify_cor(varechem, varespec, group = grp) %>%
  polarcor(no.axis = TRUE, split.by.group = TRUE, group.space = 2) +
  geom_arc_colour(aes(fill = r), colour = "grey90", size = 0.1) +
  set_arc_axis_x(mapping = aes(colour = .group), size = 2.8) +
  set_arc_axis_y(size = 2.8) +
  coord_fixed(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))

#对于变量少的加上标签
correlate(varechem, varespec, cor.test = TRUE) %>%
  polarcor(no.axis = TRUE) +
  geom_arc_colour(aes(fill = r), size = 0.25) +
  geom_point(mapping = aes(x, y), shape = 4,
             data = function(data) filter(data, p.value > 0.05)) +
  set_arc_axis_x() +
  set_arc_axis_y(size = 2.8) +
  scale_fill_gradient2n(colours = c("blue", "white", "red")) +
  coord_fixed(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))

#对于变量更少的，我们完全可以加个系数和显著性标记。
correlate(mtcars, cor.test = TRUE) %>%
  polarcor(cluster = TRUE) +
  geom_arc_colour(aes(fill = r), size = 0.25) +
  geom_mark(aes(x, y, r = r, p.value = p.value, angle = angle), 
            size = 3) +
  scale_fill_gradient2n(colours = c("blue", "white", "red")) +
  coord_fixed(xlim = c(-1.05, 1.05), ylim = c(-1.05, 1.05))

#一般性的热图
a <- cor_tbl(extra.mat = list(mat = varespec[,1:10])) %>%
  polarcor(no.axis = TRUE) +
  geom_arc_colour(aes(fill = mat), size = 0.1, colour = "white") +
  set_arc_axis_x(size = 2.8) +
  set_arc_axis_y(size = 2) +
  scale_fill_gradientn(colours = c("green", "black", "red")) +
  coord_fixed(xlim = c(-1.15, 1.15), ylim = c(-1.1, 1.15))

```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200329145423151.png" alt="image-20200329145423151" style="zoom:50%;" />

#### heatmap&pheatmap热图

[使用ComplexHeatmap包绘制个性化热图](http://blog.sciencenet.cn/blog-3334560-1161717.html)

[R语言pheatmap热图色条控制小技巧](https://www.jianshu.com/p/6b765e83d723)

[获取pheatmap聚类后和标准化后的结果 ](https://mp.weixin.qq.com/s?__biz=MzI5MTcwNjA4NQ==&mid=2247495051&idx=1&sn=bc8b05cce13672ff1bc055ca5d4a8167&chksm=ec0e2801db79a117b9a9ed1c9303a2d0538c01692012145a26778f2868d41b70c7715fbe3037&mpshare=1&scene=1&srcid=0713O1ieBDC0C4zFs8x81FpW&sharer_sharetime=1594602043008&sharer_shareid=a9c678b9d5b98f3f89a861807cb45f79&key=983943efdc6388dd7fad72da017af9280423d3eceadff51bbc47ed01237f41cf54bdebbb3f389031afb82d8e65d3df2c92334d1bf2a482d7f8b30416799a462cebc4abe75bb80a988dbd9768e465d4ec&ascene=1&uin=Mjc2NjUwNjgzOA%3D%3D&devicetype=Windows+10+x64&version=62090529&lang=zh_CN&exportkey=A1%2FC%2BbQB1WSHJS3WnKkRSlI%3D&pass_ticket=zRQscdCzzQbn2ab7JhXCh5Oq1fUXCjVf4lIT%2FGeGOLVpzd0XWGFCg6hXQVOItnyl)

***

```R
##heatmap.2画热图
data("attitude")
Ca <- cor(attitude)#cor的结果就是矩阵

library(gplots)
library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)#换个好看的颜色
hM <- format(round(Ca, 2))#对数据保留2位小数

heatmap.2(Ca,
trace="none",#不显示trace
col=coul,#修改热图颜色
density.info = "none",#图例取消density
key.xlab ='Correlation',
key.title = "",
cexRow = 1,cexCol = 1,#修改横纵坐标字体
Rowv = F,Colv = F, #去除聚类
margins = c(6, 6),
cellnote = hM,notecol='black'#添加相关系数的值及修改字体颜色
            )
```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200408161407313.png" alt="image-20200408161407313" style="zoom:50%;" />

```R
###### pheatmap画图
library(pheatmap)
library(rmarkdown)
#将Rmd文件转换为html
render("./pheatmap.Rmd","html_document")
rm(list=ls())
test = matrix(rnorm(200),20,10)
test[1:10, seq(1, 10, 2)] = test[1:10, seq(1, 10, 2)] + 3 
test[11:20, seq(2, 10, 2)] = test[11:20, seq(2, 10, 2)] + 2 
test[15:20, seq(2, 10, 2)] = test[15:20, seq(2, 10, 2)] + 4 
colnames(test) = paste("Test", 1:10, sep = "") 
rownames(test) = paste("Gene", 1:20, sep = "")
head(test[,1:6])
pheatmap(test)
# scale = "row"参数对行进行归一化
# clustering_method参数设定不同聚类方法，默认为"complete",可以设定为'ward', 'ward.D', 'ward.D2', 'single', 'complete', 'average', 'mcquitty', 'median' or 'centroid'
pheatmap(test,scale = "row", clustering_method = "average")
#表示行聚类使用皮尔森相关系数聚类，默认为欧氏距离"euclidean"
pheatmap(test, scale = "row", clustering_distance_rows = "correlation") 

pheatmap(test, display_numbers = TRUE)
pheatmap(test, display_numbers = TRUE, number_format = "\%.1e")
pheatmap(test, display_numbers = matrix(ifelse(res$p <= 0.01,"**",
    ifelse(res$p <= 0.05, "*", "")), nrow(res$p))#还可以自己设定要显示的内容；
 display_numbers=T,number_format="%.3f",
#自定义颜色
pheatmap(test, color = colorRampPalette(c("navy", "white", "firebrick3"))(50)) 
#颜色赋予变量 
hmcol <- rev(colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(255))
# 设置对角距离为NA，增强颜色的展示力  适用于相关性热图
diag(dists) <- NA
#行 列是否聚类，cluster_row ,cluster_col 
pheatmap(test, cluster_row = FALSE,cluster_col = TRUE) 
# Show text within cells pheatmap(test, display_numbers = TRUE) 
#pheatmap(test, display_numbers = TRUE, number_format = "%.1e") # number_format = "%.1e"参数设定数值的显示格式
pheatmap(test, display_numbers = matrix(ifelse(test > 5, "*", ""), nrow(test))) 
#设定legend展示的值
#legend_breaks参数设定图例显示范围，legend_labels参数添加图例标签
pheatmap(test, cluster_row = FALSE, legend_breaks = -1:4, legend_labels = c("0", "1e-4", "1e-3", "1e-2", "1e-1", "1"))
#去掉legend
pheatmap(test, legend = FALSE)
         angle_col = 45#调角度
# border=FALSE参数去掉边框线
pheatmap(test, border=FALSE)
# border_color参数设定每个热图格子的边框色
pheatmap(test, border_color = "red")
# treeheight_row和treeheight_col参数设定行和列聚类树的高度，默认为50
pheatmap(test, treeheight_row = 30, treeheight_col = 50)
# Fix cell sizes and save to file with correct size 
pheatmap(test, cellwidth = 15, cellheight = 12, main = "Example heatmap") 

#设置字体大小，设置行、列的聚类方法()
drows<-vegdist(data,method="bray")
dcols<-vegdist(t(data),method="bray")
pheatmap(data,cellwidth = NA, cellheight =NA, treeheight_row = 50, treeheight_col = 50 ,color = colorRampPalette(c("green", "black","red"))(100), scale="row", legend=TRUE,border_color=NA, fontsize_row=8, fontsize_col=10,clustering_distance_rows =drows, clustering_distance_cols = dcols, clustering_method="average",main="Heatmap")

**********控制图例范围**********
data1<-c(runif(20,min=-2,max=2),rnorm(380,2,2))
data1<-matrix(sample(data1,400,replace = F),nrow=20)
range(data1)
#breaks
bk <- c(seq(-3,-0.1,by=0.01),seq(0,3,by=0.01))
# 做热图：
pheatmap::pheatmap(data1,
         scale = "none",
         color = c(colorRampPalette(colors = c("blue","white"))(length(bk)/2),colorRampPalette(colors = c("white","red"))(length(bk)/2)),
         legend_breaks=seq(-3,3,2),
         breaks = bk)
**********************************

#设置颜色
col = colorRampPalette(c("lightblue","yellow","orange","red"),bias=3)(300)
#设置注释信息 自定义图例颜色
ann_colors=list(Group=c(H="blue",D="red"),Class=c(Bacteroidetes="#EE7AE9",Firmicutes="#795EA2",Proteobacteria="#3370CC"))
#gaps_col =  3 第几列断开 cut_tree = 2#分几分
********************************
# gaps_row = c(10, 14)参数在第10和14行处添加gap, 要求对行不进行聚类
pheatmap(genu,display_numbers = TRUE,number_color = TRUE,scale = "row",
         cellwidth = NA , cellheight = NA,
         cluster_rows=FALSE,treeheight_row = 50, treeheight_col = 50, 
         cluster_cols=TRUE,color = colorRampPalette(c("green", "black","red"))(100),border_color = NA,cutree_cols = 2,legend = TRUE)
min(genu)
drows <- vegdist(genu,method = "bray")
dcols <- vegdist(t(genu),method = "bray")
pheatmap(genu,cellwidth = NA, cellheight = NA, treeheight_row = 50, treeheight_col = 50 
         ,color = colorRampPalette(c("green", "black","red"))(100), legend_breaks = c(1:5), legend_labels = c("1.0","2.0","3.0","4.0","5.0"),
         scale ="row", legend = TRUE,border_color = NA, cutree_rows = 3,cutree_cols = 3,
         fontsize_row = 8, fontsize_col = 10,clustering_distance_rows = drows, 
         clustering_distance_cols = dcols, clustering_method ="average",main ="Heatmap")

##针对聚类树添加与否 会改变相应的位置问题
p <- pheatmap(test) #默认加聚类树
r = rownames(test)[p$tree_row[["order"]]] #提取出来
c = colnames(test)[p$tree_col[["order"]]] 
new_test = test[r,c]
pheatmap(new_test,cluster_rows = F,cluster_cols = F)
#当然另一种简单方法直接设置树高：很巧妙，设置高度为0，就不显示了
pheatmap(diy1,treeheight_row = 0,treeheight_col = 0)
```

#### **相关性热图 (ggcor包)**

[corrplot](file:///E:/R/R-3.6.2/library/corrplot/doc/corrplot-intro.html)

[相关性热图还能玩出什么花样](https://www.omicshare.com/forum/thread-6316-1-2.html)

[物种与代谢物相关性热图是怎么画的？](https://www.omicshare.com/forum/thread-5780-1-11.html)

```R
####计算相关性时候的技巧
#将 spearman 相关系数低于 0.7 的关系剔除，即 r>=0.7
library(Hmisc)
r <- genus_corr$r  
r[abs(r) < 0.7] <- 0
p <- genus_corr$P
p <- p.adjust(p, method = 'BH')    #可选 p 值校正，这里使用 BH 法校正 p 值
p[p>=0.05] <- -1
p[p<0.05 & p>=0] <- 1
p[p==-1] <- 0
#根据上述筛选的 r 值和 p 值保留数据
z <- r * p
diag(z) <- 0    #将相关矩阵中对角线中的值（代表了自相关）转为 0

Spearman秩相关系数，Pearson线性相关系数要求连续变量的取值服从正态分布，不服从正态分布的变量、分类或等级变量之间的关联性可采用Spearman秩相关系数，也称等级相关系数来描述。Pearson要求两个变量均符合正态分布，而当其中一个变量不符合正态分布时，就需要使用Spearman。

 # 使用shapiro.test函数检验每个变量是否与正太分布具有显著性差异
for(i in 1:ncol(attitude)){
  p_value <- apply(attitude,2,shapiro.test)[[i]]$p.value
  # 输出结果变量名和p值。
  print(paste(names(attitude)[i],p_value,sep="    "))
} #判断下变量是否符合正太分布

#箱线图查看下分布情况
library(reshape)
library(ggplot2)
# 宽格式变为长格式：
attitude_melt <- melt(attitude)  
# 作图查看每个变量的离群值：
ggplot(attitude_melt)+
  geom_boxplot(aes(x=variable,y=value,fill=variable),outlier.colour = "red",outlier.size = 3)+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 30,hjust=1,vjust=1)
  )
```

```R
#读取数据
dat <- read.table('data.txt', sep = '\t', row.names = 1, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
dat_env <- dat[1:6]	#只包含环境因子数据
dat_phylum <- dat[8:17]	#只包含细菌类群丰度数据

##协方差和常见的相关系数
#协方差计算，cov()
cov_pearson <- cov(dat, method = 'pearson')
cov_spearman <- cov(dat, method = 'spearman')
cov_kendall <- cov(dat, method = 'kendall')

#相关系数计算，cor()
cor_pearson <- cor(dat, method = 'pearson')
cor_spearman <- cor(dat, method = 'spearman')
cor_kendall <- cor(dat, method = 'kendall')

#输出，例如
#提取“cor_pearson”中相关系数 >0.5 或 <-0.5 的结果输出为 csv 样式
cor_pearson[abs(cor_pearson) <= 0.5] <- 0
write.csv(cor_pearson, 'cor_pearson.csv', quote = FALSE)
write.table(genu_fun_corr_final, 'lia_cor.txt', row.names = TRUE, sep = '\t', quote = FALSE)
#指定分组的相关性分析，cor()
#此处计算“dat_phylum”和“dat_env”中数据的相关性，即只关注细菌类群丰度与环境因子之间的相关性
phylum_env_spearman <- cor(dat_phylum, dat_env, method = 'spearman')

#hetcor()，以计算“dat_phylum”中两两细菌类群间多分格相关系数为例
library(polycor)

cor_polychoric <- hetcor(dat_phylum, type = 'Polychoric')

#偏相关，pcor()
#此处计算“dat_phylum”中，Proteobacteria 和 Acidobacteria 的偏相关系数
library(ggm)

select <- c('Proteobacteria', 'Acidobacteria')
delet <-  names(dat_phylum)[-which(names(dat_phylum) %in% select)]
pcor(c(select, delet), cov(dat_phylum, method = 'spearman'))

##相关性的显著性检验
#cor.test()，此处使用“dat”中 Proteobacteria 和 Acidobacteria 列的数据
#意在判断“cor_pearson”中 Proteobacteria 和 Acidobacteria 的相关系数是否显著
cor.test(dat[,'Proteobacteria'], dat[,'Acidobacteria'], method = 'pearson')

#corr.test()  
library(psych)

phylum_corr <- corr.test(dat_phylum, method = 'spearman')
phylum_env_corr <- corr.test(dat_phylum, dat_env, method = 'spearman')

#以“phylum_corr”为例，查看数据
phylum_corr$r	#相关系数值
phylum_corr$p	#显著性 p 值

#筛选，例如
#“phylum_corr”中，根据 p(<0.05) 值和 r(>=0.3 or <=-0.3) 值做保留
phylum_corr$p[phylum_corr$p >= 0.05] <- -1
phylum_corr$p[phylum_corr$p < 0.05 & phylum_corr$p >= 0] <- 1
phylum_corr$p[phylum_corr$p == -1] <- 0
phylum_corr$r[abs(phylum_corr$r) < 0.3] <- 0
phylum_corr_final <- phylum_corr$r * phylum_corr$p
#或者occor.r[occor.p>0.05|abs(occor.r)<0.6] = 0
write.csv(phylum_corr_final, 'phylum_corr_final.csv', quote = FALSE)

# 数据量小时可以用psych包corr.test求相关性矩阵，数据量大时，可应用WGCNA中corAndPvalue, 但p值需要借助其他函数矫正
occor = corr.test(otu,use="pairwise",method="spearman",adjust="fdr",alpha=0.05)
occor.r = occor$r # 取相关性矩阵R值
occor.p = occor$p # 取相关性矩阵p值
# 确定物种间存在相互作用关系的阈值，将相关性R矩阵内不符合的数据转换为0
occor.r[occor.p>0.05|abs(occor.r)<0.6] = 0

#计算相关矩阵及显著性水平。
library(Hmisc)
#所有变量间相关系数的对称矩阵
rcorr_matrix <- rcorr(as.matrix(mtcars), type = 'pearson')
rcorr_matrix$r    #相关矩阵
rcorr_matrix$P    #p 值矩阵
#给定两组变量间相关系数的非对称矩阵
x <- as.matrix(mtcars[c('mpg', 'cyl', 'disp', 'hp')])
y <- as.matrix((mtcars[c('drat', 'wt', 'qsec')]))
rcorr_matrix <- rcorr(x, y, type = 'pearson')
rcorr_matrix$r    #相关矩阵
rcorr_matrix$P    #p 值矩阵
 
##相关图绘制示例，corrplot 包
library(corrplot)

#第一个示例，根据筛选后的数据“phylum_corr_final”作图
corrplot(phylum_corr_final, method = 'number', number.cex = 0.8, diag = FALSE, tl.cex = 0.8)
corrplot(phylum_corr_final, add = TRUE, type = 'upper', method = 'pie', diag = FALSE, tl.pos = 'n', cl.pos = 'n')

#第二个示例，在作图时，通过指定过滤参数（p<0.05），作图时自动屏蔽不显著的值
corrplot(phylum_env_corr$r, p.mat = phylum_env_corr$p, method = 'number', insig = 'blank', sig.level = 0.05, addCoef.col = 'black',  number.cex = 0.8, tl.cex = 0.8)

#读取相关性数据（已经计算好的相关性结果数据）
pearson1 <- as.matrix(read.table('pearson1.txt', sep = '\t', row.names = 1, header = TRUE))
pearson2 <- as.matrix(read.table('pearson2.txt', sep = '\t', row.names = 1, header = TRUE))

##############################
####corrplot 包相关图示例
library(corrplot)

#默认
corrplot(pearson1)

#method 参数，调整相关系数的展示方式
corrplot(pearson1, method = 'color')
corrplot(pearson1, method = 'number')
corrplot(pearson1, method = 'pie')

#diag = FALSE，隐藏对角线值
corrplot(pearson1, method = 'square', diag = FALSE)

#tl.pos、tl.cex、tl.col、tl.offset、tl.srt 等参数，调整变量标签字体属性
#例如调整大小及颜色
corrplot(pearson1, method = 'pie', diag = FALSE, tl.cex = 0.8, tl.col = 'black')

#cl.pos、cl.lim、cl.length、cl.cex、cl.ratio、cl.align.text、cl.offset 等参数，调整相关性标尺属性
#例如将标尺调整至图的下方，并调整刻度值的间隔
corrplot(pearson1, method = 'pie', diag = FALSE, cl.pos = 'b', cl.length = 9)

#number.cex、number.font、number.digits 等参数，调整相关性数字属性（当 method = 'number' 时可调整）
#例如调整展示数值的尺寸
corrplot(pearson1, method = 'number', diag = FALSE, number.cex = 0.8)

#order、hclust.method（当 order = 'hclust' 时可调整）等参数，可根据相关性强度对变量的展示顺序重新排列
#例如将变量根据 ward 最小方差聚类排序，并将变量聚为 3 类
corrplot(pearson1, method = 'color', order = 'hclust', hclust.method = 'ward.D', addrect = 3)

#type 可设置相关图的展示形式，默认展示全部
#例如仅展示对称矩阵的上三角部分
corrplot(pearson1, method = 'color', type = 'upper')

#添加 add = TRUE 时（默认 FALSE），可将新图叠加在原图上方
#例如一个组合样式的相关图
corrplot(pearson1, method = 'number', number.cex = 0.8, diag = FALSE, tl.cex = 0.8)
corrplot(pearson1, add = TRUE, type = 'upper', method = 'pie', diag = FALSE, tl.pos = 'n', cl.pos = 'n')

#col 属性自定义相关性的颜色设置，bg 可设置背景色
#例如自定义相关系数的颜色范围
col1 <- colorRampPalette(c('red3', 'orange', 'white', 'blue', 'blue4'))
corrplot(pearson1, method = 'number', diag = FALSE, col = col1(21), tl.cex = 0.8)
corrplot(pearson1, add = TRUE, type = 'upper', method = 'circle', diag = FALSE, col = col1(21), tl.pos = 'n', cl.pos = 'n')

#当 method 不为 “number”时，可使用 addgrid.col 设置颜色显示相关系数，同时配合 number.font 等参数使用
#例如当 method = 'color' 时，在图中显示相关系数数值，并 number.cex 调整字体大小
corrplot(pearson2, method = 'color', addCoef.col = 'black', number.cex = 0.8)

#当存在显著性检验结果时，可使用 sig.level、insig 等参数根据显著性 p 值屏蔽不显著的相关系数
#通常与 pch、pch.col、pch.cex 等参数来使用，这些参数设置屏蔽区的展示方式
#此处借用 psych 包中的 corr.test()，基于原始数据计算相关性，并执行显著性检验，之后根据结果绘制相关图
library(psych)

top10_phylum <- read.csv('top10_phylum.csv', row.names = 1)
spearman1 <- corr.test(top10_phylum, method = 'spearman')

corrplot(spearman1$r, method = 'square', type = 'lower', p.mat = spearman1$p, sig.level = 0.05, insig = 'blank', addCoef.col = 'black', diag = FALSE, number.cex = 0.8, tl.cex = 0.8)

##############################
####corrgram 包相关图示例
library(corrgram)

#默认
corrgram(pearson1)

#order = TRUE 时，可根据相关性强度对变量的展示顺序重新排列
corrgram(pearson1, order = TRUE)

#lower.panel、upper.panel 等参数可以设置相关系数的展示方式
#例如设置下三角区域填充图样式，上三角区域饼图样式
corrgram(pearson1, lower.panel = panel.fill, upper.panel = panel.pie)

#text.panel、label.pos 、label.srt、cex.labels、font.labels 等参数可用于调整面板的外观显示样式
#例如调整变量标签的字体大小
corrgram(pearson1, lower.panel = panel.shade, upper.panel = NULL, cex.labels = 1)

#col.regions 可用于自定义颜色范围，例如
col1 <- colorRampPalette(c('red3', 'orange', 'white', 'blue', 'blue4'))
corrgram(pearson1, lower.panel = panel.shade, upper.panel = panel.pie, col.regions = col1)

#可以直接读取原始数据，通过指定相关系数类型，作图展示
#相较于直接读取相关矩阵，此时可以添加散点图或拟合线图类型展示变量间的相关性
top10_phylum <- read.csv('top10_phylum.csv', row.names = 1)
corrgram(top10_phylum, cor.method = 'pearson', lower.panel = panel.pts, upper.panel = panel.ellipse)

##############################
####pairs()，绘制变量间两两相关性散点图矩阵
top10_phylum <- as.matrix(read.csv('top10_phylum.csv', row.names = 1))

panel.hist <- function(x, ...) {
    usr <- par('usr'); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = 'cyan', ...)
}

pairs(top10_phylum, panel = panel.smooth, diag.panel = panel.hist)

##############################
####GGally 包相关图示例
library(GGally)

#默认
ggcorr(pearson1)

#label = TRUE 时，在图中展示相关系数数值，此时可使用 label_alpha、label_color、label_round、label_size 等参数设置字体属性
ggcorr(pearson1, label = TRUE, label_alpha = TRUE)

#low、mid、high 等参数可自定义颜色范围，例如
ggcorr(pearson1, low = 'red', mid = 'white', high = 'blue')

#读取原始数据，指定所需计算的相关系数类型，自定义作图样式
top10_phylum <- read.csv('top10_phylum.csv', row.names = 1)

ggcorr(
	top10_phylum, 
	method = c('pairwise', 'pearson'), 
	name = expression(rho),
	geom = 'circle',
	max_size = 15,
	min_size = 5,
	size = 3,
	hjust = 0.75,
	nbreaks = 6,
	palette = 'PuOr' )

#ggpairs() 绘制变量间两两相关性散点图矩阵
top10_phylum <- read.csv('top10_phylum.csv', row.names = 1)
ggpairs(top10_phylum, columns = 1:6)


```

#### ggcor包组合相关矩阵和Mantel相

```R
*********ggcor包组合相关矩阵和Mantel相********
rm(list = ls())
library(vegan) ## 获取数据
library(ggcor)
data("varespec")
data("varechem")

mantel <- mantel_test(varespec, varechem)
combination_layout(mantel, type = "upper", show.diag = FALSE,
                   col.names = names(varechem))
'''
type、show.diag、row.names、col.names都是相关系数矩阵相关的参数，
若觉得这些有点多难得处理，完全可以传递一个cor_tbl对象给cor_tbl参数，这样所有的问题完美解决。如下：
'''
corr <- correlate(varechem, cor.test = TRUE) %>% 
  as_cor_tbl(type = "upper", show.diag = FALSE)
combination_layout(mantel, cor_tbl = corr)

combination_layout(mantel, cor_tbl = corr) %>% 
  ggplot() + 
  geom_link(aes(color = r), curvature = 0.05) +
  geom_start_point(fill = "red", shape = 23, size = 4) +
  geom_end_point(fill = "blue", shape = 21, size = 4) +
  geom_start_label(aes(x = x - 0.5), hjust = 1, size = 5) +
  geom_end_label(aes(x = xend + 0.5), hjust = 0, size = 5) +
#  scale_fill_gradient2(midpoint = 0, low = 'blue', mid = 'white', high = 'red', space = 'Lab') +#环境变量相关系数颜色赋值
#  scale_size(range = c(0, 1)) +  #根据 Mantel 相关指定线条粗细
  remove_axis('all') + #去除原 x、y 轴上的环境变量标签
  scale_color_gradient(low = 'red', high = 'blue') + #根据 Mantel 相关 r 值指定线条颜色
  coord_cartesian() +
  theme_void()

###多组的情况下
mantel2 <- mantel_test(varespec, varechem, mantel.fun = 'mantel.randtest',spec.dist.method = 'bray', env.dist.method = 'euclidean', 
                       spec.select = list(Spec01 = 1:7,
                                          Spec02 = 8:18,
                                          Spec03 = 19:24))

'''
有个细节需要注意，对于这个图，默认的起点都是群落，终点是环境，当然完全可以通过start.var、end.var参数来改变默认设置。
'''
##过程中，各样本基于物种丰度转化为 Bray-curtis 距离，基于环境变量使用欧式距离 ,mantel.fun可选择不同包中的mantel检验
combination_layout(mantel2, cor_tbl = corr) %>% #设置参数start.var = env2 type = "upper" 可调整线条方向 lower朝下
  ggplot() + 
  geom_link(aes(colour = r,size = r),curvature = 0.05) +
  geom_start_point(fill = "red", shape = 23, size = 4) +
  geom_end_point(fill = "blue", shape = 21, size = 4) +
  geom_start_label(aes(x = x - 0.5), hjust = 1, size = 5) +
  geom_end_label(aes(x = xend + 0.5), hjust = 0, size = 5) +
  scale_color_gradient(low = 'red', high = 'blue') +
  scale_size(range = c(0, 1)) + 
  coord_cartesian(xlim = c(-5, 14)) +
  theme_void()

#组合图
#第一种
mantel2 <- mantel_test(varespec, varechem, mantel.fun = 'mantel.randtest',spec.dist.method = 'bray', env.dist.method = 'euclidean', 
                       spec.select = list(Spec01 = 1:7,
                                          Spec02 = 8:18,
                                          Spec03 = 19:24))

mantel4 <- mantel2[-which(mantel2$p.value>0.05),] #可以做一定得筛选
##划定p值r值范围 变为离散型
#作图根据定义的区间设置连线风格，区分显著的相关系数和不显著的 Mantel 相关
mantel2 <- mutate(mantel2, 
                  r = cut(r, breaks = c(-Inf, 0.25, 0.5, Inf), 
                          labels = c('<0.25', '0.25-0.5', '>=0.5'), right = FALSE),
                  p.value = cut(p.value, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                                labels = c('<0.001', '0.001-0.01', '0.01-0.05', '>=0.05'), right = FALSE))


#对于重要的相关系数的取舍，除了选择了直接剔除或者不在图中展示。此外，也可以选择将它们保留在图中，使用其它颜色区分即可。
df <- combination_layout(mantel2, cor_tbl = corr) ## 这里需要存储下结果
options(ggcor.link.inherit.aes = FALSE)#我们可以通过全局变量ggcor.link.inherit.aes来改变link相关的图层映射参数继承。
quickcor(corr) + geom_square() + #还有更多样式，和 corrplot 包中的风格是一致的
  geom_link(aes(colour = p.value,size = r), data = df) + # 添加连接线
  geom_start_point(fill = "red", shape = 23, size = 4, data = df) +
  geom_end_point(fill = "blue", shape = 21, size = 4, data = df) +
  geom_start_label(aes(x = x - 0.5), hjust = 1, size = 5, data = df) +
  #scale_fill_gradient2(midpoint = 0, low = 'blue', mid = 'white', high = 'red', space = 'Lab')+
  scale_fill_gradient2(midpoint = 0, low = "#56B4E9", mid = "white",high = "#E69F00", space = "Lab" )+
  #scale_color_gradient(low = 'red', high = 'blue') +
  scale_color_manual(values=c("#56B4E9", "#E69F00", "#999999"))+
  scale_size_manual(values = c(0.1, 0.5, 1))  +
  geom_end_label(aes(x = xend + 0.5), hjust = 0, size = 3.8, data = df) +
  expand_axis(x = c(-6, 14.5)) +
  remove_y_axis()

```



### **计算Alpha多样性指数**

```R
#更多参数见2018_Book_StatisticalAnalysisOfMicrobiom书第6章
library(vegan)
rm(list = ls())

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
otu <- read.delim('OTU_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- t(otu)
tree <- read.tree('otu_phylo.tre')

#不包含谱系多样性，无需指定进化树；Shannon 公式的 log 底数我们使用 2
alpha_all <- alpha(otu, base = 2)
#包含谱系多样性时，指定进化树文件；Shannon 公式的 log 底数我们使用 2
alpha_all <- alpha(otu, tree, base = 2)
#输出，例如 csv 格式
write.csv(alpha_all, 'alpha.csv', quote = FALSE)

```









### **Go条形图&气泡图**

```R
rm(list=ls())
library(xlsx)
library(ggplot2)
data <- read.xlsx('GO_bar.xlsx',header = T,sheetIndex = 1)
p <- ggplot(data,aes(x=Go_term,y=Counts,fill = Go_category))+geom_bar(stat = "identity",width = 0.8)
p
#将GO_term设定为factor即可按照顺序输出
GO_term_order=factor(as.integer(rownames(data)),labels=data$Go_term)
ggplot(data,aes(x=GO_term_order,y=Counts,fill = Go_category))+geom_bar(stat = "identity",width = 0.8)+
   coord_flip() +  xlab("GO term") + ylab("Num of Genes") + theme_bw()
#或者将坐标轴调整角度
COLS <- c("#66C3A5", "#8DA1CB", "#FD8D62")
ggplot(data=data, aes(x=GO_term_order,y=Counts, fill=Go_category)) +
  geom_bar(stat="identity", width=0.8) + 
  scale_fill_manual(values = COLS) + theme_bw()  +
  xlab("GO term") + ylab("Num of Genes") + labs(title = "The Most Enriched GO Terms")+
  theme(axis.text.x=element_text(face = "bold", color="black",angle = 70,vjust = 1, hjust = 1 )) +
  scale_y_continuous(limits = c(0,170),expand = c(0,0))+
theme(axis.text = element_text(size = 12), axis.title = element_text(size = 13), legend.title = element_blank(),legend.text = element_text(size = 11))

##我们还可以将不同分组的Go_term的名字都映射颜色
table(go_enrich_df$type) ## 使用table函数统计每一组中包含的个数，方便后面进行颜色的映射
 theme(axis.text.y = element_text(angle=0,hjust=1, vjust=1, color  = rep(c("#FD8D62","#8DA1CB", "#66C3A5"), c(12,4,20)))#根据算出的数目映射
```

![image-20200221131437960](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221131437960.png)

![image-20200221131729053](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221131729053.png)



![image-20200702164552869](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200702164552869.png)



```R
##GO气泡图
rm(list = ls())
library(GOplot)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)

data(EC)
head(EC$david)
head(EC$genelist)
circ <- circle_dat(EC$david, EC$genelist)
#直接气泡图
GOBubble(circ, labels = 4)
#labels: Sets a threshold for the displayed labels. The threshold refers to the -log(adjusted p-value) (default=5)
#略调整参数之后可以对图的布局、颜色等进行调整
GOBubble(circ, title = 'Bubble plot', colour = c('orange', 'darkred', 'gold'), display = 'multiple', labels = 3)

circ2 <- circ[!duplicated(circ$ID),-5]
head(circ2)

##ggplot画图
ggplot(circ2,aes(x=zscore,y=-log10(adj_pval)))+
  geom_point(aes(size=count,color=category),alpha=0.6)+
  scale_size(range=c(1,12))+
  scale_color_brewer(palette = "Accent")+
  theme_bw()+
  theme(
    #legend.position = c("none")
  )+
  geom_text_repel(
    data = circ2[-log10(circ2$adj_pval)>3,],
    aes(label = ID),
    size = 3,
    segment.color = "black", show.legend = FALSE )+
  facet_grid(.~category)

```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200408164259955.png" alt="image-20200408164259955" style="zoom:33%;" />

### **富集气泡图**

```R
library(ggplot2)
pathway = read.delim('KEGG_pathwa.txt',check.names = FALSE,header = T,sep = '\t')
p <- ggplot(pathway,aes(GeneRatio,Description))+geom_point(aes(size = Count,color = -1*log10(qvalue)))+
  scale_color_gradient(low = "green",high = 'red')+
  labs(color = expression(-log[10](qvalue)),size = 'Gene',
       x = 'GeneRatio',
       y = 'Go_term')+
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), strip.text = element_text(size = 12)) +
  theme(axis.text = element_text(size = 13), axis.title = element_text(size = 13), legend.key = element_blank(), legend.text = element_text(size = 11))

p        
ggsave("T01vsT07_up.pdf",p,width = 10,height = 7)



```

![image-20200221131837334](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221131837334.png)

![image-20200221131846900](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221131846900.png)

### **火山图**

```R
setwd('../huoshantuGGplot/')
library(ggplot2)

#读取数据
diff_stat <- read.delim('diff_stat.txt', sep = '\t', stringsAsFactors = FALSE)

##横轴 log2 Fold Change（log2FC），纵轴 -log10 FDR（-log10(FDR)）
#散点图轮廓
p1 <- ggplot(diff_stat, aes(x = log2FC, y = -log10(FDR))) +
  geom_point(size = 0.5) +
  labs(x = 'log2 Fold Change', y = '-log10 FDR p-value')

p1

#根据差异水平赋值颜色，例如 |log2FC| >= 1 & FDR p-value < 0.05
diff_stat[which(diff_stat$FDR < 0.05 & diff_stat$log2FC >= 1),'diff'] <- 'up'
diff_stat[which(diff_stat$FDR < 0.05 & diff_stat$log2FC <= -1),'diff'] <- 'dowm'
diff_stat[!(diff_stat$diff %in% c('up', 'dowm')),'diff'] <- 'no'

p1 <- ggplot(diff_stat, aes(x = log2FC, y = -log10(FDR))) +
  geom_point(aes(color = diff), size = 0.5) +
  scale_colour_manual(limits = c('up', 'dowm', 'no'), values = c('blue', 'red', 'gray40'), labels = c('Enriched OTUs', 'Depleted OTUs', 'No diff OTUs')) +
  labs(x = 'log2 Fold Change', y = '-log10 FDR p-value')

p1

#背景外观，调整示例
p1 <- p1 +
  theme(panel.grid.major = element_line(color = 'gray', size = 0.2), panel.background = element_rect(color = 'black', fill = 'transparent')) +
  geom_vline(xintercept = c(-1, 1), color = 'gray', linetype = 2, size = 0.5) + 
  geom_hline(yintercept = -log10(0.05), color = 'gray', linetype = 2, size = 0.5)

p1

#图例，调整示例
p1 <- p1 +
  theme(legend.title = element_blank(), legend.key = element_rect(fill = 'transparent'), legend.background = element_rect(fill = 'transparent'), legend.position = c(0.2, 0.9))

p1

#输出图片至本地
ggsave('volcano_plot1.pdf', p1, width = 3.5, height = 4.5)
ggsave('volcano_plot1.png', p1, width = 3.5, height = 4.5)

##横轴 log2 Fold Change（log2FC），纵轴 log10 baseMean（log10baseMean），示例
p2 <- ggplot(diff_stat, aes(x = log2FC, y = log10baseMean)) +
  geom_point(aes(color = diff), size = 0.5) +	#绘制散点
  scale_colour_manual(limits = c('up', 'dowm', 'no'), values = c('blue', 'red', 'gray40'), labels = c('Enriched OTUs', 'Depleted OTUs', 'No diff OTUs')) +	#赋值点的颜色，并设置图例展示标签
  labs(x = 'log2 Fold Change', y = 'log10 Average Abundance') +	#设置坐标轴标题
  theme(panel.grid.major = element_line(color = 'gray', size = 0.2), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.title = element_blank(), legend.key = element_rect(fill = 'transparent'))	#主体设置，包括背景、网格线、图例等的调整

p2

#如需翻转 x、y 轴
p2 + coord_flip() 

#带分面的火山图示例
#这里把上述数据框“diff_stat”重复上 3 次，且当作 3 个不同分组的比较结果吧，仅用于演示分面图
dat1 <- diff_stat
dat1$group <- 'group1 vs 2'
gene <- diff_stat
gene$group <- 'group2 vs 3'
dat3 <- diff_stat
dat3$group <- 'group1 vs 3'
dat <- rbind(dat1, gene, dat3)

p3 <- ggplot(dat, aes(x = log2FC, y = log10baseMean)) +
  geom_point(aes(color = diff), size = 0.5, show.legend = FALSE) +	#绘制散点（这里隐藏了图例）
  scale_colour_manual(limits = c('up', 'dowm', 'no'), values = c('blue', 'red', 'gray40'), labels = c('Enriched OTUs', 'Depleted OTUs', 'No diff OTUs')) +	#赋值点的颜色，并设置图例展示标签
  labs(x = 'log2 Fold Change', y = 'log10 Average Abundance') +	#设置坐标轴标题
  theme(panel.grid.major = element_line(color = 'gray', size = 0.2), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.title = element_blank(), legend.key = element_rect(fill = 'transparent')) +	#主体设置，包括背景、网格线、图例等的调整
  coord_flip() +	#翻转坐标轴
  facet_wrap(~group, ncol = 3, scale = 'free')	#添加分面，根据数据中 group 列的信息将图片划分为 3 列展示

p3

#输出图片至本地
ggsave('volcano_plot2.pdf', p2, width = 5, height = 4.5)
ggsave('volcano_plot2.png', p2, width = 5, height = 4.5)

##grid 多图组合示例
library(grid)

png('volcano_plot.png', width = 2300, height = 1500, res = 300, units = 'px')
grid.newpage()	#新建画板
pushViewport(viewport(layout = grid.layout(1, 2)))	#分割画板为两片区域，分别放置 p1 和 p2
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))	#放置 p1
print(p2 + guides(color = 'none'), vp = viewport(layout.pos.row = 1, layout.pos.col = 2))	#放置 p2，两张图共用一张图例即可，组合时顺便去掉 p2 的图例
dev.off()

##带双 logFC 信息的二维散点图
#读取另一个作图数据
gene <- read.csv('gene_two.csv')

#标记显著性（默认 p < 0.05）
gene[which(gene$FDR_A < 0.05 & gene$FDR_B < 0.05),'type1'] <- 'sign'
gene[which(gene$FDR_A >= 0.05 | gene$FDR_B >= 0.05),'type1'] <- 'no'

#标记差异倍数（默认 |log2FC| >= 1）
gene[which(gene$logFC_A <= -1 & gene$logFC_B <= -1),'type2'] <- 'a_down.b_down'
gene[which(gene$logFC_A >= 1 & gene$logFC_B <= -1),'type2'] <- 'a_up.b_down'
gene[which(gene$logFC_A <= -1 & gene$logFC_B >= 1),'type2'] <- 'a_down.b_up'
gene[which(gene$logFC_A >= 1 & gene$logFC_B >= 1),'type2'] <- 'a_up_b_up'
gene[is.na(gene$type2),'type2'] <- 'no'

#合并显著性和差异倍数，用于标记差异基因
gene$type3 <- paste(gene$type1, gene$type2, sep = '.')

#排序，为了使作图时显著的点绘制在前方（减少被遮盖）
gene$type3 <- factor(gene$type3, levels = c('sign.a_down.b_down', 'sign.a_up.b_down', 'sign.a_down.b_up', 'sign.a_up_b_up', 'no.a_down.b_down', 'no.a_up.b_down', 'no.a_down.b_up', 'no.a_up_b_up', 'sign.no', 'no.no'))
gene <- gene[order(gene$type3, decreasing = TRUE), ]

#ggplot2 作图
p <- ggplot(gene, aes(logFC_A, logFC_B)) +
  geom_point(aes(color = type3, size = logCPM), alpha = 0.6, show.legend = FALSE) +	#设置点颜色（按基因差异类型）、大小（按基因表达量 CPM 值），透明度，并取消图例
  scale_size(range = c(0, 4)) +	#控制点大小的范围
  scale_color_manual(limits = c('sign.a_down.b_down', 'sign.a_up.b_down', 'sign.a_down.b_up', 'sign.a_up_b_up', 'no.a_down.b_down', 'no.a_up.b_down', 'no.a_down.b_up', 'no.a_up_b_up', 'sign.no', 'no.no'), values = c('red', 'orange', 'purple', 'blue', 'gray', 'gray', 'gray', 'gray', 'gray', 'gray')) +	#根据不同类型赋值颜色
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) +	#调整背景
  geom_vline(xintercept = c(-1, 1), lty = 2) + 
  geom_hline(yintercept = c(-1, 1), lty = 2) +
  labs(x = 'log2FC (group A vs C)', y = 'log2FC (group B vs C)')

p

p <- p +	#在合适的位置添加文字标记（当然，选择 AI、PS 后续添加也很方便）
  annotate('text', label = 'A Down\nB Down', -7, -9) +
  annotate('text', label = 'A Down\nB Up', -7, 12) +
  annotate('text', label = 'A Up\nB Down', 9, -9) +
  annotate('text', label = 'A Up\nB Up', 9, 12)

p

```



![image-20200221133026519](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221133026519.png)

![image-20200221133115823](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221133115823.png)

### **Deseq2**

```R
#建立与 Bioconductor 连接
source('https://bioconductor.org/biocLite.R')
#若连接失败可更换下面命令试下
source('http://bioconductor.org/biocLite.R')
#安装 DESeq2
biocLite('DESeq2')

###############################################
library(DESeq2)
library(ggplot2)
library(grid)

##导入 OTU 丰度表和分组文件
otu_file <- read.delim('otu_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
group_file <- read.delim('group.txt', row.names = 1, sep = '\t')

##DESeq2 默认流程，可使用 ?DESeqDataSet 等查看更多内容
#第一步，构建 DESeqDataSet 对象
dds <- DESeqDataSetFromMatrix(countData = otu_file, colData = group_file, design = ~group)

#第二步，差异分析
dds <- DESeq(dds)
suppressMessages(dds)

#第三步，提取结果
res <- results(dds, contrast=c('group', 'treat', 'control'))

#简要查看结果
res
#或
summary(res)

##结果提取
deseq_res <- as.data.frame(res[order(res$padj), ])
#可选使用下命令将“baseMean”这一列转化为相对丰度数值
#deseq_res$baseMean <- deseq_res$baseMean / sum(deseq_res$baseMean)

#输出
deseq_res$otu_id <- rownames(deseq_res)
write.table(deseq_res[c(7, 1:6)], 'DESeq2.txt', row.names = FALSE, sep = '\t', quote = FALSE)

##ggplot2 差异火山图
#统计类型归类（根据 |log2FC| >= 1 & FDR p-value < 0.05 ）
for (i in 1:nrow(deseq_res)) {
	if (abs(deseq_res[i,'log2FoldChange']) >= 1) deseq_res[i,'select_change'] <- 'y' else deseq_res[i,'select_change'] <- 'n'
	if (deseq_res[i,'padj'] %in% NA | abs(deseq_res[i,'padj']) >= 0.05) deseq_res[i,'select_pvalue'] <- 'n' else deseq_res[i,'select_pvalue'] <- 'y'
	deseq_res[i,'select'] <- paste(deseq_res[i,'select_change'], deseq_res[i,'select_pvalue'], sep = '')
}
deseq_res$select <- factor(deseq_res$select, levels = c('nn', 'ny', 'yn', 'yy'), labels = c('p >= 0.05, FC < 2', 'p < 0.05, FC < 2', 'p >= 0.05, FC >= 2', 'p < 0.05, FC >= 2'))

#纵轴为显著性 p 值
volcano_plot_pvalue <- ggplot(deseq_res, aes(log2FoldChange, -log(padj, 10))) +
geom_point(aes(color = select), alpha = 0.6, show.legend = FALSE) +
scale_color_manual(values = c('gray30', 'green4', 'red2', 'blue2')) +
theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) +
geom_vline(xintercept = c(-1, 1), color = 'gray', size = 0.5) + 
geom_hline(yintercept = -log(0.05, 10), color = 'gray', size = 0.5) +
labs(x = 'log2 Fold Change', y = '-log10 p-value')

#纵轴为 OTU 丰度
volcano_plot_abundance <- ggplot(deseq_res, aes(log2FoldChange, 100 * baseMean / sum(deseq_res$baseMean))) +
geom_point(aes(color = select), alpha = 0.6) +
scale_color_manual(values = c('gray30', 'green4', 'red2', 'blue2')) +
theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) +
theme(legend.title = element_blank(), legend.key = element_rect(fill = 'transparent'), legend.background = element_rect(fill = 'transparent')) +
geom_vline(xintercept = c(-1, 1), color = 'gray', size = 0.5) + 
labs(x = 'log2 Fold Change', y = 'Abundance (%)')

#输出单图
#ggsave('volcano_plot_pvalue.pdf', volcano_plot_pvalue, width = 5, height = 5)
#ggsave('volcano_plot_abundance.png', volcano_plot_abundance, width = 5, height = 5)

#组合并输出
png('volcano_plot.png', width = 3000, height = 1600, res = 300, units = 'px')
	grid.newpage()
	pushViewport(viewport(layout = grid.layout(1, 2)))
	print(volcano_plot_pvalue, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
	print(volcano_plot_abundance, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

```

### **镜面柱形图**

```R
library(ggplot2)
library(reshape2)
library(doBy)

#读入数据，合并数据
plant30 <- read.csv('30days.csv')
plant30$days <- rep('30', nrow(plant30))

plant45 <- read.csv('45days.csv')
plant45$days <- rep('45', nrow(plant45))

plant60 <- read.csv('60days.csv')
plant60$days <- rep('60', nrow(plant60))

plant75 <- read.csv('75days.csv')
plant75$days <- rep('75', nrow(plant75))

plant90 <- read.csv('90days.csv')
plant90$days <- rep('90', nrow(plant90))

plant_all <- rbind(plant30, plant45, plant60, plant75, plant90)

#统计均值、标准差
plant_all <- melt(plant_all, id = c('plant', 'days'))
plant_all_stat <- summaryBy(value~variable+plant+days, plant_all, FUN = c(mean, sd))

#作图
plant_all_stat[which(plant_all_stat$variable == 'root_length'), c('value.mean', 'value.sd')] <- plant_all_stat[which(plant_all_stat$variable == 'root_length'), c('value.mean', 'value.sd')] * -1

ggplot(plant_all_stat, aes(days, value.mean, fill = plant)) +
  geom_col(position = position_dodge(width = 0.5), width = 0.5, size = 0.3, colour = 'black') +	#柱形图绘制
  geom_errorbar(aes(ymin = value.mean - value.sd, ymax = value.mean + value.sd), width = 0.3, size = 0.3, position = position_dodge(0.5)) +	#误差线绘制
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.title = element_blank()) + 	#调整背景
  labs(x = 'Time (days)', y = 'Length (cm)') +	#设置坐标轴标签
  geom_hline(yintercept = 0, size = 0.3) +	#在 y = 0 处添加横线
  scale_y_continuous(breaks = seq(-45, 45, 15), labels = as.character(abs(seq(-45, 45, 15))), limits = c(-45, 45)) +	#调整纵轴刻度
  annotate('text',label = 'Stem', 1, 43) +	#这两句添加“Stem”和“Root”标签
  annotate('text',label = 'Root', 1, -43)

ggsave('plant.pdf', width = 6, height = 4)
ggsave('plant.png', width = 6, height = 4)

##############################
#在统计均值、标准差之前，也可先将根长数据取负值
plant_all <- rbind(plant30, plant45, plant60, plant75, plant90)
plant_all$root_length <- plant_all$root_length * -1

#统计均值、标准差
plant_all <- melt(plant_all, id = c('plant', 'days'))
plant_all_stat <- summaryBy(value~variable+plant+days, plant_all, FUN = c(mean, sd))

#作图
ggplot(plant_all_stat, aes(days, value.mean, fill = plant)) +
  geom_col(position = position_dodge(width = 0.5), width = 0.5, size = 0.3, colour = 'black') +	#柱形图绘制
  geom_errorbar(aes(ymin = value.mean - value.sd, ymax = value.mean + value.sd), width = 0.3, size = 0.3, position = position_dodge(0.5)) +	#误差线绘制
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.title = element_blank()) + 	#调整背景
  labs(x = 'Time (days)', y = 'Length (cm)') +	#设置坐标轴标签
  geom_hline(yintercept = 0, size = 0.3) +	#在 y = 0 处添加横线
  scale_y_continuous(breaks = seq(-45, 45, 15), labels = as.character(abs(seq(-45, 45, 15))), limits = c(-45, 45)) +	#调整纵轴刻度
  annotate('text',label = 'Stem', 1, 43) +	#这两句添加“Stem”和“Root”标签
  annotate('text',label = 'Root', 1, -43)

ggsave('plant.pdf', width = 6, height = 4)
ggsave('plant.png', width = 6, height = 4)
```

![image-20200221134145387](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221134145387.png)

![image-20200221134355528](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221134355528.png)

### 丰度饼图

[patternplot包：用ggplot解决你对线性填充，不！所有填充的全部幻想](https://mp.weixin.qq.com/s/s7umGxtvNhJljhTPXVafVQ)

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200610091212834.png" alt="image-20200610091212834" style="zoom:50%;" />

```R
############################################
#读取数据
phylum <- read.delim('phylum.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

##pie()
#pdf('pie_plot.pdf')
png('pie_plot.png', width = 2000, height = 2000, res = 300, units = 'px')
par(mar = c(5, 0, 5, 0))
pie(phylum$a1, col = c('blue', 'orange', 'red', 'green2', 'purple', 'cyan'), labels = phylum$phylum, font = 3, main = 'Sample: a1\nPhylum level')
dev.off()


##pie3D()
library(plotrix)

#pdf('pie3D_plot.pdf', width = 6.5, height = 6)
png('pie3D_plot.png', width = 2100, height = 2000, res = 300, units = 'px')
pie3D(phylum$a1, col = c('blue', 'orange', 'red', 'green2', 'purple', 'cyan'), explode = 0.05, height = 0.1, radius = 0.85, labels = phylum$phylum, labelcex = 1, main = 'Sample: a1\nPhylum level')
dev.off()


##fan.plot()
library(plotrix)

#pdf('fan_plot.pdf')
png('fan_plot.png', width = 2000, height = 2000, res = 300, units = 'px')
fan.plot(phylum$a1, col = c('blue', 'orange', 'red', 'green2', 'purple', 'cyan'), labels = phylum$phylum, main = 'Sample: a1\nPhylum level')
dev.off()


##ggplot2
library(ggplot2)

p <- ggplot(phylum, aes(x = '', y = a1, fill = phylum)) + 
geom_bar(stat = 'identity', width = 1) +
coord_polar(theta = 'y') +
scale_fill_manual(values = rev(c('blue', 'orange', 'red', 'green2', 'purple', 'cyan'))) +
theme(panel.grid = element_blank(), panel.background = element_blank(), axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
theme(legend.text = element_text(face = 'italic'), legend.title = element_blank()) +
labs(x = '', y = '', title = 'Sample: a1', fill = 'Phylum')

#ggsave('ggplot2_plot.pdf', p, width = 5, height = 5)
ggsave('ggplot2_plot.png', p, width = 5, height = 5)


############################################
#一个组合样式的 ggplot2 饼图示例
library(doBy)
library(ggplot2)
library(grid)

##预处理
#读取数据
abundance <- read.delim('abundance.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

#按类群丰度排序
abundance$abundance_sum <- apply(abundance[1:3], 1, sum)
stat <- summaryBy(abundance_sum~phylum, abundance, FUN = sum)
abundance$phylum <- factor(abundance$phylum, levels = c({stat[order(stat$abundance_sum.sum), ]}$phylum))
abundance <- abundance[order(abundance$phylum, abundance$abundance_sum, decreasing = TRUE), ]

#挑选 top10 属
top10_genus <- as.vector(abundance[order(abundance$abundance_sum, decreasing = TRUE), ]$genus[1:10])

##ggplot2 饼图
abundance$phylum <- factor(abundance$phylum, levels = rev(levels(abundance$phylum)))
abundance$genus <- factor(abundance$genus, levels = as.vector(abundance$genus))

#定义颜色
#length(unique(abundance$phylum))
#length(unique(abundance$genus))
color_phylum <- c('blue', 'orange', 'red', 'yellow2', 'green2', 'purple', 'cyan')
color_genus <- sample(colors(1), 50, replace = FALSE)	#随机颜色

#定义函数，#在 ggplot2 中提取图例，参考自
#https://stackoverflow.com/questions/13712574/how-to-change-position-of-grid-draw
g_legend <- function(gg_plot) {
    tmp <- ggplot_gtable(ggplot_build(gg_plot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == 'guide-box')
    leg <- tmp$grobs[[leg]]
    leg
}

#内饼（属）
genus <- ggplot(abundance, aes(x = '', y = a1, fill = genus)) + 
geom_bar(stat = 'identity', width = 1) +
coord_polar(theta = 'y') +
scale_fill_manual(values = color_genus) +
theme(panel.grid = element_blank(), panel.background = element_blank(), axis.text.x = element_blank(), plot.background = element_blank(), legend.text = element_text(face = 'italic')) +
labs(x = '', y = '', title = '', fill = 'top10 genus')

genus_pie <- genus + theme(legend.position = 'none')
genus_legend <- g_legend(genus + scale_fill_manual(values = color_genus[which(abundance$genus %in% top10_genus)], limits = top10_genus))

#外环（门）
phylum <- ggplot(abundance, aes(x = '', y = a1, fill = phylum)) + 
geom_bar(stat = 'identity', width = 0.1) +
coord_polar(theta = 'y') +
scale_fill_manual(values = color_phylum) +
theme(panel.grid = element_blank(), panel.background = element_blank(), axis.text.x = element_blank(), plot.background = element_blank(), legend.text = element_text(face = 'italic')) +
labs(x = '', y = '', title = '')

phylum_pie <- phylum + theme(legend.position = 'none')
phylum_legend <- g_legend(phylum)

##grid 组合
#pdf('grid_plot.pdf', width = 7, height = 5)
png('grid_plot.png', width = 2500, height = 1500, res = 300, units = 'px')
grid.newpage()

print(phylum_pie, vp = viewport(x = 0.3, y = 0.5, width = 1.6, height = 1.6))
print(genus_pie, vp = viewport(x = 0.3, y = 0.5, width = 0.7, height = 0.7))

phylum_legend$vp$x <- unit(0.7, 'npc')
phylum_legend$vp$y <- unit(0.57, 'npc')
grid.draw(phylum_legend)

genus_legend$vp$x <- unit(0.9, 'npc')
genus_legend$vp$y <- unit(0.5, 'npc')
grid.draw(genus_legend)

dev.off()

```

![image-20200301165922925](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200301165922925.png)

### **Venn图**

#### **先制作分组文件  输入OTU表类型文件**

![image-20200409092800692](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200409092800692.png)

```R
# make_veen_flower.R
#!/usr/bin/env Rscript

#用法参考
#Rscript make_venn_flower.r otu_table.txt group1.txt venn.txt
#Rscript make_venn_flower.r otu_table.txt group2.txt flower.txt
#flower.txt 为单样品的分组   
#venn.txt 为每组的样品
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

```

#### **制作Venn图**

```R
####韦恩图（VennDiagram 包，适用样本数 2-5）
library(VennDiagram)

#读入作图文件，预处理
venn_dat <- read.delim('venn.txt', header = T, sep = '\t', stringsAsFactors = F, check.names = F)
venn_list <- list(venn_dat[,1], venn_dat[,2])
names(venn_list) <- colnames(venn_dat)

#作图
venn.diagram(venn_list, filename = 'venn.png', fill = c('red', 'blue'), alpha = 0.50, col = 'black', cex = 1, fontfamily = 'serif', cat.col = c('black', 'black'), cat.cex = 1, cat.fontfamily = 'serif', margin = 0.2)

####韦恩图（venn 包，适用样本数 2-7）
library(venn)

#读入作图文件，预处理
venn_dat <- read.delim('flower.txt', header = T, sep = '\t', stringsAsFactors = F, check.names = F)[1:7]
venn_list <- list(venn_dat[,1], venn_dat[,2], venn_dat[,3], venn_dat[,4], venn_dat[,5], venn_dat[,6], venn_dat[,7])
names(venn_list) <- colnames(venn_dat)

#作图
png('venn_7.png', width = 1500, height = 1500, res = 200, units = 'px')
venn(venn_list,zcolor='style')
dev.off()

####韦恩图（UpSetR 包，不受样本数限制）
library(UpSetR)

#读入作图文件，预处理
otu <- read.delim('otu_table.txt', header = T, row.names = 1, sep = '\t', stringsAsFactors = F, check.names = F)[1:10]
otu[otu > 0] <- 1

#作图
png('venn_10.png', width = 1500, height = 1500, res = 200, units = 'px')
upset(otu, nsets = 12, order.by = "freq",mainbar.y.label = "IntersectionSize",sets.x.label = "",point.size = 2,line.size = 0.8)
dev.off()

####花瓣图
library(plotrix)

#读入做图文件，预处理
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

#构建作图函数（参考自 https://www.cnblogs.com/xudongliang/p/7884667.html）
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

#作图
png('flower.png', width = 1500, height = 1500, res = 200, units = 'px')
flower_plot(sample = sample_id, otu_num = otu_num, core_otu = core_num, 
	start = 90, a = 0.5, b = 2, r = 1, ellipse_col = ellipse_col, circle_col = 'white')
dev.off()
#注：参数a和b用于设置花瓣椭圆的尺寸，ellipse_col用于设置花瓣椭圆的颜色；参数r用于设置中心圆圈尺寸，circle_col用于设置中心圆圈的颜色
```

![image-20200221135549703](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221135549703.png)

```R
#Upset也可以自自己
library(UpSetR)
require(ggplot2);
require(plyr);
require(gridExtra); 
require(grid);
input <- c(
  'cancer1'=  1578,
  'cancer2' =  1284,
  'cancer3' = 2488,
  'cancer1&cancer2'  =205,
  'cancer1&cancer3'  = 828,
  'cancer2&cancer3'  =589,
  'cancer1&cancer2&cancer3'   =120
)

data <- fromExpression(input)
p1 <- upset(data, nsets = 9, 
            sets = c('cancer1',
                     'cancer2' ,
                     'cancer3'),
            keep.order = TRUE,
            # number.angles = 30, 
            point.size = 5, 
            line.size = 1.3, 
            mainbar.y.label = "IntersectionSize", 
            sets.x.label = "",
            mb.ratio = c(0.60, 0.40),
            text.scale = c(4, 4, 0.5, 0.5,3, 4))

p1

#例如我们关注所有分组的交集、c1 分组的特有集，以及 Proteobacteria 在各交集中所含 OTUs 的种类数量
#默认最多展示 5 组数据（nset = 5）的 40 种交集（nintersects = 40）
upset(phylum1, nset = 8, nintersects = 100)
#也可自定义指定数据集名称
upset(phylum1, sets = c('B1','C1'), nintersects = 100)
upset(phylum1, nset = 6, nintersects = 100, order.by = c('freq', 'degree'), decreasing = c(TRUE, TRUE),
      queries = list(list(query = intersects, params = 'H2', color = 'red'),
                     list(query = intersects, params = c('H1', 'H2', 'H3', 'H4', 'H5', 'H6'), color = 'blue')
                     ))#list(query = elements, params = c('taxonomy', 'Proteobacteria'), color = 'orange', active = TRUE) 增加一列分类

```

![image-20200409100007091](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200409100007091.png)



### **排序类型图**

[排序分析简介](https://mp.weixin.qq.com/s?__biz=MzAwOTg5ODYyNQ==&mid=2247484064&idx=1&sn=1ef21a14e077128c6b9d373a905ee99e&chksm=9b59dd4eac2e5458871af146e9b5fe7c85947c0cf3ebb75880b18a897abf3dedcd1ab38141a3&mpshare=1&scene=1&srcid=&sharer_sharetime=1591547318282&sharer_shareid=59b64c339c4e339bb243a53b299dcf8d&key=983943efdc6388ddfc3c7ab4677aa5f8aca09b18bbe6c3eb13769eb1d5dd9d7fe3df7379cbb2d48564056b6e0cbb71bff8ff9b41309e35e5608bdcb97519dbb6f57a3781cd64c2fc4d4b0e064064ee7c&ascene=1&uin=Mjc2NjUwNjgzOA%3D%3D&devicetype=Windows+10+x64&version=62090070&lang=zh_CN&exportkey=Awbo2jQq6WoFdR7ln9U%2Bbu8%3D&pass_ticket=OgFI03cbOodZF9BCd6N4rzrUDZTAvGjmnuNy2Ome2pNqRzYbuy8inleYEzRU7qCf)

[PCA、PCoA和NMDS有什么区别](https://www.omicshare.com/forum/thread-6351-1-1.html)

- **在非限制性排序中，16S和宏基因组数据分析通常用到的是PCA分析和PCoA分析。两者的区别在于：****PCA分析是基于原始的物种组成矩阵所做的排序分析，而PCoA分析则是基于由物种组成计算得到的距离矩阵得出的。
- 如果大于4.0，就应该选单峰模型；
- 如果3.0-4.0之间，选线性模型或者单峰模型均可；
- 如果小于3.0, 线性模型的结果要好于单峰模型
- 查看结果中的“Axis lengths”的第一轴DCA1的值，根据该值判断该采用线性模型还是单峰模型：

```R
#判断选择用单峰模型还是线性模型 decorana()
install.packages('vegan')
library('vegan')
data(varespec)
vare_dca<-decorana(varespec)
vare_dca
```

**输入文件一般都为分组文件及丰度表**

![image-20200608104456601](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200608104456601.png)

#### **PLS-DA分析**

```R
library(mixOmics)
library(ggplot2)

##读入文件
#门水平丰度表
phylum <- read.delim('phylum_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
phylum <- data.frame(t(phylum))

#样本分组文件
group <- read.delim('group.txt', sep = '\t', stringsAsFactors = FALSE)

##PLS-DA 分析
#基于门水平丰度表，只展示前 3 个排序轴
phylum <- phylum[group$names, ]
plsda_result <-plsda(phylum, group$group, ncomp = 3)

#简要查看结果
plsda_result
#或
names(plsda_result)

#查看排序轴解释量
plsda_result$explained_variance$X
#查看样本排序坐标
plsda_result$variates$X
#查看细菌门类群排序坐标
plsda_result$loadings$X

#使用 plotIndiv() 绘制 PLS-DA 分析结果
plotIndiv(plsda_result, ind.names = TRUE, style = 'ggplot2')
plotIndiv(plsda_result, ind.names = TRUE, style = '3d')

#提取坐标轴解释量（前两轴）
plsda_result_eig <- {plsda_result$explained_variance$X}[1:2]

#提取样本点坐标（前两轴）
sample_site <- data.frame(plsda_result$variates)[1:2]

#为样本点坐标添加分组信息
sample_site$names <- rownames(sample_site)
names(sample_site)[1:2] <- c('plsda1', 'plsda2')
sample_site <- merge(sample_site, group, by = 'names', all.x = TRUE)

#可选输出各样本的 PLS-DA 分析结果
write.table(sample_site, 'plsda_sample.txt', row.names = FALSE, sep = '\t', quote = FALSE)

#使用 ggplot2 简单绘制 PLS-DA 结果图
plsda_plot <- ggplot(sample_site, aes(plsda1, plsda2, color = group, label = names)) +
geom_point(size = 1.5, alpha = 0.6) + 
stat_ellipse(show.legend = FALSE) +	#添加 95% 置信椭圆
scale_color_manual(values = c('#1D7ACC', '#F67433', '#00815F')) +
theme(panel.grid = element_line(color = 'grey50'), panel.background = element_rect(color = 'black', fill = 'transparent')) + 
theme(legend.title = element_blank(), legend.key = element_rect(fill = 'transparent')) +
labs(x = paste('PLS-DA axis1 ( explained variance ', round(100 * plsda_result_eig[1], 2), '% )', sep = ''), y = paste('PLS-DA axis2 ( explained variance ', round(100 * plsda_result_eig[2], 2), '% )', sep = ''))

#ggsave('plsda_plot.pdf', plsda_plot, width = 6, height = 5)
ggsave('plsda_plot.png', plsda_plot, width = 6, height = 5)

```

![image-20200221135403039](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221135403039.png)

#### **NMDS分析**

```R
library(vegan)	#排序分析
#使用OTU丰度表作为输入文件还是现有的距离矩阵作为输入文件，NMDS排序的结果是不一致的。
#使用OTU丰度表作为输入文件的NMDS排序分析能够更充分地进行信息挖掘
rm(list = ls())
library(GUniFrac)
setwd("./Abundance/Nr_abundance/Distance/")
library(ggplot2)	#作图
library(ape)	#可选，用于读取进化树文件，见最后
##############################
##NMDS 排序（基于 OTU 丰度表）
#读入 OTU 丰度表
otu <- read.delim('Genu_0.001.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))
distance <- vegdist(otu, method = 'bray')
distance <- as.matrix(distance)
write.table(distance, 'bray-curtis_distance.txt', col.names = NA, sep = '\t', quote = FALSE)
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

#可选择将结果输出并保存在本地，例如将样本坐标输出为 csv 格式
write.csv(nmds1.point, 'nmds.sample.csv')

#简要绘图展示
nmds_plot <- nmds1
nmds_plot$species <- {nmds_plot$species}[1:10, ]
plot(nmds_plot, type = 't', main = paste('Stress =', round(nmds1$stress, 4)))

##############################
##NMDS 排序（基于现有的距离矩阵）
#读入现有的距离矩阵
dis <- read.delim('bray-curtis_distance.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
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
##NMDS 评估  可以通过比较NMDS排序图内对象的距离与原始对象距离去评估NMDS结果（Shepard图）。若R2越大，则NMDS结果越合理。
stressplot(nmds_plot, main = 'Shepard 图')
gof <- goodness(nmds_plot)
plot(nmds_plot,type = 't', main = '拟合度')
points(nmds_plot, display = 'sites', cex = gof * 200, col = 'red')

##############################
##ggplot2 作图（使用基于 OTU 丰度表的 NMDS 排序结果，预设 2 个排序轴）
#读入 OTU 丰度表
otu <- read.delim('Genu_0.001.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))

#读入样本分组文件
group <- read.delim('sample.txt', sep = '\t', stringsAsFactors = FALSE)

#排序，预设 2 个排序轴
nmds1 <- metaMDS(otu, distance = 'bray', k = 2)

#提取样本点坐标（前两轴）
sample_site <- nmds1.point[1:2]
sample_site$names <- rownames(sample_site)
names(sample_site)[1:2] <- c('NMDS1', 'NMDS2')


#为样本点坐标添加分组信息
sample_site <- merge(sample_site, group, by = 'names', all.x = TRUE)

#使用 ggplot2 绘制 NMDS 排序图
nmds_plot <- ggplot(sample_site, aes(NMDS1, NMDS2, group = group,color=group)) +
  geom_point(aes(color = group), size = 1.5, alpha = 0.8) + #可在这里修改点的透明度、大小
  scale_shape_manual(values = c(17, 16)) + #可在这里修改点的形状
  stat_ellipse(level = 0.95, show.legend = F) +
  scale_color_manual(values = c('red', 'blue')) + #可在这里修改点的颜色
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) + #去掉背景框
  theme(legend.key = element_rect(fill = 'transparent'), legend.title = element_blank()) + #去掉图例标题及标签背景
  labs(x = 'NMDS axis1', y = 'NMDS axis2', title = paste('Stress =', round(nmds1$stress, 4))) +
  theme(plot.title = element_text(hjust = 0.5))+ #标题居中
  annotate('text', label = 'Diarrhea', x = -0.3, y = 0.06, size = 5, colour = 'red') +
  annotate('text', label = 'Healthy', x = 0.25, y = 0.25, size = 5, colour = 'blue') 
  #geom_text(aes(label = group), data = species_site, color = 'green4', size = 2) #添加物种排序（top10 OTU，展示为标签）
nmds_plot
#ggsave('Genu_nmds.pdf', nmds_plot, width = 6, height = 5)
ggsave('NMDS.png', nmds_plot, width = 6, height = 5)

##############################
##基于 Weighted UniFrac 距离的 NMDS 排序（测试）

#加载函数


#OTU 丰度表
otu <- read.delim('test_otu.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))

#进化树（使用 ape 包中的 read.tree() 读取，此处进化树必须为有根树）
otu_tree <- read.tree('test_tree.tre')

#距离矩阵（weighted unifrac 距离矩阵，使用 GUniFrac 包中的命令计算）
unifrac <- GUniFrac(otu, otu_tree)$unifracs
wei_unif_dis <- as.dist(unifrac[, , 'd_1']) #加权
unwei_unif_dis <- as.dist(unifrac[, , 'd_UW']) #非加权
#以非加权为例矩阵形式保存
unwei_unif_dis <- as.matrix(unwei_unif_dis)
write.table(unwei_unif_dis,'unweighted-unifrac_distance.txt',col.names = NA , sep = '\t',quote=FALSE)

#分组文件
group <- read.delim('test_group.txt', sep = '\t', stringsAsFactors = FALSE)

#NMDS 排序（基于 OTU 丰度表和进化树文件，使用 weighted unifrac 距离）
nmds_un1 <- metaMDS2(wei_unif_dis)
plot(nmds_un1, type = 't', main = paste('Stress =', round(nmds2$stress, 4)))
#NMDS 排序（直接基于 unweighted unifrac 距离矩阵）
nmds_un2 <- metaMDS(unwei_unif_dis)

#分别作图展示
sample_site2<- data.frame(nmds_un1$point); sample_site2 <- data.frame(nmds_un2$point)
sample_site1$names <- rownames(sample_site1); sample_site2$names <- rownames(sample_site2)
names(sample_site1)[1:2] <- c('NMDS1', 'NMDS2'); names(sample_site2)[1:2] <- c('NMDS1', 'NMDS2')
sample_site1 <- merge(sample_site1, group, by = 'names', all.x = TRUE); sample_site2 <- merge(sample_site2, group, by = 'names', all.x = TRUE)

ggplot(sample_site1, aes(NMDS1, NMDS2, color = group)) + geom_point()
ggplot(sample_site2, aes(NMDS1, NMDS2, color = group)) + geom_point()

```

![image-20200221135959529](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221135959529.png)

**nmds实战例子（基于OTU表）**

```R
#载入分析包
library(vegan)
#载入分析数据
otu <- read.table("otu.txt",header=T,row.names=1,sep="\t")
#对分析数据进行转置
otu <- t(otu)
#进行NMDS分析
nmds <- metaMDS(otu)
#保存stress结果
capture.output(nmds,file = "Stress.txt" )
#保存scores结果
nmds_scores=scores(nmds, choices = c(1,2))
write.table(nmds_scores,file="NMDS_scores.txt")

scores <-  read.table("NMDS_scores.txt",header=T)
groups <- read.table("group1.txt", head=F,sep = "\t",colClasses = c("character"))
pich=c(21:24)
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442")
Palette <- c("#000000","#000000","#000000","#000000")
plotdata <- data.frame(rownames(scores),scores$NMDS1,scores$NMDS2,groups$V2)
colnames(plotdata)=c("sample","MDS1","MDS2","group")
plotdata$sample <- factor(plotdata$sample)
plotdata$MDS1 <- as.numeric(as.vector(plotdata$MDS1))
plotdata$MDS2 <- as.numeric(as.vector(plotdata$MDS2))

library(ggplot2)
ggplot(plotdata, aes(MDS1, MDS2)) +
  geom_point(aes(colour=group,shape=group,fill=group),size=10)+
  scale_shape_manual(values=pich)+
  scale_colour_manual(values=Palette)+
  scale_fill_manual(values=cbbPalette)+
  labs(title="NMDS Plot") + xlab(paste("MDS1")) + ylab(paste("MDS2"))+
  theme(text=element_text(size=18))+
  geom_vline(aes(xintercept = 0),linetype="dotted")+
  geom_hline(aes(yintercept = 0),linetype="dotted")+
  geom_text(aes(x=max(MDS1),y=max(MDS2)),hjust=1,vjust=0,size=8,label=paste("Stress = ",round(nmds$stress,3),sep = ""),colour="black")+
  theme(panel.background = element_rect(fill='white', colour='black'), panel.grid=element_blank(), 
        axis.title = element_text(color='black',size=18),axis.ticks.length = unit(0.4,"lines"), 
        axis.ticks = element_line(color='black'),axis.line = element_line(colour = "black"), 
        axis.title.x=element_text(colour='black', size=18),axis.title.y=element_text(colour='black', size=18),axis.text=element_text(colour='black',size=18),
        legend.title=element_blank(),legend.text=element_text(family="Arial", size=18),legend.key=element_blank())+
  theme(plot.title = element_text(size=20,colour = "black",face = "bold"))

```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200608104829353.png" alt="image-20200608104829353" style="zoom:50%;" />

#### aPCOA分析

```R
#用法
aPCoA(formula,data,maincov,drawEllipse=TRUE,drawCenter=TRUE)

#formula：Y~ A,Y为不像似性距离。
#data: 需要包含所有的协变量
#maincov： 研究的目标协变量，必须是因子
#drawEllipse: 95%置信椭圆
#drawCenter：中心点和其他点连线

#实例
library(mvabund)
library(vegan)
#install.packages("aPCoA")
library(aPCoA)
data("Tasmania")
data<-data.frame(treatment=Tasmania$treatment,block=Tasmania$block)
data

       treatment block
B1D1   Disturbed     1
B1D2   Disturbed     1

bray<-vegdist(Tasmania$abund, method="bray")
rownames(data)<-rownames(as.matrix(bray))
opar<-par(mfrow=c(1,2),
          mar=c(3.1, 3.1, 3.1, 5.1),
          mgp=c(2, 0.5, 0),
          oma=c(0, 0, 0, 4))
#treatment为研究的目标协变量，排除其他协变量的影响
result<-aPCoA(bray~block,data,treatment)
par(opar)
```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200523123315808.png" alt="image-20200523123315808" style="zoom:50%;" />



#### **PCOA分析**

[PCOA示例](https://mp.weixin.qq.com/s/aCyx_r81nRd6W2PHVvuSyw)

```R
library(vegan)	#排序分析
#无论使用OTU丰度表作为输入文件还是现有的距离矩阵作为输入文件，PCoA排序的结果是完全一致的。
rm(list = ls())

library(ggplot2)	#作图
library(ape)
library(plyr)
#OTU 丰度表
otu <- read.delim('Genu_0.001.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))
#或者现有的距离矩阵
dis <- read.delim('bray-curtis_distance.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
#样本分组文件
group <- read.delim('sample.txt', sep = '\t', stringsAsFactors = FALSE)

#物种数据 Hellinger 预转化（处理包含很多 0 值的群落物种数据时，推荐使用）
otu<- decostand(otu, method = 'hellinger')

#排序（基于 OTU 丰度表）
distance <- vegdist(otu, method = 'bray')
bray <- as.matrix(bray_dis)
write.table(bray, 'bray-curtis_distance.txt', col.names = NA, sep = '\t', quote = FALSE)
pcoa <- cmdscale(distance, k = (nrow(otu) - 1), eig = TRUE)
#或者（基于现有的距离矩阵）
pcoa <- cmdscale(as.dist(dis), k = (nrow(dis) - 1), eig = TRUE)

#对于上述计算得到的样本间距离 distance，可转换为矩阵格式后输出，例如输出为 csv 格式
write.csv(as.matrix(distance), 'distance.csv', quote = F)

#使用vegan内置命令ordiplot()简要做图展示
ordiplot(scores(pcoa)[ ,c(1, 2)], type = 't')
#或者查看排序简要
summary(pcoa)
#查看主要排序轴的特征值和各样本在各排序轴中的坐标值
pcoa$eig
point <- data.frame(pcoa$point)

#可将样本坐标转化为数据框后导出，例如导出为 csv 格式
write.csv(point, 'pcoa.sample.csv')
#可使用 wascores() 计算物种坐标
species <- wascores(pcoa$points[,1:2], otu)

#可将物种坐标转化为数据框后导出，例如导出为 csv 格式
write.csv(species, 'pcoa.otu.csv')

#使用ggplot2画图
#坐标轴解释量（前两轴）
pcoa_eig <- (pcoa$eig)[1:2] / sum(pcoa$eig)

#提取样本点坐标（前两轴）
sample_site <- data.frame({pcoa$point})[1:2]
sample_site$names <- rownames(sample_site)
names(sample_site)[1:2] <- c('PCoA1', 'PCoA2')

#为样本点坐标添加分组信息
sample_site <- merge(sample_site, group, by = 'names', all.x = TRUE)

#可选输出，例如输出为 csv 格式
write.csv(sample_site, 'sample_site.csv', quote = F)

sample_site$group <- factor(sample_site$group, levels = c('D', 'H'))
##ddply() 计算同分组样本排序坐标顶点
group_border <- ddply(sample_site, 'group', function(df) df[chull(df[[2]], df[[3]]), ])##df[[2]]、df[[3]]代表第二、三列，即PCoA1和PCoA2坐标列
#绘图
pcoa_plot <- ggplot(sample_site, aes(PCoA1, PCoA2, group = group,color=group)) +
  theme(panel.grid = element_line(color = 'gray', linetype = 2, size = 0.1), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.key = element_rect(fill = 'transparent')) + #去掉背景框
  geom_vline(xintercept = 0, color = 'gray', size = 0.4) + 
  #geom_polygon(data = group_border, aes(fill = group)) + #绘制多边形区域
  stat_ellipse(level = 0.95, show.legend = F) + #绘制置信椭圆
  geom_hline(yintercept = 0, color = 'gray', size = 0.4)+
  geom_point(aes(color = group), size = 2, alpha = 0.8)+#可在这里修改点的透明度、大小
 # scale_shape_manual(values = c(17, 16))+#可在这里修改点的形状
  scale_color_manual(values = c('red', 'blue')) + #可在这里修改点的颜色
  #theme(legend.position = 'none') + # 是否展示图例
  scale_fill_manual(values = c('#C673FF2E', '#73D5FF2E')) + #可在这里修改区块的颜色
  guides(fill = guide_legend(order = 1), shape = guide_legend(order = 2), color = guide_legend(order = 3)) + #设置图例展示顺序
  labs(x = paste('PCoA axis1: ', round(100 * pcoa_eig[1], 2), '%'), y = paste('PCoA axis2: ', round(100 * pcoa_eig[2], 2), '%')) +
annotate('text', label = 'Diarrhea', x = -0.19, y = 0.06, size = 5, colour = 'red') +
  annotate('text', label = 'Healthy', x = 0.1, y = 0.20, size = 5, colour = 'blue') 

pcoa_plot
ggsave('genu_pcoa.pdf', pcoa_plot, width = 6, height = 5)
```

![image-20200221140327412](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221140327412.png)

**pcoa实战（基于OTU表）**

```R
setwd('./Script/Red_queen/PCOA/')
#载入绘图所需的包
library(vegan)
library(ape)
library(ggplot2)
library(ggrepel)

#分组数较少、组内生物学重复较多
data <- read.csv("otu.txt", head=TRUE,sep="\t",row.names = 1)
groups <- read.table("group1.txt",sep = "\t",header = F,colClasses = c("character"))
groups <- as.list(groups)
data <- t(data)
data[is.na(data)] <- 0
data <- vegdist(data,method = "bray")
pcoa<- pcoa(data, correction = "none", rn = NULL)
PC1 = pcoa$vectors[,1]
PC2 = pcoa$vectors[,2]
plotdata <- data.frame(rownames(pcoa$vectors),PC1,PC2,groups$V2)
colnames(plotdata) <-c("sample","PC1","PC2","group")
pich=c(21:24)
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442")
Palette <- c("#000000", "#000000", "#000000", "#000000")
pc1 <-floor(pcoa$values$Relative_eig[1]*100)
pc2 <-floor(pcoa$values$Relative_eig[2]*100)

otu.adonis=adonis(data~V2,data = groups,distance = "bray")

ggplot(plotdata, aes(PC1, PC2)) +
  geom_point(aes(colour=group,shape=group,fill=group),size=12)+
  geom_text(aes(x = 0.05,y = 0.35,label = paste("PERMANOVA:\n    Group_A VS Group_B\n    p-value = ",otu.adonis$aov.tab$`Pr(>F)`[1],sep = "")),size = 10,hjust = 0)+
  stat_ellipse(aes(fill = group),geom = "polygon",level = 0.95,alpha = 0.3)+
  scale_shape_manual(values=pich)+
  scale_colour_manual(values=Palette)+
  scale_fill_manual(values=cbbPalette)+
  labs(title="PCoA - The composition of gut microbiome") + 
  xlab(paste("PC1 ( ",pc1,"%"," )",sep="")) + 
  ylab(paste("PC2 ( ",pc2,"%"," )",sep=""))+
  theme(text=element_text(size=30))+
  geom_vline(aes(xintercept = 0),linetype="dotted")+
  geom_hline(aes(yintercept = 0),linetype="dotted")+
  theme(panel.background = element_rect(fill='white', colour='black'),
        panel.grid=element_blank(), 
        axis.title = element_text(color='black',size=34),
        axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_text(colour='black', size=34),
        axis.title.y=element_text(colour='black', size=34),
        axis.text=element_text(colour='black',size=28),
        legend.title=element_blank(),
        legend.text=element_text(size=34),
        legend.key=element_blank(),legend.position = c(0.12,0.88),
        legend.background = element_rect(colour = "black"),
        legend.key.height=unit(1.6,"cm"))+
  theme(plot.title = element_text(size=34,colour = "black",hjust = 0.5,face = "bold"))

#同时显示两种分类方法
data <- read.csv("otu_taxa_table.txt", head=TRUE,sep="\t",row.names = 1)
groups <- read.table("group.list.txt",sep = "\t",header = F,colClasses = c("character"))
groups <- as.list(groups)
data <- t(data)
data[is.na(data)] <- 0
data <- vegdist(data,method = "bray")
pcoa<- pcoa(data, correction = "none", rn = NULL)
PC1 = pcoa$vectors[,1]
PC2 = pcoa$vectors[,2]
plotdata <- data.frame(rownames(pcoa$vectors),PC1,PC2,groups$V2,groups$V3)
colnames(plotdata) <-c("sample","PC1","PC2","Treatment","Time")
pich=c(21:25)
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442")
pc1 <-floor(pcoa$values$Relative_eig[1]*100)
pc2 <-floor(pcoa$values$Relative_eig[2]*100)

ggplot(plotdata, aes(PC1, PC2)) +
  geom_point(aes(shape=Time,fill=Treatment,colour=Treatment),size=10)+ 
  scale_shape_manual(values=pich)+
  scale_colour_manual(values=cbbPalette)+
  scale_fill_manual(values=cbbPalette)+
  labs(title="PCoA - PC1-VS-PC2") + 
  xlab(paste("PC1 ( ",pc1,"%"," )",sep="")) + 
  ylab(paste("PC2 ( ",pc2,"%"," )",sep=""))+
  theme(text=element_text(size=30))+
  geom_vline(aes(xintercept = 0),linetype="dotted")+
  geom_hline(aes(yintercept = 0),linetype="dotted")+
  theme(panel.background = element_rect(fill='white', colour='black'),
        panel.grid=element_blank(), 
        axis.title = element_text(color='black',size=34),
        axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_text(colour='black', size=34),
        axis.title.y=element_text(colour='black', size=34),
        axis.text=element_text(colour='black',size=28),
        legend.title=element_text(colour = "black",size = 34,face = "bold"),
        legend.text=element_text(size=34),
        legend.key=element_blank(),
        legend.background = element_rect(colour = "black"),
        legend.key.height=unit(1.6,"cm"))+
  theme(plot.title = element_text(size=34,colour = "black",hjust = 0.5,face = "bold"))

#分组数目较多、组内生物学重复较少
data <- read.csv("otu 2.txt", head=TRUE,sep="\t",row.names = 1)
data <- t(data)
data[is.na(data)] <- 0
data <- vegdist(data)
pcoa<- pcoa(data, correction = "none", rn = NULL)
groups <- read.table("group 2.txt",sep = "\t",header = F,colClasses = c("character"))
groups <- as.list(groups)
groups$V2 <- factor(groups$V2,levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
PC1 = pcoa$vectors[,1]
PC2 = pcoa$vectors[,2]
plotdata <- data.frame(rownames(pcoa$vectors),PC1,PC2,groups$V2)
colnames(plotdata) <-c("sample","PC1","PC2","group")
pc1 <-floor(pcoa$values$Relative_eig[1]*100)
pc2 <-floor(pcoa$values$Relative_eig[2]*100)
pich=rep(c(21:24),3)
library(RColorBrewer)
cbbPalette <- brewer.pal(12,"Set3")
Palette <- c(rep("#000000",12))

ggplot(plotdata, aes(PC1, PC2)) +
  geom_point(aes(colour=group,shape=group,fill=group),size=8)+
  geom_label_repel(aes(PC1,PC2,label = sample),fill = "white",color = "black",
                   box.padding = unit(0.6,"lines"),segment.colour = "grey50",
                   label.padding = unit(0.35,"lines")) +
  scale_shape_manual(values=pich)+
  scale_colour_manual(values=Palette)+
  scale_fill_manual(values=cbbPalette)+
  labs(title="PCoA - The composition of microbiome in sediment") + 
  xlab(paste("PC1 ( ",pc1,"%"," )",sep="")) + 
  ylab(paste("PC2 ( ",pc2,"%"," )",sep=""))+
  theme(text=element_text(size=30))+
  geom_vline(aes(xintercept = 0),linetype="dotted")+
  geom_hline(aes(yintercept = 0),linetype="dotted")+
  theme(panel.background = element_rect(fill='white', colour='black'),
        panel.grid=element_blank(), 
        axis.title = element_text(color='black',size=34),
        axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_text(colour='black', size=34),
        axis.title.y=element_text(colour='black', size=34),
        axis.text=element_text(colour='black',size=28),
        legend.title=element_blank(),
        legend.text=element_text(size=28),
        legend.key=element_blank(),legend.position = "right",
        legend.background = element_rect(colour = "black"),
        legend.key.height=unit(1.55,"cm"))+
  theme(plot.title = element_text(size=34,colour = "black",hjust = 0.5,face = "bold"))


```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200608105513655.png" alt="image-20200608105513655" style="zoom:50%;" />

#### **PCA分析**

PCA（principal component analysis）中文名字叫主成分分析：基于特征向量的排序方法，分析对象是原始的定量数据。排序图展示样方之间的欧氏距离。

PCoA（principal coordinate analysis）中文名字叫主坐标分析：分析对象为距离矩阵，而非原始的样方-变量矩阵。因此可以灵活选着关联测度。

这样我们就发现了这两种非约束排序的区别：PCA是基于原始物种所做的排序分析，而PCoA则是基于物种组成计算得到的距离矩阵。

```R
library(ggplot2)
df <-  iris[c(1,2,3,4)]
df_pca <- prcomp(df) #计算主成分
head(df_pca,3) #查看主成分结果
plot(df_pca$x[,1],df_pca$x[,2])
df_pcs <-data.frame(df_pca$x, Species = iris$Species) #生成数据框
df_pcs
#按Species分颜色
ggplot(df_pcs,aes(x=PC1,y=PC2,color=Species))+  #去除背景及网格线
  geom_point()+ 
  theme_bw() +
  theme(panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line= element_line(colour = "black"))
percentage <- round(df_pca$sdev/sum(df_pca$sdev)*100,2)
percentage
percentage <- paste(colnames(df_pcs),"(",paste(as.character(percentage),"%",")",sep = ""))
ggplot(df_pcs,aes(x=PC1,y=PC2,color = Species))+
  geom_point()+xlab(percentage[1])+
  ylab(percentage[2])
#添加置信椭圆
ggplot(df_pcs,aes(x=PC1,y=PC2,color = Species))+geom_point()+stat_ellipse(level = 0.95,show.legend = F)+
  annotate('text', label = 'setosa', x = -2, y = -1.25, size = 5, colour = '#f8766d') +
  annotate('text', label = 'versicolor', x = 0, y = - 0.5, size = 5, colour = '#00ba38') +
  annotate('text', label = 'virginica', x = 3, y = 0.5, size = 5, colour = '#619cff')
#查看各变量对于PCA的贡献
df_r <- as.data.frame(df_pca$rotation)
df_r$feature <- rownames(df_r)
#贡献度绘图
ggplot(df_r,aes(x = PC1,y= PC2 ,label = feature ,color = feature))+geom_point()+ geom_text(size = 3)
#绘图总展示
ggplot(df_pcs,aes(x=PC1,y=PC2,color=Species )) + geom_point()+xlab(percentage[1]) + ylab(percentage[2]) + stat_ellipse(level = 0.95, show.legend = F) +
  annotate('text', label = 'setosa', x = -2, y = -1.25, size = 5, colour = '#f8766d') +
  annotate('text', label = 'versicolor', x = 0, y = - 0.5, size = 5, colour = '#00ba38') +
  annotate('text', label = 'virginica', x = 3, y = 0.5, size = 5, colour = '#619cff') + labs(title="Iris PCA Clustering", 
                                                                                             subtitle=" PC1 and PC2 principal components ",       caption="Source: Iris") + theme_classic()

**********一种老方法********
pca_plot = function(dddd,ggggg){
  library("FactoMineR")
  library("factoextra")
  df.pca <- PCA(t(dddd), graph = FALSE)
  fviz_pca_ind(df.pca,
               #axes = c(2,3),
               geom.ind = "point",
               col.ind = ggggg ,#分组
               addEllipses = TRUE,
               legend.title = "Groups"
  )
}
pca_plot(otu_file,group$group)
```

![image-20200221140501980](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221140501980.png)

 **另一种PCA方法(基于ggbiplot包)**

```R
library(devtools)
install_github("vqv/ggbiplot") #安装
library(ggbiplot)
design <- read.table("sample.txt", header=T, row.names= 1, sep="\t")  #导入分组信息表 
count <- read.delim("Genu_0.001.txt", row.names= 1,  header=T, sep="\t") #导入相对丰度表
# 转换原始数据为百分比 norm = t(t(count)/colSums(count,na=T)) * 100
otu.pca <- prcomp(t(count), scale. = TRUE) 
ggbiplot(otu.pca, obs.scale = 1, var.scale = 1,groups = design$group, ellipse = TRUE,var.axes = F)
OTUs_mad.5 = head(count[order(apply(count,1,mad), decreasing=T),],n=8) 
otu.pca <- prcomp(t(OTUs_mad.5))
ggbiplot(otu.pca, obs.scale = 1, var.scale = 1, groups = design$group, ellipse = TRUE,var.axes = T)

# 我们仅用中值绝对偏差(mad)最大的8个OTUs进行主成分分析，即可将三组样品明显分开。
# 图中向量长短代表差异贡献，方向为与主成分的相关性。可以看到最长的向量Streptophyta与X轴近平行，
# 表示PC1的差异主要由此菌贡献。其它菌与其方向相反代表OTUs间可能负相关；夹角小于90%的代表两个OTUs有正相关

```

![image-20200221140617086](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221140617086.png)

**另外基于ggord的PCA**

```R
library(ggord) #使用geom_ord_ellipse这个图层呢，随便你加置信区间，想加几个就加几个
#不仅针对LDA，也支持其它的ordination plot
devtools::install_github('fawda123/ggord')
library(ggord) 
library(MASS)
ord <- prcomp(iris[, -5])
p <- ggord(ord, iris$Species) ;p
library(yyplot) 
p + geom_ord_ellipse(ellipse_pro = .96, color='firebrick', size=1, lty=3) + 
  geom_ord_ellipse(ellipse_pro = .99, lty=2)  
```

![image-20200221142110678](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221142110678.png)

**PCA**

```R

#载入分析包
library(FactoMineR)
library(ggrepel)
#载入绘图数据
df <- read.table("otu_taxa_table.txt",header = TRUE,row.names = 1,sep = "\t")
#载入分组数据
groups <- read.table("group.list.txt",header = FALSE,sep = "\t",
                     colClasses=c("character"))
#绝对丰度表转化为相对丰度表
df1 <- matrix(0,nrow = nrow(df), ncol = ncol(df))
for(i in 1:ncol(df)){
  df1[,i] = df[ ,i]/sum(df[ ,i])
}
colnames(df1) <- colnames(df)
rownames(df1) <- rownames(df)
df <- t(df1)

#PCA        
pca <- PCA(df[,1:ncol(df)],scale.unit = FALSE,graph = FALSE)
#PCA结果保存
write.csv(pca$ind$coord,file="pca.csv")


#支持一种分组方式和两种分组方式，
pc1 <- pca$ind$coord[,1]
pc2 <- pca$ind$coord[,2]
ncol <- ncol(groups)
group1 <- c()
group2 <- c() 
#通过循环式的分组文件中样本的顺序与pca结果文件中一致
for(i in 1:length(groups$V1)){
  Order <- grep(rownames(pca$ind$coord)[i],groups$V1)
  group1[i] <- groups$V2[Order]
  if(ncol==3){
    group2[i] <- groups$V3[Order]
  }}
#使用if来根据分组文件中的分组方式分别建立绘图文件
if(ncol==2){
  plotdata <- data.frame(rownames(pca$ind$coord),pc1,pc2,group1)
  colnames(plotdata) <- c("sample","PC1","PC2","group")
  point <- geom_point(aes(color = group,fill = group,shape = group),
                      size = 8)
}else if(ncol==3){
  plotdata <- data.frame(rownames(pca$ind$coord),pc1,pc2,group1,group2)
  colnames(plotdata) <- c("sample","PC1","PC2","group1","group2")
  point <- geom_point(aes(color = group1,fill = group1,shape = group2),
                      size = 8)
}

#通过向量指定绘图中的一些参数，包括坐标轴文字、样本点形状和颜色。
pc1 <- floor(pca$eig[1,2]*100)/100
pc2 <- floor(pca$eig[2,2]*100)/100
pich <- c(22:26)
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442")


#计算与PCA中前两轴相关的物种
di <- dimdesc(pca, axes = c(1,2), proba = 0.05)
#根据相关性结果从大到小排序，每个轴选择排名前3的物种
dim1.cor <- pca$var$cor[row.names(pca$var$coord) %in% 
                          row.names(di$Dim.1$quanti[order(di$Dim.1$quanti[,2])[1:3],]),] 
dim2.cor <- pca$var$cor[row.names(pca$var$coord) %in% 
                          row.names(di$Dim.2$quanti[order(di$Dim.2$quanti[,2])[1:3],]),]
#建立绘图所需文件
dim12.cor <- rbind(dim1.cor,dim2.cor)
di.cor <- dim12.cor[,1:2]
di.cor <- as.data.frame(di.cor)

#载入绘图包
library(ggplot2)
library(ggrepel)
#图像绘制
p <- ggplot(plotdata,aes(PC1,PC2)) +
  #添加样本名称
  geom_label_repel(aes(PC1,PC2,label = sample),fill = "white",color = "black",
                   box.padding = unit(0.6,"lines"),segment.colour = "grey50",
                   label.padding = unit(0.35,"lines"),family = "Arial") + 
  point + 
  scale_shape_manual(values = pich)+
  scale_fill_manual(values=cbbPalette)+
  scale_color_manual(values=cbbPalette)+
  labs(title="PCA Plot") + 
  xlab(paste("PC1 ( ",pc1,"%"," )",sep="")) + 
  ylab(paste("PC2 ( ",pc2,"%"," )",sep=""))+
  #添加区分物种箭头
  geom_segment(data = di.cor,aes(x = 0,y = 0,xend = di.cor[,1]*0.06,
                                 yend = di.cor[,2]*0.06),
               arrow = arrow(angle = 40,length = unit(0.4,"cm")),size = 1.5) +
  #添加区分物种名称
  geom_label_repel(data = di.cor,aes(x = di.cor[,1]*0.06,y = di.cor[,2]*0.06,
                                     label = rownames(di.cor)),
                   fill = "grey50",color = "white",box.padding = unit(0.6,"lines"),
                   segment.colour = "grey50",label.padding = unit(0.35,"lines"),
                   family = "Arial") +
  theme(text=element_text(family = "Arial",size = 34))+
  geom_vline(aes(xintercept = 0),linetype = "dotted")+
  geom_hline(aes(yintercept = 0),linetype = "dotted")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid=element_blank(), 
        axis.title = element_text(color = "black",size = 34),
        axis.ticks.length = unit(0.4,"lines"), 
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_text(colour = "black", size = 34),
        axis.title.y=element_text(colour = "black", size = 34),
        axis.text=element_text(colour = "black",size=28),
        legend.title=element_text(colour = "black",size = 34,face = "bold"),
        legend.key=element_blank(),
        legend.background = element_rect(colour = "black"),
        legend.key.height=unit(1.2,"cm"),
        plot.title = element_text(size=34,colour = "black",hjust = 0.5,
                                  face = "bold"))
#保存为png图片
png(filename="PCA.png",res=700,height=5400,width=7200)
p
dev.off()
#保存为pdf格式
cairo_pdf(filename="PCA.pdf",height=10,width=10)
p
dev.off()

#通过ggsymbol包添加更多点样式
ggplot(plotdata, aes(PC1, PC2)) +
  geom_symbol(aes(symbol=group2,fill=group1,colour=group1),size=10)+ 
  scale_symbol_manual(values=pich)+
  scale_colour_manual(values=cbbPalette)+
  scale_fill_manual(values=cbbPalette)+
  geom_label_repel(aes(PC1,PC2,label = sample),fill = "white",color = "black",
                   box.padding = unit(0.6,"lines"),segment.colour = "grey50",
                   label.padding = unit(0.35,"lines"),family = "Arial") + 
  #添加区分物种箭头
  geom_segment(data = di.cor,aes(x = 0,y = 0,xend = di.cor[,1]*0.06,
                                 yend = di.cor[,2]*0.06),
               arrow = arrow(angle = 40,length = unit(0.4,"cm")),size = 1.5) +
  #添加区分物种名称
  geom_label_repel(data = di.cor,aes(x = di.cor[,1]*0.06,y = di.cor[,2]*0.06,
                                     label = rownames(di.cor)),
                   fill = "grey50",color = "white",box.padding = unit(0.6,"lines"),
                   segment.colour = "grey50",label.padding = unit(0.35,"lines"),
                   family = "Arial") +
  labs(title="PCoA - PC1-VS-PC2") + 
  xlab(paste("PC1 ( ",pc1,"%"," )",sep="")) + 
  ylab(paste("PC2 ( ",pc2,"%"," )",sep=""))+
  theme(text=element_text(size=30))+
  geom_vline(aes(xintercept = 0),linetype="dotted")+
  geom_hline(aes(yintercept = 0),linetype="dotted")+
  theme(panel.background = element_rect(fill='white', colour='black'),
        panel.grid=element_blank(),
        axis.title = element_text(color='black',size=34),
        axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(colour='black', size=34),
        axis.title.y=element_text(colour='black', size=34),
        axis.text=element_text(colour='black',size=28),
        legend.title=element_text(colour = "black",size = 34,face = "bold"),
        legend.text=element_text(size=34),
        legend.key=element_blank(),
        legend.background = element_rect(colour = "black"),
        legend.key.height=unit(1.6,"cm"))+
  theme(plot.title = element_text(size=34,colour = "black",hjust = 0.5,face = "bold"))

```

![image-20200608104646835](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200608104646835.png)

### **群落bar图**

[R语言画堆叠图实例](https://mp.weixin.qq.com/s?__biz=MzIxNzc1Mzk3NQ==&mid=2247483757&idx=1&sn=e3f6e387d6afef876dbd2af8b34e69dd&chksm=97f5b175a0823863f9bf0ccc3272d1360a0b12869511dc82a0704a9aeeb5f4fb9fdaff8ed89a&token=497100570&lang=zh_CN&scene=21#wechat_redirect)

```R
setwd("./Abundance/Nr_abundance/")
getwd()
####读取并挑选 top10 丰度门 
#读取数据 
rm(list = ls())
phylum_top10 <- read.delim('Genu.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
phylum <- apply(phylum_top10, 2, function(x){x/sum(x)})#百分比化
relative_abund_table<-decostand(abund_table, method = "total")#该方法也可直接计算百分比
phylum_filter <- data.frame(phylum[which(apply(phylum, 1, function(x){mean(x)})
                                >0.001),], check.names=F) #mean abundance > 0.001
phylum_filter <-data.frame(phylum[apply(phylum,1,max)>0.001,])#去除至少有1个样品中大于%0.1的物种
phylum_filter$sum <- rowSums(phylum_filter)#求各类群的丰度总和，并排序
phylum_filter <- phylum_filter[order(phylum_filter$sum, decreasing = TRUE), ]
#挑选 top10 门类群，并将 top10 外的类群合并为“Others”
phylum_top10 <- phylum_filter[-ncol(phylum_filter)]
phylum_top10['Others', ] <- 1 - colSums(phylum_top10)

#可选输出（如 csv 格式）
write.table(phylum_top10, 'Genu_0.001.txt', quote = FALSE,sep = '\t')

####barplot 作图（一个简单示例）
#定义作图整体分布
tiff('barplot_plot.tiff', width = 1000, height = 600)
par(xpd = TRUE, mar = par()$mar + c(1, 3, 1, 16))
par(mar=c(6,8,5,5),xpd=T,las=2,mgp=c(4,1,0))#mpg三个值分别控制轴标签、轴刻度值、轴刻度线与轴的距离
#barplot作图，包含了颜色、字体大小、图例位置等信息
barplot(as.matrix(100 * phylum_top10), col = c('blue', 'orange', 'green', 'yellow', 'red', 'hotpink', 'cyan','purple', 'burlywood1', 'skyblue','#8b8378' ,'#458b74','#f0ffff','#eeb422','#ee6aa7','#8b3a62','#cd5c5c','#ee6363','#f0e68c','#e6e6fa','#add8e6','#bfefff','#f08080','#d1eeee','#7a8b8b','#8b814c','#8b5f65','gray'),
        legend = rownames(phylum_top10), 
        cex.axis = 2, cex.names = 2, ylim = c(0, 100), las = 1, width = 0.5, space = 0.5, beside = FALSE,
        args.legend = list(x = 'right', bty = 'n', inset = -0.18, cex = 2, y.intersp = 1.2, x.intersp = 0.7, text.width = 1))
mtext('Relative Abundance(%)', cex = 2, side = 2, line = 4)
dev.off()

####ggplot2 作图（一个简单示例）
library(reshape2)	#用于排列数据
library(ggplot2)	#ggplot2 作图
#可以提前预设一个色板
Palette1 <- c("#B2182B","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#CC6666","#9999CC","#66CC99","#999999","#ADD1E5")
Palette2 <- c('blue', 'orange', 'green', 'yellow', 'red', 'hotpink', 'cyan','purple', 'burlywood1', 'skyblue','#8b8378' ,'#458b74','#f0ffff','#eeb422','#ee6aa7','#8b3a62','#cd5c5c','#ee6363','#f0e68c','#e6e6fa','#add8e6','#bfefff','#f08080','#d1eeee','#7a8b8b','#8b814c','#8b5f65','gray')

#调整数据布局
phylum_top10$Taxonomy <- factor(rownames(phylum_top10), levels = rev(rownames(phylum_top10)))
phylum_top10 <- melt(phylum_top10, id = 'Taxonomy')

#添加分组
group <- read.delim('sample.txt', sep = '\t', stringsAsFactors = FALSE)
names(group)[1] <- 'variable'
phylum_top10 <- merge(phylum_top10, group, by = 'variable')

#绘制带分面的柱状图
p <- ggplot(phylum_top10, aes(variable, 100 * value, fill = Taxonomy)) +
  geom_col(position = 'stack', width = 0.6) +
  scale_fill_manual(values =  Palette2) +
  labs(x = '', y = 'Relative Abundance(%)') +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), strip.text = element_text(size = 12)) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 13), legend.title = element_blank(), legend.text = element_text(size = 11))+
  theme_classic()+scale_y_continuous(expand = c(0,0))
#scale_y_continuous(labels = scales::percent) 设置Y轴数值为百分比数值

p1 <- p+ theme(text = element_text(family = "Times"))+scale_y_continuous(limits = c(0,1),expand = c(0,0))#去除空白及设置字体
+ theme(axis.text.x=element_text(colour="black",size=12,face = "bold",angle = -90,vjust = 0.5,hjust = 0)) 
 ,
p
ggsave('Order_plot.pdf', p, width = 10, height = 6)

##冲击图
p2 <- ggplot(data = phylum_top10,aes(x = variable, y = value, alluvium = Taxonomy, stratum = Taxonomy))+
  geom_alluvium(aes(fill = Taxonomy),alpha = .5,width = 0.6) + 
  geom_stratum(aes(fill = Taxonomy),width = 0.6)+scale_fill_manual(values = rev(c(colour)))+
theme_classic()+scale_y_continuous(expand = c(0,0))+
   theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), strip.text = element_text(size = 12)) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 13), legend.title = element_blank(), legend.text = element_text(size = 11))
ggsave('plpt.tiff', p2, width = 10, height = 6)
```

#### 带聚类树的bar图

 [R语言画聚类数实例](https://mp.weixin.qq.com/s?__biz=MzIxNzc1Mzk3NQ==&mid=2247484616&idx=1&sn=b7ed9e20bac5d8a8b3b72ded3d28227f&chksm=97f5b4d0a0823dc6a365108ec7c5daf76a9402d28f1188b963fe307401eec3a52e8118e2570e&token=497100570&lang=zh_CN&scene=21#wechat_redirect)

[R语言聚类树图小例子](https://www.jianshu.com/p/f28d9fbd99f3)

```R

hc<-hclust(dist(USArrests),method="ave")
library(ggdendro)
df1<-dendro_data(hc,type="rectangle")
ggplot(segment(df1))+
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend))
ggplot(segment(df1))+
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend))+
  geom_text(data=df1$labels,aes(x=x,y=y-1,label=label),
            angle=90,hjust=1,vjust=0.3,size=3)+
  scale_y_continuous(expand=c(0.2,0))+
  theme_bw()+
  geom_point(data=df1$labels,aes(x=x,y=y),color="red")

#给不同样品添加颜色
df1$labels$Group<-c(rep("A",16),rep("B",14),rep("C",20))
ggplot(segment(df1))+
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend))+
  geom_text(data=df1$labels,aes(x=x,y=y-1,label=label,color=Group),
            angle=90,hjust=1,vjust=0.3,size=3)+
  scale_y_continuous(expand=c(0.2,0))+
  theme_bw()+labs(x="",y="")+
  geom_point(data=df1$labels,aes(x=x,y=y,color=Group,shape=Group))+
  scale_color_brewer("",palette = "Set1")+
  theme(legend.position="none",
        panel.grid=element_blank(),
        panel.border=element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


#不同样品加上阴影效果
ggplot(segment(df1))+
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend))+
  geom_text(data=df1$labels,aes(x=x,y=y-1,label=label,color=Group),
            angle=90,hjust=1,vjust=0.3,size=3)+
  scale_y_continuous(expand=c(0.2,0))+
  theme_bw()+labs(x="",y="")+
  geom_point(data=df1$labels,aes(x=x,y=y,color=Group,shape=Group))+
  scale_color_brewer("",palette = "Set1")+
  theme(legend.position="none",
        panel.grid=element_blank(),
        panel.border=element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=0,xmax=16.5,ymin=0,ymax=90),color="red",fill="red",alpha=0.005)+
  geom_rect(aes(xmin=16.5,xmax=30.5,ymin=0,ymax=90),color="blue",fill="blue",alpha=0.003)+
  geom_rect(aes(xmin=30.5,xmax=50,ymin=0,ymax=90),color="green",fill="green",alpha=0.005)
```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200707161125971.png" alt="image-20200707161125971" style="zoom:50%;" />

```R
#导入分组文件及丰度表文件
#层次聚类
#读取 OTU 丰度表
dat <- read.delim('phylum_top10.txt', row.names = 1, sep = '\t', head = TRUE, check.names = FALSE)
 
#计算样本间距离，以群落分析中常用的 Bray-curtis 距离为例
dis_bray <- vegan::vegdist(t(dat), method = 'bray')
 
#层次聚类，以 UPGMA 为例
tree <- hclust(dis_bray, method = 'average')
tree
 
plot(tree)
```

![img](C:\Users\Administrator\AppData\Local\YNote\data\weixinobU7Vjm7VL-oN3s6RJxCgkr_0ghI\d1f67aa2e4144608b02aa37c22b9de25\6f52b7b5e089b55c1a019cd6f5a1cb38.jpg)

```R
#接下来就是对聚类树进行调整。
具体做法，首先定义一个画板，将聚类树放在画板的左侧，并按样本的已知分组信息给分支上色。聚类树的一些常见的可视化调整方法
##聚类树绘制
#样本分组颜色、名称等
group <- read.delim('group.txt', row.names = 1, sep = '\t', head = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
grp <- group[2]
group_col <- c('red', 'blue')
names(group_col) <- c('1', '2')
group_name <- c('Control', 'Treat')
 
#样本分组标签
layout(t(c(1, 2, 2, 2, 3)))
par(mar = c(5, 2, 5, 0))
 
plot(0, type = 'n', xaxt = 'n', yaxt = 'n', frame.plot = FALSE, xlab = '', ylab = '',
    xlim = c(-max(tree$height), 0), ylim = c(0, length(tree$order)))
legend('topleft', legend = group_name, pch = 15, col = group_col, bty = 'n', cex = 1)
 
#聚类树绘制，按分组给分支上色
treeline <- function(pos1, pos2, height, col1, col2) {
    meanpos = (pos1[1] + pos2[1]) / 2
    segments(y0 = pos1[1] - 0.4, x0 = -pos1[2], y1 = pos1[1] - 0.4, x1 = -height,  col = col1,lwd = 2)
    segments(y0 = pos1[1] - 0.4, x0 = -height,  y1 = meanpos - 0.4, x1 = -height,  col = col1,lwd = 2)
    segments(y0 = meanpos - 0.4, x0 = -height,  y1 = pos2[1] - 0.4, x1 = -height,  col = col2,lwd = 2)
    segments(y0 = pos2[1] - 0.4, x0 = -height,  y1 = pos2[1] - 0.4, x1 = -pos2[2], col = col2,lwd = 2)
}
 
meanpos = matrix(rep(0, 2 * length(tree$order)), ncol = 2)
meancol = rep(0, length(tree$order))
for (step in 1:nrow(tree$merge)) {
    if(tree$merge[step, 1] < 0){
        pos1 <- c(which(tree$order == -tree$merge[step, 1]), 0)
        col1 <- group_col[as.character(grp[tree$labels[-tree$merge[step, 1]],1])]
    } else {
        pos1 <- meanpos[tree$merge[step, 1], ]
        col1 <- meancol[tree$merge[step, 1]]
    }
    if (tree$merge[step, 2] < 0) {
        pos2 <- c(which(tree$order == -tree$merge[step, 2]), 0)
        col2 <- group_col[as.character(grp[tree$labels[-tree$merge[step, 2]],1])]
    } else {
        pos2 <- meanpos[tree$merge[step, 2], ]
        col2 <- meancol[tree$merge[step, 2]]
    }
    height <- tree$height[step]
    treeline(pos1, pos2, height, col1, col2)
    meanpos[step, ] <- c((pos1[1] + pos2[1]) / 2, height)
    if (col1 == col2) meancol[step] <- col1 else meancol[step] <- 'grey'
}
```

![img](C:\Users\Administrator\AppData\Local\YNote\data\weixinobU7Vjm7VL-oN3s6RJxCgkr_0ghI\890ccfe55d0b4571a55404eecb6518af\e1ab8ccada70aefdd00c4e50679402a8.jpg)

```R
##堆叠柱形图
#样本顺序调整为和聚类树中的顺序一致
dat <- dat[ ,tree$order]
 
#物种颜色设置
phylum_color <- c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462', '#B3DE69', '#FCCDE5', '#BC80BD', '#CCEBC5', 'gray')
names(phylum_color) <- rownames(dat)
 
#堆叠柱形图
par(mar = c(5, 2, 5, 0))
 
bar <- barplot(as.matrix(dat), col = phylum_color, space = 0.4, width = 0.7, cex.axis = 1, horiz = TRUE, cex.lab = 1.2,
    xlab = 'Relative Abundance', yaxt = 'n', las = 1, ylim = c(0, ncol(dat)), family = 'mono')
 
mtext('Top 10 phylums', side = 3, line = 1, cex = 1)
text(x = -0.05, y = bar, labels = colnames(dat), col = group_col[group[tree_order, 2]], xpd = TRUE)
#首先按聚类树中样本的顺序重新调整丰度表中样本的顺序，以保持二者能够对应，并定义颜色属性。之后绘制堆叠柱形图，放置在画板右侧区域
```

<img src="C:\Users\Administrator\AppData\Local\YNote\data\weixinobU7Vjm7VL-oN3s6RJxCgkr_0ghI\3d674c26339f47fd88f76985ef1b34db\63d5b1477db019d48e5e77272378c67e.jpg" alt="img" style="zoom:50%;" />

```R
#柱形图图例
par(mar = c(5, 1, 5, 0))
plot(0, type = 'n', xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = '')
legend('left', pch = 15, col = phylum_color, legend = names(phylum_color), bty = 'n', cex = 1)
```

**另一种方法聚类树柱图**

```R
rm(list = ls())
getwd()

library(tidyverse)
library(ggplot2)
library(ggtree)
library(treeio)
library(ggsci)
library(cowplot)

# 导入OTU文件
otu = read.table('phylum_top10.txt', header =  T)

# 计算距离后均值聚类并建树及可视化
tree = hclust(vegan::vegdist(t(otu), method = 'bray'), 
              method = 'average') %>%
  as.phylo()
tree
# 选择节点，方便后续分开上色
tree = groupClade(tree, .node=c(16))

# 绘制聚类图
p1 = ggtree(tree, aes(color=group, linetype=group)) + 
  geom_tiplab(aes(color=group))+geom_point(color='firebrick')+
scale_color_manual(values = c('red','blue'))+theme(legend.position = "none")
p1
#+geom_text2(aes(subset=!isTip, label=node), hjust=-.3)


# 绘制物种组成柱状图
otu$mean = apply(otu, 1, mean)
otu = otu[order(otu$mean),]
otu$phylum = factor(rownames(otu), levels = rownames(otu))
otu = otu[, -c(ncol(otu)-1)]
p2 = otu %>%
  reshape2::melt(id.vars = 'phylum') %>%
  ggplot(aes(variable, value, fill = phylum))+
  geom_bar(stat = 'identity', position = 'fill')+
  scale_x_discrete(limits = c('t1','t2','t5','t4','t3','t6',
                              'c1','c2','c6','c4','c3','c5'))+
  scale_fill_igv()+
  scale_y_continuous(expand = c(0,0))+
 scale_y_continuous(labels = scales::percent)+
  coord_flip()+
  theme_classic()+
  theme(axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank())+
  labs(y = 'Percentage')

ggdraw()+
  draw_plot(p1, 0, 0.06, 0.5, 0.95)+
  draw_plot(p2, 0.49, 0, 0.5, 1)
```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200615171621957.png" alt="image-20200615171621957" style="zoom:50%;" />



#### 堆叠面积图

```R
library(reshape2)
library(ggplot2)

##读取并挑选 top10 丰度门
#读取数据
phylum <- read.delim('phylum_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

#求各类群的丰度总和，并排序
phylum$sum <- rowSums(phylum)
phylum <- phylum[order(phylum$sum, decreasing = TRUE), ]

#挑选 top10 门类群，并将 top10 外的类群合并为“Others”
phylum_top10 <- phylum[1:10, -ncol(phylum)]
phylum_top10['Others', ] <- 1 - colSums(phylum_top10)

#可选输出（如 csv 格式）
write.csv(phylum_top10, 'phylum_top10.csv', quote = FALSE)

#表格样式重排，借助 reshape2 包 melt() 实现
phylum_top10$Taxonomy <- factor(rownames(phylum_top10), levels = rev(rownames(phylum_top10)))
phylum_top10 <- melt(phylum_top10, id = 'Taxonomy')
names(phylum_top10)[2] <- 'sample'

#合并分组信息
group <- read.delim('group.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
phylum_top10 <- merge(phylum_top10, group, by = 'sample', all.x = TRUE)

##ggplot2 堆叠面积图
#初始
p <- ggplot(phylum_top10, aes(x = times, y = 100 * value, fill = Taxonomy)) +
geom_area() +
labs(x = 'Times', y = 'Relative Abundance(%)', title = '', fill = 'Top10 Phylum')
p

#稍作调整后
p <- p + 
scale_fill_manual(values = c('gray', 'skyblue', 'burlywood1', 'purple', 'cyan', 'hotpink', 'red', 'yellow', 'green', 'orange', 'blue')) +	#设置颜色
theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) +	#调整背景
scale_x_continuous(breaks = 1:15, labels = as.character(1:15), expand = c(0, 0)) +	#调整坐标轴轴刻度
scale_y_continuous(expand = c(0, 0))
p

#ggsave('ggplot2_area.pdf', p, width = 7.5, height = 4.5)
ggsave('ggplot2_area.png', p, width = 7.5, height = 4.5)

#展示部分类群，例如挑选出 top5 丰度类群，其余（包括 Others）去除不展示
phylum_top5 <- subset(phylum_top10, Taxonomy %in% c('Proteobacteria', 'Acidobacteria', 'Bacteroidetes', 'Actinobacteria', 'Gemmatimonadetes'))

p <- ggplot(phylum_top5, aes(x = times, y = 100 * value, fill = Taxonomy)) +
geom_area() +
labs(x = 'Times', y = 'Relative Abundance(%)', title = '', fill = 'Top5 Phylum') +
scale_fill_manual(values = c('red', 'yellow', 'green', 'orange', 'blue')) +	#设置颜色
theme(panel.grid = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = 'black')) +	#调整背景
scale_x_continuous(breaks = 1:15, labels = as.character(1:15), expand = c(0, 0)) +	#调整坐标轴轴刻度
scale_y_continuous(expand = c(0, 0))

#ggsave('ggplot2_area2.pdf', p, width = 7.5, height = 4.5)
ggsave('ggplot2_area2.png', p, width = 7.5, height = 4.5)
```

![image-20200301165250371](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200301165250371.png)

#### 丰度星星图

```R
##读取并挑选 top10 丰度门
#读取数据
phylum <- read.delim('phylum_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

#求各类群的丰度总和，并排序
phylum$sum <- rowSums(phylum)
phylum <- phylum[order(phylum$sum, decreasing = TRUE), ]

#挑选 top10 门类群，并将 top10 外的类群合并为“Others”
phylum_top10 <- phylum[1:10, -ncol(phylum)]
phylum_top10['Others', ] <- 1 - colSums(phylum_top10)

#可选输出（如 csv 格式）
write.csv(phylum_top10, 'phylum_top10.csv', quote = FALSE)

##stars() 作图（一个简单示例）
t_phylum_top10 <- data.frame(t(phylum_top10))
color <- c('blue', 'orange', 'green', 'yellow', 'red', 'hotpink', 'cyan','purple', 'burlywood1', 'skyblue', 'gray')

png('stars_plot.png', width = 1500, height = 1500, res = 300, units = 'px')
stars(t_phylum_top10, scale = FALSE, draw.segments = TRUE, col.segments = color, nrow = 2, ylim = c(1, 2), key.loc = c(6, -1) , frame.plot = FALSE, main = 'Top10 Phylum')
dev.off()

##ggplot2 作图（一个简单示例）
library(reshape2)	#用于排列数据
library(ggplot2)	#ggplot2 作图

#调整数据布局
phylum_top10$Taxonomy <- factor(rownames(phylum_top10), levels = rownames(phylum_top10))
phylum_top10_melt <- melt(phylum_top10, id = 'Taxonomy')

#以样本 c1 为例的星图，单一柱形图的坐标转换
phylum_top10_c1 <- subset(phylum_top10_melt, variable == 'c1')

p_c1 <- ggplot(phylum_top10_c1, aes(Taxonomy, value, fill = Taxonomy)) +
geom_bar(stat = 'identity', width = 1) +
coord_polar(theta = 'x') +
scale_fill_manual(values = c('blue', 'orange', 'green', 'yellow', 'red', 'hotpink', 'cyan','purple', 'burlywood1', 'skyblue', 'gray')) +
labs(x = '', y = '', title = 'Sample: c1', fill = 'Top10 Phylum') +
theme(panel.grid = element_blank(), panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5))

#ggsave('ggplot2_c1_plot.pdf', p_c1, width = 5, height = 5)
ggsave('ggplot2_c1_plot.png', p_c1, width = 5, height = 5)

#所有样本，堆叠柱形图的坐标转换
p_all <- ggplot(phylum_top10_melt, aes(variable, value, fill = Taxonomy)) +
geom_bar(stat = 'identity', width = 0.6) +
coord_polar(theta = 'x') +
scale_fill_manual(values = c('blue', 'orange', 'green', 'yellow', 'red', 'hotpink', 'cyan','purple', 'burlywood1', 'skyblue', 'gray')) +
labs(x = '', y = '', fill = 'Top10 Phylum') +
theme(panel.grid = element_blank(), panel.background = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())

#ggsave('ggplot2_all_plot.pdf', p_all, width = 5, height = 5)
ggsave('ggplot2_all_plot.png', p_all, width = 5, height = 5)

##一个 ggplot2 的多图组合样式，借助 grid 包完成
library(grid)

#获取所有样本名称
sample_name <- levels(phylum_top10_melt$variable)

#创建画板，根据样本数（此处为 8 个）预设图片位置
#pdf('grid_plot.pdf', width = 9, height = 5)
png('grid_plot.png', width = 3000, height = 1500, res = 300, units = 'px')
grid.newpage()

n <- 8
split_x <- c(0.1, 0.3, 0.5, 0.7, 0.1, 0.3, 0.5, 0.7)	#预设 8 张子图的横坐标
split_y <- c(0.6, 0.6, 0.6, 0.6, 0.1, 0.1, 0.1, 0.1)	#预设 8 张子图的纵坐标

#使用循环，使用 ggplot2 分别绘制 8 个样本的物种丰度星图，并放置在画板对应的位置中
for (i in 1:n) {
	p_i <- ggplot(subset(phylum_top10_melt, variable == sample_name[i]), aes(Taxonomy, value, fill = Taxonomy)) +
		geom_bar(stat = 'identity', width = 1) +
		coord_polar(theta = 'x') +
		scale_fill_manual(values = c('blue', 'orange', 'green', 'yellow', 'red', 'hotpink', 'cyan','purple', 'burlywood1', 'skyblue', 'gray')) +
		labs(title = paste('Sample:', sample_name[i]), y = '', x = '', fill = 'Top10 Phylum') +
		theme(plot.background = element_blank(), panel.grid = element_blank(), panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5))
	
	print(p_i + theme(legend.position = 'none'), vp = viewport(x = split_x[i], y = split_y[i], width = 1, height = 0.8))
}

#该函数用于在 ggplot2 中提取图例，参考自 https://stackoverflow.com/questions/13712574/how-to-change-position-of-grid-draw
#前述博文“饼图（扇形图）”中也使用到了这个方法提取 ggplot2 的图例，可参见 http://blog.sciencenet.cn/blog-3406804-1170610.html
g_legend <- function(gg_plot) {
	tmp <- ggplot_gtable(ggplot_build(gg_plot))
	leg <- which(sapply(tmp$grobs, function(x) x$name) == 'guide-box')
	leg <- tmp$grobs[[leg]]
	leg
}

#截取图例添加至组合图中
phylum_legend <- g_legend(p_i)
phylum_legend$vp$x <- unit(0.92, 'npc')	#指定图例在 grid 画板中的横坐标位置
phylum_legend$vp$y <- unit(0.5, 'npc')	#指定图例在 grid 画板中的纵坐标位置
grid.draw(phylum_legend)

#完成后关闭
dev.off()
```

![image-20200301165350255](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200301165350255.png)

#### 圆角柱形图

```R
library(ggplot2)
library(RColorBrewer)
install.packages("ggchicklet", repos = "https://cinc.rud.is")
library(ggchicklet)
x<-LETTERS[1:14]
y<-1:14
df<-data.frame(x,y)
df

ggplot(df,aes(x,y))+
  geom_chicklet(aes(fill=x))+
  scale_fill_manual(name="",
                    values =c(brewer.pal(4,"Paired"),
                              brewer.pal(10,"Paired")))+
  theme_bw()+labs(x="",y="")
#实现圆角柱形图只需要将geom_col()函数换成geom_chicklet()函数就可以了,ggchicklet包下载

#组成图
df<-data.frame(Year=c(rep("2019",12),rep("2018",12),
                      rep("2017",12),rep("2017",12),
                      rep("2016",12),rep("2015",12)),
               Month=c(rep(1:12,6)),
               n=sample(1:15,12*6,replace = T))
dim(df)
head(df)
library(ggplot2)
library(RColorBrewer)
library(ggchicklet)
ggplot(df,aes(x = factor(Year), y = n)) + 
  geom_chicklet(aes(fill = factor(Month)),
                width = 0.25, 
                radius = grid::unit(3, "pt")) + 
  theme_bw()+
  scale_fill_brewer(name = "Month",
                    palette = "Paired",
                    breaks = 1:12,
                    labels = month.name)+
  labs(x="",y="")
```

![image-20200323104618751](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200323104618751.png)

### **批量差异检验**

```R
##wilcox 检验批处理示例
setwd("../metagenomic/Abundance/Nr_abundance/")
library(doBy)	#使用其中的 summaryBy() 以方便按分组计算均值、中位数
rm(list = ls())
getwd()
gene <- read.table('pathway_0.01.txt', sep = '\t', row.names = 1, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
group <- read.table('sample.txt', sep = '\t', header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
gene <- gene[-nrow(gene),]
result <- NULL
for (n in 1:nrow(gene)) {
  gene_n <- data.frame(t(gene[n,subset(group, group %in% c("D", "H"))$sample]))
  gene_id <- names(gene_n)[1]
  names(gene_n)[1] <- 'gene'
  gene_n$sample <- rownames(gene_n)
  gene_n <- merge(gene_n, group, by = 'sample', all.x = TRUE)
  gene_n$group <- factor(gene_n$group)
  p_value <- wilcox.test(gene~group, gene_n,exact = FALSE)$p.value
  if (!is.na(p_value) & p_value < 0.05) {
    stat <- summaryBy(gene~group, gene_n, FUN = c(mean, sd))
    result <- rbind(result, c(gene_id, as.character(stat[1,1]), stat[1,2], stat[1,3], as.character(stat[2,1]), stat[2,2], stat[2,3], p_value))
  }
}

result <- data.frame(result)
names(result) <- c('Species', 'group1', 'mean1', 'sd1', 'group2', 'mean2', 'sd2', 'p_value')
result <- result[order(result$p_value),]  # 从小到大排序
# P.adjust  p 值校正的过程 
#先将p值那列提出转换为矩阵形式 
a <- as.matrix(result$p_value)
p_adjust <- p.adjust(a,method = 'BH',n = length(a)) # 这里使用BH 方法 和fdr结果相似
result <- cbind(result,p_adjust)
write.table(result, 'pathway_0.01_diff.txt', sep = '\t', row.names = FALSE, quote = FALSE)

```

### **绘制箱线图**

```R
#Create data
names <- c(rep("Maestro", 20) , rep("Presto", 20) , 
      rep("Nerak", 20), rep("Eskimo", 20), rep("Nairobi", 20), rep("Artiko", 20))
value <- c(  sample(3:10, 20 , replace=T) , sample(2:5, 20 , replace=T) , 
      sample(6:10, 20 , replace=T), sample(6:10, 20 , replace=T) , 
      sample(1:7, 20 , replace=T), sample(3:10, 20 , replace=T) )
data <- data.frame(names,value)

# Prepare a vector of colors with specific color for Nairobi and Eskimo
myColors <- ifelse(levels(data$names)=="Nairobi" , rgb(0.1,0.1,0.7,0.5) , 
              ifelse(levels(data$names)=="Eskimo", rgb(0.8,0.1,0.3,0.6),
              "grey90" ) )

# Build the plot
boxplot(data$value ~ data$names , 
    col=myColors , 
    ylab="disease" , xlab="- variety -")
 
# Add a legend
legend("bottomleft", legend = c("Positiv control","Negativ control") , 
    col = c(rgb(0.1,0.1,0.7,0.5) , rgb(0.8,0.1,0.3,0.6)) , bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz = FALSE, inset = c(0.03, 0.1))

#循环箱线图
par(mfrow=c(3,2), mar=c(2,2,2,2)); 分区画图，mfrow：3行2列，mar每组图的边界设置。

for(i in 1:6){
boxplot(a[(i*2):(i*2+1)],range=1.5,width=NULL,varwidth=FALSE,notch= FALSE,outline=TRUE,names(a[(i*2):(i*2+1)]),plot=TRUE,col=c("green","orange"),log="",horizontal=FALSE,add=FALSE,at=NULL);}
```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200601160101113.png" alt="image-20200601160101113" style="zoom:50%;" />

```R
#画特定物种的Boxplot

library(doBy)	#使用其中的 summaryBy() 以方便按分组计算均值、中位数
rm(list = ls())
library(dplyr)
gene <- read.table('Phylum.txt', sep = '\t', row.names = 1, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
group <- read.table('sample.txt', sep = '\t', header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

library(reshape2)	#用于排列数据
library(ggplot2)	#ggplot2 作图
gene$Taxonomy <- factor(rownames(gene), levels = rownames(gene))
gene <- melt(gene, id = 'Taxonomy')
names(group)[1] <- 'variable'
gene <- merge(gene, group, by = 'variable')

gene$Taxonomy <- factor(gene$Taxonomy)

p2 <- filter(gene,Taxonomy%in%c('g_Faecalibacterium','g_Escherichia-Shigella','g_Alloprevotella','g_Clostridium_sensu_stricto_1'))

p <- ggplot(data=p2,aes(x=group,y=value,fill=group))+
  stat_boxplot(geom = "errorbar")+geom_boxplot(outlier.colour = NA)+
  scale_fill_manual(values = c("red", "blue"))+
  xlab("")+ylab("Relative abundance")+
  facet_grid(~Taxonomy)+#legend.title = element_blank()
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black'), legend.title = element_blank(),legend.key = element_blank())
p
ggsave(plot=p,"E:/g_Alloprevotella.pdf",height=5,width=7,dpi=300)
library(export)
graph2ppt(file="2.pptx", width=7, height=5 #导入ppt中 相当于AI
          
          #可以写个函数
plotGeneCounts <- function(genes) {
 
}
那么画图就是plotGeneCounts(c("STM","KNAT1","CLV1","CLV3"))
```

### 曼哈顿图

```R
library(ggplot2)

#读取数据
otu_stat <- read.delim('otu_sign.txt', sep = '\t')

#门水平排序，这里直接按首字母排序了
otu_stat <- otu_stat[order(otu_stat$phylum), ]
otu_stat$otu_sort <- 1:nrow(otu_stat)

#其它自定义排序，例如你想根据 OTU 数量降序排序
#phylum_num <- phylum_num[order(phylum_num, decreasing = TRUE)]
#otu_stat$phylum <- factor(otu_stat$phylum, levels = names(phylum_num))
#otu_stat <- otu_stat[order(otu_stat$phylum), ]
#otu_stat$otu_sort <- 1:nrow(otu_stat)

##ggplot2 作图示例
#初始
p <- ggplot(otu_stat, aes(otu_sort, -log(p_value, 10))) +	#
geom_point(aes(size = abundance, color = phylum, shape = enrich)) +	#点的大小表示 OTUs 丰度，颜色表示分类，形状表示是否为富集 OTUs
scale_size(range = c(1, 5))+	#点大小范围
scale_shape_manual(limits = c('sign', 'no-sign'), values = c(16, 1)) +	#点形状，实心点为富集 OTUs，空心点为非富集 OTUs	
labs(x = NULL, y = '-log10(P)', size = 'relative abundance (%)', shape = 'enriched') +	#坐标轴标题、图例标题等
theme(panel.grid = element_blank(), axis.line = element_line(colour = 'black'), panel.background = element_rect(fill = 'transparent'), legend.key = element_rect(fill = 'transparent')) +	#主题调整，去除背景线等
guides(color = 'none') +	#隐藏颜色图例（OTUs 分类）
geom_hline(yintercept = -log10(0.01), color = 'gray', linetype = 2, size = 1)	#在 p=0.01 处的 -log10 位置（即 y = 2）绘制条横虚线，表示 p<0.01 为差异 OTUs 的判定标准之一

p

#计算 x 轴标签、矩形区块对应的 x 轴位置
phylum_num <- summary(otu_stat$phylum)

phylum_range <- c(0, phylum_num[1])
phylum_name <- phylum_num[1] / 2
for (i in 2:length(phylum_num)) {
	phylum_range[i+1] <- phylum_range[i] + phylum_num[i]
	phylum_name[i] <- phylum_range[i] + phylum_num[i] / 2
}

#添加 x 轴刻度标签
p <- p +
scale_x_continuous(breaks = phylum_name, labels = names(phylum_num), expand = c(0, 0)) +
theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#添加矩形区块，交替绘制为不同深度的灰色
#由于先绘制的散点，后绘制的矩形，即矩形图层在上，故需要设置很高的透明度才可
for (i in 1:(length(phylum_range) - 1)) p <- p + annotate('rect', xmin = phylum_range[i], xmax = phylum_range[i+1], ymin = -Inf, ymax = Inf, alpha = 0.1, fill = ifelse(i %% 2 == 0, 'gray60', 'gray40'))

p

##推荐先调整好背景（矩形区块），再绘制散点，这样散点位于矩形区块图层的上面，更清晰
#背景、标签布局
p <- ggplot(otu_stat, aes(otu_sort, -log(p_value, 10))) +
labs(x = NULL, y = '-log10(P)', size = 'relative abundance (%)', shape = 'significantly enriched') +
theme(panel.grid = element_blank(), axis.line = element_line(colour = 'black'), panel.background = element_rect(fill = 'transparent'), legend.key = element_rect(fill = 'transparent')) +
scale_x_continuous(breaks = phylum_name, labels = names(phylum_num), expand = c(0, 0)) +
theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#这样也没必要再为矩形设置透明度了
for (i in 1:(length(phylum_range) - 1)) p <- p + annotate('rect', xmin = phylum_range[i], xmax = phylum_range[i+1], ymin = -Inf, ymax = Inf, fill = ifelse(i %% 2 == 0, 'gray95', 'gray85'))

#最后绘制散点
p <- p + 
geom_point(aes(size = abundance, color = phylum, shape = enrich)) +
scale_size(range = c(1, 5))+
scale_shape_manual(limits = c('sign', 'no-sign'), values = c(16, 1)) +	
guides(color = 'none') +
geom_hline(yintercept = -log10(0.01), color = 'gray', linetype = 2, size = 1)

p

#输出图片至本地
ggsave('manhattan.pdf', p, width = 10, height = 5)
ggsave('manhattan.png', p, width = 10, height = 5)

##可再使用 AI、PS 等后期调整

##再来个 GWAS 的数据
#直接使用了 qqman 包中的 gwasResults 数据集
gwasResults <- qqman::gwasResults
fix(gwasResults)	#查看

#qqman 包 manhattan()绘制曼哈顿图
library(qqman)
manhattan(gwasResults, col = c('gray90', 'gray80'))

#这里借助 doBy 中的 summaryBy() 计算各染色体中 SNP 序号的中值位置，以放置染色体标签
library(doBy)

gwasResults$SNP1 <- seq(1, nrow(gwasResults), 1)
gwasResults$CHR <- factor(gwasResults$CHR, levels = unique(gwasResults$CHR))
chr <- summaryBy(SNP1~CHR, gwasResults, FUN = median)

#ggplot2 作图
p <- ggplot(gwasResults, aes(SNP1, -log(P, 10), color = CHR)) +
theme(panel.grid = element_blank(), axis.line = element_line(colour = 'black'), panel.background = element_rect(fill = 'transparent')) +
geom_point(aes(color = CHR), show.legend = FALSE) +
labs(x = 'Chromosome', y = expression(''~-log[10]~'(P)')) +
scale_x_continuous(breaks = chr$SNP1.median, labels = chr$CHR, expand = c(0, 0)) +
geom_hline(yintercept = c(5, 7), color = c('blue', 'red'), size = 0.5)

p

#保存
ggsave('GWAS.pdf', p, width = 10, height = 4)
ggsave('GWAS.png', p, width = 10, height = 4)

```

![image-20200301175957191](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200301175957191.png)

### **桑葚图**

![image-20200221143659531](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221143659531.png)

```R

#install.packages("ggalluvial")
library(ggalluvial)
library(ggplot2)
library(dplyr)
#读入LIHC临床数据
LIHC <- read.csv("TCGA_lihc.csv",header=TRUE)
#展示数据情况
head(LIHC)
summary(LIHC)


#分组计算频数
LIHCData <- group_by(LIHC,AGE,SEX,AJCC_PATHOLOGIC_TUMOR_STAGE,OS_STATUS) %>% summarise(., count = n())
#查看宽数据格式
head(LIHCData)

#绘制桑基图

ggplot(as.data.frame(LIHCData),
       aes(axis1 = AJCC_PATHOLOGIC_TUMOR_STAGE, axis2 = SEX, axis3 = AGE,
           y= count)) +
  scale_x_discrete(limits = c("AJCC_STAGE", "SEX", "AGE"), expand = c(.1, .05)) +
  geom_alluvium(aes(fill = OS_STATUS)) +
  geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
  theme_minimal() +
  ggtitle("Patients in the TCGA-LIHC cohort",
          "stratified by demographics and survival")


#2 长数据示例

#to_lodes_form生成alluvium和stratum列，主分组位于key列中
LIHC_long <- to_lodes_form(data.frame(LIHCData),
                           key = "Demographic",
                           axes = 1:3)
head(LIHC_long)

# 绘制桑基图
ggplot(data = LIHC_long,
       aes(x = Demographic, stratum = stratum, alluvium = alluvium,
           y = count, label = stratum)) +
  geom_alluvium(aes(fill = OS_STATUS)) +
  geom_stratum() + geom_text(stat = "stratum") +
  theme_minimal() +
  ggtitle("Patients in the TCGA-LIHC cohort",
          "stratified by demographics and survival")		 



#3 状态变化的趋势

data(vaccinations)
levels(vaccinations$response) <- rev(levels(vaccinations$response))
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           y = freq,
           fill = response, label = response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("vaccination survey responses at three points in time")

#4 更多细节

vignette(topic = "ggalluvial", package = "ggalluvial") 

```

![image-20200221143447073](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221143447073.png)

### **散点图**

[回归曲线添加方程](http://t-redactyl.io/blog/2016/05/creating-plots-in-r-using-ggplot2-part-11-linear-regression-plots.html)

![image-20200221143933370](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221143933370.png)

```R
setwd('./Script/scatterplot/')
rm(list = ls())
library(readxl)  # 加载readxl包
Scatter <- read.delim("111.txt",sep = '\t') # 读取Scatter数据
View(Scatter)  # 预览数据  也可运行  head(Scatter) 显示数据框前六行
names(Scatter)[1] <- 'Height'
attach(Scatter)  # 绑定Scatter数据框
opar <- par(no.readonly = TRUE) #修改图形参数
pdf("Scatter plot.pdf", width = 5, height = 5)  ## 输出名为Scatter plot，宽度和高度为5的PDF格式的图形
plot(Height,high,  # 变量为x1，y1
     xlab = "X-axis of scatter plot",  #添加X轴标签
     ylab = "Y-axis of scatter plot",  #添加Y轴标签
     main = "Scatter plot title", # 添加标题
     font.axis=2,font.lab=2,  #设置坐标轴和标签字体（2为粗体）
     cex.lab=1.30, cex.main=1.55,cex.axis=1.15,  #设置标签、标题和坐标轴缩放倍数
     family= "serif")   #设置字体为衬线，如 Time new roman字体
detach(Scatter)
par(opar)
dev.off()

#ggplot画图
library(ggplot2)
sp1 <- ggplot(Scatter, aes(x = Height, y = high, shape = sex)) + geom_point(); sp1
# 指定 Scatter 数据框和 X 和 Y 轴，将分组变量 sex 映射给shape

#添加拟合线
sp2 <- sp1 +  geom_point(size = 6, colour = "grey60") +   # 散点
  scale_shape_manual(values = c(16,17)) +   # 设置分组 散点 形状
  stat_smooth(method = lm,   # 添加线性拟合线，默认拟合 loess 曲线
              se = TRUE,  # 添加置信区间，FALSE则不添加
              level = 0.95,  # 设置CI水平
              colour = "black")  # 线的颜色，默认为蓝色
sp2

#去除背景网格
sp3 <- sp2 + theme_bw() +  # 移除背景
  theme(panel.grid=element_blank(),  # 去除网格
        panel.border=element_blank(),
        axis.line=element_line(size=1,colour="black")); sp3  # 添加 x轴和 y轴
#修改坐标轴
windowsFonts(myFont1 = windowsFont("Times New Roman"))  # 设定文字字体
sp4 <- sp3 + scale_y_continuous(expand = c(0,0))+   # y轴起点从0开始
  theme(axis.text.x = element_text(size = 10, color = "black", face = "bold", family = "myFont1")) +  # X、Y坐标轴文字格式
  theme(axis.title.x = element_text(size = 10, color = "black", face = "bold", family = "myFont1")) +
  theme(axis.text.y = element_text(size = 10, color = "black", face = "bold", family = "myFont1")) +
  theme(axis.title.y = element_text(size = 10, color = "black", face = "bold", family = "myFont1")); sp4

#添加标签
sp5 <- sp4 + geom_text(aes(y = high+0.8, label= ID),  # 将ID 映射给 label 属性，增加y的取值
                       size = 4,  # 设置标签大小
                       vjust = 0,  # 调节标签上下位置
                       hjust = 0.5,
                       color = "black",
                       family = "myFont1"); sp5  # 调节标签左右位置
ggsave("output1.tiff", sp4,  # 保存的图片文件名及格式
       width = 15, height = 12, units = "cm", # 图片宽度、高度和单位，默认输出当前图形的大小，单位英寸。
       dpi = 1200,  # 图片分辨率
       compression = "lzw")  # tiff格式压缩类型

```

[ggstar绘制星星散点图](https://mp.weixin.qq.com/s?src=11&timestamp=1585557391&ver=2247&signature=yOovH*r9fbYYqVM0C*Bw5zR7-6gXYTG8FYtcO3gicvTEVbhIfdhFMTEorSI30FiTKT6BgJooR5wKA6UvsPy0phrO02WJJrUSPqAb1U*iwBPbdddynq0l0qvrCq6u2d9J&new=1)

```R
if(!requireNamespace("remotes"))
  install.packages("remotes")
remotes::install_github("xiangpin/ggstar")#不行就多试几次
ggplot(data=iris, aes(x=Sepal.Length,y=Sepal.Width,
                           starshape=Species, fill=Species)) + geom_star(show.legend=FALSE, size=5,starshape = 1) +
  scale_fill_manual(values=c("#66C2A5", "#FC8D62", "#8DACB"))
```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200330165215068.png" alt="image-20200330165215068" style="zoom:33%;" />

#### ggsymbol绘制更多点样式

```R
library(ggplot2)
library(ggsymbol)
d <- data.frame(p=c(0:127),f=c(rep("g",26), rep("s", 7), rep("g", 95)))
d$f <- factor(d$f, levels=c("g", "s"))
p <- ggplot() + 
  geom_symbol(data=d, 
              mapping=aes(x=p%%16, y=p%/%16, symbol=p, fill=f), 
              size=4, stroke=0.5, show.legend=FALSE) +
  geom_text(data=d, 
            mapping=aes(x=p%%16, y=p%/%16+0.25, label=p), 
            size=3) +
  scale_symbol_identity() +
  scale_fill_manual(values=c("red", "blue")) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.ticks=element_blank(),
        axis.text=element_blank())
p
#例子
p <- ggplot(data=iris, aes(x=Sepal.Width,y=Sepal.Length)) +
  geom_symbol(aes(symbol=Species, fill=Species), 
              color="grey60", size=2.5,
              stroke=0.5) +
  scale_fill_manual(values=c("#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_symbol_manual(values=c(28, 10, 31))

p
```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200522163132945.png" alt="image-20200522163132945" style="zoom:50%;" />



### 箱线图和小提琴图

#### ggplot2配对散点箱线图

```
x <-  rnorm(10)
y <- rnorm(10)
d <- data.frame(x=x, y=y)
d$g <- rownames(d)
require(tidyr)
dd <- pivot_longer(d, 1:2)

require(ggplot2)
ggplot(dd, aes(name, value)) + geom_boxplot() +
  geom_line(aes(group=g), color='firebrick') +
  geom_point(aes(color=name), size=3)

t1 <- rnorm(10)
t2 <- rnorm(10) + 1
t3 <- rnorm(10) + 2
t4 <- rnorm(10) + 3

d <- data.frame(t1=t1, t2=t2, t3=t3, t4=t4)
d$g <- rownames(d)
require(tidyr)
dd <- pivot_longer(d, 1:4)
require(ggplot2)
ggplot(dd, aes(name, value)) + geom_boxplot() +
  geom_line(aes(group=g), color='firebrick') +
  geom_point(aes(color=name), size=3)
```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200612105619537.png" alt="image-20200612105619537" style="zoom:33%;" />

#### 箱线图实例

```R
#读取数据
library(reshape2)

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

```

#### ggpubr箱线图

```R
View(ToothGrowth)
library(ggplot2)
library(ggpubr)
library(ggsignif)
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
ggboxplot(ToothGrowth, #数据对象
          x = "dose", # 选择x轴用那一列数据
          y = "len", #选择y轴用什么数据
          fill = "dose", #颜色根据哪一列决定
          bxp.errorbar = T, #是否添加error bar
          bxp.errorbar.width = 0.2, #error bar的长度
          palette = "npg", #颜色风格
          add = "point" # 是否添加boxplot上面的点点
) +
  labs(title = "Effect of Vitamin C on Tooth Growth", # 添加主标题
       subtitle = "Plot of length by dose", # 添加次标记
       caption = "Data source: ToothGrowth", #添加脚注
       x = "Dose (mg)", # x轴的名字
       y = "Teeth length" # y轴的名字
  )
ggboxplot(ToothGrowth, #数据对象
          x = "dose", # 选择x轴用那一列数据
          y = "len", #选择y轴用什么数据
          fill = "dose", #颜色根据哪一列决定
          bxp.errorbar = T, #是否添加error bar
          bxp.errorbar.width = 0.2, #error bar的长度
          palette = "npg", #颜色风格
          add = "point" # 是否添加boxplot上面的点点
) +
  labs(title = "Effect of Vitamin C on Tooth Growth", # 添加主标题
       subtitle = "Plot of length by dose", # 添加次标记
       caption = "Data source: ToothGrowth", #添加脚注
       x = "Dose (mg)", # x轴的名字
       y = "Teeth length" # y轴的名字
  ) +
  geom_signif(comparisons = list(c("0.5", "1"), c("1","2"), c("0.5","2")), # 设置要对比的组
              y_position = c(34,36,38), #设置3个显著性标记的高度
              tip_length = c(0), #设置显著性那条横线两头向下的长度
              map_signif_level = T, #设置是否标记显著性的*号，还是直接标记数值
              test = t.test #设置显著性计算方式
  )



#最后就是设置线宽度和字体大小，及其他的主题theme()参数：

ggboxplot(ToothGrowth, #数据对象
          x = "dose", # 选择x轴用那一列数据
          y = "len", #选择y轴用什么数据
          fill = "dose", #颜色根据哪一列决定
          bxp.errorbar = T, #是否添加error bar
          bxp.errorbar.width = 0.2, #error bar的长度
          palette = "npg", #颜色风格
          add = "point" # 是否添加boxplot上面的点点
) +
  labs(title = "Effect of Vitamin C on Tooth Growth", # 添加主标题
       subtitle = "Plot of length by dose", # 添加次标记
       caption = "Data source: ToothGrowth", #添加脚注
       x = "Dose (mg)", # x轴的名字
       y = "Teeth length" # y轴的名字
  ) +
  geom_signif(comparisons = list(c("0.5", "1"), c("1","2"), c("0.5","2")), # 设置要对比的组
              y_position = c(34,36,38), #设置3个显著性标记的高度
              tip_length = c(0), #设置显著性那条横线两头向下的长度
              map_signif_level = T, #设置是否标记显著性的*号，还是直接标记数值
              test = t.test #设置显著性计算方式
  ) +
  theme(
    plot.title    = element_text(color = "black", size   = 16, hjust = 0.5),
    plot.subtitle = element_text(color = "black", size   = 16,hjust = 0.5),
    plot.caption  = element_text(color = "black", size   = 16,face = "italic", hjust = 1),
    axis.text.x   = element_text(color = "black", size = 16, angle = 0),
    axis.text.y   = element_text(color = "black", size = 16, angle = 0),
    axis.title.x  = element_text(color = "black", size = 16, angle = 0),
    axis.title.y  = element_text(color = "black", size = 16, angle = 90),
    legend.title  = element_text(color = "black", size  = 16),
    legend.text   = element_text(color = "black", size   = 16),
    axis.line.y = element_line(color = "black", linetype = "solid"), # y轴线特征
    axis.line.x = element_line (color = "black",linetype = "solid"), # x轴线特征
    panel.border = element_rect(linetype = "solid", size = 1.2,fill = NA) # 图四周框起来
  )

```

![image-20200221150416877](https://gitee.com/kai_kai_he/PicGo/raw/master/img/image-20200221150416877.png)

#### 分组均值连线箱线图

```
library(ggplot2)
```





### **Anosim分析**

```R
library(vegan)

#若是单纯以计算俩组beta距离值画图的话
dist.abun_D <- as.vector(vegdist(dat_genu[c(1:6),],method = 'bray'))
dist.abun_H <- as.vector(vegdist(dat_genu[c(7:12),],method = 'bray'))
sum <- data.frame(dist.abun_D,dist.abun_H)
boxplot(sum)

##读入文件
#现有的距离矩阵
dis <- read.delim('bray.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
dis <- as.dist(dis)	#将导入的样本间距离转化为 dist 类型
#或者直接使用 OTU 丰度表
otu <- read.delim('otu_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))

#样本分组文件
group <- read.delim('group.txt', sep = '\t', stringsAsFactors = FALSE)

##ANOSIM 分析（所有分组间比较，即整体差异）
#（1）若是已经提供好了距离矩阵，则直接使用现有的距离矩阵进行分析即可
anosim_result_dis <- anosim(dis, group$site, permutations = 999)	#根据 group$site 这一列样本分组信息进行 ANOSIM 分析，随机置换检验 999 次

#（2）若是使用 OTU 丰度表，则需要在计算时指定所依据的距离类型，这里依然使用 Bray-Curtis 距离
anosim_result_otu <- anosim(otu, group$site, permutations = 999, distance = 'bray') 	#这条命令的详情和上述命令所表示的信息一致

#（3）或者首先根据丰度表计算样本距离，在将所的距离数据作为输入
dis1 <- vegdist(otu, method = 'bray')
anosim_result_dis1 <- anosim(dis1, group$site, permutations = 999)	#同上所述

#查看结果，上述 3 条命令所计算的内容一致，以其中一个为例
anosim_result_dis
#或者
summary(anosim_result_dis)
#或者
names(anosim_result_dis)
anosim_result_dis$signif	#p 值
anosim_result_dis$statistic	#R 值

#作图展示
#pdf(paste('anosim.all.pdf', sep = ''), width = 10, height = 5)
png(paste('anosim.all.png', sep = ''), width = 800, height = 400)
plot(anosim_result_dis, col = c('gray', 'red', 'green', 'blue', 'orange', 'purple'))
dev.off()

##ANOSIM 分析（使用循环处理，进行小分组间比较，如两组间）
#推荐使用 OTU 丰度表作为输入数据，每次筛选分组后重新计算样本距离，避免由于样本数减少可能导致的距离变动而造成误差
group_name <- unique(group$site)

dir.create('anosim_two', recursive = TRUE)
anosim_result_two <- NULL
for (i in 1:(length(group_name) - 1)) {
	for (j in (i + 1):length(group_name)) {
		group_ij <- subset(group, site %in% c(group_name[i], group_name[j]))
		otu_ij <- otu[group_ij$names, ]
		anosim_result_otu_ij <- anosim(otu_ij, group_ij$site, permutations = 999, distance = 'bray')	#随机置换检验 999 次
		
		#每次循环提取 R 值和 p 值（同时标记 “*” 显著性）
		if (anosim_result_otu_ij$signif <= 0.001) Sig <- '***'
		else if (anosim_result_otu_ij$signif <= 0.01) Sig <- '**'
		else if (anosim_result_otu_ij$signif <= 0.05) Sig <- '*'
		else Sig <- NA
		anosim_result_two <- rbind(anosim_result_two, c(paste(group_name[i], group_name[j], sep = '/'), 'Bray-Curtis', anosim_result_otu_ij$statistic, anosim_result_otu_ij$signif, Sig))
		
		#每次循环输出图片
		#pdf(paste('anosim_two/anosim.', group_name[i], '_', group_name[j], '.pdf', sep = ''), width = 7, height = 5)
		png(paste('anosim_two/anosim.', group_name[i], '_', group_name[j], '.png', sep = ''), width = 600, height = 400)
		plot(anosim_result_otu_ij, col = c('gray', 'red', 'blue'))
		dev.off()
	}
}

#输出带 R 值和 p 值的表格
anosim_result_two <- data.frame(anosim_result_two, stringsAsFactors = FALSE)
names(anosim_result_two) <- c('group', 'distance', 'R', 'P_value', 'Sig')
write.table(anosim_result_two, 'anosim_two/ANOSIM.result_two.txt', row.names = FALSE, sep = '\t', quote = FALSE, na = '')

```

![image-20200221144316760](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221144316760.png)





### **分面柱状图**

![image-20200221150015602](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221150015602.png)

```R
library(ggplot2)
library(reshape2)
library(dplyr)
data_m <- read.table(text=data_ori, header=T, sep=";", quote="")
head(data_m)
#首先看下每个基因在不同组的表达情况, facet_grid和facet_wrap可以对图形分面显示。
# scales: free_y 表示不同子图之间使用独立的Y轴信息
#         但x轴使用同样的信息。
#         其它可选参数有free_x, free, fixed
p <- ggplot(data_m, aes(x=Gene, y=Expr)) + 
  geom_bar(stat="identity", position="dodge", aes(fill=Group)) +
  facet_grid(Condition~., scales="free_y")
p
# 如果没有图形界面，运行下面的语句把图存在工作目录下的Rplots.pdf文件中
#dev.off()
#柱子有点多，也可以利用mean±SD的形式展现
# 获取平均值和标准差
# 分组时不只Gene一个变量了，还需要考虑Condition
data_m_sd_mean <- data_m %>% group_by(Gene, Condition) %>% dplyr::summarise(sd=sd(Expr), value=mean(Expr))
data_m_sd_mean <- as.data.frame(data_m_sd_mean)
data_m_sd_mean
p <- ggplot(data_m_sd_mean, aes(x=Gene, y=value)) + 
  geom_bar(stat="identity", aes(fill=Gene)) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(width=0.75)) +
  facet_wrap(~Condition, ncol=1)
p
#每组里面各个基因的相对表达, 纵轴的显示改为百分比
# position="fill" 展示的是堆积柱状图各部分的相对比例
# position="stack" 展示的是堆积柱状图的原始值，可以自己体现下看卡差别
p <- ggplot(data_m, aes(x=Group, y=Expr)) +
  geom_bar(stat="identity", position="fill", aes(fill=Gene)) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~Condition, ncol=1)
p
#在柱子中标记百分比值 (计算百分比值需要注意了, 文本显示位置还是跟之前一致)
# group_by: 按照给定的变量分组，然后按组操作
# mutate: 在当前数据表增加新变量
# 第一步增加每个组 (Group和Condition共同定义分组)的加和，第二步计算比例
data_m <- data_m %>% group_by(Group, Condition) %>% mutate(count=sum(Expr)) %>% mutate(freq=round(100*Expr/count,2))
p <- ggplot(data_m, aes(x=Group, y=Expr, group=Group)) +
  geom_bar(stat="identity", position="fill", aes(fill=Gene)) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label=freq), position=position_fill(vjust=0.5)) +
  facet_wrap(~Condition, ncol=1)
p
#文本显示位置没有问题，但柱子的位置有些奇怪，使得两组之间不可比。


#先对数据做下排序，然后再标记文本
# with: 产生一个由data_m组成的局部环境，再这个环境里，列名字可以直接使用
data_m <- data_m[with(data_m, order(Condition, Group, Gene)),] 
p <- ggplot(data_m, aes(x=Group, y=Expr, group=Group)) +
  geom_bar(stat="identity", position="fill", aes(fill=Gene)) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label=freq), position=position_fill(vjust=0.5)) +
  facet_wrap(~Condition, ncol=2)
p

```

![image-20200221145852209](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200221145852209.png)

### **ID转换**

```R
#source("https://bioconductor.org/biocLite.R") 
#biocLite('org.Hs.eg.db')
#基因的entrez ID 跟symbol等其它ID的转换程序 
#http://www.bio-info-trainee.com/710.html
keytypes(org.Hs.eg.db)
geneIDannotation <- function(geneLists=c(1,2,9),name=T,map=T,ensemble=F,accnum=F){
  ## input ID type : So far I just accept entrezID or symbol
  ## default, we will annotate the entrezID and symbol, chromosone location and gene name 
  
  suppressMessages(library("org.Hs.eg.db"))
  all_EG=mappedkeys(org.Hs.egSYMBOL) 
  EG2Symbol=toTable(org.Hs.egSYMBOL)
  if( all(! geneLists %in% all_EG) ){
    inputType='symbol'
    geneLists=data.frame(symbol=geneLists)
    results=merge(geneLists,EG2Symbol,by='symbol',all.x=T)
  }else{
    inputType='entrezID'
    geneLists=data.frame(gene_id=geneLists)
    results=merge(geneLists,EG2Symbol,by='gene_id',all.x=T)
  }
  
  if ( name ){
    EG2name=toTable(org.Hs.egGENENAME)
    results=merge(results,EG2name,by='gene_id',all.x=T)
  }
  if(map){
    EG2MAP=toTable(org.Hs.egMAP)
    results=merge(results,EG2MAP,by='gene_id',all.x=T)
  }
  if(ensemble){
    EG2ENSEMBL=toTable(org.Hs.egENSEMBL)
    results=merge(results,EG2ENSEMBL,by='gene_id',all.x=T)
  }
  if(accnum){
    EG2accnum=toTable(org.Hs.egREFSEQ) 
    results=merge(results,EG2MAP,by='gene_id',all.x=T)
  }
  return(results)
}
geneIDannotation()
geneIDannotation(c('TP53','BRCA1','KRAS','PTEN'))           
```



### 稀释、累积曲线和Rank_abundance

```R
#需要用到的 R 包
library(vegan)	#用于计算 Shannon 熵指数、Simpson 指数、Chao1 指数、ACE 指数等，同时用于抽样
library(picante)	#用于计算 PD_whole_tree，若不计算它就无需加载。事实上，picante 包加载时默认同时加载 vegan
library(ggplot2)	#用于 ggplot2 作图
library(doBy)	#用于分组统计（第 97 行使用到）
library(ggalt)	#用于绘制拟合曲线（第 138 行使用到）
library(BiodiversityR)	#用于绘制 Rank-abundance 曲线（第 160 行使用到）

##定义函数
#计算多种 Alpha 多样性指数，结果返回至向量
#各子函数的用法详见 http://blog.sciencenet.cn/blog-3406804-1179983.html
alpha_index <- function(x, method = 'richness', tree = NULL, base = exp(1)) {
	if (method == 'richness') result <- rowSums(x > 0)	#丰富度指数
	else if (method == 'chao1') result <- estimateR(x)[3, ]	#Chao1 指数
	else if (method == 'ace') result <- estimateR(x)[5, ]	#ACE 指数
	else if (method == 'shannon') result <- diversity(x, index = 'shannon', base = base)	#Shannon 指数
	else if (method == 'simpson') result <- diversity(x, index = 'simpson')	#Gini-Simpson 指数
	else if (method == 'pielou') result <- diversity(x, index = 'shannon', base = base) / log(estimateR(x)[1, ], base)	#Pielou 均匀度
	else if (method == 'gc') result <- 1 - rowSums(x == 1) / rowSums(x)	#goods_coverage
	else if (method == 'pd' & !is.null(tree)) {	#PD_whole_tree
		pd <- pd(x, tree, include.root = FALSE)
		result <- pd[ ,1]
		names(result) <- rownames(pd)
	}
	result
}

#根据抽样步长（step），统计每个稀释梯度下的 Alpha 多样性指数，结果返回至列表
alpha_curves <- function(x, step, method = 'richness', rare = NULL, tree = NULL, base = exp(1)) {
	x_nrow <- nrow(x)
	if (is.null(rare)) rare <- rowSums(x) else rare <- rep(rare, x_nrow)
	alpha_rare <- list()
	
	for (i in 1:x_nrow) {
		step_num <- seq(0, rare[i], step)
		if (max(step_num) < rare[i]) step_num <- c(step_num, rare[i])
		
		alpha_rare_i <- NULL
		for (step_num_n in step_num) alpha_rare_i <- c(alpha_rare_i, alpha_index(x = rrarefy(x[i, ], step_num_n), method = method, tree = tree, base = base))
		names(alpha_rare_i) <- step_num
		alpha_rare <- c(alpha_rare, list(alpha_rare_i))
	}
	
	names(alpha_rare) <- rownames(x)
	alpha_rare
}

#读取 OTU 丰度表
otu <- read.delim('otu_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- t(otu)
#最简单的用vegan包中自带的rarecurve函数制作
rarecurve(otu, step=10, col = factor(rownames(otu)),lwd=2, ylab="ASVs")
##测试

#统计 OTU 丰度表中各样本的 Shannon 指数，对数底数使用 e
shannon_index <- alpha_index(otu, method = 'shannon', base = exp(1))
#以 1000 条序列为抽样步长，依次对 OTU 表稀释抽样，直到最大序列深度；并统计各抽样梯度下的 OTU 丰度表中各样本的 Shannon 指数，对数底数使用 e
shannon_curves <- alpha_curves(otu, step = 1000, method = 'shannon', base = exp(1))

##以下以物种丰富度指数为例绘制 Alpha 多样性曲线（当为丰富度指数时，另一个名称即为常说的稀释曲线，或物种累计曲线）

#以 2000 步长（step=2000）为例统计
richness_curves <- alpha_curves(otu, step = 2000, method = 'richness')

#获得 ggplot2 作图文件
plot_richness <- data.frame()
for (i in names(richness_curves)) {
	richness_curves_i <- (richness_curves[[i]])
	richness_curves_i <- data.frame(rare = names(richness_curves_i), alpha = richness_curves_i, sample = i, stringsAsFactors = FALSE)
	plot_richness <- rbind(plot_richness, richness_curves_i)
}

rownames(plot_richness) <- NULL
plot_richness$rare <- as.numeric(plot_richness$rare)
plot_richness$alpha <- as.numeric(plot_richness$alpha)

#ggplot2 作图
ggplot(plot_richness, aes(rare, alpha, color = sample)) +
geom_line() +
labs(x = 'Number of sequences', y = 'Richness', color = NULL) +
theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black'), legend.key = element_rect(fill = 'transparent')) +
geom_vline(xintercept = min(rowSums(otu)), linetype = 2) +
scale_x_continuous(breaks = seq(0, 30000, 5000), labels = as.character(seq(0, 30000, 5000)))

##多计算几次以获取均值 ± 标准差，然后再展示出也是一个不错的选择
#重复抽样 5 次
plot_richness <- data.frame()

for (n in 1:5) {
	richness_curves <- alpha_curves(otu, step = 2000, method = 'richness')
	
	for (i in names(richness_curves)) {
		richness_curves_i <- (richness_curves[[i]])
		richness_curves_i <- data.frame(rare = names(richness_curves_i), alpha = richness_curves_i, sample = i, stringsAsFactors = FALSE)
		plot_richness <- rbind(plot_richness, richness_curves_i)
	}
}

#计算均值 ± 标准差（doBy 包中的 summaryBy() 函数）
plot_richness_stat <- summaryBy(alpha~sample+rare, plot_richness, FUN = c(mean, sd))
plot_richness_stat$rare <- as.numeric(plot_richness_stat$rare)
plot_richness_stat[which(plot_richness_stat$rare == 0),'alpha.sd'] <- NA

#ggplot2 作图
ggplot(plot_richness_stat, aes(rare, alpha.mean, color = sample)) +
geom_line() +
geom_point() +
geom_errorbar(aes(ymin = alpha.mean - alpha.sd, ymax = alpha.mean + alpha.sd), width = 500) +
labs(x = 'Number of sequences', y = 'Richness', color = NULL) +
theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black'), legend.key = element_rect(fill = 'transparent')) +
geom_vline(xintercept = min(rowSums(otu)), linetype = 2) +
scale_x_continuous(breaks = seq(0, 30000, 5000), labels = as.character(seq(0, 30000, 5000)))

##对于 Shannon 指数等，方法类似
#以 2000 步长（step=2000）为例统计每个稀释梯度下的 Shannon 指数，Shannon 公式的对数底数默认为 e，若有需要可更改（例如 2）
shannon_curves <- alpha_curves(otu, step = 2000, method = 'shannon', base = 2)

#获得 ggplot2 作图文件（略，参见上述）
#ggplot2 作图（略，参见上述）

##若简单的“geom_line()”样式波动幅度过大，不平滑等，可以尝试拟合曲线的样式
#获得作图数据。前面多生成一个点，使得 Shannon 拟合曲线更加平滑（你把 shannon_curves1 注释掉就知道我说的啥了）
shannon_curves1 <- alpha_curves(otu, step = 200, rare = 200, method = 'shannon')
shannon_curves2 <- alpha_curves(otu, step = 2000, method = 'shannon')
shannon_curves <- c(shannon_curves1, shannon_curves2)

plot_shannon <- data.frame()
for (i in 1:length(shannon_curves)) {
	shannon_curves_i <- shannon_curves[[i]]
	shannon_curves_i <- data.frame(rare = names(shannon_curves_i), alpha = shannon_curves_i, sample = names(shannon_curves)[i], stringsAsFactors = FALSE)
	plot_shannon <- rbind(plot_shannon, shannon_curves_i)
}

rownames(plot_shannon) <- NULL
plot_shannon$rare <- as.numeric(plot_shannon$rare)
plot_shannon$alpha <- as.numeric(plot_shannon$alpha)
plot_shannon <- plot_shannon[order(plot_shannon$sample, plot_shannon$rare), ]

#ggplot2 作图（使用到 ggalt 包的 geom_xspline() 绘制平滑拟合线）
ggplot(plot_shannon, aes(rare, alpha, color = sample)) +
geom_xspline() +
labs(x = 'Number of sequences', y = 'Shannon', color = NULL) +
theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black'), legend.key = element_rect(fill = 'transparent')) +
geom_vline(xintercept = min(rowSums(otu)), linetype = 2) +
scale_x_continuous(breaks = seq(0, 30000, 5000), labels = as.character(seq(0, 30000, 5000)))

##对于 PD_whole_tree，除了 OTU 丰度表，还使用到进化树文件
#加载 OTU 丰度表和进化树文件
otu <- read.delim('otu_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- t(otu)
tree <- read.tree('otu_tree.tre')

#以 2000 步长（step=2000）为例统计
pd_curves <- alpha_curves(otu, tree = tree, step = 2000, method = 'pd')

#统计及做图方法同上述

##Rank-abundance 曲线
#统计（BiodiversityR 包 rankabundance() 实现 OTU 排序）
otu_relative <- otu / rowSums(otu)
rank_dat <- data.frame()
for (i in rownames(otu_relative)) {
	rank_dat_i <- data.frame(rankabundance(subset(otu_relative, rownames(otu_relative) == i), digits = 6))[1:2]
	rank_dat_i$sample <- i
	rank_dat <- rbind(rank_dat, rank_dat_i)
}
rank_dat <- subset(rank_dat, rank_dat$abundance != 0)

#ggplot2 作图
ggplot(rank_dat, aes(rank, log(abundance, 10), color = sample)) +
geom_line() +
labs(x = 'OTUs rank', y = 'Relative adundance (%)', color = NULL) +
theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black'), legend.key = element_rect(fill = 'transparent')) +
scale_y_continuous(breaks = 0:-5, labels = c('100', '10', '1', '0.1', '0.01', '0.001'), limits = c(-5, 0))

```

```R
#累积曲线
BCI这个数据，它的每一行代表了一个样本，不同样本采样的地点不同，每一列是1个物种的丰度
library(vegan)
data(BCI)
sp1 <- specaccum(BCI, method="random")
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp1, col="yellow", add=TRUE, pch="+")
```

![image-20200413154323602](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200413154323602.png)

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200413154641666.png" alt="image-20200413154641666" style="zoom:50%;" />

### pls_da最小二乘法分析

```R
library(mixOmics)
library(ggplot2)

##读入文件
#门水平丰度表
phylum <- read.delim('phylum_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
phylum <- data.frame(t(phylum))

#样本分组文件
group <- read.delim('group.txt', sep = '\t', stringsAsFactors = FALSE)

##PLS-DA 分析
#基于门水平丰度表，只展示前 3 个排序轴
phylum <- phylum[group$names, ]
plsda_result <-plsda(phylum, group$group, ncomp = 3)

#简要查看结果
plsda_result
#或
names(plsda_result)

#查看排序轴解释量
plsda_result$explained_variance$X
#查看样本排序坐标
plsda_result$variates$X
#查看细菌门类群排序坐标
plsda_result$loadings$X

#使用 plotIndiv() 绘制 PLS-DA 分析结果
plotIndiv(plsda_result, ind.names = TRUE, style = 'ggplot2')
plotIndiv(plsda_result, ind.names = TRUE, style = '3d')

#提取坐标轴解释量（前两轴）
plsda_result_eig <- {plsda_result$explained_variance$X}[1:2]

#提取样本点坐标（前两轴）
sample_site <- data.frame(plsda_result$variates)[1:2]

#为样本点坐标添加分组信息
sample_site$names <- rownames(sample_site)
names(sample_site)[1:2] <- c('plsda1', 'plsda2')
sample_site <- merge(sample_site, group, by = 'names', all.x = TRUE)

#可选输出各样本的 PLS-DA 分析结果
write.table(sample_site, 'plsda_sample.txt', row.names = FALSE, sep = '\t', quote = FALSE)

#使用 ggplot2 简单绘制 PLS-DA 结果图
plsda_plot <- ggplot(sample_site, aes(plsda1, plsda2, color = group, label = names)) +
geom_point(size = 1.5, alpha = 0.6) + 
stat_ellipse(show.legend = FALSE) +	#添加 95% 置信椭圆
scale_color_manual(values = c('#1D7ACC', '#F67433', '#00815F')) +
theme(panel.grid = element_line(color = 'grey50'), panel.background = element_rect(color = 'black', fill = 'transparent')) + 
theme(legend.title = element_blank(), legend.key = element_rect(fill = 'transparent')) +
labs(x = paste('PLS-DA axis1 ( explained variance ', round(100 * plsda_result_eig[1], 2), '% )', sep = ''), y = paste('PLS-DA axis2 ( explained variance ', round(100 * plsda_result_eig[2], 2), '% )', sep = ''))

#ggsave('plsda_plot.pdf', plsda_plot, width = 6, height = 5)
ggsave('plsda_plot.png', plsda_plot, width = 6, height = 5)
```

### 物种丰度关联弦图

```R
#!/usr/bin/env Rscript

##加载R包，初始传递命令
library(circlize) #使用该包绘制 circos 图
library(reshape2) #在某步排列表格用（第 48 行“plot_data <- melt(otu_table, id = 'otu_ID')”）
library(ComplexHeatmap) #可用此包添加图例
library(grid) #可用此包调整画板

otu_table_file <- 'otu_table.txt'
group_file <- 'group.txt'
taxonomy_file <- 'taxonomy.txt'
color_otu <- c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462', '#B3DE69', '#FCCDE5', '#BC80BD', '#CCEBC5', '#FFED6F', '#E41A1C', '#377EB8', '#4DAF4A', '#984EA3', '#FF7F00', '#FFFF33', '#A65628', '#F781BF', '#66C2A5')
color_sample <- c('#6181BD', '#F34800', '#64A10E', '#FF00FF', '#c7475b', '#049a0b')
color_phylum <- c('#BEAED4', '#FDC086', '#FFFF99', '#386CB0', '#F0027F')
color_group <- c('#4253ff', '#ff4308')

####预处理
##读取数据
#依据 taxonomy_file 的内容，获取“OTU/分类”排序
taxonomy <- read.delim(taxonomy_file, sep = '\t', stringsAsFactors = F)
tax_phylum <- unique(taxonomy$phylum)
taxonomy$phylum <- factor(taxonomy$phylum, levels = tax_phylum)
all_otu <- taxonomy$OTU_ID
taxonomy$OTU_ID <- factor(taxonomy$OTU_ID, levels = all_otu)

#依据 group_file 的内容，获取“样本/分组”排序
group <- read.delim(group_file, sep = '\t', stringsAsFactors = F)
all_group <- unique(group$group_ID)
group$group_ID <- factor(group$group_ID, levels = all_group)
all_sample <- group$sample_ID

#基于上述排序结果，预处理 otu_table_file
otu_table <- read.delim(otu_table_file, sep = '\t')
otu_table <- merge(taxonomy, otu_table, by = 'OTU_ID')
otu_table <- otu_table[order(otu_table$phylum, otu_table$OTU_ID), ]
rownames(otu_table) <- otu_table$OTU_ID
otu_table <- otu_table[all_sample]

##生成绘图文件
#circlize 外圈属性数据
all_ID <- c(all_otu, all_sample)
accum_otu <- rowSums(otu_table)
accum_sample <- colSums(otu_table)
all_ID_xlim <- cbind(rep(0, length(all_ID)),data.frame(c(accum_otu, accum_sample)))

#circlize 内圈连线数据
otu_table$otu_ID <- all_otu
plot_data <- melt(otu_table, id = 'otu_ID') #此处使用了reshape2包中的melt()命令
colnames(plot_data)[2] <- 'sample_ID'
plot_data$otu_ID <- factor(plot_data$otu_ID, levels = all_otu)
plot_data$sample_ID <- factor(plot_data$sample_ID, levels = all_sample)
plot_data <- plot_data[order(plot_data$otu_ID, plot_data$sample_ID), ]
plot_data <- plot_data[c(2, 1, 3, 3)]

#颜色设置
names(color_otu) <- all_otu
names(color_sample) <- all_sample

####circlize 绘图
pdf('circlize_plot.pdf', width = 20, height = 8)
circle_size = unit(1, 'snpc')

##整体布局
gap_size <- c(rep(3, length(all_otu) - 1), 6, rep(3, length(all_sample) - 1), 6)
circos.par(cell.padding = c(0, 0, 0, 0), start.degree = 270, gap.degree = gap_size)
circos.initialize(factors = factor(all_ID, levels = all_ID), xlim = all_ID_xlim)

##绘制 OTU 分类、样本分组区块（第一圈）
circos.trackPlotRegion(
	ylim = c(0, 1), track.height = 0.03, bg.border = NA, 
	panel.fun = function(x, y) {
		sector.index = get.cell.meta.data('sector.index')
		xlim = get.cell.meta.data('xlim')
		ylim = get.cell.meta.data('ylim')
	} )

for (i in 1:length(tax_phylum)) {
	tax_OTU <- {subset(taxonomy, phylum == tax_phylum[i])}$OTU_ID
	highlight.sector(tax_OTU, track.index = 1, col = color_phylum[i], text = tax_phylum[i], cex = 0.5, text.col = 'black', niceFacing = FALSE)
}

for (i in 1:length(all_group)) {
	group_sample <- {subset(group, group_ID == all_group[i])}$sample_ID
	highlight.sector(group_sample, track.index = 1, col = color_group[i], text = all_group[i], cex = 0.7, text.col = 'black', niceFacing = FALSE)
}

##各 OTU、样本绘制区
#添加百分比注释（第二圈）
circos.trackPlotRegion(
	ylim = c(0, 1), track.height = 0.05, bg.border = NA, 
	panel.fun = function(x, y) {
		sector.index = get.cell.meta.data('sector.index')
		xlim = get.cell.meta.data('xlim')
		ylim = get.cell.meta.data('ylim')
	} )

circos.track(
	track.index = 2, bg.border = NA, 
	panel.fun = function(x, y) {
		xlim = get.cell.meta.data('xlim')
		ylim = get.cell.meta.data('ylim')
		sector.name = get.cell.meta.data('sector.index')
		xplot = get.cell.meta.data('xplot')
		
		by = ifelse(abs(xplot[2] - xplot[1]) > 30, 0.25, 1)
		for (p in c(0, seq(by, 1, by = by))) circos.text(p*(xlim[2] - xlim[1]) + xlim[1], mean(ylim) + 0.4, paste0(p*100, '%'), cex = 0.4, adj = c(0.5, 0), niceFacing = FALSE)
		
		circos.lines(xlim, c(mean(ylim), mean(ylim)), lty = 3)
	} )

#绘制 OTU、样本主区块（第三圈）
circos.trackPlotRegion(
	ylim = c(0, 1), track.height = 0.03, bg.col = c(color_otu, color_sample), bg.border = NA, track.margin = c(0, 0.01),
	panel.fun = function(x, y) {
		xlim = get.cell.meta.data('xlim')
		sector.name = get.cell.meta.data('sector.index')
		circos.axis(h = 'top', labels.cex = 0.4, major.tick.percentage = 0.4, labels.niceFacing = FALSE)
		circos.text(mean(xlim), 0.2, sector.name, cex = 0.4, niceFacing = FALSE, adj = c(0.5, 0))
	} )

#绘制 OTU、样本副区块（第四圈）
circos.trackPlotRegion(ylim = c(0, 1), track.height = 0.03, track.margin = c(0, 0.01))

##绘制 OTU-样本关联连线（最内圈）
for (i in seq_len(nrow(plot_data))) {
	circos.link(
		plot_data[i,2], c(accum_otu[plot_data[i,2]], accum_otu[plot_data[i,2]] - plot_data[i,4]),
		plot_data[i,1], c(accum_sample[plot_data[i,1]], accum_sample[plot_data[i,1]] - plot_data[i,3]),
		col = paste0(color_otu[plot_data[i,2]], '70'), border = NA )
	
	circos.rect(accum_otu[plot_data[i,2]], 0, accum_otu[plot_data[i,2]] - plot_data[i,4], 1, sector.index = plot_data[i,2], col = color_sample[plot_data[i,1]], border = NA)
	circos.rect(accum_sample[plot_data[i,1]], 0, accum_sample[plot_data[i,1]] - plot_data[i,3], 1, sector.index = plot_data[i,1], col = color_otu[plot_data[i,2]], border = NA)
	
	accum_otu[plot_data[i,2]] = accum_otu[plot_data[i,2]] - plot_data[i,4]
	accum_sample[plot_data[i,1]] = accum_sample[plot_data[i,1]] - plot_data[i,3]
}

##添加图例
otu_legend <- Legend(
		at = all_otu, labels = taxonomy$detail, labels_gp = gpar(fontsize = 8),    
		grid_height = unit(0.5, 'cm'), grid_width = unit(0.5, 'cm'), type = 'points', pch = NA, background = color_otu)

pushViewport(viewport(x = 0.85, y = 0.5))
grid.draw(otu_legend)
upViewport()
		
##清除 circlize 样式并关闭画板
circos.clear()
dev.off()

```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200413154751166.png" alt="image-20200413154751166" style="zoom:33%;" />

### 绘制细菌基因组重测序变异圈图

```R
#!/usr/bin/env Rscript

##加载 R 包 & 初始传递命令
library(stringr)	#方便处理字符串
library(circlize)	#绘制圈图
library(ComplexHeatmap)	#绘制图例
library(grid)	#组合画板，圈图 + 图例

##命令传递
sample_name <- 'Bacillus_subtilis'	#测序样本名称
ref_name <- 'Bacillus_subtilis str168'	#参考基因组名称

genome_gff <- 'Bacillus_subtilis.str168.gff'	#参考基因组 gff 注释文件
snp_vcf <- 'Bacillus_subtilis.snp.vcf'	#SNP 检测结果 vcf 文件
indel_vcf <- 'Bacillus_subtilis.indel.vcf'	#InDel 检测结果 vcf 文件
cnv_cnv <- 'Bacillus_subtilis.cnv'	#cnv 检测结果文件
sv_ctx <- 'Bacillus_subtilis.ctx'	#sv 检测结果文件
depth_base_stat <- 'Bacillus_subtilis.depth_base.txt'	#测序深度、碱基含量统计结果文件
seq_split <- 2000	#滑窗大小，与 depth_base_stat 中使用的滑窗大小对应

out_dir = 'output'	#生成一个目录，用于存放结果文件
if (!file.exists(out_dir)) dir.create(out_dir)

#####################
##参考基因组长度、GC 统计 & 测序覆盖度、深度统计
depth_base <- read.delim(depth_base_stat, stringsAsFactors = FALSE)
genome_size <- sum(depth_base$seq_end - depth_base$seq_start + 1) 
genome_GC <- round(mean(depth_base$GC), 2)

depth_exist <- subset(depth_base, depth != 0)
coverage <- round(100 * sum(depth_exist$seq_end - depth_exist$seq_start + 1) / genome_size, 2)
average_depth <- round(mean(depth_base$depth), 0)

seq_stat <- NULL
for (seq_id in unique(depth_base$seq_ID)) seq_stat <- rbind(seq_stat, c(seq_id, 1, max(subset(depth_base, seq_ID == seq_id)$seq_end)))
seq_stat <- data.frame(seq_stat, stringsAsFactors = FALSE)
colnames(seq_stat) <- c('seq_ID', 'seq_start', 'seq_end')
rownames(seq_stat) <- seq_stat$seq_ID
seq_stat$seq_start <- as.numeric(seq_stat$seq_start)
seq_stat$seq_end <- as.numeric(seq_stat$seq_end)

write.table(seq_stat, str_c(out_dir, '/', sample_name, '.genome_stat.txt'), row.names = FALSE, sep = '\t', quote = FALSE)

##参考基因组基因信息，CDS & rRNA & tRNA
gene <- read.delim(genome_gff, header = FALSE, stringsAsFactors = FALSE, comment.char = '#')[c(1, 3, 4, 5, 7)]
gene <- subset(gene, V3 %in% c('CDS', 'rRNA', 'tRNA'))[c(1, 3, 4, 2, 5)]
names(gene) <- c('seq_ID', 'seq_start', 'seq_end', 'type', 'strand')

gene[which(gene$type == 'CDS'),'type'] <- 1
gene[which(gene$type == 'rRNA'),'type'] <- 2
gene[which(gene$type == 'tRNA'),'type'] <- 3
gene$type <- as.numeric(gene$type)
gene <- list(subset(gene, strand == '-')[-5], subset(gene, strand == '+')[-5])

##读取 SNP 检测结果
#读取 vcf 文件，统计 SNP 类型
snp <- read.delim(snp_vcf, header = FALSE, colClasses = 'character', comment.char = '#')[c(1, 2, 4, 5)]
snp$V2 <- as.numeric(snp$V2)
snp$change <- str_c(snp$V4, snp$V5)

change <- which(snp$change == 'AT')
snp[change,'type1'] <- 'A>T|T>A'; snp[change,'type2'] <- 'tv'
change <- which(snp$change == 'AG')
snp[change,'type1'] <- 'A>G|T>C'; snp[change,'type2'] <- 'ti'
change <- which(snp$change == 'AC')
snp[change,'type1'] <- 'A>C|T>G'; snp[change,'type2'] <- 'tv'

change <- which(snp$change == 'TA')
snp[change,'type1'] <- 'A>T|T>A'; snp[change,'type2'] <- 'tv'
change <- which(snp$change == 'TG')
snp[change,'type1'] <- 'A>C|T>G'; snp[change,'type2'] <- 'tv'
change <- which(snp$change == 'TC')
snp[change,'type1'] <- 'A>G|T>C'; snp[change,'type2'] <- 'ti'

change <- which(snp$change == 'GA')
snp[change,'type1'] <- 'G>A|C>T'; snp[change,'type2'] <- 'ti'
change <- which(snp$change == 'GT')
snp[change,'type1'] <- 'G>T|C>A'; snp[change,'type2'] <- 'tv'
change <- which(snp$change == 'GC')
snp[change,'type1'] <- 'G>C|C>G'; snp[change,'type2'] <- 'tv'

change <- which(snp$change == 'CA')
snp[change,'type1'] <- 'G>T|C>A'; snp[change,'type2'] <- 'tv'
change <- which(snp$change == 'CT')
snp[change,'type1'] <- 'G>A|C>T'; snp[change,'type2'] <- 'ti'
change <- which(snp$change == 'CG')
snp[change,'type1'] <- 'G>C|C>G'; snp[change,'type2'] <- 'tv'

snp_ti <- length(which(snp$type2 == 'ti'))
snp_tv <- length(which(snp$type2 == 'tv'))

snp_at <- length(which(snp$type1 == 'A>T|T>A'))
snp_ag <- length(which(snp$type1 == 'A>G|T>C'))
snp_ac <- length(which(snp$type1 == 'A>C|T>G'))
snp_ga <- length(which(snp$type1 == 'G>A|C>T'))
snp_gt <- length(which(snp$type1 == 'G>T|C>A'))
snp_gc <- length(which(snp$type1 == 'G>C|C>G'))

#统计 SNP 密度
snp <- snp[c(1, 2, 5, 6, 7)]
colnames(snp)[1:2] <- c('seq_ID', 'seq_site')

snp_stat <- NULL
seq_ID <- unique(snp$seq_ID)

for (seq_ID_n in seq_ID) {
	snp_subset <- subset(snp, seq_ID == seq_ID_n)
	seq_end <- seq_split
	snp_num <- 0
	
	for (i in 1:nrow(snp_subset)) {
		if (snp_subset[i,'seq_site'] <= seq_end) snp_num <- snp_num + 1
		else {
			snp_stat <- rbind(snp_stat, c(seq_ID_n, seq_end - seq_split + 1, seq_end, snp_num))
			
			seq_end <- seq_end + seq_split
			snp_num <- 0
			while (snp_subset[i,'seq_site'] > seq_end) {
				snp_stat <- rbind(snp_stat, c(seq_ID_n, seq_end - seq_split + 1, seq_end, snp_num))
				seq_end <- seq_end + seq_split
			}
			snp_num <- snp_num + 1
		}
	}
	
	while (seq_end < seq_stat[seq_ID_n,'seq_end']) {
		snp_stat <- rbind(snp_stat, c(seq_ID_n, seq_end - seq_split + 1, seq_end, snp_num))
		seq_end <- seq_end + seq_split
		snp_num <- 0
	}
	snp_stat <- rbind(snp_stat, c(seq_ID_n, seq_end - seq_split + 1, seq_stat[seq_ID_n,'seq_end'], snp_num))
}

snp_stat <- data.frame(snp_stat, stringsAsFactors = FALSE)
names(snp_stat) <- c('seq_ID', 'seq_start', 'seq_end', 'snp_num')
snp_stat$seq_start <- as.numeric(snp_stat$seq_start)
snp_stat$seq_end <- as.numeric(snp_stat$seq_end)
snp_stat$snp_num <- as.numeric(snp_stat$snp_num)

write.table(snp_stat, str_c(out_dir, '/', sample_name, '.snp_stat.txt'), row.names = FALSE, sep = '\t', quote = FALSE)

##读取 InDel 检测结果
#读取 vcf 文件，统计 InDel 长度
indel <- read.delim(indel_vcf, header = FALSE, colClasses = 'character', comment.char = '#')[c(1, 2, 4, 5)]
indel$V2 <- as.numeric(indel$V2)
indel$length <- str_length(indel[ ,4]) - str_length(indel[ ,3])
indel_insert <- length(which(indel$length > 0))
indel_delet <- length(which(indel$length < 0))

#统计 InDel 密度
indel <- indel[c(1, 2, 5)]
colnames(indel)[1:2] <- c('seq_ID', 'seq_site')

indel_stat <- NULL
seq_ID <- unique(indel$seq_ID)
for (seq_ID_n in seq_ID) {
	indel_subset <- subset(indel, seq_ID == seq_ID_n)
	seq_end <- seq_split
	indel_num <- 0
	
	for (i in 1:nrow(indel_subset)) {
		if (indel_subset[i,'seq_site'] <= seq_end) indel_num <- indel_num + 1
		else {
			indel_stat <- rbind(indel_stat, c(seq_ID_n, seq_end - seq_split + 1, seq_end, indel_num))
			
			seq_end <- seq_end + seq_split
			indel_num <- 0
			while (indel_subset[i,'seq_site'] > seq_end) {
				indel_stat <- rbind(indel_stat, c(seq_ID_n, seq_end - seq_split + 1, seq_end, indel_num))
				seq_end <- seq_end + seq_split
			}
			indel_num <- indel_num + 1
		}
	}
	
	while (seq_end < seq_stat[seq_ID_n,'seq_end']) {
		indel_stat <- rbind(indel_stat, c(seq_ID_n, seq_end - seq_split + 1, seq_end, indel_num))
		seq_end <- seq_end + seq_split
		indel_num <- 0
	}
	indel_stat <- rbind(indel_stat, c(seq_ID_n, seq_end - seq_split + 1, seq_stat[seq_ID_n,'seq_end'], indel_num))
}

indel_stat <- data.frame(indel_stat, stringsAsFactors = FALSE)
names(indel_stat) <- c('seq_ID', 'seq_start', 'seq_end', 'indel_num')
indel_stat$seq_start <- as.numeric(indel_stat$seq_start)
indel_stat$seq_end <- as.numeric(indel_stat$seq_end)
indel_stat$indel_num <- as.numeric(indel_stat$indel_num)

write.table(indel_stat, str_c(out_dir, '/', sample_name, '.indel_stat.txt'), row.names = FALSE, sep = '\t', quote = FALSE)

##读取 CNV 检测结果
cnv <- read.delim(cnv_cnv, header = FALSE, stringsAsFactors = FALSE)[1:2]
for (i in 1:nrow(cnv)) {
	str_split <- unlist(str_split(cnv[i,2], '[:-]'))
	cnv[i,'seq_ID'] <- str_split[1]
	cnv[i,'seq_start'] <- str_split[2]
	cnv[i,'seq_end'] <- str_split[3]
}

cnv <- cnv[c(3:5, 1)]
cnv$seq_start <- as.numeric(cnv$seq_start)
cnv$seq_end <- as.numeric(cnv$seq_end)

cnv_dup <- length(which(cnv[[4]] == 'duplication'))
cnv_del <- length(which(cnv[[4]] == 'deletion'))

##读取 SV 检测结果
sv <- read.delim(sv_ctx, header = FALSE, stringsAsFactors = FALSE, comment.char = '#')[c(1, 2, 4, 5, 7)]

sv_in_de <- subset(sv, sv[[5]] %in% c('INS', 'DEL'))[c(1, 2, 4, 5)]
sv_ITX <- subset(sv, sv[[5]] %in% 'ITX')
sv_CTX <- subset(sv, sv[[5]] %in% 'CTX')
sv_INV <- subset(sv, sv[[5]] %in% 'INV')

sv_ins <- length(which(sv[[5]] == 'INS'))
sv_del <- length(which(sv[[5]] == 'DEL'))
sv_itx <- nrow(sv_ITX)
sv_ctx <- nrow(sv_CTX)
sv_inv <- nrow(sv_INV)

#####################
##circlize 绘图
pdf(str_c(out_dir, '/', sample_name, '.circlize.pdf'), width = 14, height = 8)
circle_size = unit(1, "snpc")
circos.par(gap.degree = 2)
circos.genomicInitialize(seq_stat, plotType = 'axis')

circos.track(
	ylim = c(0, 1), track.height = 0.05, bg.border = NA, bg.col = '#8DD3C7',
	panel.fun = function(x, y) {
		xlim = CELL_META$xlim
		ylim = CELL_META$ylim
		seq_ID = CELL_META$sector.index
		circos.text(mean(xlim), mean(ylim), seq_ID, cex = 0.7, col = 'black', facing = 'inside', niceFacing = FALSE)
	} )

#GC% 含量图
circos.genomicTrack(
	depth_base[c(1:3, 5)], track.height = 0.08, bg.col = '#EEEEEE6E', bg.border = NA,
	panel.fun = function(region, value, ...) {
		circos.genomicLines(region, value, col = 'blue', lwd = 0.35, ...)
		circos.lines(c(0, max(region)), c(genome_GC, genome_GC), col = 'blue2', lwd = 0.15, lty = 2)
	circos.yaxis(labels.cex = 0.2, lwd = 0.1, tick.length = convert_x(0.15, 'mm'))
	} )

gc_legend <- Legend(
	at = 1, labels = c(str_c('GC % ( Average: ', genome_GC, ' % )')), labels_gp = gpar(fontsize = 8),
	grid_height = unit(0.5, 'cm'), grid_width = unit(0.5, 'cm'), type = 'lines', background = '#EEEEEE6E', 
	legend_gp = gpar(col = 'blue', lwd = 0.5))

#覆盖度 & 深度
circos.genomicTrack(
	depth_base[1:4], track.height = 0.08, ylim = c(0, (max(depth_base$depth) + 1)), bg.col = '#EEEEEE6E', bg.border = NA,
	panel.fun = function(region,value, ...) {
		circos.genomicRect(region, value, ytop.column = 1, ybottom = 0, border = 'white', lwd = 0.02, col = 'red', ...)
		circos.lines(c(0, max(region)), c(average_depth, average_depth), col = 'red3', lwd = 0.15, lty = 2)
		circos.yaxis(labels.cex = 0.2, lwd = 0.1, tick.length = convert_x(0.15, 'mm'))
	} )

depth_legend <- Legend(
	at = 1, labels = str_c(' Depth ( average: ', average_depth, ' X )'), labels_gp = gpar(fontsize = 8),
	title = str_c('Coverage: ', coverage , ' %'), title_gp = gpar(fontsize = 9),
	grid_height = unit(0.4, 'cm'), grid_width = unit(0.4, 'cm'), type = 'points', pch = NA, background = 'red')

#CDS & rRNA & tRNA
color_assign <- colorRamp2(breaks = c(1, 2, 3), col = c('#00ADFF', 'orange', 'green2'))

circos.genomicTrackPlotRegion(
	gene, track.height = 0.12, stack = TRUE, bg.border = NA,
	panel.fun = function(region, value, ...) {
		circos.genomicRect(region, value, col = color_assign(value[[1]]), border = NA, ...)
	} )

gene_legend <- Legend(
	at = c(3, 2, 1), labels = c(' CDS', ' rRNA', ' tRNA'), labels_gp = gpar(fontsize = 8),
	title = 'CDS | rRNA | tRNA', title_gp = gpar(fontsize = 9), 
	grid_height = unit(0.4, 'cm'), grid_width = unit(0.4, 'cm'), type = 'points', pch = NA, background = c('#00ADFF', 'orange', 'green2'))

#SNP 密度
value_max <- max(snp_stat$snp_num)
colorsChoice <- colorRampPalette(c('white', '#245B8E'))
color_assign <- colorRamp2(breaks = c(0:value_max), col = colorsChoice(value_max + 1))

circos.genomicTrackPlotRegion(
	snp_stat, track.height = 0.08, stack = TRUE, bg.border = NA,
	panel.fun = function(region, value, ...) {
		circos.genomicRect(region, value, col = color_assign(value[[1]]), border = NA, ...)
	} )

snp_legend <- Legend(
	at = round(seq(0, value_max, length.out = 6), 0), labels_gp = gpar(fontsize = 8),
	col_fun = colorRamp2(round(seq(0, value_max, length.out = 6), 0), colorsChoice(6)),
	title_position = 'topleft', title = 'SNP density', legend_height = unit(4, 'cm'), title_gp = gpar(fontsize = 9))

#InDel 密度
value_max <- max(indel_stat$indel_num)
colorsChoice <- colorRampPalette(c('white', '#7744A4'))
color_assign <- colorRamp2(breaks = c(0:value_max), col = colorsChoice(value_max + 1))

circos.genomicTrackPlotRegion(
	indel_stat, track.height = 0.08, stack = TRUE, bg.border = NA,
	panel.fun = function(region, value, ...) {
		circos.genomicRect(region, value, col = color_assign(value[[1]]), border = NA, ...)
	} )

indel_legend <- Legend(
	at = round(seq(0, value_max, length.out = 6), 0), labels_gp = gpar(fontsize = 8),
	col_fun = colorRamp2(round(seq(0, value_max, length.out = 6), 0), colorsChoice(6)),
	title_position = 'topleft', title = 'InDel density', legend_height = unit(4, 'cm'), title_gp = gpar(fontsize = 9))

#CNV 变异 duplication、deletion，1 插入、2 缺失
for (i in 1:nrow(cnv)) {
	if (cnv[i,4] == 'duplication') cnv[i,4] <- 1
	if (cnv[i,4] == 'deletion') cnv[i,4] <- 2
}
cnv[[4]] <- as.numeric(cnv[[4]])

color_assign <- colorRamp2(breaks = c(1, 2), color = c('#FB564A', '#74C476'))
circos.genomicTrackPlotRegion(
	cnv, track.height = 0.06, stack = TRUE, bg.border = NA,
	panel.fun = function(region, value, ...) {
		circos.genomicRect(region, value, col = color_assign(value[[1]]), border = NA, ...)
	} )

cnv_legend <- Legend(
	at = c(1, 2), labels = c(' Duplication', ' Deletion'), labels_gp = gpar(fontsize = 8), title = 'CNV type', title_gp = gpar(fontsize = 9),  
	grid_height = unit(0.5, 'cm'), grid_width = unit(0.5, 'cm'), type = 'points', pch = NA, background = c('#FB564A', '#74C476'))

#SV 变异
#INS、DEL，1 插入、2 缺失
for (i in 1:nrow(sv_in_de)) {
	if (sv_in_de[i,4] == 'INS') sv_in_de[i,4] <- 1
	if (sv_in_de[i,4] == 'DEL') sv_in_de[i,4] <- 2
}
sv_in_de[[4]] <- as.numeric(sv_in_de[[4]])

color_assign <- colorRamp2(breaks = c(1, 2), color = c('#FB564A', '#74C476'))
circos.genomicTrackPlotRegion(
	sv_in_de, track.height = 0.06, stack = TRUE, bg.border = NA,
	panel.fun = function(region, value, ...) {
		circos.genomicRect(region, value, col = color_assign(value[[1]]), border = NA, ...)
	} )

#ITX，内易位
circos.genomicLink(sv_ITX[c(1, 2, 2)], sv_ITX[c(3, 4, 4)], col = '#FF46346E', lwd = 0.5)

#CTX，间易位
circos.genomicLink(sv_CTX[c(1, 2, 2)], sv_CTX[c(3, 4, 4)], col = '#66CB716E', lwd = 0.5)

#INV，倒位
circos.genomicLink(sv_INV[c(1, 2, 2)], sv_INV[c(3, 4, 4)], col = '#53C6FF6E', lwd = 0.5)

sv_legend <- Legend(
	at = c(1, 2, 3, 4, 5), labels = c(' INS', ' DEL', ' ITX',' CTX', ' INV'),
	labels_gp = gpar(fontsize = 8), grid_height = unit(0.5, 'cm'), grid_width = unit(0.5, 'cm'), 
	type = c('points', 'points', 'lines', 'lines', 'lines'), pch = NA, background = c('#FB564A', '#74C476', NA, NA, NA),
	legend_gp = gpar(col = c(NA, NA, '#FF4634', '#66CB71', '#53C6FF'), lwd = 1),
	title = 'SV type', title_gp = gpar(fontsize = 9))

#添加图例
y_coord <- 0.8
x_coord <- 0.87

pushViewport(viewport(x = x_coord + 0.011, y = y_coord))
grid.draw(gc_legend)
y_coord <- y_coord - 0.06
upViewport()

pushViewport(viewport(x = x_coord + 0.005, y = y_coord))
grid.draw(depth_legend)
y_coord <- y_coord - 0.1
upViewport()

pushViewport(viewport(x = x_coord - 0.0063, y = y_coord))
grid.draw(gene_legend)
y_coord <- y_coord - 0.19
upViewport()

pushViewport(viewport(x = x_coord - 0.0205, y = y_coord))
grid.draw(snp_legend)
y_coord <- y_coord
upViewport()

pushViewport(viewport(x = x_coord + 0.0505, y = y_coord))
grid.draw(indel_legend)
y_coord <- y_coord - 0.195
upViewport()

pushViewport(viewport(x = x_coord - 0.018, y = y_coord))
grid.draw(cnv_legend)
y_coord <- y_coord
upViewport()

pushViewport(viewport(x = x_coord + 0.04, y = y_coord - 0.0373))
grid.draw(sv_legend)
y_coord <- y_coord
upViewport()

#统计总览（涵括了上文大部分的统计结果信息，例如 SNP 替换类型统计等）
stat_legend <- Legend(
	at = 1, labels = '1', labels_gp = gpar(fontsize = 0), title_gp = gpar(fontsize = 9), 
	grid_height = unit(0, 'cm'), grid_width = unit(0, 'cm'), type = 'points', pch = NA, background = NA, 
	title = str_c('Sample: ', sample_name, '\nRefer species: ', ref_name, '\nRefer size: ', genome_size, ' bp\nRefer GC: ', genome_GC, ' %\n\n\nTotal SNP: ', snp_ti + snp_tv, '\nTransitions: ', snp_ti, '\nTransversions: ', snp_tv, '\nTi/Tv: ', round(snp_ti / snp_tv, 2), '\nA>T|T>A: ', snp_at, '\nA>G|T>C: ', snp_ag, '\nA>C|T>G: ', snp_ac, '\nG>A|C>T: ', snp_ga, '\nG>T|C>A: ', snp_gt, '\nG>C|C>G: ', snp_gc, '\n\n\nTotal InDel: ', indel_insert + indel_delet, '\nInsert: ', indel_insert, '\nDelet: ', indel_delet, '\n\n\nTotal CNV: ', cnv_dup + cnv_del, '\nDuplication: ', cnv_dup, '\nDeletion: ', cnv_del, '\n\n\nTotal SV: ', sv_ins + sv_del + sv_itx + sv_ctx + sv_inv, '\nINS: ', sv_ins,'\nDEL: ', sv_del, '\nITX: ', sv_itx, '\nCTX: ', sv_ctx, '\nINV: ', sv_inv))

pushViewport(viewport(x = 0.12, y = 0.5))
grid.draw(stat_legend)
upViewport()

circos.clear()
dev.off()

```

### 三元相图

```R
#示例数据“data.txt”
#第一列，OTU 名称
#第二、三、四列，OTU 在 3 个样本中的丰度信息
#第五列，标注了这些 OTU 在哪个样本中发生了富集
#第六列，OTU 的门类群

#读取数据
dat <- read.delim('data.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

##ternaryplot() 三元图
library(vcd)

dat1 <- dat

#定义图中点的大小，这里取 3 个样本的平均值的 0.4 次方再除以 10......
#丰度差异过大，调整图中点大小确实是件麻烦事
dat1$size <- (apply(dat1[2:4], 1, mean))^0.4 / 10

#定义富集 OTUs 的颜色，按富集样本划分
dat1[which(dat1$rich == 'env1'),'color'] <- 'red'
dat1[which(dat1$rich == 'env2'),'color'] <- 'blue'
dat1[which(dat1$rich == 'env3'),'color'] <- 'green3'
dat1[which(dat1$rich == '0'),'color'] <- 'gray'

#ternaryplot() 示例，按 OTUs 富集区域着色
#pdf('ternaryplot.pdf')
png('ternaryplot.png', width = 2000, height = 2000, res = 300, units = 'px')
ternaryplot(dat1[2:4], scale = NULL, col = dat1$color, prop_size = FALSE, cex = dat1$size, main = 'Enriched OTUs')
grid_legend(x = 0.8, y = 0.7, pch = c(19, 19, 19), col = c('red', 'blue', 'green3'), label = c('env1', 'env2', 'env3'), title = FALSE, frame = FALSE)
dev.off()

##ggtern 三元图
library(ggtern)

dat2 <- dat

#用于定义图中点的大小，这里取 3 个样本的平均值的 0.5 次方......
dat2$size <- (apply(dat2[2:4], 1, mean))^0.5

#ggtern，按 OTUs 富集区域着色
p_rich <- ggtern(dat2, aes(env1, env2, env3)) +
geom_mask() +
geom_point(aes(color = rich, size = size), alpha = 0.8, show.legend = FALSE) +
scale_size(range = c(0, 6)) +
scale_colour_manual(values  = c('red', 'blue', 'green3', 'gray'), limits = c('env1', 'env2', 'env3', '0')) +
theme_bw() +
theme(axis.text = element_blank(), axis.ticks = element_blank())

#ggtern，按 OTUs 类群着色
p_taxonomy <- ggtern(dat2, aes(env1, env2, env3)) +
geom_mask() +
geom_point(aes(color = taxonomy, size = size), alpha = 0.8) +
scale_size(range = c(0, 6)) +
theme_bw() +
theme(axis.text = element_blank(), axis.ticks = element_blank(), legend.title = element_blank()) +
guides(size = 'none')

#ggsave('ggtern.pdf', p_rich, width = 6, height = 6)
ggsave('ggtern.png', p_rich, width = 6, height = 6)

#密度图举例
ggtern(dat2, aes(env1, env2, env3)) +
stat_density_tern()
```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200301165119323.png" alt="image-20200301165119323" style="zoom:33%;" />

### 生存曲线图

```R
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("survival", version = "3.8")
BiocManager::install("ggplot2", version = "3.8")
BiocManager::install("survminer")
library(survival)
library(ggplot2)
library(survminer)
#读取个人采集的生存曲线数据表
survival_time<-read.csv("survival_time.csv",header = T,row.names = 1, sep=",")
#创建生存对象surv（time，status）不变，分组变量可根据自己实验分组，此处以性别sex分类
fit <- survfit(Surv(time, status) ~ sex, data = survival_time) 
#最基本绘图
ggsurvplot(fit)

#绘制个性化生存曲线
ggsurvplot(fit,risk.table=TRUE,#生存统计统计表
           conf.int=TRUE,#添加置信区间带
           palette = c("skyblue", "red"),#颜色设置
           pval=TRUE,#log-rank检验
           pval.method=TRUE)#添加检验text

****示例数据**********
library(survival)
#示例数据，详情 ?lung
data(lung)
head(lung)
#Kaplan-Meier 分析，详情 ?survfit
KM <- survfit(Surv(time, status) ~ sex, data = lung, type = 'kaplan-meier', conf.type = 'log')
KM
summary(KM)
#结果提取，例如重要的统计值
summary(KM)$table
#对数秩检验，详情 ?survdiff
survdiff(Surv(time, status) ~ sex, data = lung)
#绘制生存曲线，反映了尚在世的患者数量比例和时间的关系
plot(KM, main = 'Kaplan-Meier ', xlab = 'Time (days)', ylab = 'Overall survival', 
     lwd = 2, col = c('blue', 'red'))
legend(x = 'topright', col = c('blue', 'red'), lwd = 2, legend = c('1: male', '2: female'))

#绘制累积风险曲线，反映了疾病风险和时间的关系，与累积的去世患者数量有关
plot(KM, main = 'Cumulative hazard', xlab = 'Time (days)', ylab = 'Cumulative hazard', 
     lwd = 2, col = c('blue', 'red'), fun = 'cumhaz')
legend(x = 'topright', col = c('blue', 'red'), lwd = 2, legend = c('1: male', '2: female'))
library(survminer)
#生存曲线，详情 ?ggsurvplot
ggsurvplot(KM, conf.int = TRUE, palette = c('blue', 'red'), risk.table = TRUE, pval = TRUE)
#累积风险曲线
ggsurvplot(KM, conf.int = TRUE, palette = c('blue', 'red'), fun = 'cumhaz')
```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200305190347917.png" alt="image-20200305190347917" style="zoom:50%;" />

### 风玫瑰图

```R
#基于疫情数据
require(nCov2019)
library(dplyr)
library(ggplot2)
y = load_nCov2019(lang = 'zh')
y
d = y['global']
dd <-  filter(d, time == time(y)& country != '中国')%>%
  arrange(desc(cum_confirm))
dd = dd[1:40,]
dd$country = factor(dd$country,levels = dd$country)

dd$angle = 1:40*360/40	
ggplot(dd,aes(country,cum_confirm,fill=cum_confirm))+
  geom_col(width = 1,color = 'grey90')+
  geom_col(aes(y=I(2)),width = 1,fill = 'white')+
  scale_y_log10()+
  scale_fill_gradientn(colors=c("darkgreen","green","orange","firebrick","red"),trans="log")+
  geom_text(aes(label=paste(country,cum_confirm,sep ="\n"),
                y = cum_confirm*.8,angle=angle),
            data=function(d)d[d$cum_confirm > 100,],
            color = "white",fontface = "bold", vjust =1)+
  geom_text(aes(label = paste0(cum_confirm,"例",country),
                y = cum_confirm *2, angle = angle+90),
            data = function(d) d[d$cum_confirm < 100,],
            vjust = 0)+
  coord_polar(direction = -1)+
  theme_void()+
  theme(legend.position = "none")
```

**画cog注释的玫瑰图**

输入文件

![image-20200330105219382](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200330105219382.png)

```R
#载入绘图包
library(ggplot2)
#导入绘图数据
f<-read.table("cog_lab", header=T, sep="\t")
#对图例标签进行处理
myLabel = as.vector(f$lab)
#根据数据计算每个功能分类所占的比例并添加到图例标签中
myLabel = paste(myLabel, " (", round(f$mag / sum(f$mag) * 100, 2), "%) ", sep = "")
#绘制图像
p<-ggplot(f, aes(x=dir, y=factor(mag), fill=dir))+
geom_bar(stat='identity')+
coord_polar()+
labs(x = "", y = "Number of Unigenes", title = "COG Function Classification of Unigene Sequence")+
theme(axis.ticks = element_blank())+
theme(legend.title = element_blank())+
theme(panel.background=element_blank())+
scale_fill_discrete(breaks = f$dir, labels = myLabel)
#图像输出
png("COG.png",width = 8200,height = 5400,res = 600,type = "cairo")
p
dev.off()
```

![image-20200330105311499](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200330105311499.png)



### 制作六边形Logo

```R
library(hexSticker)
library(ggplot2)
library(ggstar)
data <- data.frame(x = rep(c(1:4), 3), y = rep(1:3, each = 4,
                                               len = 12), group = letters[c(1:12)])

p <- ggplot(data=iris, aes(x=Sepal.Length,y=Sepal.Width,
                           starshape=Species, fill=Species)) + geom_star(show.legend=FALSE, size=1.8) +
  scale_fill_manual(values=c("#66C2A5", "#FC8D62", "#8DA0CB"))
p
p <- p + theme_void() + theme(axis.line.x.bottom = element_line(color="white", size=0.22),
                              axis.line.y.left = element_line(color="white", size=0.22)) + theme_transparent()
p
sticker(p, package="ggstar", p_size=14, s_x=1, s_y=.75, s_width=1.3, s_height=0.68,
        h_fill="black", h_color="#B3B3B3", p_color="#FFDF00",
        filename="ggstarlog.png")
#package指的是输入字体 p_color字体颜色 h_color边框颜色 h_fill背景
```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200330165934324.png" alt="image-20200330165934324" style="zoom:33%;" />

### Chord diagram

```R
#http://houyun.xyz/post/2019/12/08/chord-diagram/
if(!require(devtools))
  install.packages(devtools)
devtools::install_github("houyunhuang/nivochord")
library(nivochord)
nivochord(data.matrix(mtcars[1:8, 1:8]), LETTERS[1:8])
#第一个参数是类矩阵的数据，需要是行列数相等，且都是数值型，
#第二个参数keys是每个类的名称，若为NULL，赋值为第一个参数的行名或者列名。
library(RColorBrewer)
m <- matrix(runif(36), ncol = 6)
rownames(m) <- paste0("row", 1:6)
col <- brewer.pal(3, "Set2")
nivochord(m, colors = col)
nivochord(m, colors = col, innerRadiusRatio = 0.8)#innerRadiusOffset = 0.05;innerRadiusOffset用来控制ribbon和圆环之间的距离，默认是0。
#innerRadiusRatio用来控制内部（ribbon）所在区域的比较，
#默认是0.9（即90%），这里我们设置成0.8，圆环宽度就相应的变大了。
#padAngle = 0.05 padAngle参数比较好玩，可以在每个小的圆环之间增加空白
#margin = .margin(90, 60, 10, 60)四个参数分别是下、左上、右四个方位的空白，单位是像素（px）。
colors = c("red", "blue", "yellow")
nivochord(m, colors = colors, legends = .legends(anchor = "top",
                                                 translateY = -45))

```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200412153635347.png" alt="image-20200412153635347" style="zoom:50%;" />



### Maptree

```R
#导入otu表格
otu = read.delim("./otutab.txt",row.names = 1)
head(otu)
otu = as.matrix(otu)
# str(otu)
#导入注释文件
tax = read.delim("./taxonomy.txt",row.names = 1)
head(tax)
#将物种注释文件转化为矩阵，方便使用phyloseq封装
tax = as.matrix(tax)
#基本函数，画出maptree
mapdata = data_to_maptree (otu,tax,200)
#参数N代表选取OTU的数量（按照丰度排序，选取丰度最高的前N个OTU做展示），这里我设置N = 200
#提取图形部分并保存
p1= mapdata[[1]]
p1
ggsave("./maptree1.pdf", p1, width = 12, height =10 )

#按照物种分类信息上色
#按照平均丰度修改大小和按照门水平上色颜色
mapadd = maptree_add1_plot(mapdata)
p2 = mapadd[[1]]
p2
#d导出ggplot对象的优点就是可以随心所欲的修改图形上的内容
p2 +scale_fill_brewer()
# p2 + scale_color_gradient2()
ggsave("./maptree.pdf", p2, width = 12, height =10)

#ggraph版本的物种分类树
graph = mapadd[[2]]

#展示物种分类关系
ggraph(graph, 'dendrogram', circular = TRUE) +
  geom_edge_elbow() +
   scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_point(aes( x = x, y=y, filter = leaf,size=mean,colour=Phylum, alpha=0.2)) +
  geom_node_text(aes(x = x*1.05, y=y*1.05, filter = leaf, label=name,
   hjust='outward', angle = -((-node_angle(x, y)+90)%%180)+90, size=3,
   colour=Phylum), size=0.8, alpha=1)  + theme_void()

#另一种形式
graph = mapadd[[2]]
# data = create_layout(graph, layout = 'dendrogram', circular = TRUE)
# head(data)

#展示为无数种分类树，这种方式也是我知道
ggraph(graph, layout = 'dendrogram', circular = TRUE) +
   geom_edge_diagonal(colour="blue") +
   scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_point(aes( x = x, y=y, filter = leaf,size=mean,colour=Phylum, alpha=0.2)) +
  geom_node_text(aes(x = x*1.05, y=y*1.05, filter = leaf, label=name,
   hjust='outward', angle = -((-node_angle(x, y)+90)%%180)+90, size=3,
   colour=Phylum), size=0.5, alpha=1)  + theme_void()
```

![image-20200519093716444](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200519093716444.png)

### 3D柱状图

```R
set.seed(1234)
data <- data.frame(a = sample(1:50, 10),
                                       b = sample(1:50, 10),
                                       c = sample(1:50, 10),
                                       d = sample(1:50, 10))
rownames(data) <- LETTERS[1:10]
library(rgl)
library(barplot3d)
library(RColorBrewer)
install.packages('barplot3d')
barplot3d(rows = 10, cols = 4, #先根据数据构造一个画图矩阵
                    #画图要用的数据，必须是matrix类型
                  z = data %>% as.matrix(),
                    #展示角度，正面观
                    theta=30,
                   #展示角度，顶面观（出的图是可旋转的，所以这些都无关紧要）
                    phi=30,
                  #调整出图的高矮胖瘦的，挺重要！
                    scalexy=20,
                    #柱子（什么鬼）的侧面颜色，可调整透明度
                    sidecolors=brewer.pal(4,"Dark2"), alpha=0.4,
                    #顶面颜色
                    topcolors = brewer.pal(4,"Dark2"),
                    #x轴刻度标注，向量形式
                    xlabels = colnames(data),
                    #y轴刻度标注，向量形式
                    ylabels = rownames(data),
                   #z轴刻度，逻辑值
                    zlabels = TRUE)


#再用barplot包里的测试数据画个“乐高图”
x=system.file("extdata", "signature_probabilities.txt", package = "barplot3d")
sigdata=read.table(x,header=TRUE,stringsAsFactors = FALSE)

# Plot signature 2 without axis labels, with Sanger colors and some transparency so we can see all bars
legoplot3d(contextdata=sigdata$Signature_7,
                      labels=FALSE,
                       scalexy=0.01,
                      sixcolors="sanger",
                       alpha=0.4)
```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200630190614314.png" alt="image-20200630190614314" style="zoom:50%;" />

### Dotplot气球图

[balloonplot](http://rpkgs.datanovia.com/ggpubr/reference/ggballoonplot.html)

```R
#第一种就是简单的用ggplot画的散点图
setwd('./Script/Dot_plot/')

# 加载ggplot2包
library(ggplot2)

#读入数据
dt<-read.table("testdata.txt",sep = "\t",header = T)

#查看数据框dt的前6行
head(dt)

#加载reshape2包
library(reshape2)

#将“宽型”数据框转成“长型”数据框
dt2<-melt(dt)

#查看数据框dt2的前6行
head(dt2)

#指定纵轴标签顺序
dt2$Genus<-factor(dt2$Genus,levels = rev(unique(dt2$Genus)),ordered = TRUE)

#绘制正方形“点”，带描边
p1<-ggplot(dt2, aes(variable, Genus))+geom_point(aes(size=value),shape=22,color="steelblue",fill="white")
p1

#绘制“圆点”，不带描边颜色
p2<-ggplot(dt2, aes(variable, Genus))+geom_point(aes(size=value),shape=16,color="steelblue")
p2

#建立颜色与数据的关系,这里让不同的样本的点显示不同的颜色
p3<-ggplot(dt2, aes(variable, Genus))+geom_point(aes(size=value,color=variable),shape=16)
p3

#自定义颜色，这里使用RColorBrewer的颜色集
p4<-p3+scale_color_brewer(palette="Dark2")
p4

#更改图表的主题，实现类似iTOL的点状图效果
p5<-p4+theme_minimal()
p5

#不想要网格线也可以去掉，甚至图例也可用legend.position = "none"去掉
p6<-p5+theme(panel.grid = element_blank())
p6

#保存图表为pdf格式
ggsave("p6.pdf",width = 4.4,height = 6,units = "in")


#第二种就是用ggpubr中的函数画气球图
my_cols <- c("#0D0887FF", "#6A00A8FF", "#B12A90FF",
             "#E16462FF", "#FCA636FF", "#F0F921FF")

library(ggpubr)
data <- read.delim(
  system.file("demo-data/housetasks.txt", package = "ggpubr"),
  row.names = 1
)
data 
ggballoonplot(data)
#试试上述的图
dt%>%tibble::column_to_rownames('Genus') %>% ggballoonplot(.,fill = "value")+
  scale_fill_gradientn(colors = my_cols)

#加上颜色
ggballoonplot(data, color = "#0073C2FF", fill = "#0073C2FF")

# Change color according to the value of table cells
ggballoonplot(data, fill = "value")+
  scale_fill_gradientn(colors = my_cols)


# Change the plotting symbol shape
ggballoonplot(data, fill = "value",  shape = 23)+
  gradient_fill(c("blue", "white", "red"))


# Set points size to 8, but change fill color by values
# Sow labels
ggballoonplot(data, fill = "value", color = "lightgray",
              size = 10, show.label = TRUE)+
  gradient_fill(c("blue", "white", "red"))

```

<img src="C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20200703152802170.png" alt="image-20200703152802170" style="zoom:50%;" />

### maptools绘制地图

[全网最完整的中国地图                ](https://mp.weixin.qq.com/s?src=11&timestamp=1595140132&ver=2469&signature=hVai8izanau0-IT-M0oKEsO-ICwxEbgakCTpOW9qhvdZKNLX*Rdqm5ceayc2QJBkFDLZyL1ImsNiWwckEAixeBD-*p7Nn3ivj5Kge58kD04tBOzKMNJQNGWqot*76WaR&new=1)