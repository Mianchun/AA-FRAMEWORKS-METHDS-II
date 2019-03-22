setwd('C:/personal/columbia university/AA method 2/assignment')

fastfood = read.csv(file = 'fastfood_survey (1).csv',stringsAsFactors = F)

fastfood$dollars_avg_meal=factor(fastfood$dollars_avg_meal, labels = c('0-10','10-15','15-20','20+'))
fastfood$marital_status=factor(fastfood$marital_status, labels = c('single','married','other'))
fastfood$gender=factor(fastfood$gender, labels = c('male','female'))
fastfood$own_rent=factor(fastfood$own_rent, labels = c('rent','own'))
fastfood$dwelling=factor(fastfood$dwelling, labels = c('house','apt','duplex'))
fastfood$occupation=factor(fastfood$occupation, labels = c('managerial','skilled trade','laborer',
                                                           'office worker','technical','professional',
                                                           'stay at home mom','student','retired'))
fastfood$education=factor(fastfood$education, labels = c('High School or less','Some college',
                                                         'College graduate','Graduate degree'))
fastfood$age=factor(fastfood$age, labels = c('0-30','30-39','40-49','50-59','60+'))
fastfood$income=factor(fastfood$income, labels = c('0-40000','40000-59999','60000-79999','80000+'))

data_cluster=fastfood[,1:11]
head(data_cluster)

sum(is.na(data_cluster$cleanliness))

try=data_cluster
try=na.omit(try)

# install.packages('mice') # install mice package if you don't have it
library(mice)
set.seed(1706)
data_cluster = complete(mice(data_cluster))
data_cluster$cleanliness[10]

data_cluster = scale(data_cluster)
data_cluster[1:11,1:11]

d = dist(x = data_cluster,method = 'euclidean') 
clusters = hclust(d = d,method='ward.D2')

plot(clusters)
cor(cophenetic(clusters),d)

plot(cut(as.dendrogram(clusters),h=5)$upper)
rect.hclust(tree=clusters,k = 2,border='tomato')

install.packages('dendextend')
library(dendextend)
plot(color_branches(as.dendrogram(clusters),k = 2,groupLabels = F))

h_segments_2 = cutree(tree = clusters,k=2)
table(h_segments_2)

h_segments_3 = cutree(tree = clusters,k=3)
table(h_segments_3)


set.seed(1706)
km = kmeans(x = data_cluster,centers = 2,iter.max=100,nstart=25)

table(km$cluster)

set.seed(1706)
km3 = kmeans(x = data_cluster,centers = 3,iter.max=100,nstart=25)
table(km3$cluster)

library(ggplot2)
within_ss = sapply(2:10,FUN = function(x) kmeans(x = data_cluster,centers = x,iter.max = 100,nstart = 25)$tot.withinss)
ggplot(data=data.frame(cluster = 2:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

install.packages('cluster')
library(cluster)
pam(data_cluster,k = 2)$silinfo$avg.width

pam(data_cluster,k = 3)$silinfo$avg.width

library(cluster)
library(ggplot2)
silhoette_width = sapply(2:11,FUN = function(x) pam(x = data_cluster,k = x)$silinfo$avg.width)
ggplot(data=data.frame(cluster = 2:11,silhoette_width),aes(x=cluster,y=silhoette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))

pam(data_cluster,k = 5)$silinfo$avg.width
pam(data_cluster,k = 6)$silinfo$avg.width
pam(data_cluster,k = 7)$silinfo$avg.width

library(mclust)
clusters_mclust = Mclust(data_cluster)
summary(clusters_mclust)

clusters_mclust_2 = Mclust(data_cluster,G=2)
summary(clusters_mclust_2)

table(h_segments_2)
k_segments = km$cluster
table(k_segments)

data2 = cbind(fastfood,h_segments_2, k_segments)
library(dplyr)
data2 %>%
  select(speed_of_service:taste_burgers,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()


m_clusters = Mclust(data = data_cluster,G = 2)
m_segments = m_clusters$classification
table(m_segments)

data3 = cbind(fastfood,m_segments, k_segments)
difference=data3%>%select(speed_of_service:taste_burgers,k_segments,m_segments)%>%filter(m_segments==k_segments)

set.seed(1706)
km3 = kmeans(x = data_cluster,centers = 3,iter.max=100)
k_segments3 = km3$cluster
data_km = cbind(fastfood,k_segments3)

library(dplyr)
data_km %>%
  select(speed_of_service:taste_burgers,k_segments3)%>%
  group_by(k_segments3)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()

library(dplyr); library(ggplot2); library(tidyr)
data_km %>%
  select(speed_of_service:taste_burgers,k_segments3)%>%
  group_by(k_segments3)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,speed_of_service:taste_burgers)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments3)))+
  geom_col(position='dodge')+
  coord_flip()


table(data_km$k_segments3,data_km$dollars_avg_meal)
round(prop.table(table(data_km$k_segments3,data_km$dollars_avg_meal),1),2)*100

round(prop.table(table(data_km$k_segments3,data_km$marital_status),1),2)*100

round(prop.table(table(data_km$k_segments3,data_km$gender),1),2)*100

round(prop.table(table(data_km$k_segments3,data_km$number_children),1),2)*100

round(prop.table(table(data_km$k_segments3,data_km$own_rent),1),2)*100

round(prop.table(table(data_km$k_segments3,data_km$dwelling),1),2)*100

round(prop.table(table(data_km$k_segments3,data_km$occupation),1),2)*100

round(prop.table(table(data_km$k_segments3,data_km$education),1),2)*100

round(prop.table(table(data_km$k_segments3,data_km$age),1),2)*100

round(prop.table(table(data_km$k_segments3,data_km$income),1),2)*100

cluster2=data_km%>%filter(k_segments3==2)
try_cluster2=na.omit(cluster2)
mean(try_cluster2$number_children)
sum(cluster2$number_children)

cluster3=data_km%>%filter(k_segments3==3)
try_cluster3=na.omit(cluster3)
mean(try_cluster3$number_children)