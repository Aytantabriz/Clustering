
#-----------------------------------------------K-clustering-H2O-----------------------------------------------
library(h2o)
library(tidyquant)
library(ggrepel)
# Run Cluster with 8g memory
h2o.init(max_mem_size = '8g')

# datani elave edin
data("iris")

# h2o formatına çevirin
iris.hex<-as.h2o(iris)

#Hesablama
aml<-h2o.kmeans(training_frame = iris.hex, k = 3, x = 1:4)

# Proqnoz edin
h2o.predict(aml,iris.hex) %>% as_tibble()->plot_data2
#table(iris$Species, plot_data2$predict)


centers=as.data.frame(h2o.cluster_sizes(aml)) %>% `colnames<-`(c('center'))


ggplot(iris,aes(iris$Sepal.Width,iris$Petal.Width,color = as.factor(plot_data2$predict), shape=iris$Species))+
  geom_point(size=3) +
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"),
        legend.position=c(.94, .8),
        plot.title = element_text(hjust = 0.5, size = 22, colour = 'red'), 
        plot.subtitle = element_text(hjust = 0.5, size = 20, color = 'darkgreen'),
        plot.background = element_rect(fill = 'white'),
        panel.border = element_rect(linetype = "dotted", fill = NA),
        legend.key = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(colour = "black",lineend = 'round',linetype = 'dotted'),
        legend.background = element_rect(colour = 'red', fill = 'white', size = 0.5, linetype ='dotted'))+
        labs(color = 'Clusters', shape = 'Species', x = paste(colnames(iris[2])),
                        y = paste(colnames(iris[4])), title = 'K-Clustering', 
                        subtitle = 'Overall = 3 clusters') +
        scale_colour_manual(values = c("red", "blue", "yellow")) +
  facet_wrap(~ Species)

#ggrepel

ggplot(iris,aes(iris$Sepal.Width,iris$Petal.Width,color = as.factor(plot_data2$predict), shape=iris$Species
               ))+ geom_label_repel(aes(label = iris$Species, fontface = "bold"))+
  geom_point(size=3) +
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"),
        legend.position=c(.94, .8),
        plot.title = element_text(hjust = 0.5, size = 22, colour = 'red'), 
        plot.subtitle = element_text(hjust = 0.5, size = 20, color = 'darkgreen'),
        plot.background = element_rect(fill = 'white'),
        panel.border = element_rect(linetype = "dotted", fill = NA),
        legend.key = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(colour = "black",lineend = 'round',linetype = 'dotted'),
        legend.background = element_rect(colour = 'red', fill = 'white', size = 0.5, linetype ='dotted'))+
  labs(color = 'Clusters', shape = 'Species', x = paste(colnames(iris[2])),
       y = paste(colnames(iris[4])), title = 'K-Clustering', 
       subtitle = 'Overall = 3 clusters') +
  scale_colour_manual(values = c("red", "blue", "gold4"))

#-----------------------------------------------PCA-H2O-----------------------------------------------

iris.pca <- h2o.prcomp(training_frame =iris.hex, transform = "STANDARDIZE",k = 4,impute_missing = T)
h2o.predict(iris.pca,iris.hex) %>% as.tibble() %>% add_column(labels=iris$Species)->plot_data

ggplot(plot_data, aes(x = PC1, y = PC2, colour = labels)) +
  geom_point() + labs(x='PC1-65.3%',y='PC2-20.9%')+
  geom_label_repel(aes(label = iris$Species, fontface = "bold"))

library(glue)

#  H2o modelinden Variance-i list formasında cixarmaq
iris.pca@model %>% as.list()->lll

# Cədvəl formasına çevirmək
lll$model_summary %>% as.data.frame()->kkk
kkk


#Vizuallaşdlrmaq! Qeyd: "glue" avtomatik olaraq PC1 və PC2 göstəricilərini yapışdıracaq 'x' və 'y' oxlarına
ggplot(plot_data, aes(x = PC1, y = PC2, colour = labels)) +
  geom_point() + labs(x=glue('PC1 - {kkk$pc1[2] %>% round(.,2)}'),
                      y=glue('PC1 - {kkk$pc2[2] %>% round(.,2)}'))+
  geom_label_repel(aes(label = iris$Species, fontface = "bold"))

library(corrplot)
plot_data[1:4] %>% cbind(iris[1:4]) %>% scale(center = T,scale = T) %>% as.tibble() %>% 
  cor() %>% .[5:8,1:4]->correlation_data
corrplot(correlation_data)

#-----------------------------------------------Multiple Correspondence analysis----------------------------
library(tidyquant)
df <- read_delim('bank-full.csv',delim = ';') %>% mutate_if(is.character,as.factor)

df[sample(nrow(df)),]->df

# ilk 200 sətrin götürülməsi
df_only_factors=df %>% select_if(is.factor) %>% .[1:200,]

# Alternativ variant, 96-ci ve 99-cu setrler ucun
df_only_factors = sample_n(df,100)


library(FactoMineR)

# MCA analizi
mca <- MCA(df_only_factors, ncp = 2, graph = F) #ncp dimension sayı

library("factoextra")


fviz_screeplot(mca, addlabels = TRUE, ylim = c(0, 45))  + labs(title = 'Variance vs Dim')

fviz_mca_biplot(mca, 
                repel = TRUE, # yazıların bir-birinə girməsinin qarşısını alır
                label = "var", # bu sətri silsək, sətrlərin nömrələri əks olunacaq, həmçinin "all", "none"
                ggtheme = theme_minimal())

fviz_mca_var(mca, choice = "mca.cor", #correlation - əlaqənin əks olunması
             repel = TRUE, 
             ggtheme = theme_minimal()) + labs(title = 'Multiple Correspondence Analysis',
                                               subtitle = 'Correlation')


fviz_mca_var(mca, # sadəcə sütunlar və kateqoriyalar
             repel = TRUE, 
             ggtheme = theme_minimal(),col.var = "blue")


p3=fviz_mca_ind(mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, 
             label = "var", # "all" etsəniz sətr nömrələri əks olunacaqdır
             ggtheme = theme_tq())




p4<-fviz_ellipses(mca, c("marital", "poutcome"), # həmin sütunlar üçün mərkəzlərin çəkilməsi
              addlabels = TRUE,
              addEllipses = TRUE, 
              repel = TRUE,
              geom = c("point"), # 'text'
              ggtheme = theme_tq(),
              pointsize = 3, alpha=0.5)


# bir neçə plota ad qoyaraq save etmək
library(ggpubr)
ggexport(plotlist = list(p3,p4), 
         filename = "MCA.pdf")













