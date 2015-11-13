setwd("~/Desktop/Courses/Coursera/Data Science Specialization/CapstoneYelp/data/yelp_dataset_challenge_academic_dataset")

library(quanteda)
library(ggplot2)
library(gridExtra)
library(grid)

options(scipen = 999) # remove sci notation

Yelp_reviews = read.table("Yelp_reviews.txt",stringsAsFactors = FALSE)

five = subset(Yelp_reviews, Yelp_reviews$stars==5)
four = subset(Yelp_reviews, Yelp_reviews$stars==4)
two = subset(Yelp_reviews, Yelp_reviews$stars==2)
one = subset(Yelp_reviews, Yelp_reviews$stars==1)
 
five_text=five$text
four_text=four$text
pos=c(four_text,five_text)

one_text = one$text
two_text = two$text
neg=c(one_text,two_text)

#### make n-grams

Uni_neg = dfm(neg, ngrams = 1,verbose = TRUE, toLower = FALSE, removeNumbers = FALSE, 
                removePunct = FALSE, removeSeparators = TRUE,
                keptFeatures = NULL)

Uni_negTop = topfeatures(Uni_neg, 50)#####
Uni_negTopDF = data.frame(Uni_negTop)
Uni_negTopDF$ngrams = rownames(Uni_negTopDF) 


Bi_neg=dfm(neg, ngrams = 2,verbose = TRUE, toLower = FALSE, removeNumbers = FALSE, 
           removePunct = FALSE, removeSeparators = TRUE,
           keptFeatures = NULL)

Bi_negTop = topfeatures(Bi_neg, 50)#####
Bi_negTopDF=data.frame(Bi_negTop)
Bi_negTopDF$ngrams <- rownames(Bi_negTopDF) 


Tri_neg=dfm(neg, ngrams = 3,verbose = TRUE, toLower = FALSE, removeNumbers = FALSE, 
            removePunct = FALSE, removeSeparators = TRUE,
            keptFeatures = NULL)

Tri_negTop = topfeatures(Tri_neg, 50)
Tri_negTopDF=data.frame(Tri_negTop)
Tri_negTopDF$ngrams <- rownames(Tri_negTopDF) 


Quad_neg=dfm(neg, ngrams = 4,verbose = TRUE, toLower = FALSE, removeNumbers = FALSE, 
             removePunct = FALSE, removeSeparators = TRUE,
             keptFeatures = NULL)

Quad_negTop = topfeatures(Quad_neg, 50)
Quad_negTopDF=data.frame(Quad_negTop)
Quad_negTopDF$ngrams <- rownames(Quad_negTopDF) 


write.csv(Uni_negTopDF, "Uni_neg.csv",row.names = FALSE)
write.csv(Bi_negTopDF, "Bi_neg.csv",row.names = FALSE)
write.csv(Tri_negTopDF, "Tri_neg.csv",row.names = FALSE)
write.csv(Quad_negTopDF, "Quad_neg.csv",row.names = FALSE)

################
##### pos reviews
################

Uni_pos=dfm(pos, ngrams = 1,verbose = TRUE, toLower = FALSE, removeNumbers = FALSE, 
            removePunct = FALSE, removeSeparators = TRUE,
            keptFeatures = NULL)

Uni_posTop = topfeatures(Uni_pos, 50)
Uni_posTopDF=data.frame(Uni_posTop)
Uni_posTopDF$ngrams <- rownames(Uni_posTopDF) 


Bi_pos=dfm(pos, ngrams = 2,verbose = TRUE, toLower = FALSE, removeNumbers = FALSE, 
           removePunct = FALSE, removeSeparators = TRUE,
           keptFeatures = NULL)

Bi_posTop = topfeatures(Bi_pos, 50)
Bi_posTopDF=data.frame(Bi_posTop)
Bi_posTopDF$ngrams <- rownames(Bi_posTopDF) 


Tri_pos=dfm(pos, ngrams = 3,verbose = TRUE, toLower = FALSE, removeNumbers = FALSE, 
            removePunct = FALSE, removeSeparators = TRUE,
            keptFeatures = NULL)

Tri_posTop = topfeatures(Tri_pos, 50)
Tri_posTopDF=data.frame(Tri_posTop)
Tri_posTopDF$ngrams <- rownames(Tri_posTopDF) 


Quad_pos=dfm(pos, ngrams = 4,verbose = TRUE, toLower = FALSE, removeNumbers = FALSE, 
             removePunct = FALSE, removeSeparators = TRUE,
             keptFeatures = NULL)

Quad_posTop = topfeatures(Quad_pos, 50)
Quad_posTopDF=data.frame(Quad_posTop)
Quad_posTopDF$ngrams <- rownames(Quad_posTopDF) 


write.csv(Uni_posTopDF, "Uni_pos.csv",row.names = FALSE)
write.csv(Bi_posTopDF, "Bi_pos.csv",row.names = FALSE)
write.csv(Tri_posTopDF, "Tri_pos.csv",row.names = FALSE)
write.csv(Quad_posTopDF, "Quad_pos.csv",row.names = FALSE)

Uni_neg = read.csv("Uni_neg.csv")
Bi_neg = read.csv("Bi_neg.csv")
Tri_neg = read.csv("Tri_neg.csv")
Quad_neg = read.csv("Quad_neg.csv")

Uni_pos = read.csv("Uni_pos.csv")
Bi_pos = read.csv("Bi_pos.csv")
Tri_pos = read.csv("Tri_pos.csv")
Quad_pos = read.csv("Quad_pos.csv")

Bi_neg$ngrams <- gsub('_', ' ', Bi_neg$ngrams)
Tri_neg$ngrams <- gsub('_', ' ', Tri_neg$ngrams)
Quad_neg$ngrams <- gsub('_', ' ', Quad_neg$ngrams)

Uni_neg = Uni_neg[1:10,]
Bi_neg = Bi_neg[1:10,]
Tri_neg = Tri_neg[1:10,]
Quad_neg = Quad_neg[1:10,]

Uni_neg = transform(Uni_neg,ngrams = reorder(ngrams, -Uni_negTop))
Bi_neg = transform(Bi_neg,ngrams = reorder(ngrams, -Bi_negTop))
Tri_neg = transform(Tri_neg,ngrams = reorder(ngrams, -Tri_negTop))
Quad_neg = transform(Quad_neg,ngrams = reorder(ngrams, -Quad_negTop))

Bi_pos$ngrams <- gsub('_', ' ', Bi_pos$ngrams)
Tri_pos$ngrams <- gsub('_', ' ', Tri_pos$ngrams)
Quad_pos$ngrams <- gsub('_', ' ', Quad_pos$ngrams)

Uni_pos = Uni_pos[1:10,]
Bi_pos = Bi_pos[1:10,]
Tri_pos = Tri_pos[1:10,]
Quad_pos = Quad_pos[1:10,]

Uni_pos = transform(Uni_pos,ngrams = reorder(ngrams, -Uni_posTop))
Bi_pos = transform(Bi_pos,ngrams = reorder(ngrams, -Bi_posTop))
Tri_pos = transform(Tri_pos,ngrams = reorder(ngrams, -Tri_posTop))
Quad_pos = transform(Quad_pos,ngrams = reorder(ngrams, -Quad_posTop))



#### charts
Uni_negGG = ggplot(Uni_neg, aes(x=ngrams,y=Uni_negTop)) +xlab("") + ylab("frequency")
Uni_negGG = Uni_negGG + geom_bar(stat="Identity", fill="indianred1") 
Uni_negGG = Uni_negGG + theme_light()  + theme(plot.margin=unit(c(.25,0,.9,0), "cm"))
Uni_negGG = Uni_negGG + theme(axis.text.x = element_text(angle = 45, hjust = 1))
Uni_negGG = Uni_negGG + scale_x_discrete(expand = c(0,0))+scale_y_continuous(breaks = seq(0,200000, by = 25000),expand = c(0,0))


Bi_negGG = ggplot(Bi_neg, aes(x=ngrams,y=Bi_negTop)) +xlab("")+ ylab("")
Bi_negGG = Bi_negGG + geom_bar(stat="Identity", fill="indianred2") 
Bi_negGG = Bi_negGG + theme_light()  + theme(plot.margin=unit(c(.25,0,-.2,0), "cm"))
Bi_negGG = Bi_negGG + theme(axis.text.x = element_text(angle = 45, hjust = 1))
Bi_negGG = Bi_negGG + scale_x_discrete(expand = c(0,0))+scale_y_continuous(breaks = seq(0,14000, by = 2500),expand = c(0,0))


Tri_negGG = ggplot(Tri_neg, aes(x=ngrams,y=Tri_negTop)) +xlab("") + ylab("")
Tri_negGG = Tri_negGG + geom_bar(stat="Identity", fill="indianred3") 
Tri_negGG = Tri_negGG + theme_light()  + theme(plot.margin=unit(c(.25,0,-.5,0), "cm"))
Tri_negGG = Tri_negGG + theme(axis.text.x = element_text(angle = 45, hjust = 1))
Tri_negGG = Tri_negGG + scale_x_discrete(expand = c(0,0))+scale_y_continuous(breaks = seq(0,3000, by = 500),expand = c(0,0))


Uni_posGG = ggplot(Uni_pos, aes(x=ngrams,y=Uni_posTop)) +xlab("") + ylab("frequency")
Uni_posGG = Uni_posGG + geom_bar(stat="Identity", fill="seagreen2") + ggtitle("Uni-gram")
Uni_posGG = Uni_posGG + theme_light()  + theme(plot.margin=unit(c(.25,0,.8,0), "cm"))
Uni_posGG = Uni_posGG + theme(axis.text.x = element_text(angle = 45, hjust = 1))
Uni_posGG = Uni_posGG + scale_x_discrete(expand = c(0,0))+scale_y_continuous(breaks = seq(0,500000, by = 50000),expand = c(0,0))


Bi_posGG = ggplot(Bi_pos, aes(x=ngrams,y=Bi_posTop)) +xlab("") + ylab("")
Bi_posGG = Bi_posGG + geom_bar(stat="Identity", fill="seagreen3") + ggtitle("Bi-gram")
Bi_posGG = Bi_posGG + theme_light()  + theme(plot.margin=unit(c(.25,0,0.15,0), "cm"))
Bi_posGG = Bi_posGG + theme(axis.text.x = element_text(angle = 45, hjust = 1))
Bi_posGG = Bi_posGG + scale_x_discrete(expand = c(0,0))+scale_y_continuous(breaks = seq(0,18000, by = 2500),expand = c(0,0))


Tri_posGG = ggplot(Tri_pos, aes(x=ngrams,y=Tri_posTop)) +xlab("") + ylab("")
Tri_posGG = Tri_posGG + geom_bar(stat="Identity", fill="seagreen4") + ggtitle("Tri-gram")
Tri_posGG = Tri_posGG + theme_light()  + theme(plot.margin=unit(c(.25,0,-.85,0), "cm"))
Tri_posGG = Tri_posGG + theme(axis.text.x = element_text(angle = 45, hjust = 1))
Tri_posGG = Tri_posGG + scale_x_discrete(expand = c(0,0))+scale_y_continuous(breaks = seq(0,4100, by = 1000),expand = c(0,0))


barCharts=arrangeGrob(Uni_posGG, Bi_posGG, Tri_posGG, Uni_negGG, Bi_negGG, Tri_negGG,
                       nrow = 2, ncol = 3,left=textGrob(c("Positive","Negative"),y = c(.86,.38) ,gp = gpar(fontsize=c(18,18), face = c("bold","bold"),col=c("seagreen4","indianred3"))))
grid1=grid.draw(barCharts)


ggsave(filename="N_grams.png",barCharts,width = 45,height = 25, units = "cm")
