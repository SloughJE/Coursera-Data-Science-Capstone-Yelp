# Data Science Capstone: Yelp Dataset

setwd("~/Desktop/Courses/Coursera/Data Science Specialization/CapstoneYelp/data/yelp_dataset_challenge_academic_dataset")
options(scipen=999) # remove sci notation
library(jsonlite)
library(tm)
library(stringr)
library(plyr)

business <- stream_in(file("yelp_academic_dataset_business.json"))
reviews <- stream_in(file("yelp_academic_dataset_review.json"))

saveRDS(business, "business.rds")
saveRDS(reviews, "reviews.rds")

business=readRDS("business.rds")

# name = name of business
# categories = type of business (too many)
# business_id = identifier

# grep the restaurant/food categories

id=business$business_id[grep("Restaurants|Food|Pubs",business$categories)]
length(unique(id))

reviews=readRDS("reviews.rds")

text_star=data.frame(reviews$business_id,reviews$text,reviews$stars)
str(text_star)
nrow(text_star)
colnames(text_star)=c("id","text","stars")

restDT=as.data.frame(id)
str(restDT)
str(id)
combined=merge(text_star,restDT,by="id")

# get proportion pos/neg

nrow(combined)
five=subset(combined, combined$stars==5)
four=subset(combined, combined$stars==4)
(nrow(five)+nrow(four))/nrow(combined)
two=subset(combined, combined$stars==2)
one=subset(combined, combined$stars==1)
(nrow(two)+nrow(one))/nrow(combined)

# emoticons

library(qdap)

emot_pos=emoticon[c(2,5,13,21,32,33,34,35,37,41,42,46,47,54,66,67,77,78,79),]
emot_neg=emoticon[c(3,8,11,12,14,15,16,18,25,26,27,28,29,39,45,48,56,59,60,61,62,63,64,65,70,80),]
emot_pos$word="emotismiley"
emot_pos=emot_pos[,-1]

emot_neg$word="emotifrowney"
emot_neg=emot_neg[,-1]

# write.table(emot_neg,"emot_neg.txt",row.names = FALSE)
# write.table(emot_pos,"emot_pos.txt",row.names = FALSE)
# emot_neg=read.table("emot_neg.txt")
# emot_pos=read.table("emot_pos.txt")


nlines=nrow(combined)
combined_samp=combined[sample(nrow(combined), nlines*0.01,replace=FALSE), ]

## substitute emoticons for word "emotismiley" or "emotifrowney"

combined_samp$text=mgsub(as.character(emot_neg[[1]]), as.character(emot_neg[[2]]), combined_samp$text)
combined_samp$text=mgsub(as.character(emot_pos[[1]]), as.character(emot_pos[[2]]), combined_samp$text)

combined$text=mgsub(as.character(emot_neg[[1]]), as.character(emot_neg[[2]]), combined$text)
combined$text=mgsub(as.character(emot_pos[[1]]), as.character(emot_pos[[2]]), combined$text)

length(grep("emotismiley",combined$text))
length(grep("emotifrowney",combined$text))

# clean the text 

clean=function(text){
  text=removeNumbers(as.character(text))
  text=removePunctuation(text)
  text=tolower(text)
  text=str_replace_all(text, "[\r\n]" , " ")
  return(text)
}

combined$text=clean(combined$text)
write.table(combined,"combined_emoti.txt")
write.csv(combined,"combined_emoti.csv",row.names = FALSE)
saveRDS(combined,"combined_emoti.RDS")

combined_samp$text=clean(combined_samp$text)
write.table(combined_samp,"samp_emoti.txt")
length(unique(combined$id)) # not same as length(unique(id)), why? 

# on to Python using "combined_emotic.txt" or "samp_emoti.txt"

