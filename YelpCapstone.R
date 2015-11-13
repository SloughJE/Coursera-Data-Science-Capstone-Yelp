# Data Science Capstone: Yelp Dataset

setwd("~/Desktop/Courses/Coursera/Data Science Specialization/CapstoneYelp/data/yelp_dataset_challenge_academic_dataset")

library(jsonlite)
library(tm)
library(stringr)
library(qdap)
library(quanteda)


#business <- stream_in(file("yelp_academic_dataset_business.json"))
#reviews <- stream_in(file("yelp_academic_dataset_review.json"))

#saveRDS(business, "business.rds")
#saveRDS(reviews, "reviews.rds")

business=readRDS("business.rds")

# name = name of business
# categories = type of business
# business_id = identifier

# get only USA reviews for American English

USA = business[(business$state == "AZ") | (business$state == "NV") | (business$state == "WI")
               | (business$state == "PA") | (business$state == "PA") | (business$state == "IL")
               | (business$state == "NC"), ]

# grep the restaurant/food categories

id = USA$business_id[grep("Restaurants|Food|Pubs",USA$categories)]
length(unique(id))

reviews = readRDS("reviews.rds")

text_star = data.frame(reviews$business_id,reviews$text,reviews$stars)
colnames(text_star)=c("id","text","stars")
nrow(text_star)

restDT=as.data.frame(id)

Yelp_reviews=merge(text_star,restDT,by="id")

# get proportion pos/neg

nrow(Yelp_reviews)
five=subset(Yelp_reviews, Yelp_reviews$stars==5)
four=subset(Yelp_reviews, Yelp_reviews$stars==4)
(nrow(five)+nrow(four))/nrow(Yelp_reviews)
two=subset(Yelp_reviews, Yelp_reviews$stars==2)
one=subset(Yelp_reviews, Yelp_reviews$stars==1)
(nrow(two)+nrow(one))/nrow(Yelp_reviews)

###########
# emoticons
###########
# get all positive and negative emoticons from qdap list

# emot_pos=emoticon[c(2,5,13,21,32,33,34,35,37,41,42,46,47,54,66,67,77,78,79),]
# emot_neg=emoticon[c(3,8,11,12,14,15,16,18,25,26,27,28,29,39,45,48,56,59,60,61,62,63,64,65,70,80),]

# emot_pos=emot_pos[,-1]
# 

# emot_neg=emot_neg[,-1]

# write.table(emot_neg,"emot_neg.txt",row.names = FALSE)
# write.table(emot_pos,"emot_pos.txt",row.names = FALSE)
emot_neg=read.table("emot_neg.txt")
emot_pos=read.table("emot_pos.txt")

## substitute emoticons for word "emotismiley" or "emotifrowney"

Yelp_reviews$text=mgsub(as.character(emot_neg[[1]]),"emotifrowney" , Yelp_reviews$text)
Yelp_reviews$text=mgsub(as.character(emot_pos[[1]]), "emotismiley", Yelp_reviews$text)

length(grep("emotismiley",Yelp_reviews$text))
length(grep("emotifrowney",Yelp_reviews$text))

# process the text 
Yelp_reviews$text=str_replace_all(Yelp_reviews$text, "[\n]" , " ")
Yelp_reviews$text=str_replace_all(Yelp_reviews$text, "[^[:alnum:][:space:]']", " ")
Yelp_reviews$text=removeNumbers(Yelp_reviews$text)
Yelp_reviews$text=toLower(Yelp_reviews$text)
Yelp_reviews$text=removeWords(Yelp_reviews$text,stopwords("english"))

#remove multiple white spaces
Yelp_reviews$text=gsub("(?<=[\\s])\\s*|^\\s+$","", Yelp_reviews$text, perl=TRUE)
#remove trailing white space
Yelp_reviews$text=str_trim(Yelp_reviews$text, side = "both")

write.table(Yelp_reviews,"Yelp_reviews.txt")
write.csv(Yelp_reviews,"Yelp_reviews.csv",row.names = FALSE)

num_rows=nrow(Yelp_reviews)

Yelp_reviews_sample = Yelp_reviews[sample(num_rows, num_rows*.05), ]

write.table(Yelp_reviews_sample,"Yelp_reviews_sample.txt")
write.csv(Yelp_reviews_sample,"Yelp_reviews_sample.csv",row.names = FALSE)
