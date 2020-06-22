
library(MASS)
library(matrixStats)
library(scales)
library(ggplot2)
library(NbClust)
library(grid)
library(gridExtra)

library(cowplot)
library(iNEXT)
library(ape)
library(Rmisc)
library(dplyr)
library(vegan)
library(reshape)
library(scales)
library(tibble)
library(eHOF)
library(mgcv)
library(nlme)
library(lattice)
library(data.table)
library(stats)
library(caret)
library(randomForest)
library(schoolmath)
library(gambin)
library(ggridges)
library(multcomp)
library(sandwich)
library(taRifx)  


library(spectral)
library(ptest)

setwd("d:/user/yus14/Desktop/R_SweaveWorks/log file analysis")





df = read.table('out.log',sep = ",",header=T)

head(df)


#### remove the factor structure 
df <- remove.factors(df)

str(df)

df$userid_from <- as.character(df$userid_from)
df$userid_to <- as.character(df$userid_to)


users_from <- unique(df$userid_from)
users_to <- unique(df$userid_to)


#### extract the age information of each userid_from,  
#### the userid_to might not be present in the list of userid_from, this kinds of users will not have age information

age_list <-c()
for (i in 1:length(users_from)) {
  temp <- df[df$userid_from==users_from[i],"sender_age"]
  
  age_list <- c(age_list,temp[1])
}


### made the userid_from age into datafram
userid_from_age <- as.data.frame(cbind(users_from,age_list))
userid_from_age <- remove.factors(userid_from_age)

str(userid_from_age)



#####  extract the gender information of each userid_from
gender_list <-c()
for (i in 1:length(users_from)) {
  temp <- df[df$userid_from==users_from[i],"sender_gender"]
  
  gender_list <- c(gender_list,temp[1])
}

### add gender infromation to the userid_from_age

userid_from_age$gender_list <- gender_list


###  statistical summary of the platform
table(df$sender_platform)

record_time <- max(df$seconds_since_start)




############################################
##### one example of single user  ##########
############################################

one_user <- users_from[1]

one_user <- "141110"

#### one user's original log information
one_user_infor <- df[df$userid_from==one_user,]


### how many platform this user use
num_platform <- length(unique(one_user_infor$sender_platform))


### the mostly used platform

most_used_platform <- names(table(one_user_infor$sender_platform)[1])



### number of message this user send out
num_message <- nrow(one_user_infor)


### who are the message receivers, how many message for each receiver
contact_user<-as.data.frame(table(one_user_infor$userid_to))
colnames(contact_user) <- c("userid_to","num_message")


### the message receivers original log information
contact_user_infor <-df[df$userid_from %in% contact_user$userid_to,] 


contact_user_age <- userid_from_age[userid_from_age$users_from %in% contact_user$userid_to,]


dist_contact_user_age <- as.data.frame(table(contact_user_age$age_list))

dist_contact_user_gender <- as.data.frame(table(contact_user_age$gender_list))

### how many man are contacted
ratio_male_contact <- dist_contact_user_gender$Freq[1]/nrow(contact_user_age)

### how many different age type for the receivers
num_receiver_age <- nrow(dist_contact_user_age) 

### how many message receivers
num_user_to <- nrow(contact_user)


### average message for each receivers
ave_message <- num_message/num_user_to



### who send the message to this user, the original log information 
feedback_message <-  df[df$userid_to==one_user,]


###  all the users who send message to this user
feedback_user <- unique(feedback_message$userid_from)


### which users reply to this user
reply_user <- intersect(feedback_user, unique(one_user_infor$userid_to))
num_reply_user <- length(reply_user)

#### how many of the reply user
length(reply_user) < length(feedback_user)

#### the original log information of the reply users
user_reply <- feedback_message[feedback_message$userid_from %in% reply_user,]

num_reply_message <- nrow(user_reply)


reply_percent <- num_reply_user / num_user_to

ave_message_reply <- num_reply_message/num_reply_user 



### ratio of sending message to the received message

ratio_send_to_receive <- num_reply_message/num_message




### the time lag between two adjacent messages of this user
message_time_inter <- one_user_infor$seconds_since_start[2:num_message]-one_user_infor$seconds_since_start[1:(num_message-1)]
### average of time interval
ave_time_inter <- mean(message_time_inter)
### median of time interval 
median_time_inter <- median(message_time_inter)
### how many times of rapid sending message
num_short_time_inter <- sum(message_time_inter<10)

#### active time

online_time <- one_user_infor$seconds_since_start[num_message]- one_user_infor$seconds_since_start[1]

### relax_time:  the time interval larger than 2h

relax_time<-message_time_inter[message_time_inter>7200]  

active_time <- online_time-sum(relax_time)





#### fft analysis to find the periodic information
library(stats)
library(spectral)
library(ptest)

fft_message <- fft(message_time_inter)
plot(abs(fft_message))


cpgram(fft_message)
spectrum(fft_message)


#### test of the periodity of the message time, if it is periodic, it is probably a bot
ptestReg(message_time_inter[1:100],method="LS")



#### collect the statistical summary of userid_from

c(active_time,online_time,num_message,ave_message,ave_time_inter,median_time_inter,
  ave_message_reply,gender_ratio_contact,most_used_platform,num_platform,
  num_short_time_inter,num_receiver_age,num_reply_message,num_reply_user,num_user_to,reply_percent)







###############################################################
######  collect all the user's statistical information ########
###############################################################

userid_from_stat <-c()


for(i in 1:length(users_from)){
  
  one_user <- users_from[i]
  
  #### one user's original log information
  one_user_infor <- df[df$userid_from==one_user,]
  
  ### how many platform this user use
  num_platform <- length(unique(one_user_infor$sender_platform))
  
  ### the mostly used platform
  
  most_used_platform <- names(table(one_user_infor$sender_platform)[1])
  
  
  ### number of message this user send out
  num_message <- nrow(one_user_infor)
  
  
  ### who are the message receivers, how many message for each receiver
  contact_user<-as.data.frame(table(one_user_infor$userid_to))
  colnames(contact_user) <- c("userid_to","num_message")
  
  
  ### the message receivers original log information
  contact_user_infor <-df[df$userid_from %in% contact_user$userid_to,] 
  
  
  contact_user_age <- userid_from_age[userid_from_age$users_from %in% contact_user$userid_to,]
  
  
  dist_contact_user_age <- as.data.frame(table(contact_user_age$age_list))
  
  dist_contact_user_gender <- as.data.frame(table(contact_user_age$gender_list))
  
  ### how many man are contacted
  ratio_male_contact <- dist_contact_user_gender$Freq[1]/nrow(contact_user_age)
  
  ### how many different age type for the receivers
  num_receiver_age <- nrow(dist_contact_user_age) 
  
  ### how many message receivers
  num_user_to <- nrow(contact_user)
  
  
  ### average message for each receivers
  ave_message <- num_message/num_user_to
  
  
  ### who send the message to this user, the original log information 
  feedback_message <-  df[df$userid_to==one_user,]
  
  
  ###  all the users who send message to this user
  feedback_user <- unique(feedback_message$userid_from)
  
  
  ### which users reply to this user
  reply_user <- intersect(feedback_user, unique(one_user_infor$userid_to))
  num_reply_user <- length(reply_user)
  
  #### how many of the reply user
  length(reply_user) < length(feedback_user)
  
  #### the original log information of the reply users
  user_reply <- feedback_message[feedback_message$userid_from %in% reply_user,]
  
  num_reply_message <- nrow(user_reply)
  
  
  reply_percent <- num_reply_user / num_user_to
  
  ave_message_reply <- num_reply_message/num_reply_user 
  
  
  ### ratio of sending message to the received message
  
  ratio_send_to_receive <- num_reply_message/num_message
  
  
  ### the time lag between two adjacent messages of this user
  message_time_inter <- one_user_infor$seconds_since_start[2:num_message]-one_user_infor$seconds_since_start[1:(num_message-1)]
  ### average of all the time interval 
  ave_time_inter <- mean(message_time_inter)
  ### median of all the time interval 
  median_time_inter <- median(message_time_inter)
  ### how many times of rapid sending message
  num_short_time_inter <- sum(message_time_inter<10)
  
  #### active time
  
  online_time <- one_user_infor$seconds_since_start[num_message]- one_user_infor$seconds_since_start[1]
  
  ### relax_time:  the time interval larger than 2h
  
  relax_time<-message_time_inter[message_time_inter>7200]  
  
  active_time <- online_time-sum(relax_time)
  
  
  ### test the periodic
  if (length(message_time_inter)>=100){
    test_results<- ptestReg(message_time_inter[1:100],method="LS")
    periodic_pvalue <- test_results$pvalue
  }else if (length(message_time_inter)<5){
    periodic_pvalue <-1
  }else{
    test_results<- ptestReg(message_time_inter,method="LS")
    periodic_pvalue <- test_results$pvalue
    }
  
  
  
  #### collect the statistical summary of userid_from
  
  collect_infor<- c(active_time,online_time,num_message,ave_message,ave_time_inter,median_time_inter,
    ave_message_reply,ratio_male_contact,most_used_platform,num_platform,
    num_short_time_inter,num_receiver_age,num_reply_message,num_reply_user,num_user_to,reply_percent,periodic_pvalue)
  
  userid_from_stat <-rbind(userid_from_stat,collect_infor)
  
  
}

userid_from_stat<- as.data.frame(userid_from_stat)

colnames(userid_from_stat)<- c("active_time","online_time","num_message","ave_message",
                               "ave_time_inter","median_time_inter",
                               "ave_message_reply","ratio_male_contact",
                               "most_used_platform","num_platform",
                               "num_short_time_inter","num_receiver_age",
                               "num_reply_message","num_reply_user","num_user_to",
                               "reply_percent","periodic_pvalue")

userid_from_stat$userid_from <- users_from

rownames(userid_from_stat) <- users_from

userid_from_stat <-remove.factors(userid_from_stat)

#### change the numerical variable into the numerical values from the characters format
userid_from_stat$active_time <- as.numeric(userid_from_stat$active_time)
userid_from_stat$online_time <- as.numeric(userid_from_stat$online_time)
userid_from_stat$num_message <- as.numeric(userid_from_stat$num_message)
userid_from_stat$ave_message <- as.numeric(userid_from_stat$ave_message)
userid_from_stat$ave_time_inter <- as.numeric(userid_from_stat$ave_time_inter)
userid_from_stat$median_time_inter <- as.numeric(userid_from_stat$median_time_inter)
userid_from_stat$ave_message_reply <- as.numeric(userid_from_stat$ave_message_reply)
userid_from_stat$ratio_male_contact <- as.numeric(userid_from_stat$ratio_male_contact)
userid_from_stat$num_platform<- as.numeric(userid_from_stat$num_platform)
userid_from_stat$num_short_time_inter <- as.numeric(userid_from_stat$num_short_time_inter)
userid_from_stat$num_receiver_age <- as.numeric(userid_from_stat$num_receiver_age)
userid_from_stat$num_reply_message <- as.numeric(userid_from_stat$num_reply_message)
userid_from_stat$num_reply_user <- as.numeric(userid_from_stat$num_reply_user)
userid_from_stat$num_user_to <- as.numeric(userid_from_stat$num_user_to)
userid_from_stat$reply_percent <- as.numeric(userid_from_stat$reply_percent)
userid_from_stat$periodic_pvalue <- as.numeric(userid_from_stat$periodic_pvalue)



#######################################################
##### distribution of the key attributors
########################################################


### only chose the numerical attributors
userid_from_stat_num <- userid_from_stat[,c(-9,-18)]


str(userid_from_stat_num)

summary(userid_from_stat_num)



### replace NA with 0, because these user only send one message, 
### there is no way to calculate their active time, 
### remove the column ratio_male_contact, because this attributor can not be a index for the bot user

userid_from_stat_num[is.na(userid_from_stat_num)] <- 0

userid_from_stat_num<- userid_from_stat_num[,-8]

#userid_from_stat_num$ave_mess_unit_time <- userid_from_stat_num$num_message/ userid_from_stat_num$active_time







x <- melt(userid_from_stat_num)

plt <- ggplot(data = x, aes(x = variable, y = value))
plt + geom_boxplot() + 
  facet_wrap(vars(variable), scales = "free")







#####################################################
#########  clustering analysis
#####################################################


mydata <- userid_from_stat_num
mydata <- scale(mydata) # standardize variables 



# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 





###### hierarchical clustering


###  get the feeling, how is the clustering structure in all the users
###   it is quite clear, there are mainly two different groups

# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit_hierarchical <- hclust(d, method="ward")
plot(fit_hierarchical) # display dendogram
groups <- cutree(fit_hierarchical, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit_hierarchical, k=5, border="red") 


############### cut the cluster into two groups and plot
#############################



# K-Means Cluster Analysis
fit <- kmeans(mydata, 2) # 2 cluster solution
# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster) 





######   Plotting Cluster Solutions 


# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster)
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster) 

##############

table(mydata$fit.cluster )

mydata_cluster2 <- mydata[mydata$fit.cluster==2,]

mydata_cluster2_orginal_infor <- userid_from_stat[rownames(mydata_cluster2),]

mydata_cluster1 <- mydata[mydata$fit.cluster==1,]

mydata_cluster1_orginal_infor <- userid_from_stat[rownames(mydata_cluster1),]



summary(mydata_cluster1_orginal_infor)  #### this cluster contain the bot users

summary(mydata_cluster2_orginal_infor)




####  criteria  for the bot user

###  (1) high  "num_user_to"
###  (2) low   "ave_message"
###  (3) low   "ave_time_inter"
###  (4) high  "active time", "online time"
###  (5) low   "ave_message_reply"
###  (6) high  "ave_message_unit_time"  (num_message)/ active_time
###  (7) low   "periodic_pvalue"


class_data <- userid_from_stat_num

#class_data <- scale(class_data,center = F)


class_data_scale <- as.data.frame(apply(class_data,2,function(x) scales::rescale(x,to = c(1,100))))
#class_data_scale$score <-class_data_scale$active_time*class_data_scale$num_message/(class_data_scale$ave_message)*class_data_scale$num_user_to/class_data_scale$median_time_inter/class_data_scale$reply_percent


#### use the criteria  for the bot user to calculate the score for each user, the higher the score is, the higher probability as the bot users
class_data_scale$score <-class_data_scale$active_time*(class_data_scale$num_message)^2/(class_data_scale$ave_message)*class_data_scale$num_user_to/class_data_scale$median_time_inter/class_data_scale$reply_percent/class_data_scale$periodic_pvalue

class_data$score <- class_data_scale$score

#hist(class_data_scale$score,breaks = 50)


### the possible bot: high score user

### order the dataframe by score

class_data <- class_data[order(class_data$score,decreasing = T),]
possible_bot <- class_data[class_data$score>50,]












#####################################################################
#####################################################################
#####  clustering the bot users into groups 

mydata_bot <- class_data_scale[rownames(possible_bot),-ncol(class_data_scale)]

# Determine number of clusters
wss <- (nrow(mydata_bot)-1)*sum(apply(mydata_bot,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata_bot,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


###### hierarchical clustering in bot users


###  get the feeling, how is the clustering structure in all the possible bot users
### 

# Ward Hierarchical Clustering
d_bot <- dist(mydata_bot, method = "euclidean") # distance matrix
fit_hierarchical_bot <- hclust(d_bot, method="ward")
plot(fit_hierarchical_bot) # display dendogram
groups <- cutree(fit_hierarchical_bot, k=4) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit_hierarchical_bot, k=4, border="red") 


############### cut the cluster into two groups and plot
#############################



# K-Means Cluster Analysis
fit_bot <- kmeans(mydata_bot, 4) # 2 cluster solution
# get cluster means
aggregate(mydata_bot,by=list(fit_bot$cluster),FUN=mean)
# append cluster assignment
mydata_bot <- data.frame(mydata_bot, fit_bot$cluster) 


#### aggregate the information for each groups
user_bot<- class_data[rownames(possible_bot),-ncol(class_data_scale)]
user_bot$cluster <- mydata_bot$fit_bot.cluster

agg_bot <- aggregate(user_bot ,by = list(user_bot$cluster),
                FUN = median)

### the group 3 and 4 are very special, they are the bots with high possibility. Their logs have the clear non rondomness pattern, they send each receiver 5 message in less than 2 second, and can receive also 5 message back in a very shor time.
### these feedback message probably are the automatically generated


##############################################
########## summary of the Userid
##########################################
userid_from_stat$bot <- 0 
userid_from_stat[rownames(possible_bot),"bot"] <- 1

### the possibility of the bot, based on the score value

test <- as.data.frame(class_data$score,"bot")
rownames(test)<- rownames(class_data)
colnames(test) <- "score"


test$prob <- apply(test,1,function(x) sum(test$score< x)/length(users_from))
test$Userid <- rownames(test)
test <- test[,c(3,1,2)]

proba_bot_user <- test[,c("Userid","prob")]

################################################
##### save data into txt files 
###############################################


write.table(proba_bot_user, "probability_bot.log", sep="\t",row.names = F,quote = F) 

write.table(userid_from_stat, "user_stat_infor_class.log", sep="\t",row.names = F,quote = F) 

write.table(userid_from_stat[,18:19], "users_classification.log", sep="\t",row.names = F,quote = F) 


write.table(user_bot, "bot_classification.log", sep="\t",row.names = F,quote = F) 

