system("gdown --id 1kSAaGB0jU2X_6Q4hoRf7klDTOSQMCd_a")
cookie_cats = read.csv("cookie_cats.csv")
library(readr)
cookie_cats <- read_csv("cookie_cats.csv")
#View(cookie_cats)

library(infer)
library(naniar)

#check null values
#is.na(cookie_cats)
#which(is.na(cookie_cats))

#####################################

#convert retention_1 to integer (0,1) and sum the ones
x <- as.logical(cookie_cats$retention_1)
as.integer(x)
sum(x)

barplot(x,col = "red")

summary(x)


#convert retention_7 to integer (0,1) and sum the ones
y <- as.logical(cookie_cats$retention_7)
as.integer(y)
sum(y)

barplot(y, col = "blue")

summary(y)
###############################################

'''

class(cookie_cats$version)
as.character(cookie_cats$version)
sum(cookie_cats$version[which(cookie_cats$version=="gate_30",2)])
sum(cookie_cats$version[which(cookie_cats$version=="gate_40",2)])


group_a<-cookie_cats %>%
group_by(cookie_cats$version) %>%
summarise(count = length(unique(x)))

lapply(cookie_cats, unique)
rapply(cookie_cats, function(x) length(unique(as.integer(x))))

ones = sum(as.integer(x))
zeors = abs(length(cookie_cats) - ones)

install.packages("data.table")
library("data.table")

d_table = data.table(cookie_cats)
d_group = d_table[ , .(count = length(unique(x))), by = c("TRUE","FLASE")]

group_b<-cookie_cats %>%
  group_by(cookie_cats$version=="gate_40") %>%
  summarise(count=n_distinct(cookie_cats$version=="gate_40"))

'''

library(dplyr)

group_a <- cookie_cats %>% count(version)
print(group_a)
#sample

#sample_num=(cookie_cats$retention_7)
#print(sum(sample_num))

sample_num= subset(cookie_cats, retention_7 == TRUE & sum_gamerounds <= 100 , select = c(sum_gamerounds))
# & retention_1 == TRUE  
# retention_7 == FALSE

iter=100
means<-rep(NA,iter)

for(i in 1:iter)
{
 # d<-sample(cookie_cats$sum_gamerounds,iter)
 d<-sample(sample_num$sum_gamerounds,iter)
 means[i]<-mean(d)
}

hist(means)
abline( v= mean(means),col='gray60')


##################
ret1=sum(cookie_cats$retention_1)
ret11=(ret1/90189)*100
print(ret11)


ret7=sum(cookie_cats$retention_7)
ret77=(ret7/90189)*100
print(ret77)


groupaa=subset(cookie_cats$version,cookie_cats$version=="gate_30")
print(groupaa)



groupb=subset(cookie_cats$version,cookie_cats$version=="gate_40")
print(groupb)



#group_a_ret_1=subset(cookie_cats$retention_1==TRUE,cookie_cats$version,cookie_cats$version=="gate_30")
#print(sum((group_a_ret_1/90189)*100))

group_40 = subset(cookie_cats, version == "gate_40", select = c(version, retention_1, retention_7))
group_30 = subset(cookie_cats, version == "gate_30", select = c(version, retention_1, retention_7))

n_data = length(cookie_cats$version)

n_subset_30 = length(group_30$version)
n_subset_40 = length(group_40$version)

# print(n_subset / n_data * 100)

group_30_ret_1 = subset(group_30, retention_1, select = c(version, retention_1, retention_7))
print(length(group_30_ret_1$version) / n_subset_30 * 100)


group_40_ret_1 = subset(group_40, retention_1, select=(version))
print(length(group_40_ret_1$version) / n_subset_40 * 100)



group_30_ret_7 = subset(group_30, retention_7, select = c(version, retention_1, retention_7))
print(length(group_30_ret_7$version) / n_subset_30 * 100)


group_40_ret_7= subset(group_40, retention_7, select=(version))
print(length(group_40_ret_7$version) / n_subset_40 * 100)


#######################################


iter_poot=500
means_poot<-rep(NA,iter_poot)

for(i in 1:iter_poot)
{
  d_poot<- cookie_cats[ sample(seq_len(length(cookie_cats)) ,size = iter_poot, replace= TRUE), ]
  # df[sample(seq_len(df_len), size = 3), ]
  
  n_poot = length(d_poot$version)
  d_poot <- subset(d_poot, version == "gate_30" & retention_1 ,select = c(version, sum_gamerounds))
  # gates_poot[i] = length(d_poot$version)
  means_poot[i] = length(d_poot$version) / n_poot * 100
  
  # means_poot[i]<-mean(d_poot$sum_gamerounds)
}

hist(means_poot)
abline( v= mean(means_poot),col='gray60')
#############

iter_poot=500
means_poot_40_1<-rep(NA,iter_poot)

for(i in 1:iter_poot)
{
  d_poot_40_1<- cookie_cats[ sample(seq_len(length(cookie_cats)) ,size = iter_poot, replace= TRUE), ]
  # df[sample(seq_len(df_len), size = 3), ]
  
  n_poot_40_1 = length(d_poot_40_1$version)
  d_poot_40_1 <- subset(d_poot_40_1, version == "gate_40" & retention_1 ,select = c(version, sum_gamerounds))
  # gates_poot[i] = length(d_poot$version)
  means_poot_40_1[i] = length(d_poot_40_1$version) / n_poot_40_1 * 100
  
  # means_poot[i]<-mean(d_poot$sum_gamerounds)
}

hist(means_poot_40_1)
abline( v= mean(means_poot_40_1),col='gray60')

####

d_sample = cookie_cats[ sample(seq_len(length(cookie_cats)) ,size = iter_poot, replace= TRUE), ]

d_sample %>%
 group_by(version) %>%
summarise(dif = mean(retention_1))  %>%
summarise(diff(dif))


cookie_cats %>%
 group_by(version) %>%
  summarise(dif = mean(retention_7))  %>% 
  summarise(diff(dif))


#p_val<- dist %>%
 # get_p_value(group_by(version), direction="right")


#cor.test(x1,y1)

#gr = d_sample %>%
 # group_by(version)

























