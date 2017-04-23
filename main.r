library("caret")
library ("e1071")
library ("klaR")
library("rpart")

setwd('/Users/birupakhya/Desktop')

# read csv, convert date to proper format, 
# choose only required columns, rename adjusted close to avoid ambiguity
snp500 <- read.csv('s&p500_1990_2017.csv')
snp500$Date <- as.Date(snp500$Date, '%Y-%m-%d')
snp500 <- snp500[,c('Date', 'Adj.Close')]
colnames(snp500)[2] <- 'snp_adj_close'

bond10yr <- read.csv('10_Yr_Bond_rates.csv')
bond10yr$Date <- as.Date(bond10yr$Date, '%Y-%m-%d')
bond10yr <- bond10yr[,c('Date', 'Adj.Close')]
colnames(bond10yr)[2] <- 'bond10yr_adj_close'

crudeoil <- read.csv('DCOILWTICO.csv')
crudeoil$DATE <- as.Date(crudeoil$DATE, '%Y-%m-%d')
crudeoil$crude_oil_price_usd <- as.numeric(crudeoil$crude_oil_price_usd)
colnames(crudeoil)[2] <- 'crude_oil_price_usd'

# check missing values
colSums(is.na(snp500))
colSums(is.na(bond10yr))
colSums(is.na(crudeoil))

# select common dates, exclude others from the dataset. Minimise to 6848 rows.
require(sqldf)
merged_df <- sqldf('SELECT *
                   FROM snp500 
                   INNER JOIN bond10yr ON (snp500.Date = bond10yr.Date)
                   INNER JOIN crudeoil ON (snp500.Date = crudeoil.DATE)')

# select only required rows
merged_df$sq_no <- seq(1, nrow(merged_df))
merged_df <- merged_df[, c('sq_no', 'Date', 'snp_adj_close', 'bond10yr_adj_close', 'crude_oil_price_usd')]

# compute price change for snp500
snp_pc1 = c(); snp_pc2= c(); snp_pc3 = c(); snp_pc4 = c(); snp_pc5 = c(); priceDir = c();
for (i in 7:nrow(merged_df)){
  snp_pc1[i] <- (merged_df$snp_adj_close[i - 1] - merged_df$snp_adj_close[i - 2]) 
  snp_pc2[i] <- (merged_df$snp_adj_close[i - 1] - merged_df$snp_adj_close[i - 3]) 
  snp_pc3[i] <- (merged_df$snp_adj_close[i - 1] - merged_df$snp_adj_close[i - 4]) 
  snp_pc4[i] <- (merged_df$snp_adj_close[i - 1] - merged_df$snp_adj_close[i - 5]) 
  snp_pc5[i] <- (merged_df$snp_adj_close[i - 1] - merged_df$snp_adj_close[i - 6]) 
  priceDir[i] <- ifelse((merged_df$snp_adj_close[i] - merged_df$snp_adj_close[i - 1]) > 0, 1, 0)
}

# compute price change for bond10yr
bnd_pc1 = c(); bnd_pc2= c(); bnd_pc3 = c(); bnd_pc4 = c(); bnd_pc5 = c();
for (i in 7:nrow(merged_df)){
  bnd_pc1[i] <- (merged_df$bond10yr_adj_close[i - 1] - merged_df$bond10yr_adj_close[i - 2]) 
  bnd_pc2[i] <- (merged_df$bond10yr_adj_close[i - 1] - merged_df$bond10yr_adj_close[i - 3]) 
  bnd_pc3[i] <- (merged_df$bond10yr_adj_close[i - 1] - merged_df$bond10yr_adj_close[i - 4]) 
  bnd_pc4[i] <- (merged_df$bond10yr_adj_close[i - 1] - merged_df$bond10yr_adj_close[i - 5]) 
  bnd_pc5[i] <- (merged_df$bond10yr_adj_close[i - 1] - merged_df$bond10yr_adj_close[i - 6]) 
}

# compute price change for crude oil
oil_pc1 = c(); oil_pc2= c(); oil_pc3 = c(); oil_pc4 = c(); oil_pc5 = c();
for (i in 7:nrow(merged_df)){
  oil_pc1[i] <- (merged_df$crude_oil_price_usd[i - 1] - merged_df$crude_oil_price_usd[i - 2]) 
  oil_pc2[i] <- (merged_df$crude_oil_price_usd[i - 1] - merged_df$crude_oil_price_usd[i - 3]) 
  oil_pc3[i] <- (merged_df$crude_oil_price_usd[i - 1] - merged_df$crude_oil_price_usd[i - 4]) 
  oil_pc4[i] <- (merged_df$crude_oil_price_usd[i - 1] - merged_df$crude_oil_price_usd[i - 5]) 
  oil_pc5[i] <- (merged_df$crude_oil_price_usd[i - 1] - merged_df$crude_oil_price_usd[i - 6]) 
}

# create categorical values for snp
snp.sd = sd(merged_df$snp_adj_close)
snp_cat1 = c(); snp_cat2= c(); snp_cat3 = c(); snp_cat4 = c(); snp_cat5 = c();
for (i in 7:nrow(merged_df)){
  
  snp_cat1[i] = ifelse(snp_pc1[i] < -1 * snp.sd, 'awful', 
                       (ifelse((snp_pc1[i] >= -1 * snp.sd && snp_pc1[i] < -0.3 * snp.sd), 'Bad',
                               (ifelse((snp_pc1[i] >= -0.3 * snp.sd && snp_pc1[i] < 0.3 * snp.sd), 'Unchanged',
                                       (ifelse((snp_pc1[i] >= 0.3 * snp.sd && snp_pc1[i] < snp.sd), 'Good',
                                               (ifelse((snp_pc1[i] >= snp.sd), 'Great', 'None')))))))))
  snp_cat2[i] = ifelse(snp_pc2[i] < -1 * snp.sd, 'awful', 
                       (ifelse((snp_pc2[i] >= -1 * snp.sd && snp_pc2[i] < -0.3 * snp.sd), 'Bad',
                               (ifelse((snp_pc2[i] >= -0.3 * snp.sd && snp_pc2[i] < 0.3 * snp.sd), 'Unchanged',
                                       (ifelse((snp_pc2[i] >= 0.3 * snp.sd && snp_pc2[i] < snp.sd), 'Good',
                                               (ifelse((snp_pc2[i] >= snp.sd), 'Great', 'None')))))))))
  
  snp_cat3[i] = ifelse(snp_pc3[i] < -1 * snp.sd, 'awful', 
                       (ifelse((snp_pc3[i] >= -1 * snp.sd && snp_pc3[i] < -0.3 * snp.sd), 'Bad',
                               (ifelse((snp_pc3[i] >= -0.3 * snp.sd && snp_pc3[i] < 0.3 * snp.sd), 'Unchanged',
                                       (ifelse((snp_pc3[i] >= 0.3 * snp.sd && snp_pc3[i] < snp.sd), 'Good',
                                               (ifelse((snp_pc3[i] >= snp.sd), 'Great', 'None')))))))))
  
  snp_cat4[i] = ifelse(snp_pc4[i] < -1 * snp.sd, 'awful', 
                       (ifelse((snp_pc4[i] >= -1 * snp.sd && snp_pc4[i] < -0.3 * snp.sd), 'Bad',
                               (ifelse((snp_pc4[i] >= -0.3 * snp.sd && snp_pc4[i] < 0.3 * snp.sd), 'Unchanged',
                                       (ifelse((snp_pc4[i] >= 0.3 * snp.sd && snp_pc4[i] < snp.sd), 'Good',
                                               (ifelse((snp_pc4[i] >= snp.sd), 'Great', 'None')))))))))
  snp_cat5[i] = ifelse(snp_pc5[i] < -1 * snp.sd, 'awful', 
                       (ifelse((snp_pc5[i] >= -1 * snp.sd && snp_pc5[i] < -0.3 * snp.sd), 'Bad',
                               (ifelse((snp_pc5[i] >= -0.3 * snp.sd && snp_pc5[i] < 0.3 * snp.sd), 'Unchanged',
                                       (ifelse((snp_pc5[i] >= 0.3 * snp.sd && snp_pc5[i] < snp.sd), 'Good',
                                               (ifelse((snp_pc5[i] >= snp.sd), 'Great', 'None')))))))))
  
  
}

# create categorical values for 10yrbond
bnd.sd = sd(merged_df$bond10yr_adj_close)
bnd_cat1 = c(); bnd_cat2= c(); bnd_cat3 = c(); bnd_cat4 = c(); bnd_cat5 = c();
for (i in 7:nrow(merged_df)){
  
  bnd_cat1[i] = ifelse(bnd_pc1[i] < -1 * bnd.sd, 'awful', 
                       (ifelse((bnd_pc1[i] >= -1 * bnd.sd && bnd_pc1[i] < -0.3 * bnd.sd), 'Bad',
                               (ifelse((bnd_pc1[i] >= -0.3 * bnd.sd && bnd_pc1[i] < 0.3 * bnd.sd), 'Unchanged',
                                       (ifelse((bnd_pc1[i] >= 0.3 * bnd.sd && bnd_pc1[i] < bnd.sd), 'Good',
                                               (ifelse((bnd_pc1[i] >= bnd.sd), 'Great', 'None')))))))))
  bnd_cat2[i] = ifelse(bnd_pc2[i] < -1 * bnd.sd, 'awful', 
                       (ifelse((bnd_pc2[i] >= -1 * bnd.sd && bnd_pc2[i] < -0.3 * bnd.sd), 'Bad',
                               (ifelse((bnd_pc2[i] >= -0.3 * bnd.sd && bnd_pc2[i] < 0.3 * bnd.sd), 'Unchanged',
                                       (ifelse((bnd_pc2[i] >= 0.3 * bnd.sd && bnd_pc2[i] < bnd.sd), 'Good',
                                               (ifelse((bnd_pc2[i] >= bnd.sd), 'Great', 'None')))))))))
  
  bnd_cat3[i] = ifelse(bnd_pc3[i] < -1 * bnd.sd, 'awful', 
                       (ifelse((bnd_pc3[i] >= -1 * bnd.sd && bnd_pc3[i] < -0.3 * bnd.sd), 'Bad',
                               (ifelse((bnd_pc3[i] >= -0.3 * bnd.sd && bnd_pc3[i] < 0.3 * bnd.sd), 'Unchanged',
                                       (ifelse((bnd_pc3[i] >= 0.3 * bnd.sd && bnd_pc3[i] < bnd.sd), 'Good',
                                               (ifelse((bnd_pc3[i] >= bnd.sd), 'Great', 'None')))))))))
  
  bnd_cat4[i] = ifelse(bnd_pc4[i] < -1 * bnd.sd, 'awful', 
                       (ifelse((bnd_pc4[i] >= -1 * bnd.sd && bnd_pc4[i] < -0.3 * bnd.sd), 'Bad',
                               (ifelse((bnd_pc4[i] >= -0.3 * bnd.sd && bnd_pc4[i] < 0.3 * bnd.sd), 'Unchanged',
                                       (ifelse((bnd_pc4[i] >= 0.3 * bnd.sd && bnd_pc4[i] < bnd.sd), 'Good',
                                               (ifelse((bnd_pc4[i] >= bnd.sd), 'Great', 'None')))))))))
  bnd_cat5[i] = ifelse(bnd_pc5[i] < -1 * bnd.sd, 'awful', 
                       (ifelse((bnd_pc5[i] >= -1 * bnd.sd && bnd_pc5[i] < -0.3 * bnd.sd), 'Bad',
                               (ifelse((bnd_pc5[i] >= -0.3 * bnd.sd && bnd_pc5[i] < 0.3 * bnd.sd), 'Unchanged',
                                       (ifelse((bnd_pc5[i] >= 0.3 * bnd.sd && bnd_pc5[i] < bnd.sd), 'Good',
                                               (ifelse((bnd_pc5[i] >= bnd.sd), 'Great', 'None')))))))))
  
}

# create categorical values for 10yrbond
oil.sd = sd(merged_df$crude_oil_price_usd)
oil_cat1 = c(); oil_cat2= c(); oil_cat3 = c(); oil_cat4 = c(); oil_cat5 = c();
for (i in 7:nrow(merged_df)){
  
  oil_cat1[i] = ifelse(oil_pc1[i] < -1 * oil.sd, 'awful', 
                       (ifelse((oil_pc1[i] >= -1 * oil.sd && oil_pc1[i] < -0.3 * oil.sd), 'Bad',
                               (ifelse((oil_pc1[i] >= -0.3 * oil.sd && oil_pc1[i] < 0.3 * oil.sd), 'Unchanged',
                                       (ifelse((oil_pc1[i] >= 0.3 * oil.sd && oil_pc1[i] < oil.sd), 'Good',
                                               (ifelse((oil_pc1[i] >= oil.sd), 'Great', 'None')))))))))
  oil_cat2[i] = ifelse(oil_pc2[i] < -1 * oil.sd, 'awful', 
                       (ifelse((oil_pc2[i] >= -1 * oil.sd && oil_pc2[i] < -0.3 * oil.sd), 'Bad',
                               (ifelse((oil_pc2[i] >= -0.3 * oil.sd && oil_pc2[i] < 0.3 * oil.sd), 'Unchanged',
                                       (ifelse((oil_pc2[i] >= 0.3 * oil.sd && oil_pc2[i] < oil.sd), 'Good',
                                               (ifelse((oil_pc2[i] >= oil.sd), 'Great', 'None')))))))))
  
  oil_cat3[i] = ifelse(oil_pc3[i] < -1 * oil.sd, 'awful', 
                       (ifelse((oil_pc3[i] >= -1 * oil.sd && oil_pc3[i] < -0.3 * oil.sd), 'Bad',
                               (ifelse((oil_pc3[i] >= -0.3 * oil.sd && oil_pc3[i] < 0.3 * oil.sd), 'Unchanged',
                                       (ifelse((oil_pc3[i] >= 0.3 * oil.sd && oil_pc3[i] < oil.sd), 'Good',
                                               (ifelse((oil_pc3[i] >= oil.sd), 'Great', 'None')))))))))
  
  oil_cat4[i] = ifelse(oil_pc4[i] < -1 * oil.sd, 'awful', 
                       (ifelse((oil_pc4[i] >= -1 * oil.sd && oil_pc4[i] < -0.3 * oil.sd), 'Bad',
                               (ifelse((oil_pc4[i] >= -0.3 * oil.sd && oil_pc4[i] < 0.3 * oil.sd), 'Unchanged',
                                       (ifelse((oil_pc4[i] >= 0.3 * oil.sd && oil_pc4[i] < oil.sd), 'Good',
                                               (ifelse((oil_pc4[i] >= oil.sd), 'Great', 'None')))))))))
  oil_cat5[i] = ifelse(oil_pc5[i] < -1 * oil.sd, 'awful', 
                       (ifelse((oil_pc5[i] >= -1 * oil.sd && oil_pc5[i] < -0.3 * oil.sd), 'Bad',
                               (ifelse((oil_pc5[i] >= -0.3 * oil.sd && oil_pc5[i] < 0.3 * oil.sd), 'Unchanged',
                                       (ifelse((oil_pc5[i] >= 0.3 * oil.sd && oil_pc5[i] < oil.sd), 'Good',
                                               (ifelse((oil_pc5[i] >= oil.sd), 'Great', 'None')))))))))
  
}

# combine all the columns for the final time and remove the top 7 rows that have NA value for categories
final_merged_df <- data.frame(priceDir, snp_cat1, snp_cat2, snp_cat3, snp_cat4, snp_cat5, bnd_cat1, bnd_cat2, bnd_cat3, bnd_cat4, bnd_cat5, oil_cat1, oil_cat2, oil_cat3, oil_cat4, oil_cat5 )
final_merged_df <- tail(final_merged_df, -6)

# partition dataset into training and test
train <- final_merged_df[1:5131,]
test <- final_merged_df[5132:nrow(final_merged_df),]

# Modelling using NaiveBayes
final_merged_df$bnd_cat1 = as.factor(final_merged_df$bnd_cat1, levels = c("awful","Bad","Good","Great","Unchanged"))
model.NB <- NaiveBayes(priceDir ~ snp_cat3+ snp_cat4 + snp_cat5 + bnd_cat4 + bnd_cat5 + oil_cat1 + oil_cat2 + oil_cat3 + oil_cat4 + oil_cat5 ,data=train)
predictions <- predict(model.NB, test)
confusionMatrix(test$priceDir, predictions$class)

# Modelling using Recursive Partition Tree
model.rpt <- rpart(priceDir ~ snp_cat3+ snp_cat4 + snp_cat5 + bnd_cat4 + bnd_cat5 + oil_cat1 + oil_cat2 + oil_cat3 + oil_cat4 + oil_cat5, data=train, cp=0)
plot(model.rpt)
text(model.rpt, use.n= T, digits=3, cex=0.6)
prediction.rpt <- predict(model.rpt, newdata = test, type="class")
printcp(model.rpt)
