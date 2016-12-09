training <- read.csv(file = "D://kaggle//House Prices//train.csv", header = T)
str(training)

# counts of missing values
colSums(sapply(training, is.na)) # all variables
colSums(sapply(training[sapply(training, is.numeric)], is.na)) # numeric variables
colSums(sapply(training[sapply(training, is.factor)], is.na)) # factor variables

# the number of houses that were remodeled
library(dplyr)
library(ggplot2)
training %>% select(YearBuilt, YearRemodAdd) %>% 
mutate(Remodeled = as.integer(YearBuilt != YearRemodAdd)) %>% 
ggplot(aes(x= factor(x = Remodeled, labels = c( 'No','Yes')))) + geom_bar() + xlab('Remodeled') + theme_light()

# The percentage of data missing in training
sum(is.na(training))/(nrow(training)*ncol(training))

# Check for duplicated rows.
cat('The number of duplicate rows =', nrow(training)-nrow(unique(training)))

# barplots of categorical features
barCols <- names(training)[which(sapply(training, is.factor))]
bar.plot <- function(df, cols){
        for(i in cols){
                barplot(table(df[i], useNA = "ifany"), xlab = colnames(df[i]), ylab = "Count")
        }
}
bar.plot(training, barCols)


training %>% select(LandSlope, Neighborhood, SalePrice) %>% 
        filter(LandSlope == c('Sev', 'Mod')) %>% 
        arrange(Neighborhood) %>% group_by(Neighborhood, LandSlope) %>% 
        summarize(Count = n()) %>% ggplot(aes(Neighborhood, Count)) + 
        geom_bar(aes(fill = LandSlope), position = 'dodge', stat = 'identity') + 
        theme_light() +theme(axis.text.x = element_text(angle = 90, hjust =1))
# The houses that have sever landslope are located in the Clear Creek and 
# Timberland. The houses with moderate landslope are present in more neighborhood. 
# The Clear Creek and the Crawford neighborhoods seem to have high slopes.


training %>% select(Neighborhood, SalePrice) %>% 
        ggplot(aes(factor(Neighborhood), SalePrice)) + geom_boxplot() + 
        theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Neighborhoods')
# Plotting a boxplot between the neighboorhoods and sale price shows that 
# BrookSide and South & West of Iowa State University have cheap houses. 
# While Northridge and Northridge Heights are rich neighborhoods with several
# outliers in terms of price.


# Explore the correlation
train_cont <- training[, sapply(training, is.numeric)]
correlations <- cor(na.omit(train_cont))

# correlations
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)

correlations<- correlations[row_indic, row_indic ]
library(corrplot)
corrplot(correlations, method="square")

# strong negative correlation between Yearbuilt and OverallCond
training %>% select(OverallCond, YearBuilt) %>% 
        ggplot(aes(factor(OverallCond),YearBuilt)) + geom_boxplot() + xlab('Overall Condition')
# It seems that recently built houses tend to be in worse Overall Condition

