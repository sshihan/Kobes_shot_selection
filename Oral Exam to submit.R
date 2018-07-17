# Data Science Comprehensive exam
# Sheri Boerger
# Shaheed Shihan 
# Kobe Bryant Shot Selection 

# Appendix A

library(plyr)
library(dplyr)
library(ggplot2)

kobe <- read.csv("~/Dropbox/Oral Exam/FinalData.csv")

kobe1 <- subset(kobe, select = c(-game_event_id,-lat, -lon, -game_id, -team_id,
                                 -team_name))

##Converting matchup to a binary factor variable : home and away
kobe1$matchup <- as.character(kobe1$matchup)
kobe1$matchup <- sub("\\w* vs. \\w*", "Home", kobe1$matchup, perl = TRUE)
kobe1$matchup <- sub("\\w* @ \\w*", "Away", kobe1$matchup, perl = TRUE)
kobe1$matchup <- as.factor(kobe1$matchup)

##Converting to numeric
# cols = c(3:5,9,10,25)   
# kobe1[,cols] <- lapply(kobe1[,cols], as.numeric)

# str(kobe1) #Checking

# Extracting the month for each game -interested in seeing how Kobe
# does over the seasons during the same times of the year
kobe1$game_date <- months(as.Date(as.character(kobe1$game_date),format = "%m/%d/%Y"))
kobe1$game_date <- as.factor(kobe1$game_date)

# Reducing the number of factors in action type. Grouping together shots that he made less than 20 times
# and labelling them as "Other"
kobe1$action_type <- as.character(kobe1$action_type)
kobe1 %>% count(action_type) %>%
  arrange(desc(n)) %>% filter(n < 20) -> Other
kobe1$action_type[kobe1$action_type %in% Other$action_type] <- "Other"
kobe1$action_type <- as.factor(kobe1$action_type)
kobe1$AboveAvg <- as.factor(kobe1$AboveAvg)


str(kobe1)
##Reordering to bring response variable to the beginning
kobe1 <- kobe1[,c(11,1:10,12:34)]

#str(kobe1)


##Splitting train and test
train <- kobe1[!is.na(kobe$shot_made_flag),]
test <- kobe1[is.na(kobe$shot_made_flag),]

## Creating validation set
rows <- sample(1:nrow(train),nrow(train)*0.7)
training <- train[rows,] 
validset <- train[-rows,]


# Appendix B
##Graphs

library(scales)
library(ggplot2)
library(dplyr)


## Appendix B
Prop <- function(Variable){
  p <- as.data.frame.matrix(prop.table(table(Variable, shot_made_flag),1))
  return(p)
}
attach(train)
Prop(action_type)
Prop(combined_shot_type)
#Prop(TimeRemaining) - need scatterplot
Prop(period)
Prop(playoffs)
Prop(season)
Prop(seconds_remaining)
Prop(shot_type)
Prop(shot_zone_area)
Prop(shot_zone_basic)
Prop(shot_zone_range)
Prop(matchup)
Prop(PreviousWinLoss)
Prop(Game_Half)
Prop(Rest)
Prop(Rival)

##Plotting time! 

gplot <- function(V){
  ggplot(Prop(V), aes(x = reorder(row.names(Prop(V)),Prop(V)[,2]), 
                      y = Prop(V)[,2])) + 
    geom_point(size = 3) + 
    coord_flip() + ggtitle(paste("Accuracy")) +
    labs(y = "Percentage", x = "")
}

gplot(action_type)
gplot(combined_shot_type)
gplot(minutes_remaining)
gplot(period)
gplot(seconds_remaining)
gplot(game_date)
gplot(season)
gplot(opponent)
gplot(Rest)


##Not a significant difference between matches at Home and away
ggplot(train, aes(x = matchup, fill = as.factor(shot_made_flag))) + geom_bar(position = "fill") + 
  ggtitle("Accuracy at Home vs. Away") + ylab("Percentage")
#Prop(matchup)

ggplot(train, aes(x = as.factor(playoffs), fill = as.factor(shot_made_flag))) + geom_bar(position = "fill") +
  ggtitle("Accuracy Regular Season vs. Playoffs") + ylab("Percentage") +
  scale_y_continuous(labels=percent)
#Prop(playoffs)

##Not significantly different
ggplot(train, aes(x = Rest, fill = as.factor(shot_made_flag))) + geom_bar(position = "fill") + 
  ggtitle("Accuracy by Rest Days") + ylab("Percentage") +
  scale_y_continuous(labels=percent)

## Bar plot with Game half and accuracy
ggplot(train, aes(x = Game_Half, fill = as.factor(shot_made_flag))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels=percent) +
  ggtitle("Accuracy") + ylab("Percentage")
#Prop(Game_Half)  

##Accuracy of 2pt/3pt by Period
t <- filter(train) %>% 
  group_by(period, shot_type, shot_made_flag) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(t, aes(x = period, y = n, fill = as.factor(shot_made_flag))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~shot_type) 

##Accuracy of 2pt/3pt by Playoffs
t1 <- filter(train) %>% 
  group_by(playoffs, shot_type, shot_made_flag) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(t1, aes(x = as.factor(playoffs), y = n, fill = as.factor(shot_made_flag))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~shot_type) 


## Shot Charts
library(rjson)
library(ggplot2)
library(grid)
library(gridExtra)
library(png)
library(jpeg)
library(RCurl)
library(hexbin)
library(plyr)

ggplot(train, aes(x=loc_x, y=loc_y)) +
  geom_point(aes(colour = as.factor(shot_made_flag)))


courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg" 
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

# plot using NBA court background and colour by shot zone
ggplot(train, aes(x=loc_x, y=loc_y)) + 
  annotation_custom(court, -250, 250, -50, 418) +
  geom_point(aes(colour = as.factor(shot_zone_basic), shape = as.factor(shot_made_flag))) +
  xlim(-250, 250) +
  ylim(-50, 418)

## Removing shots from Backcourt
shotDataS <- train[which(!train$shot_zone_basic=='Backcourt'), ]

# summarise shot data
library(plyr)
shotS <- ddply(shotDataS, .(shot_zone_basic), summarize, 
               SHOTS_ATTEMPTED = length(shot_made_flag),
               SHOTS_MADE = sum(as.numeric(as.character(shot_made_flag))),
               MLOC_X = mean(loc_x),
               MLOC_Y = mean(loc_y))

# calculate shot zone accuracy and add zone accuracy labels
shotS$SHOT_ACCURACY <- (shotS$SHOTS_MADE / shotS$SHOTS_ATTEMPTED)
shotS$SHOT_ACCURACY_LAB <- paste(as.character(round(100 * shotS$SHOT_ACCURACY, 1)), "%", sep="")

# plot shot accuracy per zone
ggplot(shotS, aes(x=MLOC_X, y=MLOC_Y)) + 
  annotation_custom(court, -250, 250, -52, 418) +
  geom_point(aes(colour = shot_zone_basic, size = SHOT_ACCURACY, alpha = 0.8), size = 8) +
  geom_text(aes(colour = shot_zone_basic, label = SHOT_ACCURACY_LAB), vjust = -1.2, size = 8) +
  guides(alpha = FALSE, size = FALSE) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  #ggtitle(paste("Shot Accuracy\n", unique(shotDataf$PLAYER_NAME), sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size = 12),
        plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))


## Shot chart by area
shotS1 <- ddply(shotDataS, .(shot_zone_area), summarize, 
                SHOTS_ATTEMPTED = length(shot_made_flag),
                SHOTS_MADE = sum(as.numeric(as.character(shot_made_flag))),
                MLOC_X = mean(loc_x),
                MLOC_Y = mean(loc_y))

# calculate shot area accuracy and add area accuracy labels
shotS1$SHOT_ACCURACY <- (shotS1$SHOTS_MADE / shotS1$SHOTS_ATTEMPTED)
shotS1$SHOT_ACCURACY_LAB <- paste(as.character(round(100 * shotS1$SHOT_ACCURACY, 1)), "%", sep="")


##Plot
# plot shot accuracy by area
ggplot(shotS1, aes(x=MLOC_X, y=MLOC_Y)) + 
  annotation_custom(court, -250, 250, -52, 418) +
  geom_point(aes(colour = shot_zone_area, size = SHOT_ACCURACY, alpha = 0.8), size = 8) +
  geom_text(aes(colour = shot_zone_area, label = SHOT_ACCURACY_LAB), vjust = -1.2, size = 8) +
  guides(alpha = FALSE, size = FALSE) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  #ggtitle(paste("Shot Accuracy\n", unique(shotDataf$PLAYER_NAME), sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size = 12),
        plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))



## Shot chart by range
shotS2 <- ddply(shotDataS, .(shot_zone_range), summarize, 
                SHOTS_ATTEMPTED = length(shot_made_flag),
                SHOTS_MADE = sum(as.numeric(as.character(shot_made_flag))),
                MLOC_X = mean(loc_x),
                MLOC_Y = mean(loc_y))

# calculate shot area accuracy and add area accuracy labels
shotS2$SHOT_ACCURACY <- (shotS2$SHOTS_MADE / shotS2$SHOTS_ATTEMPTED)
shotS2$SHOT_ACCURACY_LAB <- paste(as.character(round(100 * shotS2$SHOT_ACCURACY, 1)), "%", sep="")


##Plot
# plot shot accuracy by area
ggplot(shotS2, aes(x=MLOC_X, y=MLOC_Y)) + 
  annotation_custom(court, -250, 250, -52, 418) +
  geom_point(aes(colour = shot_zone_range, size = SHOT_ACCURACY, alpha = 0.8), size = 8) +
  geom_text(aes(colour = shot_zone_range, label = SHOT_ACCURACY_LAB), vjust = -1.2, size = 8) +
  guides(alpha = FALSE, size = FALSE) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  #ggtitle(paste("Shot Accuracy\n", unique(shotDataf$PLAYER_NAME), sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size = 12),
        plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))




# Appendix C
# Modelling time!
## Log Loss function as adapted from Kaggle
MultiLogLoss <- function(act, pred)
{
  eps = 1e-15;
  nr <- nrow(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  ll = sum(act*log(pred) + (1-act)*log(1-pred))
  ll = ll * -1/(nrow(act))      
  return(ll);
}

## Feature Selection using rfcv
## Random forest with recursive partitioning and 10 fold cross validation 
## for model selection
set.seed(123)
rf<- rfcv(train[,2:29], train[,1],cv.fold = 3, recursive = TRUE)
rf$n.var
with(rf, plot(n.var, error.cv, log="x", type="o", lwd=2))

## Important variables were: 
# Rival
# action_type
# loc_y
# playoffs
# shot_zone_range

## Features 
frm<- as.factor(shot_made_flag)~ action_type+
  loc_y +
  shot_zone_range +
  SeasonNumber+
  OffDays+
  GameCumPts + 
  GameCumPct+
  AboveAvg+
  win_loss+
  TimeRemaining+
  Game_Half+
  GameNumber


## rpart
set.seed(44)
library(rpart)

# 10-fold cross validation with rpart
fit3 <- rpart(frm, data=training,method = "class",model = TRUE,
              control = rpart.control(xval = 10))

# Similar to the results from rfcv
fit3$variable.importance

pred.rpart.prob <- predict(fit3, newdata=validset, type="prob")
MultiLogLoss(as.matrix(as.numeric(validset$shot_made_flag)),as.matrix(as.matrix(pred.rpart.prob)[,2]))

##[1] 0.6226635

# Confusion Matrix
library(caret)
pred.rpart.class <- predict(fit3, newdata=validset, type="class")
confusionMatrix(pred.rpart.class, validset$shot_made_flag)


## Random Forest and Bagging
set.seed(55)
mod.rf <- randomForest(frm, data=training, mtry=3, ntree=1000, importance = TRUE)
varImpPlot(mod.rf)

pred.rf <- predict(mod.rf, newdata=validset, type="prob")
MultiLogLoss(as.matrix(as.integer(validset$shot_made_flag)),as.matrix(pred.rf[,2]))

## Confusion Matrix
pred.rf.class <- predict(mod.rf, newdata = validset, type="class")
confusionMatrix(pred.rf.class, validset$shot_made_flag)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 4124 3441
# 1   65   80

# Accuracy : 0.5453

## Bagging
set.seed(33)
mod.rf.bagging <- randomForest(frm, data = training, ntree = 1000, mtry = 12, importance = TRUE)
varImpPlot(mod.rf.bagging)

## Prediction on validation set
pred.rf.bagging <- predict(mod.rf.bagging, newdata = validset, type = "prob")
MultiLogLoss(as.matrix(validset$shot_made_flag), as.matrix(pred.rf[,2]))
#[1] 0.7677377

## Confusion Matrix
pred.bagging.class <- predict(mod.rf.bagging, newdata = validset, type="class")
confusionMatrix(pred.rf.class, validset$shot_made_flag)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 4124 3441
# 1   65   80
# 
# Accuracy : 0.5453 

## GlM
set.seed(123)
glm.fit <- glm(frm, data=training, family = binomial)
glm.pred <- predict(glm.fit,validset, type = "response")
MultiLogLoss(as.matrix(as.numeric(validset$shot_made_flag)),as.matrix(glm.pred))

# [1] 0.6125995
# summary(glm.fit)

##Confusion Matrix
glm.pred.class <- ifelse(predict(glm.fit, validset, type='response') > 0.5, 1, 0)
confusionMatrix(glm.pred.class, validset$shot_made_flag)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 3598 1890
# 1  591 1631

# Accuracy : 0.6782 

## Naive Bayes with Laplace Smoothing
set.seed(333)
library(e1071)
##Using only important categorical variables
n <- naiveBayes(as.factor(shot_made_flag)~ action_type+
                  shot_zone_range +
                  SeasonNumber+
                  AboveAvg+
                  win_loss+
                  PreviousWinLoss+
                  Rival+
                  Rest +
                  Game_Half+
                  GameNumber, data = training, laplace = 3)
n.p <- predict(n, validset, "raw")
MultiLogLoss(as.matrix(as.numeric(validset$shot_made_flag)),as.matrix(as.matrix(n.p)[,2]))
# [1] 0.6253583

n1 <- naiveBayes(as.factor(shot_made_flag)~ action_type+
                   shot_zone_range +
                   SeasonNumber+
                   AboveAvg+
                   win_loss+
                   PreviousWinLoss+
                   Rival+
                   Rest +
                   Game_Half+
                   GameNumber, data = train, laplace = 3)
np1 <- predict(n1, test, "raw")


                                                   
nb.predict.class <- predict(n, validset, type = "class")
confusionMatrix(nb.predict.class, validset$shot_made_flag)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 3405 1723
# 1  861 1721
# 
# Accuracy : 0.6649 

##Kaggle score 0.62625


## Predicting on test set and submitting to Kaggle
glm.fit.entire <- glm(frm, train, family = "binomial")
glm.pred.entire <- predict(glm.fit.entire, test, type = "response")
final <- data.frame(shot_id = test$shot_id,shot_made_flag = np1[,2])
write.csv(final, file = "~/Dropbox/Oral Exam/res1.csv")
