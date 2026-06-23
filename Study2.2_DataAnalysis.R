setwd("~/Desktop/Picker/Analysis/Study 2.2")
options(scipen = 0)

library(readr)
ck<-read_csv("Told X Heard 2 X Secrecy_June 9, 2026_14.29.csv") #Where the file will be imported
ck <- ck[-c(1, 2), ]        # drop Qualtrics question-text row and ImportId row
ck <- ck[is.na(ck$honesty) | as.numeric(as.character(ck$honesty)) != 2, ]  # 2 = lied, drop them

# demographics
table(ck$gender); table(as.numeric(as.character(ck$age)));
mean(as.numeric(ck$age), na.rm=TRUE)
sd(as.numeric(ck$age), na.rm=TRUE)

head(ck)

# drop empty variables (questions not in survey flow)
dropNA<-function(x){x <<- x[,colSums(is.na(x))<nrow(x)]}
ck<-dropNA(ck)

head(ck)

names(ck) #The variables

library(reshape2)

# reshape
# get variable names with XX
cx..<-ck[c("ResponseId",grep("XX", names(ck), value = TRUE))];
head(cx..)
# make long
cx.m <- melt(cx.., id.vars = c("ResponseId"));
head(cx.m)
# extract labels within variable name
cx.m. <- data.frame(do.call('rbind', strsplit(as.character(cx.m$variable),'XX',fixed=TRUE)));
head(cx.m.)
# add in the labels within variable name
cm<-cbind(cx.m,cx.m.);
cm$value<-as.numeric(as.character(cm$value));  
head(cm)

# make wide again
cm.<-dcast(cm, ResponseId + X1 ~ X2, fun.aggregate = mean, value.var="value");
head(cm.)

# drop empty rows
cm. <- cm.[, colSums(is.na(cm.)) < nrow(cm.)]; cm. <- cm.[rowSums(is.na(cm.)) < ncol(cm.) - 2, ]
head(cm.)

length(cm.$ResponseId)

# Create average ratings for epistemic and relational motives separately for told and heard
cm.$told_epistemic <- rowMeans(cm.[, c("told_1", "told_2", "told_3")], na.rm = TRUE)
cm.$told_relational <- rowMeans(cm.[, c("told_4", "told_5", "told_6")], na.rm = TRUE)

cm.$heard_epistemic <- rowMeans(cm.[, c("heard_1", "heard_2", "heard_3")], na.rm = TRUE)
cm.$heard_relational <- rowMeans(cm.[, c("heard_4", "heard_5", "heard_6")], na.rm = TRUE)

mean(cm.$told_epistemic, na.rm = TRUE)
mean(cm.$told_relational, na.rm = TRUE)
mean(cm.$heard_epistemic, na.rm = TRUE)
mean(cm.$heard_relational, na.rm = TRUE)
sd(cm.$told_epistemic, na.rm = TRUE)
sd(cm.$told_relational, na.rm = TRUE)
sd(cm.$heard_epistemic, na.rm = TRUE)
sd(cm.$heard_relational, na.rm = TRUE)

head(cm.)

# Stack the four average ratings on top of each other

th <- cm.[c("ResponseId","X1","told_epistemic","told_relational","heard_epistemic","heard_relational")]
th.m <- melt(th, id.vars = c("ResponseId","X1"));

head(th.m)

names(th.m)[names(th.m) == "value"] <- "avg.rating"
unique(th.m$variable)

th.m$ep1rel0 <- ifelse(grepl("epistemic", th.m$variable), 1, 0) # 1 means epistemic item, and 0 means relational item
th.m$told1heard0 <- ifelse(grepl("told", th.m$variable), 1, 0) # 1 means told, 0 means heard
th.m$ep0rel1 <- ifelse(grepl("relational", th.m$variable), 1, 0) # 0 means epistemic item, and 1 means relational item
th.m$told0heard1 <- ifelse(grepl("heard", th.m$variable), 1, 0) # 0 means told, 1 means heard

th. <- th.m[, c("ResponseId", "X1", "avg.rating", "ep1rel0", "told1heard0","ep0rel1","told0heard1")]
th. <- th.[!is.na(th.$avg.rating), ]

head(th.)

library(lme4)
library(lmerTest)

summary(lmer(avg.rating ~ ep1rel0 * told1heard0 + (1|ResponseId) + (1|X1), th.))
summary(lmer(avg.rating ~ ep0rel1 * told0heard1 + (1|ResponseId) + (1|X1), th.))
summary(lmer(avg.rating ~ ep1rel0 * told0heard1 + (1|ResponseId) + (1|X1), th.))
summary(lmer(avg.rating ~ ep0rel1 * told1heard0 + (1|ResponseId) + (1|X1), th.))
confint(lmer(avg.rating ~ ep1rel0 * told1heard0 + (1|ResponseId) + (1|X1), th.))
confint(lmer(avg.rating ~ ep0rel1 * told0heard1 + (1|ResponseId) + (1|X1), th.))
confint(lmer(avg.rating ~ ep1rel0 * told0heard1 + (1|ResponseId) + (1|X1), th.))
confint(lmer(avg.rating ~ ep0rel1 * told1heard0 + (1|ResponseId) + (1|X1), th.))