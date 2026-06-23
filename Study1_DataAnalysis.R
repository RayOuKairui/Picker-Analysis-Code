setwd("~/Desktop/Picker/Analysis/Study 1 Final")

library(readr)
ck<-read_csv("Secrecy X Relationship X Work NEW_June 7, 2026_15.10.csv") #Where the file will be imported
ck <- ck[-c(1, 2), ]                                              # drop Qualtrics rows first
ck <- ck[is.na(ck$honesty) | as.numeric(as.character(ck$honesty)) != 2, ]  # 2 = lied, drop them
 
# demographics
table(ck$gender); table(as.numeric(as.character(ck$age)));

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

library(robustbase)
library(readr)

adjbox(rbind(cm.$hide, cm.$think))$fence
adjbox(rbind(cm.$hide, cm.$think))$fence[2] -> exc; exc

cm.$hide. <- cm.$hide
cm.$hide.[cm.$hide > exc] <- NA

cm.$think. <- cm.$think
cm.$think.[cm.$think > exc] <- NA

mean(as.numeric(cm.$think.), na.rm = TRUE)
mean(as.numeric(cm.$hide.), na.rm = TRUE)

table(cm.$hide); table(cm.$hide.)
table(cm.$think); table(cm.$think.)

length(cm.$hide[cm.$hide > exc]) + length(cm.$think[cm.$think > exc])
length(cm.$ResponseId) * 2

head(cm.)

cm.$relationship <- rowMeans(cm.[, c("ratings_1", "ratings_2", "ratings_3")], na.rm = TRUE)
cm.$task <- rowMeans(cm.[, c("ratings_4", "ratings_5", "ratings_6")], na.rm = TRUE)
cm.$work <- as.numeric(as.character(cm.$"ratings_7"))

#Descriptive Data
mean(as.numeric(cm.$task), na.rm = TRUE)
sd(as.numeric(cm.$task), na.rm = TRUE)
t.test(as.numeric(cm.$task),, na.rm = TRUE)$conf.int                 # 95% CI of descriptive data
mean(as.numeric(cm.$relationship), na.rm = TRUE)
sd(as.numeric(cm.$relationship), na.rm = TRUE)
t.test(as.numeric(cm.$relationship),, na.rm = TRUE)$conf.int 

library(lme4)
library(lmerTest)

summary(lmer(task~think.+hide.+(1|ResponseId)+(1|X1),cm.)) 
summary(lmer(relationship~think.+hide.+(1|ResponseId)+(1|X1),cm.))

summary(lmer(task~think.+hide.+relationship+(1|ResponseId)+(1|X1),cm.))
summary(lmer(relationship~think.+hide.+task+(1|ResponseId)+(1|X1),cm.))

confint(lmer(task~think.+hide.+relationship+(1|ResponseId)+(1|X1),cm.))
confint(lmer(relationship~think.+hide.+task+(1|ResponseId)+(1|X1),cm.))

m <- lmer(task~think.+hide.+relationship+(1|ResponseId)+(1|X1),cm.)

lmerTest::ranova(m)
lmerTest::ranova(relationship~think.+hide.+task+(1|ResponseId)+(1|X1),cm.)

summary(lmer(work~task+relationship+think.+hide.+(1|ResponseId)+(1|X1),cm.))
confint(lmer(work~task+relationship+think.+hide.+(1|ResponseId)+(1|X1),cm.))

summary(lmer(work~think.+hide.+(1|ResponseId)+(1|X1),cm.))
confint(lmer(work~think.+hide.+(1|ResponseId)+(1|X1),cm.))

iterations<-5000 # change to 1000 when you know it works

matrix(nrow=iterations,ncol=7)->boottable
as.data.frame(boottable)->boottable
names(boottable)<-c("iter", "a","a2","a3","a4","b","b2")

dim(cm.)[1]->n
for(i in 1:iterations){
  sample(c(1:n),n,replace=T)->sampnums;
  cm.[sampnums,]$think.->ithink.;
  cm.[sampnums,]$hide.->ihide.;
  cm.[sampnums,]$task->itask;
  cm.[sampnums,]$relationship->irelationship;
  cm.[sampnums,]$work->iwork;
  cm.[sampnums,]$ResponseId-> iResponseId;
  cm.[sampnums,]$X1-> iX1;
  
  
  fixef(lmer(itask~ithink.+ihide.+irelationship+(1|iResponseId)+(1|iX1)))[2]->itera; # think -> task
  fixef(lmer(itask~ithink.+ihide.+irelationship+(1|iResponseId)+(1|iX1)))[3]->itera2; # hide -> task
  fixef(lmer(irelationship~ithink.+ihide.+itask+(1|iResponseId)+(1|iX1)))[2]->itera3; # think -> rel
  fixef(lmer(irelationship~ithink.+ihide.+itask+(1|iResponseId)+(1|iX1)))[3]->itera4; # hide -> rel
  fixef(lmer(iwork~itask+irelationship+ithink.+ihide.+(1|iResponseId)+(1|iX1)))[2]->iterb; # task -> work
  fixef(lmer(iwork~itask+irelationship+ithink.+ihide.+(1|iResponseId)+(1|iX1)))[3]->iterb2; # rel -> work
  
  
  i->boottable[i,1];
  itera->boottable[i,2];
  itera2->boottable[i,3];
  itera3->boottable[i,4];
  itera4->boottable[i,5];
  iterb->boottable[i,6]
  iterb2->boottable[i,7]
}
# mean indirect effect
mean(boottable$a*boottable$b) # think -> task -> work
mean(boottable$a2*boottable$b) # hide -> task -> work
mean(boottable$a3*boottable$b2) # think -> rel -> work
mean(boottable$a4*boottable$b2) # hide -> rel -> work
#SE
sd(boottable$a*boottable$b)/sqrt(length(boottable$iter)) # think -> task -> work
sd(boottable$a2*boottable$b)/sqrt(length(boottable$iter)) # hide -> task -> work
sd(boottable$a3*boottable$b2)/sqrt(length(boottable$iter)) # think -> rel -> work
sd(boottable$a4*boottable$b2)/sqrt(length(boottable$iter)) # hide -> rel -> work
#95 CI
quantile(with(boottable,a*b),c(.025,.975)) # think -> task -> work
quantile(with(boottable,a2*b),c(.025,.975)) # hide -> task -> work
quantile(with(boottable,a3*b2),c(.025,.975)) # think -> rel -> work
quantile(with(boottable,a4*b2),c(.025,.975)) # hide -> rel -> work

# > # mean indirect effect
#   > mean(boottable$a*boottable$b) # think -> task -> work
# [1] 0.02947624
# > mean(boottable$a2*boottable$b) # hide -> task -> work
# [1] 0.01751393
# > mean(boottable$a3*boottable$b2) # think -> rel -> work
# [1] 0.03088575
# > mean(boottable$a4*boottable$b2) # hide -> rel -> work
# [1] 0.04497118
# > #SE
#   > sd(boottable$a*boottable$b)/sqrt(length(boottable$iter)) # think -> task -> work
# [1] 0.0001962684
# > sd(boottable$a2*boottable$b)/sqrt(length(boottable$iter)) # hide -> task -> work
# [1] 0.0001976897
# > sd(boottable$a3*boottable$b2)/sqrt(length(boottable$iter)) # think -> rel -> work
# [1] 0.0001860522
# > sd(boottable$a4*boottable$b2)/sqrt(length(boottable$iter)) # hide -> rel -> work
# [1] 0.000182121
# > #95 CI
#   > quantile(with(boottable,a*b),c(.025,.975)) # think -> task -> work
# 2.5%       97.5% 
#   0.004282061 0.058732073 
# > quantile(with(boottable,a2*b),c(.025,.975)) # hide -> task -> work
# 2.5%        97.5% 
#   -0.008760674  0.046228957 
# > quantile(with(boottable,a3*b2),c(.025,.975)) # think -> rel -> work
# 2.5%       97.5% 
#   0.007407492 0.058501873 
# > quantile(with(boottable,a4*b2),c(.025,.975)) # hide -> rel -> work
# 2.5%      97.5% 
#   0.02129463 0.07164713

th <- cm.[c("ResponseId", "X1", "think.", "hide.", "relationship", "task")]
# Stack think. and hide. on top of each other while keeping relationship and task as outcome variables
th.m <- melt(th, id.vars = c("ResponseId", "X1", "relationship", "task"))

head(th.m)

# Rename value as avg.rating
names(th.m)[names(th.m) == "value"] <- "avg.rating"
unique(th.m$variable)

# Create think1hide0: 1 means think., 0 means hide.
th.m$think1hide0 <- ifelse(grepl("think", th.m$variable), 1, 0)

# Keep only the final variables
th. <- th.m[, c("ResponseId", "X1", "relationship", "task", "avg.rating", "think1hide0", "think0hide1")]

# Drop rows where avg.rating is missing
th. <- th.[!is.na(th.$avg.rating), ]
head(th.)

summary(lmer(avg.rating ~ think1hide0+ (1|ResponseId) + (1|X1), th.))