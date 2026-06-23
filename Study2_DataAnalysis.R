setwd("~/Desktop/Picker/Analysis/Study 2")

library(readr)
ck<-read_csv("Told X Heard X Secrecy_June 4, 2026_13.19.csv") #Where the file will be imported
ck <- ck[-c(1, 2), ]        # drop Qualtrics question-text row and ImportId row
ck <- ck[is.na(ck$honesty) | as.numeric(as.character(ck$honesty)) != 2, ]  # 2 = lied, drop them

# demographics
table(ck$gender); table(as.numeric(as.character(ck$age)));

mean(as.numeric(ck$age), na.rm = T)
sd(as.numeric(ck$age), na.rm = T)

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

cm.$epistemic <- rowMeans(cm.[, c("told_1", "told_2", "told_3","heard_1", "heard_2", "heard_3")], na.rm = TRUE)
cm.$relationship <- rowMeans(cm.[, c("told_4", "told_5", "told_6","heard_4", "heard_5", "heard_6")], na.rm = TRUE)

head(cm.)

cm.$epISheard[!is.na(cm.$heard_1)]<-1
cm.$epISheard[!is.na(cm.$told_1)]<-0

library(lme4)
library(lmerTest)

summary(lmer(epistemic~epISheard+relationship+(1|ResponseId)+(1|X1),cm.)) # 0 = ep told, 1 = ep heard.... if coefficient is positive, then heard is greater than told; if negative, the reverse
summary(lmer(relationship~epISheard+epistemic+(1|ResponseId)+(1|X1),cm.)); # 1 = rel told, 0 = rel heard....if coefficient is negative, then heard is greater than told; if negative, the reverse

confint(lmer(epistemic~epISheard+relationship+(1|ResponseId)+(1|X1),cm.))
confint(lmer(relationship~epISheard+epistemic+(1|ResponseId)+(1|X1),cm.));

mean(cm.$epistemic[cm.$epISheard == 1],na.rm=TRUE)
mean(cm.$epistemic[cm.$epISheard == 0],na.rm=TRUE)
mean(cm.$relationship[cm.$epISheard == 1],na.rm=TRUE)
mean(cm.$relationship[cm.$epISheard == 0],na.rm=TRUE)
sd(cm.$epistemic[cm.$epISheard == 1],na.rm=TRUE)
sd(cm.$epistemic[cm.$epISheard == 0],na.rm=TRUE)
sd(cm.$relationship[cm.$epISheard == 1],na.rm=TRUE)
sd(cm.$relationship[cm.$epISheard == 0],na.rm=TRUE)