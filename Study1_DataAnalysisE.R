setwd("~/Desktop/Picker/Analysis/Study 1 Final")

library(readr)
ck<-read_csv("Secrecy X Relationship X Work NEW_June 7, 2026_15.10.csv") #Where the file will be imported
ck <- ck[-c(1, 2), ]                                              # drop Qualtrics rows first
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

library(robustbase)
library(readr)

adjbox(rbind(cm.$hide, cm.$think))$fence
adjbox(rbind(cm.$hide, cm.$think))$fence[2] -> exc; exc

cm.$hide. <- cm.$hide
cm.$hide.[cm.$hide > exc] <- NA

cm.$think. <- cm.$think
cm.$think.[cm.$think > exc] <- NA

table(cm.$hide); table(cm.$hide.)
table(cm.$think); table(cm.$think.)

length(cm.$hide[cm.$hide > exc]) + length(cm.$think[cm.$think > exc])
length(cm.$ResponseId) *2

head(cm.)

cm.$relationship <- rowMeans(cm.[, c("ratings_1", "ratings_2", "ratings_3")], na.rm = TRUE)
cm.$task <- rowMeans(cm.[, c("ratings_4", "ratings_5", "ratings_6")], na.rm = TRUE)
cm.$work <- as.numeric(as.character(cm.$"ratings_7"))

library(lme4)
library(lmerTest)

summary(lmer(task~think.+hide.+(1|ResponseId)+(1|X1),cm.)) 
summary(lmer(relationship~think.+hide.+(1|ResponseId)+(1|X1),cm.))

summary(lmer(task~think.+hide.+relationship+(1|ResponseId)+(1|X1),cm.))
summary(lmer(relationship~hide.+think.+task+(1|ResponseId)+(1|X1),cm.))

confint(lmer(task~think.+hide.+relationship+(1|ResponseId)+(1|X1),cm.))
confint(lmer(relationship~think.+hide.+task+(1|ResponseId)+(1|X1),cm.))

summary(lmer(work~task+relationship+think.+hide.+(1|ResponseId)+(1|X1),cm.))
confint(lmer(work~task+relationship+think.+hide.+(1|ResponseId)+(1|X1),cm.))

summary(lmer(work~think.+hide.+(1|ResponseId)+(1|X1),cm.))
confint(lmer(work~think.+hide.+(1|ResponseId)+(1|X1),cm.))

# --- Compare frequency: think vs. hide (like the example paper) ---
# long format: each secret contributes two rows (one think, one hide)
cm.long <- melt(cm.[, c("ResponseId", "X1", "think.", "hide.")],
                id.vars = c("ResponseId", "X1"),
                variable.name = "type",
                value.name = "frequency")

# mind-wander (think) = 1, conceal (hide) = 0
cm.long$type <- ifelse(cm.long$type == "think.", 1, 0)

summary(lmer(frequency ~ type + (1|ResponseId) + (1|X1), cm.long))
confint(lmer(frequency ~ type + (1|ResponseId) + (1|X1), cm.long))

# Convert tenure and team_size to numeric in the original participant-level dataset
ck$tenure <- as.numeric(as.character(ck$tenure))
ck$team_size <- as.numeric(as.character(ck$team_size))

summary(lmer(relationship~hide.*think.+task++(1|ResponseId)+(1|X1),cm.))
summary(lmer(task~hide.*think.+relationship++(1|ResponseId)+(1|X1),cm.))

cm.$think.c<-scale(cm.$think.)
cm.$hide.c<-scale(cm.$hide.)
mean(cm.$think.c,na.rm=T)
mean(cm.$hide.c,na.rm=T)
think.high<-cm.$think.c-sd(cm.$think.c,na.rm=T)
think.low<-cm.$think.c+sd(cm.$think.c,na.rm=T)
hide.high<-cm.$hide.c-sd(cm.$hide.c,na.rm=T)
hide.low<-cm.$hide.c+sd(cm.$hide.c,na.rm=T)


summary(lmer(relationship~hide.*think.+task++(1|ResponseId)+(1|X1),cm.))
summary(lmer(relationship~hide.c*think.c+task++(1|ResponseId)+(1|X1),cm.))
summary(lmer(relationship~hide.low*think.c+task++(1|ResponseId)+(1|X1),cm.))
summary(lmer(relationship~hide.high*think.c+task++(1|ResponseId)+(1|X1),cm.))

summary(lmer(relationship~hide.c*think.low+task++(1|ResponseId)+(1|X1),cm.))
summary(lmer(relationship~hide.c*think.high+task++(1|ResponseId)+(1|X1),cm.))

# Keep one row per participant with their tenure and team size
control_df <- unique(ck[, c("ResponseId", "tenure", "team_size")])

# Remove old control variables from cm. if they were already merged before
cm. <- cm.[, !names(cm.) %in% c(
  "tenure", "tenure.x", "tenure.y",
  "team_size", "team_size.x", "team_size.y"
)]

# Merge tenure and team_size into cm. by ResponseId
cm. <- merge(cm., control_df, by = "ResponseId", all.x = TRUE)

summary(lmer(task ~ think. * tenure + hide. * tenure + relationship +  (1 | ResponseId) + (1 | X1), cm.))
summary(lmer(relationship ~ think. * tenure + hide. * tenure + task +  (1 | ResponseId) + (1 | X1), cm.))

summary(lmer(task ~ think. * team_size + hide. * team_size + relationship +  (1 | ResponseId) + (1 | X1), cm.))
summary(lmer(relationship ~ think. * team_size + hide. * team_size + task +  (1 | ResponseId) + (1 | X1), cm.))

