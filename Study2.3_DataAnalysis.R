# =============================================================================
# Study 2.3 — Data Analysis ("Told" vs. "Heard" follow-up, task/relationship)
# -----------------------------------------------------------------------------
# Third follow-up to Study 2, same design as 2.1/2.2 but the two motive
# composites are labeled task (items 1-3) and relationship (items 4-6). Each
# participant responded about several secret categories (the X1 grouping
# variable) in a told and/or heard frame. The script:
#   (1) imports and cleans the raw Qualtrics export,
#   (2) reshapes it to one row per participant x secret-category,
#   (3) builds four cell-mean composites (told/heard x task/relationship),
#   (4) stacks them into long form with item-type and condition dummies, and
#   (5) fits 2x2 (item-type x condition) mixed-effects interaction models.
#
# NOTE: This file only adds comments/section headers for readability.
#       All code logic, variable names, paths, formulas, and outputs are
#       unchanged from the original analysis. (Some pre-existing inline comments
#       on the dummy lines still read "epistemic/relational" from the template;
#       they are kept verbatim and refer to the task/relationship items here.)
# =============================================================================


# =============================================================================
# 1. SETUP & DATA IMPORT
# =============================================================================

setwd("~/Desktop/Picker/Analysis/Study 2.3")
options(scipen = 0)   # use R's default fixed/scientific notation threshold

# readr::read_csv() — fast CSV import that returns a tibble.
library(readr)
ck<-read_csv("Told 2 X Heard X Secrecy_June 9, 2026_19.02.csv") #Where the file will be imported

# Drop the two Qualtrics header rows (question text + ImportId metadata).
ck <- ck[-c(1, 2), ]        # drop Qualtrics question-text row and ImportId row

# Honesty check: honesty == 2 flags self-reported liars; keep everyone else.
ck <- ck[is.na(ck$honesty) | as.numeric(as.character(ck$honesty)) != 2, ]  # 2 = lied, drop them


# =============================================================================
# 2. DEMOGRAPHICS
# =============================================================================

# Gender/age frequency tables plus mean and SD of age.
# demographics
table(ck$gender); table(as.numeric(as.character(ck$age)));
mean(as.numeric(ck$age), na.rm=TRUE)
sd(as.numeric(ck$age), na.rm=TRUE)


head(ck)


# =============================================================================
# 3. COLUMN CLEANING — DROP EMPTY VARIABLES
# =============================================================================

# dropNA() keeps only columns with at least one non-NA value (questions that
# were never shown in the survey flow are entirely empty).
# drop empty variables (questions not in survey flow)
dropNA<-function(x){x <<- x[,colSums(is.na(x))<nrow(x)]}
ck<-dropNA(ck)

head(ck)

names(ck) #The variables


# =============================================================================
# 4. RESHAPE — LONG THEN WIDE (one row per participant x secret-category)
# =============================================================================

# reshape2::melt() / dcast() — convert between wide and long data formats.
library(reshape2)

# Item columns are named "<category>XX<item>" (e.g. salaryXXtold_1).
# reshape
# get variable names with XX
cx..<-ck[c("ResponseId",grep("XX", names(ck), value = TRUE))];   # keep ID + all XX-tagged columns
head(cx..)
# Melt to long so the category/item label lives in one "variable" column.
# make long
cx.m <- melt(cx.., id.vars = c("ResponseId"));
head(cx.m)
# Split "variable" on "XX": X1 = secret category, X2 = item (told_1, heard_4, ...).
# extract labels within variable name
cx.m. <- data.frame(do.call('rbind', strsplit(as.character(cx.m$variable),'XX',fixed=TRUE)));
head(cx.m.)
# Attach the two label columns and coerce responses to numeric.
# add in the labels within variable name
cm<-cbind(cx.m,cx.m.);
cm$value<-as.numeric(as.character(cm$value));
head(cm)

# Cast back to wide: one row per participant x category, one column per item.
# make wide again
cm.<-dcast(cm, ResponseId + X1 ~ X2, fun.aggregate = mean, value.var="value");
head(cm.)

# Drop all-empty columns, then near-empty rows (fewer than 2 real values).
# drop empty rows
cm. <- cm.[, colSums(is.na(cm.)) < nrow(cm.)]; cm. <- cm.[rowSums(is.na(cm.)) < ncol(cm.) - 2, ]
head(cm.)

# Number of participant-category rows retained.
length(cm.$ResponseId)


# =============================================================================
# 5. VARIABLE CONSTRUCTION — FOUR CELL-MEAN COMPOSITES
# =============================================================================

# Average the three items in each cell of the design: condition (told/heard) x
# motive (task items 1-3 / relationship items 4-6).
# Create average ratings for epistemic and relational motives separately for told and heard
cm.$told_task <- rowMeans(cm.[, c("told_1", "told_2", "told_3")], na.rm = TRUE)
cm.$told_relationship <- rowMeans(cm.[, c("told_4", "told_5", "told_6")], na.rm = TRUE)

cm.$heard_task <- rowMeans(cm.[, c("heard_1", "heard_2", "heard_3")], na.rm = TRUE)
cm.$heard_relationship <- rowMeans(cm.[, c("heard_4", "heard_5", "heard_6")], na.rm = TRUE)

# Means and SDs of the four composites.
mean(cm.$told_task, na.rm = TRUE)
mean(cm.$told_relationship, na.rm = TRUE)
mean(cm.$heard_task, na.rm = TRUE)
mean(cm.$heard_relationship, na.rm = TRUE)
sd(cm.$told_task, na.rm = TRUE)
sd(cm.$told_relationship, na.rm = TRUE)
sd(cm.$heard_task, na.rm = TRUE)
sd(cm.$heard_relationship, na.rm = TRUE)

head(cm.)


# =============================================================================
# 6. LONG-FORM STACK & DUMMY CODING
# =============================================================================

# Stack the four composites into a single "avg.rating" column so the 2x2 design
# (motive x condition) can be modeled with dummy predictors.
# Stack the four average ratings on top of each other

th <- cm.[c("ResponseId","X1","told_task","told_relationship","heard_task","heard_relationship")]
th.m <- melt(th, id.vars = c("ResponseId","X1"));

head(th.m)

names(th.m)[names(th.m) == "value"] <- "avg.rating"
unique(th.m$variable)

# Two pairs of complementary dummy codes (each pair flips the reference level),
# so re-running a model with the opposite dummy reveals the other simple effects.
th.m$tk1rel0 <- ifelse(grepl("task", th.m$variable), 1, 0) # 1 means epistemic item, and 0 means relational item
th.m$told1heard0 <- ifelse(grepl("told", th.m$variable), 1, 0) # 1 means told, 0 means heard
th.m$tk0rel1 <- ifelse(grepl("relationship", th.m$variable), 1, 0) # 0 means epistemic item, and 1 means relational item
th.m$told0heard1 <- ifelse(grepl("heard", th.m$variable), 1, 0) # 0 means told, 1 means heard

# Keep the modeling columns and drop rows with a missing rating.
th. <- th.m[, c("ResponseId", "X1", "avg.rating", "tk1rel0", "told1heard0","tk0rel1","told0heard1")]
th. <- th.[!is.na(th.$avg.rating), ]

head(th.)


# =============================================================================
# 7. MIXED-EFFECTS MODELS — MOTIVE x CONDITION (2x2)
# =============================================================================

# lme4::lmer() fits linear mixed models; lmerTest adds p-values. All models use
# crossed random intercepts for participant (ResponseId) and category (X1) and
# test the task/relationship x told/heard interaction. The four runs use
# different dummy codings so each surfaces the simple effects under a different
# reference cell; the interaction term itself is equivalent across them.
library(lme4)
library(lmerTest)

summary(lmer(avg.rating ~ tk1rel0 * told1heard0 + (1|ResponseId) + (1|X1), th.))
summary(lmer(avg.rating ~ tk0rel1 * told0heard1 + (1|ResponseId) + (1|X1), th.))
summary(lmer(avg.rating ~ tk0rel1 * told1heard0 + (1|ResponseId) + (1|X1), th.))
summary(lmer(avg.rating ~ tk1rel0 * told0heard1 + (1|ResponseId) + (1|X1), th.))
# 95% confidence intervals for the same four models.
confint(lmer(avg.rating ~ tk1rel0 * told1heard0 + (1|ResponseId) + (1|X1), th.))
confint(lmer(avg.rating ~ tk0rel1 * told0heard1 + (1|ResponseId) + (1|X1), th.))
confint(lmer(avg.rating ~ tk0rel1 * told1heard0 + (1|ResponseId) + (1|X1), th.))
confint(lmer(avg.rating ~ tk1rel0 * told0heard1 + (1|ResponseId) + (1|X1), th.))
