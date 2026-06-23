# =============================================================================
# Study 2 — Data Analysis ("Told" vs. "Heard")
# -----------------------------------------------------------------------------
# Workplace-secrecy study. Each participant responded about several secret
# categories (the X1 grouping variable) in ONE of two conditions:
#   - TOLD : the participant told a secret they weren't supposed to tell, or
#   - HEARD: a coworker told the participant a secret they wish they hadn't.
# For each category the participant rated epistemic items (1-3) and
# relationship items (4-6). The script:
#   (1) imports and cleans the raw Qualtrics export,
#   (2) reshapes it to one row per participant x secret-category,
#   (3) builds the epistemic / relationship composites and a told/heard dummy,
#   (4) fits mixed-effects models comparing the two conditions, and
#   (5) reports condition means and SDs.
#
# NOTE: This file only adds comments/section headers for readability.
#       All code logic, variable names, paths, formulas, and outputs are
#       unchanged from the original analysis.
# =============================================================================


# =============================================================================
# 1. SETUP & DATA IMPORT
# =============================================================================

setwd("~/Desktop/Picker/Analysis/Study 2")

# readr::read_csv() — fast CSV import that returns a tibble.
library(readr)
ck<-read_csv("Told X Heard X Secrecy_June 4, 2026_13.19.csv") #Where the file will be imported

# Qualtrics exports two extra header rows beneath the column names
# (the full question text + the ImportId metadata); rows 1 and 2 are dropped.
ck <- ck[-c(1, 2), ]        # drop Qualtrics question-text row and ImportId row

# Attention/honesty check: honesty == 2 flags participants who admitted to
# lying. Keep rows where honesty is missing OR not equal to 2 (i.e., drop liars).
ck <- ck[is.na(ck$honesty) | as.numeric(as.character(ck$honesty)) != 2, ]  # 2 = lied, drop them


# =============================================================================
# 2. DEMOGRAPHICS
# =============================================================================

# Frequency tables for gender and age describing the sample composition.
# demographics
table(ck$gender); table(as.numeric(as.character(ck$age)));

# Mean and SD of age.
mean(as.numeric(ck$age), na.rm = T)
sd(as.numeric(ck$age), na.rm = T)

head(ck)


# =============================================================================
# 3. COLUMN CLEANING — DROP EMPTY VARIABLES
# =============================================================================

# Many survey columns are blank because those questions were never shown in the
# survey flow. dropNA() keeps only columns that contain at least one non-NA
# value (colSums of NAs strictly less than the number of rows).
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

# The item columns are named like "<category>XX<item>" (e.g. salaryXXtold_1,
# salaryXXheard_1).
# reshape
# get variable names with XX
cx..<-ck[c("ResponseId",grep("XX", names(ck), value = TRUE))];   # keep ID + all XX-tagged columns
head(cx..)

# Melt to long: one row per participant x column, so the category/item label is
# carried in the single "variable" column.
# make long
cx.m <- melt(cx.., id.vars = c("ResponseId"));
head(cx.m)

# Split each "variable" name on the "XX" separator into two pieces:
#   X1 = secret category (e.g. "salary"), X2 = item (e.g. "told_1", "heard_4").
# extract labels within variable name
cx.m. <- data.frame(do.call('rbind', strsplit(as.character(cx.m$variable),'XX',fixed=TRUE)));
head(cx.m.)

# Attach those two label columns back onto the long data, and coerce the
# response values to numeric.
# add in the labels within variable name
cm<-cbind(cx.m,cx.m.);
cm$value<-as.numeric(as.character(cm$value));
head(cm)

# Cast back to wide so each row is a participant (ResponseId) x category (X1),
# with one column per item (X2). fun.aggregate = mean collapses any duplicates.
# make wide again
cm.<-dcast(cm, ResponseId + X1 ~ X2, fun.aggregate = mean, value.var="value");
head(cm.)

# Remove all-empty columns, then remove rows that are essentially all NA
# (more NAs than item columns, i.e. fewer than 2 real values besides ID/X1).
# drop empty rows
cm. <- cm.[, colSums(is.na(cm.)) < nrow(cm.)]; cm. <- cm.[rowSums(is.na(cm.)) < ncol(cm.) - 2, ]
head(cm.)

# Number of participant-category rows retained.
length(cm.$ResponseId)


# =============================================================================
# 5. VARIABLE CONSTRUCTION — COMPOSITES & TOLD/HEARD DUMMY
# =============================================================================

# Each row has EITHER the told_* items OR the heard_* items filled (depending on
# the participant's condition). rowMeans with na.rm = TRUE therefore averages
# whichever set is present:
#   epistemic    = mean of items 1-3 (told_1:3 / heard_1:3)
#   relationship = mean of items 4-6 (told_4:6 / heard_4:6)
cm.$epistemic <- rowMeans(cm.[, c("told_1", "told_2", "told_3","heard_1", "heard_2", "heard_3")], na.rm = TRUE)
cm.$relationship <- rowMeans(cm.[, c("told_4", "told_5", "told_6","heard_4", "heard_5", "heard_6")], na.rm = TRUE)

head(cm.)

# Condition dummy epISheard: 1 = Heard (heard_1 present), 0 = Told (told_1 present).
cm.$epISheard[!is.na(cm.$heard_1)]<-1
cm.$epISheard[!is.na(cm.$told_1)]<-0


# =============================================================================
# 6. MIXED-EFFECTS MODELS — Heard vs. Told
# =============================================================================

# lme4::lmer() fits linear mixed models; lmerTest adds p-values (Satterthwaite).
# Both models use crossed random intercepts: (1|ResponseId) for participants and
# (1|X1) for secret categories. epISheard is the focal predictor (Heard vs Told),
# controlling for the other outcome.
library(lme4)
library(lmerTest)

summary(lmer(epistemic~epISheard+relationship+(1|ResponseId)+(1|X1),cm.)) # 0 = ep told, 1 = ep heard.... if coefficient is positive, then heard is greater than told; if negative, the reverse
summary(lmer(relationship~epISheard+epistemic+(1|ResponseId)+(1|X1),cm.)); # 1 = rel told, 0 = rel heard....if coefficient is negative, then heard is greater than told; if negative, the reverse

# 95% confidence intervals for the same two models.
confint(lmer(epistemic~epISheard+relationship+(1|ResponseId)+(1|X1),cm.))
confint(lmer(relationship~epISheard+epistemic+(1|ResponseId)+(1|X1),cm.));


# =============================================================================
# 7. DESCRIPTIVE STATISTICS BY CONDITION
# =============================================================================

# Condition means (Heard = 1, Told = 0) for each outcome...
mean(cm.$epistemic[cm.$epISheard == 1],na.rm=TRUE)
mean(cm.$epistemic[cm.$epISheard == 0],na.rm=TRUE)
mean(cm.$relationship[cm.$epISheard == 1],na.rm=TRUE)
mean(cm.$relationship[cm.$epISheard == 0],na.rm=TRUE)
# ...and the corresponding standard deviations.
sd(cm.$epistemic[cm.$epISheard == 1],na.rm=TRUE)
sd(cm.$epistemic[cm.$epISheard == 0],na.rm=TRUE)
sd(cm.$relationship[cm.$epISheard == 1],na.rm=TRUE)
sd(cm.$relationship[cm.$epISheard == 0],na.rm=TRUE)
