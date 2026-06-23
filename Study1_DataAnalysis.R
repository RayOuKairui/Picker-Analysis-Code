# =============================================================================
# Study 1 — Data Analysis
# -----------------------------------------------------------------------------
# Workplace-secrecy study. Each participant rated several "secret categories"
# (the X1 grouping variable) on how much they HIDE vs. THINK about the secret,
# plus relationship / task / work outcomes. The script:
#   (1) imports and cleans the raw Qualtrics export,
#   (2) reshapes it to one row per participant x secret-category,
#   (3) caps statistical outliers on the hide/think measures,
#   (4) builds composite outcome variables,
#   (5) fits mixed-effects models with crossed random effects,
#   (6) runs a bootstrap mediation (hide/think -> task/relationship -> work), and
#   (7) runs additional interaction, simple-slopes, and control-variable models
#       (Sections 11-13) merged in from Study1_DataAnalysisE.R.
#
# NOTE: Sections 1-10 are the original analysis with comments added for
#       readability (code, variable names, paths, formulas, and outputs
#       unchanged). The age mean/SD in Section 2 and Sections 11-13 were MERGED
#       IN from Study1_DataAnalysisE.R; that code is preserved verbatim.
# =============================================================================


# =============================================================================
# 1. SETUP & DATA IMPORT
# =============================================================================

setwd("~/Desktop/Picker/Analysis/Study 1 Final")

# readr::read_csv() — fast CSV import that returns a tibble.
library(readr)
ck<-read_csv("Secrecy X Relationship X Work NEW_June 7, 2026_15.10.csv") #Where the file will be imported

# Qualtrics exports two extra header rows beneath the column names
# (the full question text + the ImportId metadata); rows 1 and 2 are dropped.
ck <- ck[-c(1, 2), ]                                              # drop Qualtrics rows first

# Attention/honesty check: honesty == 2 flags participants who admitted to
# lying. Keep rows where honesty is missing OR not equal to 2 (i.e., drop liars).
ck <- ck[is.na(ck$honesty) | as.numeric(as.character(ck$honesty)) != 2, ]  # 2 = lied, drop them


# =============================================================================
# 2. DEMOGRAPHICS
# =============================================================================

# Frequency tables describing the sample composition (gender and age).
# demographics
table(ck$gender); table(as.numeric(as.character(ck$age)));

# Mean and SD of participant age (merged from Study1_DataAnalysisE.R).
mean(as.numeric(ck$age), na.rm=TRUE)
sd(as.numeric(ck$age), na.rm=TRUE)

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

# The item columns are named like "<category>XX<item>" (e.g. salaryXXhide).
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
#   X1 = secret category (e.g. "salary"), X2 = item (e.g. "hide", "think", "ratings_1").
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


# =============================================================================
# 5. OUTLIER HANDLING — ADJUSTED-BOXPLOT FENCE ON hide / think
# =============================================================================

# robustbase::adjbox() — adjusted boxplot for skewed data; its $fence gives
# robust lower/upper outlier cutoffs. readr is re-loaded (no new effect).
library(robustbase)
library(readr)

# Compute the fence over the pooled hide + think values; take the UPPER fence.
adjbox(rbind(cm.$hide, cm.$think))$fence
adjbox(rbind(cm.$hide, cm.$think))$fence[2] -> exc; exc   # exc = upper outlier cutoff

# Build capped copies: any hide/think value above the upper fence -> NA.
# The original hide/think columns are left intact; analyses use hide./think.
cm.$hide. <- cm.$hide
cm.$hide.[cm.$hide > exc] <- NA

cm.$think. <- cm.$think
cm.$think.[cm.$think > exc] <- NA

# Means after capping, and before/after frequency tables to show what changed.
mean(as.numeric(cm.$think.), na.rm = TRUE)
mean(as.numeric(cm.$hide.), na.rm = TRUE)

table(cm.$hide); table(cm.$hide.)
table(cm.$think); table(cm.$think.)

# How many values were excluded as outliers, vs. the total number of hide+think
# observations (n participants-categories x 2 measures).
length(cm.$hide[cm.$hide > exc]) + length(cm.$think[cm.$think > exc])
length(cm.$ResponseId) * 2

head(cm.)


# =============================================================================
# 6. VARIABLE CONSTRUCTION — OUTCOME COMPOSITES
# =============================================================================

# relationship = mean of rating items 1-3; task = mean of items 4-6;
# work = single rating item 7 (overall work outcome).
cm.$relationship <- rowMeans(cm.[, c("ratings_1", "ratings_2", "ratings_3")], na.rm = TRUE)
cm.$task <- rowMeans(cm.[, c("ratings_4", "ratings_5", "ratings_6")], na.rm = TRUE)
cm.$work <- as.numeric(as.character(cm.$"ratings_7"))


# =============================================================================
# 7. DESCRIPTIVE STATISTICS
# =============================================================================

# Mean, SD, and 95% CI (via one-sample t.test) for the task and relationship
# composites.
#Descriptive Data
mean(as.numeric(cm.$task), na.rm = TRUE)
sd(as.numeric(cm.$task), na.rm = TRUE)
t.test(as.numeric(cm.$task),, na.rm = TRUE)$conf.int                 # 95% CI of descriptive data
mean(as.numeric(cm.$relationship), na.rm = TRUE)
sd(as.numeric(cm.$relationship), na.rm = TRUE)
t.test(as.numeric(cm.$relationship),, na.rm = TRUE)$conf.int


# =============================================================================
# 8. MIXED-EFFECTS MODELS
# =============================================================================

# lme4::lmer() fits linear mixed models; lmerTest adds p-values (Satterthwaite).
# All models use crossed random intercepts: (1|ResponseId) for participants and
# (1|X1) for secret categories.
library(lme4)
library(lmerTest)

# Do thinking-about / hiding the secret predict task and relationship outcomes?
summary(lmer(task~think.+hide.+(1|ResponseId)+(1|X1),cm.))
summary(lmer(relationship~think.+hide.+(1|ResponseId)+(1|X1),cm.))

# Same predictors, now controlling for the other outcome (relationship in the
# task model; task in the relationship model).
summary(lmer(task~think.+hide.+relationship+(1|ResponseId)+(1|X1),cm.))
summary(lmer(relationship~think.+hide.+task+(1|ResponseId)+(1|X1),cm.))

# 95% confidence intervals for the two controlled models above.
confint(lmer(task~think.+hide.+relationship+(1|ResponseId)+(1|X1),cm.))
confint(lmer(relationship~think.+hide.+task+(1|ResponseId)+(1|X1),cm.))

# Likelihood-ratio test of the random effects (ranova) for the task model.
m <- lmer(task~think.+hide.+relationship+(1|ResponseId)+(1|X1),cm.)

lmerTest::ranova(m)
lmerTest::ranova(relationship~think.+hide.+task+(1|ResponseId)+(1|X1),cm.)

# Does the overall work outcome depend on task/relationship and/or hide/think?
# First with task + relationship as mediators, then the total (direct) effect of
# think./hide. on work without the mediators.
summary(lmer(work~task+relationship+think.+hide.+(1|ResponseId)+(1|X1),cm.))
confint(lmer(work~task+relationship+think.+hide.+(1|ResponseId)+(1|X1),cm.))

summary(lmer(work~think.+hide.+(1|ResponseId)+(1|X1),cm.))
confint(lmer(work~think.+hide.+(1|ResponseId)+(1|X1),cm.))


# =============================================================================
# 9. BOOTSTRAP MEDIATION (hide/think -> task/relationship -> work)
# =============================================================================

# Resample participants-categories with replacement; on each bootstrap sample
# refit the mediation models and store the relevant fixed-effect coefficients.
# The indirect effect for each path = a-path * b-path.
iterations<-5000 # change to 1000 when you know it works

# Pre-allocate the results table: one row per iteration, columns for the four
# a-paths (a, a2, a3, a4) and two b-paths (b, b2).
matrix(nrow=iterations,ncol=7)->boottable
as.data.frame(boottable)->boottable
names(boottable)<-c("iter", "a","a2","a3","a4","b","b2")

dim(cm.)[1]->n   # n = number of participant-category rows to resample
for(i in 1:iterations){
  sample(c(1:n),n,replace=T)->sampnums;          # bootstrap row indices
  cm.[sampnums,]$think.->ithink.;                 # pull each variable for this resample
  cm.[sampnums,]$hide.->ihide.;
  cm.[sampnums,]$task->itask;
  cm.[sampnums,]$relationship->irelationship;
  cm.[sampnums,]$work->iwork;
  cm.[sampnums,]$ResponseId-> iResponseId;
  cm.[sampnums,]$X1-> iX1;

  # a-paths: predictor -> mediator; b-paths: mediator -> work.
  fixef(lmer(itask~ithink.+ihide.+irelationship+(1|iResponseId)+(1|iX1)))[2]->itera; # think -> task
  fixef(lmer(itask~ithink.+ihide.+irelationship+(1|iResponseId)+(1|iX1)))[3]->itera2; # hide -> task
  fixef(lmer(irelationship~ithink.+ihide.+itask+(1|iResponseId)+(1|iX1)))[2]->itera3; # think -> rel
  fixef(lmer(irelationship~ithink.+ihide.+itask+(1|iResponseId)+(1|iX1)))[3]->itera4; # hide -> rel
  fixef(lmer(iwork~itask+irelationship+ithink.+ihide.+(1|iResponseId)+(1|iX1)))[2]->iterb; # task -> work
  fixef(lmer(iwork~itask+irelationship+ithink.+ihide.+(1|iResponseId)+(1|iX1)))[3]->iterb2; # rel -> work

  # Store this iteration's coefficients into the results table.
  i->boottable[i,1];
  itera->boottable[i,2];
  itera2->boottable[i,3];
  itera3->boottable[i,4];
  itera4->boottable[i,5];
  iterb->boottable[i,6]
  iterb2->boottable[i,7]
}
# Mean indirect effect (a * b) for each of the four mediation paths.
# mean indirect effect
mean(boottable$a*boottable$b) # think -> task -> work
mean(boottable$a2*boottable$b) # hide -> task -> work
mean(boottable$a3*boottable$b2) # think -> rel -> work
mean(boottable$a4*boottable$b2) # hide -> rel -> work
# Bootstrap standard error of each indirect effect.
#SE
sd(boottable$a*boottable$b)/sqrt(length(boottable$iter)) # think -> task -> work
sd(boottable$a2*boottable$b)/sqrt(length(boottable$iter)) # hide -> task -> work
sd(boottable$a3*boottable$b2)/sqrt(length(boottable$iter)) # think -> rel -> work
sd(boottable$a4*boottable$b2)/sqrt(length(boottable$iter)) # hide -> rel -> work
# Percentile 95% bootstrap CI of each indirect effect (significant if it
# excludes 0).
#95 CI
quantile(with(boottable,a*b),c(.025,.975)) # think -> task -> work
quantile(with(boottable,a2*b),c(.025,.975)) # hide -> task -> work
quantile(with(boottable,a3*b2),c(.025,.975)) # think -> rel -> work
quantile(with(boottable,a4*b2),c(.025,.975)) # hide -> rel -> work

# ---- Saved console output from a previous run (for reference) ----------------
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


# =============================================================================
# 10. LONG-FORM RECODE — think. vs hide. as a single predictor
# =============================================================================

# Subset to the columns needed, then stack think. and hide. into one long
# column so a single dummy can contrast them, keeping relationship/task as
# outcome variables that travel with each row.
th <- cm.[c("ResponseId", "X1", "think.", "hide.", "relationship", "task")]
# Stack think. and hide. on top of each other while keeping relationship and task as outcome variables
th.m <- melt(th, id.vars = c("ResponseId", "X1", "relationship", "task"))

head(th.m)

# Rename value as avg.rating
names(th.m)[names(th.m) == "value"] <- "avg.rating"
unique(th.m$variable)

# Dummy code the stacked measure: 1 = think., 0 = hide.
# Create think1hide0: 1 means think., 0 means hide.
th.m$think1hide0 <- ifelse(grepl("think", th.m$variable), 1, 0)

# Keep only the final variables, then drop rows with a missing rating.
# Keep only the final variables
th. <- th.m[, c("ResponseId", "X1", "relationship", "task", "avg.rating", "think1hide0", "think0hide1")]

# Drop rows where avg.rating is missing
th. <- th.[!is.na(th.$avg.rating), ]
head(th.)

# Final model: does the rating differ between think. and hide. (think1hide0),
# with crossed random intercepts for participant and category?
summary(lmer(avg.rating ~ think1hide0+ (1|ResponseId) + (1|X1), th.))


# =============================================================================
# 11. THINK x HIDE INTERACTION MODELS        (merged from Study1_DataAnalysisE.R)
# =============================================================================

# Section 8 entered think. and hide. additively. These models add their
# INTERACTION (think. * hide.) to test whether the effect of one depends on the
# level of the other, still controlling for the other outcome and using the same
# crossed random effects. (The "++" is a harmless stray unary plus from the
# original code and is kept verbatim.)
summary(lmer(relationship~hide.*think.+task++(1|ResponseId)+(1|X1),cm.))
summary(lmer(task~hide.*think.+relationship++(1|ResponseId)+(1|X1),cm.))


# =============================================================================
# 12. SIMPLE-SLOPES / SPOTLIGHT ANALYSIS     (merged from Study1_DataAnalysisE.R)
# =============================================================================

# Probe the think. x hide. interaction. First standardize (z-score) both
# predictors, then build shifted copies at +/- 1 SD: re-centering a predictor so
# that its high/low value sits at 0 makes the interaction model's main term
# report the SIMPLE SLOPE of the other predictor at that level.
# (Naming follows the original: ".high" subtracts 1 SD, ".low" adds 1 SD.)
cm.$think.c<-scale(cm.$think.)
cm.$hide.c<-scale(cm.$hide.)
mean(cm.$think.c,na.rm=T)   # ~0 after centering (sanity check)
mean(cm.$hide.c,na.rm=T)
think.high<-cm.$think.c-sd(cm.$think.c,na.rm=T)   # re-centered at +1 SD ("high")
think.low<-cm.$think.c+sd(cm.$think.c,na.rm=T)    # re-centered at -1 SD ("low")
hide.high<-cm.$hide.c-sd(cm.$hide.c,na.rm=T)
hide.low<-cm.$hide.c+sd(cm.$hide.c,na.rm=T)

# Centered interaction, then the simple slope of think.c at low/high hide,
# and of hide.c at low/high think.
summary(lmer(relationship~hide.c*think.c+task++(1|ResponseId)+(1|X1),cm.))
summary(lmer(relationship~hide.low*think.c+task++(1|ResponseId)+(1|X1),cm.))
summary(lmer(relationship~hide.high*think.c+task++(1|ResponseId)+(1|X1),cm.))

summary(lmer(relationship~hide.c*think.low+task++(1|ResponseId)+(1|X1),cm.))
summary(lmer(relationship~hide.c*think.high+task++(1|ResponseId)+(1|X1),cm.))


# =============================================================================
# 13. CONTROL VARIABLES — TENURE & TEAM SIZE (merged from Study1_DataAnalysisE.R)
# =============================================================================

# Robustness check: do the think./hide. effects vary with job tenure and team
# size? These two controls live at the PARTICIPANT level (in ck), so they are
# converted to numeric and merged into cm. by ResponseId before being used as
# moderators. (This merge runs last so the Section 9-10 analyses above, which
# rely on cm.'s original structure, are unaffected.)

# Convert tenure and team_size to numeric in the original participant-level dataset
ck$tenure <- as.numeric(as.character(ck$tenure))
ck$team_size <- as.numeric(as.character(ck$team_size))

# Keep one row per participant with their tenure and team size
control_df <- unique(ck[, c("ResponseId", "tenure", "team_size")])

# Remove old control variables from cm. if they were already merged before
cm. <- cm.[, !names(cm.) %in% c(
  "tenure", "tenure.x", "tenure.y",
  "team_size", "team_size.x", "team_size.y"
)]

# Merge tenure and team_size into cm. by ResponseId
cm. <- merge(cm., control_df, by = "ResponseId", all.x = TRUE)

# think./hide. x tenure interactions (each controlling for the other outcome)...
summary(lmer(task ~ think. * tenure + hide. * tenure + relationship +  (1 | ResponseId) + (1 | X1), cm.))
summary(lmer(relationship ~ think. * tenure + hide. * tenure + task +  (1 | ResponseId) + (1 | X1), cm.))

# ...and think./hide. x team_size interactions.
summary(lmer(task ~ think. * team_size + hide. * team_size + relationship +  (1 | ResponseId) + (1 | X1), cm.))
summary(lmer(relationship ~ think. * team_size + hide. * team_size + task +  (1 | ResponseId) + (1 | X1), cm.))
