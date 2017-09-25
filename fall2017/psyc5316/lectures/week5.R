## Week 5
## PSYC 5316

library("tidyverse")

# load some data
rawdata = read_csv("https://git.io/vdkRH")

# examine structure
glimpse(rawdata)
dim(rawdata)
nrow(rawdata)
ncol(rawdata)
head(rawdata)
tail(rawdata)
names(rawdata)

################################################
# Tidyverse "verbs" 
# Filter versus Select

select(rawdata, condition, error, RT)
filter(rawdata, error==0)

# piping
rawdata %>%
  filter(error==0) %>%
  select(subject, condition, RT)


# pipe with assignment
data <- rawdata %>%
  filter(error==0)
  
###############################################
# Mutate

# creating new column
data %>%
  mutate(RT_sec = RT/1000)

# recoding variable
data %>%
  mutate(dist = ifelse(distance==1 | distance==2, "close", "far"))


#################################################
# Split-apply-combine and summarize

# group by single variable
data %>%
  group_by(condition) %>%
  summarize(meanRT = mean(RT))

# group by multiple variables
data %>%
  group_by(condition, distance) %>%
  summarize(meanRT = mean(RT))

# multiple statistics
data %>%
  group_by(condition, distance) %>%
  summarize(meanRT=mean(RT), sd=sd(RT))

###################################################
# Visualizing data with ggplot

# Basic workflow
# 1. start with data
# 2. pipe to ggplot and define "aesthetics"
# 3. add a "geom" to visualize

#########################################
# basic boxplot

data %>%
  ggplot(aes(x=condition, y=RT)) +
  geom_boxplot()

# change orientation
data %>%
  ggplot(aes(x=condition, y=RT)) +
  geom_boxplot() +
  coord_flip()

# show histograms and density plots by condition
# overlaid
data %>%
  ggplot(aes(x=RT, group=condition)) +
  geom_density(aes(fill=condition))

# faceted
data %>%
  ggplot(aes(x=RT)) +
  geom_density() +
  facet_grid(condition~.)


####################################################
# collapsing data and comparing condition means

# Case 1: condition (congruent, incongruent)
# bar plot
data %>%
  group_by(condition) %>%
  summarize(meanRT=mean(RT)) %>%
  ggplot(aes(x=condition,y=meanRT)) +
  geom_bar(stat="identity", width=0.5)

# line plot
data %>%
  group_by(condition) %>%
  summarize(meanRT=mean(RT)) %>%
  ggplot(aes(x=condition, y=meanRT, group=1)) +
  geom_line() +
  geom_point()

# Case 2: distance (1,2,3,4)
data %>%
  group_by(distance) %>%
  summarize(meanRT=mean(RT)) %>%
  ggplot(aes(x=distance, y=meanRT)) +
  geom_bar(stat="identity", width=0.5)

data %>%
  group_by(distance) %>%
  summarize(meanRT=mean(RT)) %>%
  ggplot(aes(x=distance, y=meanRT, group=1)) +
  geom_line() +
  geom_point() +
  ylim(0,1500)

# Case 3: condition x distance
data %>%
  group_by(condition, distance) %>%
  summarize(meanRT=mean(RT)) %>%
  ggplot(aes(x=distance, y=meanRT, fill=condition)) +
  geom_bar(stat="identity", position=position_dodge(), color="black")

data %>%
  group_by(condition, distance) %>%
  summarize(meanRT=mean(RT)) %>%
  ggplot(aes(x=distance, y=meanRT, linetype=condition)) +
  geom_line() +
  geom_point() +
  ylim(0,1500)

  
###############################################
# analyzing data

# how many subjects?
length(unique(data$subject))

# collapse RTs to mean by condition
data %>%
  group_by(subject, condition) %>%
  summarize(meanRT=mean(RT)) %>%
  print(n=82)


# t-test: mean RTs by condition

congruent <- data %>%
  group_by(subject, condition) %>%
  summarize(meanRT=mean(RT)) %>%
  filter(condition=="congruent") %>%
  select(subject,meanRT)

incongruent <- data %>%
  group_by(subject, condition) %>%
  summarize(meanRT=mean(RT)) %>%
  filter(condition=="incongruent") %>%
  select(subject,meanRT)

t.test(incongruent$meanRT,congruent$meanRT,paired=TRUE)

# ANOVA: mean RT by distance

dataByDistance <- data %>%
  group_by(subject, distance) %>%
  summarize(meanRT=mean(RT)) %>%
  mutate(distance=as.factor(distance))
  
distance.aov = aov(meanRT~distance + 
                   Error(as.factor(subject)/distance), 
                   data=dataByDistance
                   )
summary(distance.aov)

# factorial ANOVA: mean RT by condition x distance

dataFactorial <- data %>%
  group_by(subject, condition, distance) %>%
  summarize(meanRT = mean(RT)) %>%
  mutate(distance=as.factor(distance))

factorial.aov = aov(meanRT ~ condition*distance + 
                    Error(as.factor(subject)/(condition*distance)), 
                    data=dataFactorial
                    )
summary(factorial.aov)
