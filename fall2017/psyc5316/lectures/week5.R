## Week 5
## PSYC 5316

library("tidyverse")



# load some data
rawdata = read_csv("~/github/courses/fall2017/psyc5316/lectures/week5data.csv")
glimpse(rawdata)
head(rawdata)

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

# tallying number of trials in each condition
data %>%
  group_by(condition, distance) %>%
  tally

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

# we can see outliers!
# need to filter data

data <- rawdata %>%
  filter(error==0) %>%
  filter(RT > 200 & RT < mean(RT)+3*sd(RT))

# after outliers are removed, replot the figure
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
  geom_histogram(aes(fill=condition))

# faceted
data %>%
  ggplot(aes(x=RT)) +
  geom_histogram() +
  facet_grid(condition~.)


####################################################
# collapsing data and comparing condition means

# Case 1: condition (congruent, incongruent)
# bar plot
data %>%
  group_by(condition) %>%
  summarize(meanRT=mean(RT)) %>%
  ggplot(aes(x=condition,y=meanRT)) +
  geom_bar(stat="identity")

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
  geom_bar(stat="identity")

data %>%
  group_by(distance) %>%
  summarize(meanRT=mean(RT)) %>%
  ggplot(aes(x=distance, y=meanRT, group=1)) +
  geom_line() +
  geom_point() 

# Case 2: condition x distance
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
  geom_point()
  
###############################################
# analyzing data

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

t.test(congruent$meanRT,incongruent$meanRT,paired=TRUE)

# ANOVA: mean RT by distance

dataByDistance <- data %>%
  group_by(subject, distance) %>%
  summarize(meanRT=mean(RT)) %>%
  mutate(distance=as.factor(distance))
  
distance.aov = aov(meanRT~distance + Error(as.factor(subject)/distance), data=dataByDistance)
summary(distance.aov)

# factorial ANOVA: mean RT by condition x distance

dataFactorial <- data %>%
  group_by(subject, condition, distance) %>%
  summarize(meanRT = mean(RT)) %>%
  mutate(distance=as.factor(distance))

factorial.aov = aov(meanRT ~ condition*distance + Error(as.factor(subject)/(condition*distance)), data=dataFactorial)
summary(factorial.aov)
