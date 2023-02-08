library(tidyverse)
library(ggplot2)
install.packages("choroplethr")
library(choroplethr)
library(base)
library(tidyr)
install.packages("naniar")
library(naniar)
install.packages("pspearman")
library(pspearman)
library(forcats)
install.packages("ISLR")
library(ISLR)
library(caret)

library(RColorBrewer)
display.brewer.all()
cols <- brewer.pal(3, "Blue")
cols

pal <- colorRampPalette(c("red", "yellow"))






#renaming the data file
hiv <- coded_HIV_AIDS_Data_set_STA_471_2_0
hiv <- as_tibble(hiv)
view(hiv)

#visualizing missing values

str(hiv)
colSums(is.na(hiv))


gg_miss_case(hiv)
vis_miss(hiv,cluster = TRUE)

install.packages("nycflight13")
library(nycflight13)
library(dplyr)
hiv %>%
 vis_miss()

install.packages("Amelia")
library(Amelia)
library(visdat)
missmap(hiv,legend = TRUE, main = "Missing values vs observed")

missmap(hiv, legend = TRUE, col=c("linen","cadetblue"), main="Missing values vs observed", y.cex = 0.8,
        x.cex = 0.8, csvar = NULL, tsvar = NULL,
        rank.order = TRUE, margins = c(5, 5), gap.xaxis = 1, x.las = 2)

?missmap


#The following command gives the sum of missing values in the whole data frame column wise

colSums(is.na(hiv))
col_names <- colnames(hiv)

missed <- as.vector(colSums(is.na(hiv)))

dfmissed <- data.frame(Variable = col_names, Missing = missed)



df.missed <- dfmissed %>% 
  mutate(Percentages = Missing/sum(Missing),labels = scales::percent(Percentages))


ggplot(df.missed, aes(x = "", y = Percentages, fill = Variable)) +
  geom_col(color = "black") +
  geom_label(aes(label = labels),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Missing data distribution")) +
  scale_fill_brewer() +
  coord_polar(theta = "y") + 
  theme_void()




#how many y1 = dont know

sum( hiv$Y1 == "Don't know",na.rm=TRUE ) #1570

no_of_y1_dk <- which(hiv$Y1 == "Don't know") #1570

y1_dk <- filter(hiv, Y1=="Don't know") #1570


#number of missing values Y1 in original data set
no.miss <- hiv$Y1[is.na(hiv$Y1)]  #1568 are missing


#removing y1= dont know responses

hiv_removed_y1_dk_NA <- filter(hiv,Y1 != "Don't know")
hiv.new<- hiv_removed_y1_dk_NA
view(hiv.new)
colSums(is.na(hiv.new))
str(hiv.new)


#removing other missing value rows (#43 are there in f of TV)

#hiv.1 <- subset(hiv.new,!is.na(hiv.new$`Frequency of watching televisio`))
#view(hiv.1)
#colSums(is.na(hiv.1))



#naming variables


train %>%
  vis_miss()




#view
str(hiv.new)
str(hiv.1)





#diving data into training and testing sets
set.seed(310)
indexes = sample(1:nrow(hiv.new), size=0.7*nrow(hiv.new))
train <-  hiv.new[indexes,]
test <-  hiv.new[-indexes,]
view(train)
view(test)

summary(train)
summary(test)

#categorical variables as factors##
train$Residence <- as.factor(train$Residence)
X1 <- train$Residence 
contrasts(X1)

train$Region <-as.factor(train$Region)
X2 <- train$Region 
contrasts(X2)

train$Religion <-as.factor(train$Religion )
X3 <- train$Religion 
contrasts(X3)

train$Ethnicity <-as.factor(train$Ethnicity)
X4 <- train$Ethnicity 
contrasts(X4)


train$`Age group` <- as.factor(train$`Age group`)
X5 <- train$`Age group` 
contrasts(X5)


train$`Marital state` <-as.factor(train$`Marital state`)
X6 <- train$`Marital state` 
contrasts(X6)


train$`Current marital state` <-as.factor(train$`Current marital state`)
X7 <- train$`Current marital state` 
contrasts(X7)


train$`Highest education qualification` <-as.factor(train$`Highest education qualification`)
X8 <- train$`Highest education qualification` 
contrasts(X8)


train$`Frequency of reading newspapper` <-as.factor(train$`Frequency of reading newspapper`)
X9 <- train$`Frequency of reading newspapper` 
contrasts(X9)


train$`Frequency of watching televisio` <-as.factor(train$`Frequency of watching televisio`)
X10 <- train$`Frequency of watching televisio` 
contrasts(X10)


train$`Frequency of listening to radio` <-as.factor(train$`Frequency of listening to radio`)
X11 <- train$`Frequency of listening to radio` 
contrasts(X11)


train$`Frequency of all media combined` <-as.factor(train$`Frequency of all media combined`)
X12 <- train$`Frequency of all media combined` 
contrasts(X12)


train$`Have you ever given birth` <-as.factor(train$`Have you ever given birth`)
X13 <- train$`Have you ever given birth` 
contrasts(X13)
?contrasts

train$`Current pregnancy status` <-as.factor(train$`Current pregnancy status`)
X14 <- train$`Current pregnancy status` 
contrasts(X14)



train$`Working status` <-as.factor(train$`Working status`)
X15 <- train$`Working status` 
contrasts(X15)


train$`Welth index` <-as.factor(train$`Welth index`)
X16 <- train$`Welth index` 
contrasts(X16)


train$Y1 <- as.factor(train$Y1)
Y1 <- train$Y1
contrasts(Y1)





####################################################



# getting correlation between categorical variables
chisq.test(hiv.new$`Frequency of watching televisio`, hiv.new$Y1)
chisq.test(Y1,X1) 
chisq.test(X1,Y1) 

chisq.test(Y1,X2) 
chisq.test(Y1,X3) 
chisq.test(Y1,X4) 
chisq.test(Y1,X5) 
chisq.test(Y1,X6) 
chisq.test(Y1,X7) 
chisq.test(Y1,X8) 
chisq.test(Y1,X9) 
chisq.test(Y1,X10) 
chisq.test(Y1,X11) 
chisq.test(Y1,X12) 
chisq.test(Y1,X13) 
chisq.test(Y1,X14) 
chisq.test(Y1,X15) 
chisq.test(Y1,X16) 


# Cramer's V calculation --------------------------------------------------

# Function to compute Cramer's V
# https://www.r-bloggers.com/example-8-39-calculating-cramers-v/


install.packages("gtools")
library(gtools) # combination
library(ggplot2) # graphics
library(plotly) # interactive graphics
install.packages("data.table")
library(data.table)

train.1 <- train[,c(-1,-18)]
view(train.1)
cat_var <- colnames(train.1)
train.1 <- as.data.table(train)


cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x)[1] * (min(length(unique(x))[1],length(unique(y))[1]) - 1)))
  return(as.numeric(CV))
}

# Apply the function to the combination of categorical variable
v_cramer_all <- function(cat_var, df){
  cat_var_grid <- data.table(combinations(n = length(cat_var), r = 2, v = cat_var, repeats.allowed = FALSE))
  
  do.call(rbind,
          apply(cat_var_grid, 1, function(x){
            tmp <- as.character(x)
            vec1 <- unlist(train.1[,tmp[1], with = FALSE])
            vec2 <- unlist(train.1[,tmp[2], with = FALSE])
            
            data.table(
              variable_x = tmp[1],
              variable_y = tmp[2],
              chi2 = chisq.test(x = vec1, vec2, correct=FALSE)$p.value,
              v_cramer = cv.test(x = vec1, y = vec2)
            )
          }))
  
}

results <- v_cramer_all(cat_var = cat_var, df = train.1)


# Heatmap vizualisation with ggplot2  -------------------------------------

g <- ggplot(results, aes(variable_x, variable_y)) +
  geom_tile(aes(fill = v_cramer), colour = "black") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_bw() + xlab(NULL) + ylab(NULL) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  ggtitle("Cramer's V heatmap")

ggplotly(g)










#viewing complete cases

com.hiv <-hiv.new[complete.cases(hiv.new),]



#each variable in the data set

#Residence


ggplot(train,aes(x=X1)) +
  geom_bar(aes(x=X1), fill = "cadetblue") +
  xlab("Residence") +
  ylab("Count") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))


#Region

ggplot(train,aes(x=X2)) +
  geom_bar(aes(x=X2),fill="cadetblue4") +
  xlab("Region") +
  ylab("Count") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))


#Religion

ggplot(train,aes(x=X3)) +
  geom_bar(aes(x=X3),fill="cornflowerblue") +
  xlab("Religion") +
  ylab("Count") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))



#Ethnicity
ggplot(train,aes(x=X4)) +
  geom_bar(aes(x=X4),fill="cadetblue3") +
  xlab("Ethnicity") +
  ylab("Count") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))


#Age groups

ggplot(train,aes(x=X5)) +
  geom_bar(aes(x=X5),fill="orange") +
  xlab("Age group") +
  ylab("Count") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))


#marital state

ggplot(train,aes(x=X6)) +
  geom_bar(aes(x=X6),fill="orange") +
  xlab("Marital Status") +
  ylab("Count") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

levels(X6)

#current marital status

ggplot(train,aes(x=X7)) +
  geom_bar(aes(x=X7),fill="mediumpurple2") +
  xlab("Current Marital Status") +
  ylab("Count") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))


#wealth index

levels(X16)
summary(X16)

ggplot(train,aes(x=X16)) +
  geom_bar(aes(x=X16),fill="orange2") +
  xlab("Wealth Index") +
  ylab("Count") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))



#[1] "Fourth"  "Highest" "Lowest"  "Middle"  "Second" 

X16 <- ordered(X16,levels = c("Lowest", "Second" ,"Middle","Fourth" ,"Highest"))
#changing order

levels(X16)
summary(X16)

ggplot(train,aes(x=X16)) +
  geom_bar(aes(x=X16),fill="orange2") +
  xlab("Wealth Index") +
  ylab("Count") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))




#HEQ

levels(X8)
summary(X8)

X8 <- ordered(X8,levels = c("No education (77&88)","Passed Grade 1-5","Passed Grade 6-10" ,"Passed G.C.E.(O/L) or equivalent","Passed G.C.E.(A/L) or equivalent","Degree and above"))
levels(X8)
summary(X8)


ggplot(train,aes(x=X8)) +
  geom_bar(aes(x=X8),fill="orange4") +
  xlab("Highest Edu. Qualification") +
  ylab("Count") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))




#Radio

library(ggrepel)

levels(X11)
summary(X11)

df1 <- data.frame(Frequency = c(5421, 2325,2838),
                 Category = c("At least once a week" , "Less than once a week", "Not at all" ))

df1


df11 <- df1 %>% 
  mutate(Percentages = Frequency/sum(Frequency),labels = scales::percent(Percentages))


ggplot(df11, aes(x = "", y = Percentages, fill = Category)) +
  geom_col(color = "black") +
  geom_label(aes(label = labels), color = c(1, 1, 1),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Frequency Category of Radio Listening")) +
  scale_fill_brewer() +
  coord_polar(theta = "y") + 
  theme_void()



#Magazines

levels(X9)
summary(X9)

df2 <- data.frame(Frequency = c(4676, 3475,2433),
                  Category = c("At least once a week" , "Less than once a week", "Not at all" ))

df2


df22 <- df2 %>% 
  mutate(Percentages = Frequency/sum(Frequency),labels = scales::percent(Percentages))


ggplot(df22, aes(x = "", y = Percentages, fill = Category)) +
  geom_col(color = "black") +
  geom_label(aes(label = labels), color = c(1, 1, 1),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Frequency Category of reading Newspapers/Magazines")) +
  scale_fill_brewer() +
  coord_polar(theta = "y") + 
  theme_void()



#TV

levels(X10)
summary(X10)

df3 <- data.frame(Frequency = c(8608, 1169,807),
                  Category = c("At least once a week" , "Less than once a week", "Not at all" ))

df3


df33 <- df3 %>% 
  mutate(Percentages = Frequency/sum(Frequency),labels = scales::percent(Percentages))


ggplot(df33, aes(x = "", y = Percentages, fill = Category)) +
  geom_col(color = "black") +
  geom_label(aes(label = labels), color = c(1, 1, 1),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Frequency Category of watching TV")) +
  scale_fill_brewer() +
  coord_polar(theta = "y") + 
  theme_void()



#all media combined


levels(X12)
summary(X12)

df4 <- data.frame(Frequency = c(9511, 855,218),
                  Category = c("At least once a week" , "Less than once a week", "Not at all" ))

df4


df44 <- df4 %>% 
  mutate(Percentages = Frequency/sum(Frequency),labels = scales::percent(Percentages))


ggplot(df44, aes(x = "", y = Percentages, fill = Category)) +
  geom_col(color = "black") +
  geom_label(aes(label = labels), color = c(1, 1, 1),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Frequency Category of all media combined")) +
  scale_fill_brewer() +
  coord_polar(theta = "y") + 
  theme_void()



#give birth

install.packages("lessR")
library(lessR)

levels(X13)
summary(X13)


PieChart(X13, data = train,
         fill = "purples",
         hole_fill = "white",
         main = "Birth given or not")



#working status

levels(X15)
summary(X15)


PieChart(X15, data = train,
         fill = "purples",
         hole_fill = "white",
         main = "Working status")


#current pregnancy status



levels(X14)
summary(X14)

PieChart(X14, data = train,
         fill = "purples",
         hole_fill = "white",
         main = "Current pregnancy status")

?PieChart

#response Y1

levels(Y1)
summary(Y1)


df5 <- data.frame(Frequency = c(2744,7840),
                  Category = c("Right","Wrong" ))


df55 <- df5 %>% 
  mutate(Percentages = Frequency/sum(Frequency),labels = scales::percent(Percentages))


ggplot(df55, aes(x = "", y = Percentages, fill = Category)) +
  geom_col(color = "black") +
  geom_label(aes(label = labels), color = c(1, 1),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Response to the myth that people can get HIV virus from mosquito bites")) +
  scale_fill_brewer() +
  coord_polar(theta = "y") + 
  theme_void()



#every variable with Y1

#Residence and Y1

df.1 <- data.frame(X1,Y1)
tab1 <- table(df.1)
print(tab1)

df.11 <- as.data.frame(tab1)
df.111<- df.11 %>% pivot_wider(names_from = Y1, values_from = Freq)

df.1111 <- df.111 %>% mutate(Right_percentage = Right*100/(Wrong+Right),Wrong_percentage = 100- Right_percentage )


#Region and Y1
df.2 <- data.frame(X2,Y1)
tab2 <- table(df.2)
print(tab2)

df.22 <- as.data.frame(tab2)
df.222<- df.22 %>% pivot_wider(names_from = Y1, values_from = Freq)

df.2222 <- df.222 %>% mutate(Right_percentage = Right*100/(Wrong+Right),Wrong_percentage = 100- Right_percentage )



#Religion and Y1
df.3 <- data.frame(X3,Y1)
tab3 <- table(df.3)
print(tab3)


df.33 <- as.data.frame(tab3)
df.333<- df.33 %>% pivot_wider(names_from = Y1, values_from = Freq)

df.3333 <- df.333 %>% mutate(Right_percentage = Right*100/(Wrong+Right),Wrong_percentage = 100- Right_percentage )



#Ethnicity and Y1
df.4 <- data.frame(X4,Y1)
tab4 <- table(df.4)
print(tab4)


df.44 <- as.data.frame(tab4)
df.444<- df.44 %>% pivot_wider(names_from = Y1, values_from = Freq)

df.4444 <- df.444 %>% mutate(Right_percentage = Right*100/(Wrong+Right),Wrong_percentage = 100- Right_percentage )






#Age and Y1
df.5 <- data.frame(X5,Y1)
tab5 <- table(df.5)
print(tab5)

df.55 <- as.data.frame(tab5)
df.555<- df.55 %>% pivot_wider(names_from = Y1, values_from = Freq)

df.5555 <- df.555%>% mutate(Right_percentage = Right*100/(Wrong+Right),Wrong_percentage = 100- Right_percentage )



#Marital State and Y1

df.6 <- data.frame(X6,Y1)
tab6 <- table(df.6)
print(tab6)

df.66 <- as.data.frame(tab6)
df.666<- df.66 %>% pivot_wider(names_from = Y1, values_from = Freq)

df.6666 <- df.666%>% mutate(Right_percentage = Right*100/(Wrong+Right),Wrong_percentage = 100- Right_percentage )



# Current Marital State and Y1

df.7 <- data.frame(X7,Y1)
tab7 <- table(df.7)
print(tab7)

df.77 <- as.data.frame(tab7)
df.777<- df.77 %>% pivot_wider(names_from = Y1, values_from = Freq)

df.7777 <- df.777%>% mutate(Right_percentage = Right*100/(Wrong+Right),Wrong_percentage = 100- Right_percentage )


# Wealth Index and Y1

df.16 <- data.frame(X16,Y1)
tab16 <- table(df.16)
print(tab16)

df.166 <- as.data.frame(tab16)
df.1666<- df.166 %>% pivot_wider(names_from = Y1, values_from = Freq)

df.16666 <- df.1666%>% mutate(Right_percentage = Right*100/(Wrong+Right),Wrong_percentage = 100- Right_percentage )


# HEQ and Y1

df.8 <- data.frame(X8,Y1)
tab8 <- table(df.8)
print(tab8)

df.88 <- as.data.frame(tab8)
df.888<- df.88 %>% pivot_wider(names_from = Y1, values_from = Freq)

df.8888 <- df.888%>% mutate(Right_percentage = Right*100/(Wrong+Right),Wrong_percentage = 100- Right_percentage )




# Radio and Y1

df.eleven <- data.frame(X11,Y1)
tabeleven <- table(df.eleven)
print(tabeleven)

df.eleven.1 <- as.data.frame(tabeleven)
df.eleven.11<- df.eleven.1 %>% pivot_wider(names_from = Y1, values_from = Freq)

df.eleven.111 <- df.eleven.11%>% mutate(Right_percentage = Right*100/(Wrong+Right),Wrong_percentage = 100- Right_percentage )




# NP and mag and Y1

df.9 <- data.frame(X9,Y1)
tab9 <- table(df.9)
print(tab9)

df.eleven.9 <- as.data.frame(tab9)
df.eleven.99<- df.eleven.9 %>% pivot_wider(names_from = Y1, values_from = Freq)

df.eleven.999 <- df.eleven.99 %>% mutate(Right_percentage = Right*100/(Wrong+Right),Wrong_percentage = 100- Right_percentage )


# TV and Y1

df.10 <- data.frame(X10,Y1)
tab10 <- table(df.10)
print(tab10)

df.100 <- as.data.frame(tab10)
df.1000 <- df.100 %>% pivot_wider(names_from = Y1, values_from = Freq)

df.10000 <- df.1000 %>% mutate(Right_percentage = Right*100/(Wrong+Right),Wrong_percentage = 100- Right_percentage )



# media and Y1

df.12 <- data.frame(X12,Y1)
tab12 <- table(df.12)
print(tab12)

df.122 <- as.data.frame(tab12)
df.1222<- df.122 %>% pivot_wider(names_from = Y1, values_from = Freq)

df.12222 <- df.1222%>% mutate(Right_percentage = Right*100/(Wrong+Right),Wrong_percentage = 100- Right_percentage )


# given birth and Y1

df.13 <- data.frame(X13,Y1)
tab13 <- table(df.13)
print(tab13)

df.133 <- as.data.frame(tab13)
df.1333<- df.133 %>% pivot_wider(names_from = Y1, values_from = Freq)

df.13333 <- df.1333%>% mutate(Right_percentage = Right*100/(Wrong+Right),Wrong_percentage = 100- Right_percentage )


# working status and Y1

df.15 <- data.frame(X15,Y1)
tab15 <- table(df.15)
print(tab15)

df.155 <- as.data.frame(tab15)
df.1555<- df.155 %>% pivot_wider(names_from = Y1, values_from = Freq)

df.15555 <- df.1555%>% mutate(Right_percentage = Right*100/(Wrong+Right),Wrong_percentage = 100- Right_percentage )




# current pregnancy and Y1

df.14 <- data.frame(X14,Y1)
tab14 <- table(df.14)
print(tab14)

df.144 <- as.data.frame(tab14)
df.1444<- df.144 %>% pivot_wider(names_from = Y1, values_from = Freq)

df.14444 <- df.1444%>% mutate(Right_percentage = Right*100/(Wrong+Right),Wrong_percentage = 100- Right_percentage )



# y with 2 categorical variables

#X1,X2 and Y1


dff.1 <- data.frame(X1,X2,Y1)
tab.1 <- table(dff.1)
print(tab.1)


#X1,X3 and Y1
dff.2 <- data.frame(X1,X3,Y1)
tab.2 <- table(dff.2)
print(tab.2)


#X1,X4 and Y1
dff.3 <- data.frame(X1,X4,Y1)
tab.3 <- table(dff.3)
print(tab.3)


#X1,X5 and Y1
dff.4 <- data.frame(X1,X5,Y1)
tab.4 <- table(dff.4)
print(tab.4)



#X1,X6 and Y1
dff.5 <- data.frame(X1,X6,Y1)
tab.5 <- table(dff.5)
print(tab.5)



#X1,X7 and Y1
dff.6 <- data.frame(X1,X7,Y1)
tab.6 <- table(dff.6)
print(tab.6)



#X1,X8 and Y1
dff.7 <- data.frame(X1,X8,Y1)
tab.7 <- table(dff.7)
print(tab.7)



#model building

full.model <- glm(Y1~., data = train,family=binomial)

summary(full.model)


null.model <- glm(Y1~1,data=train,family=binomial)
summary(null.model)




#stepwise

forwards = step(null.model,
                scope=list(lower=formula(null.model),upper=formula(full.model)), direction="forward")




# LEVEL 1----------------------------------------------------

model.1 <- glm(Y1~X1,data= train, family = binomial)
summary(model.1)
anova(model.1)

model.2 <- glm(Y1~X2,data= train, family = binomial)
summary(model.2)

model.3 <- glm(Y1~X4,data= train, family = binomial)
summary(model.3)


model.4 <- glm(Y1~X7,data= train, family = binomial)
summary(model.4)



model.5 <- glm(Y1~X8,data= train, family = binomial)
summary(model.5)



model.6 <- glm(Y1~X12,data= train, family = binomial)
summary(model.6)


model.7 <- glm(Y1~X15,data= train, family = binomial)
summary(model.7)



model.8 <- glm(Y1~X16,data= train, family = binomial)
summary(model.8)

# LEVEL 2 ------------------------------------------------

model.9 <- glm(Y1~X8+X1,data= train, family = binomial)
summary(model.9)


model.10 <- glm(Y1~X8+X2,data= train, family = binomial)
summary(model.10)


model.11 <-glm(Y1~X8+X4,data= train, family = binomial)
summary(model.11)



model.12 <- glm(Y1~X8+X7,data= train, family = binomial)
summary(model.12)




model.13 <- glm(Y1~X8+X12,data= train, family = binomial)
summary(model.13)




model.14 <- glm(Y1~X8+X15,data= train, family = binomial)
summary(model.14)


model.15 <- glm(Y1~X8+X16,data= train, family = binomial)
summary(model.15)



# LEVEL 3 ----------------------------------------------


model.16 <- glm(Y1~X8+X2+X8*X2,data= train, family = binomial)
summary(model.16)

1- pchisq(50,40)



# LEVEL 4 ------------------------------------------------
model.17 <- glm(Y1~X8+X2+X1,data= train, family = binomial)
summary(model.17)

model.18 <- glm(Y1~X8+X2+X4,data= train, family = binomial)
summary(model.18)

model.19 <- glm(Y1~X8+X2+X7,data= train, family = binomial)
summary(model.19)

model.20 <- glm(Y1~X8+X2+X12,data= train, family = binomial)
summary(model.20)

model.21 <- glm(Y1~X8+X2+X15,data= train, family = binomial)
summary(model.21)

model.22 <- glm(Y1~X8+X2+X16,data= train, family = binomial)
summary(model.22)

# LEVEL 5 -----------------------------------------

model.23 <- glm(Y1~X8+X2+X4+X8*X2,data= train, family = binomial)
summary(model.23)

model.24<- glm(Y1~X8+X2+X4+X8*X4,data= train, family = binomial)
summary(model.24)


model.25 <- glm(Y1~X8+X2+X4+X2*X4,data= train, family = binomial)
summary(model.25)



# LEVEL 6 -----------------------------------------



model.26 <- glm(Y1~X8+X2+X4+X2*X4+X8*X2,data= train, family = binomial)
summary(model.26)
1- pchisq(272313,139)



model.27 <- glm(Y1~X8+X2+X4+X2*X4+X8*X4,data= train, family = binomial)
summary(model.27)


# LEVEL 7-----------------------------------------

model.28 <- glm(Y1~X8+X2+X4+X2*X4+X1,data= train, family = binomial)
summary(model.28)

model.29 <- glm(Y1~X8+X2+X4+X2*X4+X7,data= train, family = binomial)
summary(model.29)

model.30 <- glm(Y1~X8+X2+X4+X2*X4+X12,data= train, family = binomial)
summary(model.30)

model.31 <- glm(Y1~X8+X2+X4+X2*X4+X15,data= train, family = binomial)
summary(model.31)

model.32 <- glm(Y1~X8+X2+X4+X2*X4+X16,data= train, family = binomial)
summary(model.32)

# LEVEL 8-----------------------------------------

model.33 <- glm(Y1~X8+X2+X4+X2*X4+X16+X8*X16,data= train, family = binomial)
summary(model.33)

model.34 <- glm(Y1~X8+X2+X4+X2*X4+X16+X2*X16,data= train, family = binomial)
summary(model.34)

model.35 <- glm(Y1~X8+X2+X4+X2*X4+X16+X4*X16,data= train, family = binomial)
summary(model.35)

# LEVEL 9-----------------------------------------

model.36 <- glm(Y1~X8+X2+X4+X2*X4+X16+X1,data= train, family = binomial)
summary(model.36)


model.37 <- glm(Y1~X8+X2+X4+X2*X4+X16+X7,data= train, family = binomial)
summary(model.37)


model.38 <- glm(Y1~X8+X2+X4+X2*X4+X16+X12,data= train, family = binomial)
summary(model.38)


model.39 <- glm(Y1~X8+X2+X4+X2*X4+X16+X15,data= train, family = binomial)
summary(model.39)


# LEVEL 10-----------------------------------------

model.40 <- glm(Y1~X8+X2+X4+X2*X4+X16+X7+X8*X7,data= train, family = binomial)
summary(model.40)

model.41 <- glm(Y1~X8+X2+X4+X2*X4+X16+X7+X2*X7,data= train, family = binomial)
summary(model.41)
model.41$coefficients


model.42 <- glm(Y1~X8+X2+X4+X2*X4+X16+X7+X4*X7,data= train, family = binomial)
summary(model.42)

model.43 <- glm(Y1~X8+X2+X4+X2*X4+X16+X7+X16*X7,data= train, family = binomial)
summary(model.43)


# LEVEL 11-----------------------------------------

model.44 <- glm(Y1~X8+X2+X4+X2*X4+X16+X7+X2*X7+X8*X7,data= train, family = binomial)
summary(model.44)


model.45 <- glm(Y1~X8+X2+X4+X2*X4+X16+X7+X2*X7+X4*X7,data= train, family = binomial)
summary(model.45)

model.46 <- glm(Y1~X8+X2+X4+X2*X4+X16+X7+X2*X7+X16*X7,data= train, family = binomial)
summary(model.46)

1 - pchisq(30,6)


# LEVEL 12-----------------------------------------

model.47 <- glm(Y1~X8+X2+X4+X2*X4+X16+X7+X2*X7+X1,data= train, family = binomial)
summary(model.47)

model.48 <- glm(Y1~X8+X2+X4+X2*X4+X16+X7+X2*X7+X12,data= train, family = binomial)
summary(model.48)

model.49 <- glm(Y1~X8+X2+X4+X2*X4+X16+X7+X2*X7+X15,data= train, family = binomial)
summary(model.49)


contrasts(Y1)
model.50 <- glm(Y1~X8+X2+X4+X16+X7,data= train, family = binomial)
summary(model.50)

length(model.41$coefficients)
length(model.50$coefficients)

model.50$formula



# GOF

install.packages("pscl")
library(pscl)
pR2(model.41)["McFadden"]
install.packages("glmtoolbox")
library(glmtoolbox)
summary(model.32)
hltest(model.50)
1-pchisq(11730,10588)
pR2(model.50)

#variable Imporatance
install.packages("InformationValue")
library(InformationValue)

varImp(model.50)

#predictions

glm.probs <- predict(model.50,newdata = test,type = "response")

glm.pred <- ifelse(glm.probs > 0.5, "Wrong", "Right")


test$Y1<-replace(test$Y1, test$Y1 == "Wrong",1) 
test$Y1<-replace(test$Y1, test$Y1 == "Right",0)

test <- data.frame(test)


str(test)
str(fitted.results)

fitted.results <- predict(model.50,newdata=test,type='response')

head(fitted.results)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Y1)
print(paste('Accuracy',1-misClasificError))



X8 <- test$`Highest education qualification`
X2 <- test$Region
X4 <- test$Ethnicity
X16 <- test$`Welth index`
X7 <- test$`Current marital state`

install.packages("shipunov")
library(shipunov)
Misclass(fitted.results,test$Y1)



prob <- model.50 %>% predict(test,type="response")
head(prob)

predicted.classes <- ifelse(prob > 0.5 , "Wrong","Right")

head(predicted.classes)

mean(predicted.classes == test$Y1)


library(caret)

confusionMatrix(predicted.classes,test$Y1)
