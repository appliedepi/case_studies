
# Step 1 ------------------------------------------------------------------

# To see your language locale
Sys.getlocale()

# To change it into English
Sys.setlocale("LC_ALL", "English")


# Ensures the package "pacman" is installed
if (!require("pacman")) {
  install.packages("pacman") }

pacman::p_load(
  
  rio,             # to import datasets
  skimr,           # for displaying your data
  janitor,         # a package for data cleaning
  epiR,            # for the adjusted risk ratios across strata
  lmtest,          # for the likelihood ratio test 
  broom,           # to generate tidy tibbles of regression analysis 
  tidyverse        # data management and visualization
)



# Step 2 ------------------------------------------------------------------

tira_clean <- import("data/tira_clean.csv") 

# Inspect your dataset and generate basic statistics using the skim function  
skim(tira_clean)
# Glimpse is an alternative to skim which gives more accurate information about the nature of the  variables
glimpse(tira_clean)
# To see the different categories of the variables
tabyl(tira_clean, beer)


# Step 3 ------------------------------------------------------------------


## 3.1 ---------------------------------------------------------------------

tabyl(tira_clean, ill)


## 3.2 ---------------------------------------------------------------------

#write the formula of the logistic regression model with 'ill' as the outcome and 'tira' as the exposure
model1 <- glm(ill ~ tira, 
              data = tira_clean, 
              family = binomial(link = "logit"))

#Using the 'summary' function, we obtain an overview of the key results.
summary(model1)

## 3.3 ---------------------------------------------------------------------

#get the odds ratio with its confidence interval using the tidy function
tidy(model1, 
     exponentiate = TRUE, 
     conf.int = TRUE)


## 3.4 ---------------------------------------------------------------------

#we get the p-value for the goodness of fit test by setting lower.tail = FALSE
pchisq(model1$deviance, model1$df.residual, lower.tail = FALSE)



## 3.5 --------------------------------------------------------------------


#write the formula saving the model in an object
model_wmousse <- glm(ill~wmousse,
                     data = tira_clean,
                     family = binomial(link = "logit"))

#get the summary of the results
summary(model_wmousse)

#exponentiate the estimates and get confidence intervals
tidy(model_wmousse, exponentiate = TRUE, conf.int = TRUE)

#assess the fitness of the model
pchisq(model_wmousse$deviance, model_wmousse$df.residual, lower.tail = FALSE)


## 3.6 ---------------------------------------------------------------------

#check class of column tportion
class(tira_clean$tportion)

#convert tportion into factor
tira_clean <- tira_clean %>% 
  mutate(tportion = factor(tportion, levels = c("0", "1", "2+")))

#checkk that the change has been effective
class(tira_clean$tportion)
tabyl(tira_clean, tportion)


#write the formula and save it as an object
model_tportion <- glm(ill ~ tportion,
                      data = tira_clean,
                      family = binomial(link = "logit"))

#take a look at the results
summary(model_tportion)

#use the tidy function to obtain the odds ratios and confidence intervals
tidy(model_tportion, exponentiate = TRUE, conf.int = TRUE)



# Step 4 ------------------------------------------------------------------


## 4.1 ---------------------------------------------------------------------

# Option 1: write the formula again and save it as an object
model2 <- glm(ill ~ tira + beer,
              data = tira_clean,
              family = binomial(link = "logit"))
# Option 2: use the update() function
model2 <- update(model1, 
                 formula = ill ~ tira + beer)
#take a look at the results
summary(model2)

#use the tidy function to obtain the odds ratios and confidence intervals
tidy(model2, exponentiate = TRUE, conf.int = TRUE)


## 4.3 ---------------------------------------------------------------------



# Remove rows with missing values in any of the adjusting variables

## create an object with the adjusting variables
adj_var <- c("ill", "tira", "beer", "age", "sex", "dmousse", "wmousse", 
             "roastbeef", "chickenwin", "salmon", "pork")

## Option 1
tira_clean_adj <- tira_clean %>% 
  select(adj_var) %>% 
  drop_na()

## Option 2
tira_clean_adj <- tira_clean %>% 
  drop_na(adj_var)


## 4.4 ---------------------------------------------------------------------

# write the formula again and save it as an object
logitmodel <- glm(ill ~ tira + beer + age + sex + 
                    dmousse + wmousse + roastbeef + 
                    chickenwin + salmon + pork,
                  data = tira_clean_adj,
                  family = binomial(link = "logit"))

#take a look at the results
summary(logitmodel)

#use the tidy function to obtain the odds ratios and confidence intervals
tidy(logitmodel, exponentiate = TRUE, conf.int = TRUE)


## 4.5 ---------------------------------------------------------------------


# Rerun model1 with the new dataset
model1 <- glm(ill ~ tira, 
              data = tira_clean_adj, 
              family = binomial(link = "logit"))

# Check performance of both models
glance(model1)   # performance metrics of the univariate model

glance(logitmodel) # performance metrics of the adjusted model


## 4.6 ---------------------------------------------------------------------


# write the formula and save it as an object
tirabeer <- glm(ill ~ tira*beer + age + sex + 
                  dmousse + wmousse + roastbeef + 
                  chickenwin + salmon + pork,
                data = tira_clean_adj,
                family = binomial(link = "logit"))

#take a look at the results
tidy(tirabeer, exponentiate = TRUE, conf.int = TRUE)

#assess which model performs better
glance(logitmodel)
glance(tirabeer)
