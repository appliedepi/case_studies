
# STEP 1: SET UP ----------------------------------------------------------

## Step 1.2: Define R language ---------------------------------------------
### To see your language locale
Sys.getlocale()

### To change it into English
Sys.setlocale("LC_ALL", "English")

### Ensures the package "pacman" is installed
if (!require("pacman")) {
  install.packages("pacman") }


## Step 1.3: Install/load packages -----------------------------------------
### install (if necessary) from CRAN and load packages to be used
pacman::p_load(
  rio,        # importing data  
  skimr,      # get overview of data
  janitor,    # data cleaning and tables
  gtsummary,  # summary statistics, tests and regressions 
  broom,      # to generate tidy tibbles of regression analysis
  rstatix,    # for statistics, including statistical tests
  ggfortify,  # data visualisation for statistical analysis results
  tidyverse  # data management and visualization
)


# STEP 2: IMPORT AND EXPLORE DATA -----------------------------------------

## Step 2.1: Import the data and brief exploration -------------------------
### Import the data
tbe <- import("data/tbe.RDS")

### Explore the data
skim(tbe)


## Step 2.2: Inspect factor columns ----------------------------------------

### One by one
tabyl(tbe, hyper)

tabyl(tbe, vac)

### All at once
tbe %>% 
  
  select(where(is.factor)) %>% #we first select only the columns that are of class 'factor'
  
  map(.f = tabyl)              #inside map() from {purrr} we specify the function we want to apply to the entire dataframe


## Step 2.3: Create the histogram with the length of hospitalisation --------
tbe %>%                                  #we call the data first and we pass it into ggplot with the pipe operator
  
  ggplot(mapping = aes(x = hospd)) +     #when drawing an histogram we only need to specify the x-axis   
  
  geom_histogram(aes(y = ..density..)) + #here we are telling ggplot2 to display the density and not the freq count
  
  #the function below will add the normal curve. 
  stat_function(fun = dnorm,  #In the fun = argument we are specifying that we want the normal curve         
                args = list(mean = mean(tbe$hospd, na.rm = T), #to draw a normal curve we need to give the mean and standard deviation of our column
                            sd   = sd(tbe$hospd, na.rm = T)),  
                
                col = "darkblue", lwd = 1) # Identify the colour and line width of the normal curve

## Step 2.4: Create a cross-table and calculate a statistical test ---------
tbe %>% 
  select(hyper, sex) %>%   #we select the columns we are interested in
  tbl_summary(by = hyper) %>% #we specify that we want by hypertension status
  add_p()                     # adding this command will calculate the most appropriate statistical test



# STEP 3: LINEAR ASSOCIATION HOSPD AGE ------------------------------------

## Step 3.1: Inspect a potential linear association ------------------------
tbe %>%    
  
  ggplot(mapping = aes(x = age,        # we put length of hospitalisation on the y-axis because this axis usually contains the dependent variable; and here we want to know if hospd depends on age
                       y = hospd)) +    
  
  geom_point() +                       # this geometry will create a scatterplot
  
  geom_smooth(method = lm) +           # this geometry will add a trend line. "lm" is for "linear model"
  
  scale_x_continuous(name = "Age" , limits = c(0,100)) +          # Format the x-axis to a range between 0 and 100 
  
  scale_y_continuous(limits = c(0,70)) +                          # Format the y axis to a range between 0 and 70
  
  labs(
    x = "Age",
    y = "Length of hospitalization in days"
  ) + 
  
  
  theme_bw()                            # Add a pre-defined theme for formatting


## Step 3.2: Check if the association between age and length of hos --------
### As separate graphs
tbe %>%    
  
  ggplot(mapping = aes(x = age,        # we put length of hospitalisation on the y-axis because this axis usually contains the dependent variable; and here we want to know if hospd depends on age
                       y = hospd)) +    
  
  geom_point() +                       # this geometry will create a scatterplot
  
  geom_smooth(method = lm) +           # this geometry will add a trend line. "lm" is for "linear model"
  
  facet_wrap(~sex)  +                   # adding this function will generate a separate graph for each category of sex
  
  
  scale_x_continuous(name = "Age" , limits = c(0,100)) +          # Format the x-axis to a range between 0 and 100 
  
  scale_y_continuous(limits = c(0,70)) +                          # Format the y axis to a range between 0 and 70
  
  labs(
    x = "Age",
    y = "Length of hospitalization in days"
  ) + 
  
  
  theme_bw()                            # Add a pre-defined theme for formatting


### Same graphs with different colours 
tbe %>%    
  
  ggplot(mapping = aes(x = age,        # we put length of hospitalisation on the y-axis because this axis usually contains the dependent variable; and here we want to know if hospd depends on age
                       y = hospd,
                       colour = sex )) + #we add the colour in the aes so that it varies according to the categories of sex   
  
  geom_point() +                       # this geometry will create a scatterplot
  
  geom_smooth(method = lm) +           # this geometry will add a trend line. "lm" is for "linear model"
  
  scale_x_continuous(name = "Age" , limits = c(0,100)) +          # Format the x-axis to a range between 0 and 100 
  
  scale_y_continuous(limits = c(0,70)) +                          # Format the y axis to a range between 0 and 70
  
  labs(
    x = "Age",
    y = "Length of hospitalization in days"
  ) + 
  
  theme_bw()                            # Add a pre-defined theme for formatting




# STEP 4: LENGTH OF HOSP. BY HYPERTENSION ---------------------------------

## Step 4.1: Simple statistical test -------------------------------------------------

# Plot the frecuency distribution by hypertension status
tbe %>%
  
  ggplot(mapping = aes(x = hospd)) +
  
    geom_histogram() + 
    
    facet_wrap(~hyper) + # add facet_wrap() to get a graph for each hyper status
    
    labs(
      x = "Length of hospitalization in days",
      y = "Frequency"
    )

# Plot the density and add a  normal curve

tbe %>%                                  
  
  ggplot(mapping = aes(x = hospd, fill = hyper)) +  #remember that for bars, fill is is the interior colour     
  
    geom_histogram(aes(y = ..density..)) + #here we are telling ggplot2 to display the density and not the freq count
  
    facet_wrap(~hyper) + # add facet_wrap() to get a graph for each hyper status

    #the function below will add the normal curve. 
    stat_function(fun = dnorm,  #The fun = argument we are specifying that we want the normal curve         
                args = list(mean = mean(tbe$hospd, na.rm = T), #to draw a normal curve we need to give the mean and standard deviation of our column
                            sd   = sd(tbe$hospd, na.rm = T)),  
                
                col = "darkblue", lwd = 1) + # Identify the colour and line width of the normal curve
    labs(
      x = "Length of hospitalization in days",
      y = "Density"
      )


# Shapiro-Wilk test
tbe %>%
  
  group_by(hyper) %>% #we first group by out independent or exposure variable
  
  
  shapiro_test(hospd)  # this function performs the Shapiro-Wilk test for all groups separately


# Student t-test 
tbe %>%
  
  t_test(hospd ~ hyper)  # we write first our dependent variable and then the exposure one





## Step 4.2: Univariate linear regression between hospd and hyper --------
###First we write the formula (lm stands for linear model) and assign the model to an object
hyper_hospd_lm <- lm(hospd ~ hyper, data= tbe)

### Prin the detailed output of the model
summary(hyper_hospd_lm)

### Print the main model parameters
results_hyper_hospd_lm <- tidy(hyper_hospd_lm) 
results_hyper_hospd_lm                     


# STEP 5: UNIVARIATE REST OF VARIABLES ------------------------------------

## Step 5.1: Univariate linear regression between hospd and age --------
### Run the linear regression and assign the output to age_hospd_lm
age_hospd_lm <- lm(hospd ~ age, data = tbe)

### Print the results using the summary() function
summary(age_hospd_lm)

### Print the results of the regression analysis with the tidy() function
tidy(age_hospd_lm)


## Step 5.2: Univariate linear regression between hospd and factor --------
### Here we write one by one for each variable the formula and tidy

####sex
sex_hospd_lm <- lm(hospd ~ sex, data = tbe)
tidy(sex_hospd_lm)

####other comorbidities
other_hospd_lm <- lm(hospd ~ other, data = tbe)
tidy(other_hospd_lm)

####vaccination
vac_hospd_lm    <- lm(hospd ~ vac, data = tbe)
tidy(vac_hospd_lm)

####monophasic course
mono_hospd_lm   <- lm(hospd ~ mono, data = tbe)
tidy(mono_hospd_lm)

####large tick at removal
tick_hospd_lm   <- lm(hospd ~ tick, data = tbe) 
tidy(tick_hospd_lm)


### Here we do it with {purrr} all in one command
tbe %>% 
  select(-hyper, -age, -hospd) %>% # we first remove the columns we have explored before + the outcome column
  map(.f = ~lm(hospd ~ .x, data = tbe)) %>%   # here we carry out the model for each column. ".x" represents all the column
  map(tidy)                                   # finally we do tidy on each model



# STEP 6: MULTIVARIATE ANALYSIS -------------------------------------------


## Step 6.1: Add age as a covariate to the model ---------------------------

### Creating a new model and adding age 
hyper_hospd_adj_lm <- lm(hospd ~ hyper + age, data = tbe)

tidy(hyper_hospd_adj_lm)



## Step 6.2: Compare the performance of both models ------------------------
glance(hyper_hospd_lm)   # performance metrics of the univariate model

glance(hyper_hospd_adj_lm) # performance metrics of the adjusted model



## (Optional) Step 6.3: Plotting effects of age by hypertension ------------
### First add a variable with the fitted values to the dataframe
data_fitted <- tbe %>%
  
  filter(!is.na(hospd)) %>%   # Remove NAs 

  mutate(fit = predict(hyper_hospd_adj_lm)) # Create a new variable with the predicted values

###### You can now open data_fitted to have a look at the new column 

### Now we do the plot with the fitted lines
plot_fitted <- data_fitted %>%
  
  ggplot(mapping = aes(x = age, 
                       y = hospd,
                       colour = hyper)) +
  
  geom_point() +                            # Adding a scatter plot
  
  geom_line(aes(y = fit)) +                 # we add a line specifying that we want the fitted and not the observed values in the y-axis

  labs(
    title = "Effect of age on TBE hospital stay length for people\nwith and without hypertension ", # Title of the plot, note that "\n" breaks the title into the next line
    x = "Age",
    y = "Length of hospitalization in days",
    colour = "Hypertension"
  ) +
  
  theme_bw()                                  # we add a predefined theme

plot_fitted

## Step 6.4: Continue building the adjusted model --------------------------

### We first add sex to the previous model
hyper_hospd_adj_lm <- lm(hospd ~ hyper + age + sex, data = tbe) # formula
tidy(hyper_hospd_adj_lm)                                # estimates
glance(hyper_hospd_adj_lm)                              # performance metrics


### TBE vaccination
hyper_hospd_adj_lm <- lm(hospd ~ hyper + age + sex + vac, data = tbe) # formula
tidy(hyper_hospd_adj_lm)                                # estimates
glance(hyper_hospd_adj_lm)                              # performance metrics


### Monophasic disease course
hyper_hospd_adj_lm <- lm(hospd ~ hyper + age + sex + vac + mono, data = tbe) # formula
tidy(hyper_hospd_adj_lm)                                # estimates
glance(hyper_hospd_adj_lm)                              # performance metrics


### Large tick at removal
hyper_hospd_adj_lm <- lm(hospd ~ hyper + age + sex + vac + mono + tick, data = tbe) # formula
tidy(hyper_hospd_adj_lm)                                # estimates
glance(hyper_hospd_adj_lm)                              # performance metrics


### Other comorbidities
hyper_hospd_adj_lm <- lm(hospd ~ hyper + age + sex + vac + mono + tick + other, data = tbe) # formula
tidy(hyper_hospd_adj_lm)                                # estimates
glance(hyper_hospd_adj_lm)                              # performance metrics

## Step 6.5: Adding an interaction term ------------------------------------
hyper_hospd_adj_lm <- lm(hospd ~ hyper + vac + mono + tick + other + age*sex, data = tbe) # formula

tidy(hyper_hospd_adj_lm)                                # estimates

glance(hyper_hospd_adj_lm)                              # performance metrics


# STEP 7: MODEL DIAGNOSTICS -----------------------------------------------

## Step 7.1: Normality assumption ------------------------------------------

### Diagnostic plots 
autoplot(hyper_hospd_adj_lm, which = 1:2)


### Histogram of residuals 

#### First add a variable with the residual values to the dataframe
data_res <- tbe %>%
  
  filter(!is.na(hospd)) %>%   # Remove NAs 
  
  mutate(res = resid(hyper_hospd_adj_lm)) # Create a new variable with the residual values

####### You can now open data_res to have a look at the new column 

#### Now we do the histogram with the residuals

data_res %>%
  
  ggplot(mapping = aes(x = res)) +
  
  geom_histogram(aes(y = ..density..)) +                        # Adding a histogram with density
  
  #the function below will add the normal curve. 
  stat_function(fun = dnorm,  #The fun = argument we are specifying that we want the normal curve         
                args = list(mean = mean(data_res$res, na.rm = T), #to draw a normal curve we need to give the mean and standard deviation of our column
                            sd   = sd(data_res$res, na.rm = T)),  
                
                col = "darkblue", lwd = 1) +
  labs(
    title = "Histogram of Residuals", 
    x = "Residuals",
    y = "Frequency"
  ) +
  
  theme_bw()                                  # we add a predefined theme


### Shapiro-Wilk on residuals 
data_res %>%
  shapiro_test(res)



## Step 7.2: Homescedasticitiy ---------------------------------------------

### Generate the scale-location plot
autoplot(hyper_hospd_adj_lm, which = 3)


