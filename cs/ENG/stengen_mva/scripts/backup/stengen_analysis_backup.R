
# STEP 1: SET UP ----------------------------------------------------------

## Step 1.2 Define R language ----------------------------------------------
### To see your language locale
Sys.getlocale()

### To change it into English
Sys.setlocale("LC_ALL", "English")


## Step 1.3: Install/load packages -----------------------------------------
### Ensures the package "pacman" is installed
if (!require("pacman")) {
  install.packages("pacman") }

pacman::p_load(
  
  rio,             # to import datasets
  skimr,           # for displaying your data
  janitor,         # a package for data cleaning
  gtsummary,       # to create frequency tables
  rstatix,         # to generate summary statistics
  epitools,        # for univariable analysis
  epikit,          # for creating age categories
  epiR,            # for the adjusted risk ratios across strata
  tidyverse  # data management and visualization
)


# STEP 2: IMPORT AND EXPLORE DATA -----------------------------------------

## Step 2.1: Import the data -----------------------------------------------
### Import the dataset 'tira.csv'  
tira_raw <- import("data/tira.csv") 

## Step 2.2: Explore the data ----------------------------------------------
### Inspect your dataset and generate basic statistics using the skim function  
skim(tira_raw)

### Glimpse is an alternative to skim which gives more accurate information about the nature of the  variables
glimpse(tira_raw)

### To see the different categories of the variables
tabyl(tira_raw, tomato)


## Step 2.3: Explore age in more detail ------------------------------------
### Generate summary statistics 
summary(tira_raw$age)

### Creating a boxplot of age by illness status
boxplot_age <- tira_raw %>% 
  
  ggplot(mapping = aes(x = ill,
                       y = age,
                       fill = ill)) + 
  
  geom_boxplot() + 
  
  labs(
    title = "Boxplot: Age distribution per illness status",
    x = "Disease status",
    y = "Age",
    fill = "Illness (0=No;1=Yes)"
  ) +
  
  theme_bw()

boxplot_age

# STEP 3: CLEAN THE DATA --------------------------------------------------


## Steps 3.1 to 3.4 --------------------------------------------------------
tira_clean <- tira_raw %>% # tira_clean will be out clean dataframe after we have performed cleaning tasks on tira_raw
  
  # this would be changing one-by-one the columns into factors
  mutate(ill = as.factor(ill)) %>% 
  
  # this would change all columns we are interested into 'factors' in only one command
  mutate(across(.cols = !c(uniquekey, dateonset, age), # with the exclamation mark before c() we are telling R that we want all columns, except those in the c() list, to be transformed into factors
                .fns = ~ as.factor(.x))) %>% 
  
  # we can do all columns at the same time with across(). Here we use recode for this example 
  mutate(across(.cols = c(salmon, horseradish, pork), .fns = ~ recode(.x, "9" = NA_character_))) %>% 
  
  # simple logical recoding with if_else()
  mutate(across(.cols = c(tportion, mportion), .fns = ~if_else(.x == "3" | .x == "2", "2+", .x))) %>% 
  
  # create age categories
  mutate(age_group = age_categories(age,           # the column to use to create the cateogories 
                                    lower = 0,     # the lower value
                                    upper = 20,    # the upper value (we want a group of 20+)
                                    by = 20))      # as we only want two groups, this is also 20



## Step 3.5: Re-create the boxplot -----------------------------------------
### Boxplot of age by illness status
boxplot_age <- tira_clean %>% 
  
  ggplot(mapping = aes(x = ill,
                       y = age,
                       fill = ill)) + 
  
  geom_boxplot() + 
  
  labs(
    title = "Boxplot: Age distribution per disease status",
    x = "Disease status",
    y = "Age",
    fill = "Illness (0=No;1=Yes)"
  ) +
  
  theme_bw()

boxplot_age

### Save the boxplot
#### A static way of saving it
ggsave(filename = "outputs/boxplot_age.png", plot = boxplot_age)
#### A dynamic way of doing it so that it contains the date of the analysis
ggsave(filename = str_glue("outputs/boxplot_age_",             #First we write the beginning of the file name until the date 
                           str_replace_all(Sys.Date(), "-", ""),  #Sys.Date() gives us the date of analysis. Str_replace_all() removes all hyphens 
                           ".png"),                               # Finally we add the ending which is the file type
       plot = boxplot_age)



# STEP 4: DESCRIPTIVE ANALYSIS --------------------------------------------

## Step 4.2: Analysis by time ----------------------------------------------
tira_clean %>% 
  
  ggplot(mapping = aes(x = dateonset)) + 
  
  geom_histogram(binwidth = 1,             # we assign the width of the bins to one day
                 color = "darkgreen",      # color of lines around bars
                 fill = "lightgreen" )  +  # color of fill within bars 
  
  scale_x_date(date_breaks = "day") +      # we want each day to be shown in the x-axis
  
  labs(
    title = "Epicurve of cases by date of onset",
    y = "Number of cases",
    x = ""                                 # we leave x-axis title blank because we already say in title it is date of onset
  ) +
  
  theme_bw() +                             # predefined theme
  
  theme(axis.text.x = element_text(angle = 90)) # we change the angle of the days in the x-axis so it is readble. Important this happens at the end

## Step 4.3: Analysis by person --------------------------------------------

tira_clean %>% 
  
  select(sex, age_group, ill) %>%  # we select the columns we're interested in
  
  tbl_summary(by = ill, percent = "column") # we assign the grouping column and define that we want the percentages by column



# STEP 5: DEVELOP AND TEST HYPOTHESIS -------------------------------------

## Step 5.1: Compute food-specific attack rates --------
tira_clean %>% 
  
  select(tira, wmousse, dmousse, wmousse, beer, 
         redjelly, fruitsalad, tomato, mince, salmon,
         horseradish, chickenwin, roastbeef, pork, ill) %>% # we select the columns we're interested in
  
  tbl_summary(by = ill, percent = "row") %>% #we assign the grouping column and define that we want the percentages by row
  
  add_overall()    # we add the overall counts


## Step 5.2: Estimate the relative risk of the different exposures ---------
### Relative risk wmousse
riskratio(tira_clean$wmousse, tira_clean$ill)

### Relative risk of the rest of variables with purrr
tira_clean %>% 
  select(-uniquekey, -age, -age_group, -sex, -dateonset, -ill) %>% # we first remove the columns we're not interested + the outcome column
  map(.f = ~riskratio(.x, tira_clean$ill))     # here we call the function for each column. ".x" represents all the columns in the database that have not been removed previously


## Step 5.3: Confounding and effect modification ---------------------------
### Group the dataframe
group_data <- tira_clean %>% 
  mutate(across(.cols = c(tira, beer, ill), .fns = ~factor(.x, levels = c(1,0)))) %>% # we change the order of the levels
  group_by(tira, beer, ill) %>% # it's important to group them in this order: strata, exposure, outcome
  summarise(n = n()) 

### Run the function to obtain M-H estimates
strat_analysis <- epi.2by2(dat = group_data, method = "cohort.count",  # the method specifies that we are dealing with a cohort study
           conf.level = 0.95, outcome = "as.columns")                  # we set up confidence intervals to 95 and we specify that the outcome variable is a column in the data
#Print the results
strat_analysis


