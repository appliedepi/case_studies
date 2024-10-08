

# Step 1: Getting ready for importing the data ----------------------------


## Step 1.2 Define R language ----------------------------------------------

# To see your language locale
Sys.getlocale()

# To change it into English
Sys.setlocale("LC_ALL", "English")



## Step 1.3 Install/load packages ------------------------------------------

# Ensures the package "pacman" is installed
if (!require("pacman")) {
  install.packages("pacman") }


# install (if necessary) from CRAN and load packages to be used
pacman::p_load(
  rio,        # importing data  
  skimr,      # get overview of data
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  epikit,     # to create age categories
  gtsummary,  # summary statistics, tests and regressions 
  apyramid,   # plotting age pyramids 
  tidyverse  # data management and visualization
)



# Step 2: Import and explore the data -------------------------------------


## Step 2.1: Import the different data frames ------------------------------


# Case-based data
cb_data_raw_csv <- import("data/E_pox_case_based_data.csv")
cb_data_raw_json <- import("data/E_pox_case_based_data.json")
cb_data_raw_xlsx <- import("data/E_pox_case_based_data.xlsx")

# Aggregated data
agg_data_raw_csv <- import("data/E_pox_aggregated_data.csv")
agg_data_raw_json <- import("data/E_pox_aggregated_data.json")
agg_data_raw_xlsx <- import("data/E_pox_aggregated_data.xlsx")


## Step 2.2: Explore the different data frames -----------------------------

skim(cb_data_raw_csv)
skim(cb_data_raw_json)
skim(cb_data_raw_xlsx)

# Explore the different categories of gender and clinical columns in one of the cb data frames
tabyl(cb_data_raw_csv, Gender)

tabyl(cb_data_raw_csv, ClinicalSymptoms)

tabyl(cb_data_raw_csv, Outcome)

tabyl(cb_data_raw_csv, HIVStatus)

tabyl(cb_data_raw_csv, SexualOrientation)

# Explore the different aggregated data frames

skim(agg_data_raw_csv)
skim(agg_data_raw_json)
skim(agg_data_raw_xlsx)

# Remove json and xlsx files as they are exactly the same as the csv ones. Within rm() we ask for the objects containing the pattern "json" or "xlsx" to be removed from the environment
rm(list = ls(pattern = "json|xlsx"))


# Step 3: Cleaning the data -----------------------------------------------


## Step 3.1: Clean the case-based data -------------------------------------

# Create a new object called cb_data which is the clean version of the raw data, applying the cleaning functions


cb_data <- cb_data_raw_csv %>% 
  
  clean_names() %>% # standardises names and puts all into lower case 
  
  #(Note: after this point all column names have changed)
  
  mutate(date_of_notification = ymd(date_of_notification)) %>%  #transform ONE column into date
  
  mutate(across(starts_with("date"), 
                .fns = ~ ymd(.x))) %>%  #transforms ALL columns starting with "date" into dates
  
  mutate(week_date = floor_date(date_of_notification, # create week column with Monday start
                                unit = "week",
                                week_start = "Monday")) %>% 
  
  mutate(across(where(is.character), 
                .fns = ~ ifelse(.x == "", NA, .x)))  %>% #transforms empty cells into NA across all character columns
  
  mutate(gender = recode(gender,
                         "F" = "Female",
                         "M" = "Male",
                         "O" = "Other",
                         "UNK" = "Unknown")) %>%
  
  
  mutate(across(where(is.character), 
                .fns = ~ ifelse(.x == "UNK", "Unknown", .x)))  %>% #transforms UNK to Unknown across all character columns
  
  mutate(outcome = ifelse(outcome == "A", "Alive", outcome)) %>%   #we can recode as well with ifelse if we want to change only one or two categories
  
  mutate(hiv_status = case_when(hiv_status == "NEG" ~ "Negative",    #for more complex recoding better case_when
                                hiv_status == "POS" ~ "Positive",
                                TRUE                ~ "Unknown/missing")) %>% 
  
  mutate(sexual_orientation = case_when(sexual_orientation == "BISEXUAL" ~ "Bisexual",
                                        sexual_orientation == "HETERO" ~ "Heterosexual",
                                        sexual_orientation == "MSM" ~ "MSM/homo or bisexual male",
                                        TRUE                        ~  "Unknown/missing")) %>% 
  
  mutate(age_group = age_categories(age, 
                                    lower = 0,      #set up the lower age
                                    upper = 70,     #set up the upper age
                                    by = 10))       #set up the age breaks




# Check that all changes have been made correctly

skim(cb_data)

tabyl(cb_data, gender)

tabyl(cb_data, clinical_symptoms)

tabyl(cb_data, outcome)

tabyl(cb_data, hiv_status)

tabyl(cb_data, sexual_orientation)

tabyl(cb_data, week_date)

tabyl(cb_data, age_group)



## Step 3.2: Clean the aggregated data -------------------------------------

# Check class of date of reporting column

class(agg_data_raw_csv$DateRep) #It is a date, so we do not need to change its class

# Create a new object called agg_data which is the clean version of the raw data, applying the cleaning functions

agg_data <- agg_data_raw_csv %>% 
  
  clean_names() %>% # standardises names and puts all into lower case 
  
  #(Note: after this point all column names have changed)
  
  mutate(week_date = floor_date(date_rep, # create week column with Monday start
                                unit = "week",
                                week_start = "Monday"))




# Step 4: Basic descriptives ----------------------------------------------


## Step 4.1: Table my place (country) --------------------------------------


# Create an object with the table
cb_country_table <- cb_data %>%
  
  select(country) %>% #select the column that we want to use in the table
  
  gtsummary::tbl_summary() # create the table

# Ask R to print the table
cb_country_table


## Step 4.2: Epicurve by week of notification (overall) --------------------

epicurve_epox <- ggplot(data = cb_data,          #data to be used
                        aes(x = week_date)) +    #with geom_histogram() you only need to assign the x axis
  
  geom_histogram(binwidth = 7,                   #binwidth 7 ensures that the width represents 7 days
                 fill="darkgreen",               #colour inside the bins
                 color="white",                  #outline colour of the bins
                 alpha=0.8) +                    #transparency of the bins
  
  scale_x_date(breaks = "2 weeks") +             #set the x axis labels to two week intervals
  
  
  labs(title="Mpox cases reported in 2022") +  #add a title
  
  theme_bw() +                                  #assign a predefined theme
  
  theme(axis.text = element_text(size=9),       #define the font size of the axis text
        axis.title = element_blank(),           #remove the titles of the x and y axis 
        axis.text.x = element_text(angle=90))   #rotate the x axis text


epicurve_epox


## Step 4.3: Epicurve by week of notification (by country) -----------------

epicurve_epox_country <- ggplot(data = cb_data,  #data to be used
                                aes(x = week_date,       
                                    fill = country)) +   #now the fill needs to be inside aes()  
  
  geom_histogram(binwidth = 7,                   #binwidth 7 ensures that the width represents 7 days
                 color="white",                  #outline colour of the bins
                 alpha=0.8) +                    #transparency of the bins
  
  scale_fill_viridis_d() +                       #we change the predefined colours
  
  scale_x_date(breaks = "2 weeks") +             #set the x axis labels to two week intervals
  
  
  labs(title="Mpox cases reported by country in 2022") +  #add a title
  
  theme_bw() +                                  #assign a predefined theme
  
  theme(legend.position = "bottom",             #legend position to the bottom
        axis.text = element_text(size=9),       #define the font size of the axis text
        axis.title = element_blank(),           #remove the titles of the x and y axis 
        axis.text.x = element_text(angle=90),   #rotate the x axis text
        legend.title = element_blank())         #remove title of legend


epicurve_epox_country



## Step 4.4: Demographic characteristics -----------------------------------

# Explore gender and age group columns
tabyl(cb_data, gender)
tabyl(cb_data, age_group)

# Table with sexual orientation 

tab_sor <- cb_data %>% 
  
  select(sexual_orientation) %>% 
  
  tbl_summary(label = list(sexual_orientation ~ "Sexual Orientation")) 

tab_sor


## Step 4.5: Clinical characteristics --------------------------------------

# Bar plot with clinical symptoms

bar_clinical <- cb_data %>% 
  
  drop_na(clinical_symptoms) %>%   # we remove those with missing clinical symptoms
  
  group_by(clinical_symptoms) %>% 
  
  summarise(n_cases = n(), .groups = "drop") %>%
  
  mutate(prop=(n_cases/sum(n_cases))*100) %>%  # we create a column with proportions
  
  ggplot(aes(y = reorder(clinical_symptoms, prop), x = prop)) +  # the reorder function ensures that categories are ordered by proportion in the graph
  
  geom_col(fill = "darkgreen") + 
  
  labs(
    title= "Frequency of clinical symptoms in Mpox cases",
    y = "",
    x = "Number of cases"
  ) +
  
  theme_bw() +
  
  theme(axis.text = element_text(size=9))       #define the font size of the axis

bar_clinical  


# Table with number and percentage of cases by outcome

tab_outcome <- cb_data %>% 
  
  select(outcome) %>% 
  
  tbl_summary(label = list(outcome = "Reported outcome")) # with the argument "label" we can change how the column name is displayed

tab_outcome

# Table with sexual orientation by HIV outcome

tab_hiv_sor <- cb_data %>% 
  
  select(hiv_status, sexual_orientation) %>% 
  
  filter(hiv_status != "Unknown/missing") %>% # we remove the Unknown
  
  tbl_summary(by = hiv_status, label = list(sexual_orientation ~ "Sexual Orientation")) %>% 
  
  add_p()                                     # this function will estimate a p value with the appropriate statistical test based on the class of the columns and the number of observations

tab_hiv_sor


# Step 5: Optional analysis -----------------------------------------------


## Step 5.1: Delay between date of onset, diagnosis and notification -------

# Estimate delay between onset and diagnosis, and between diagnosis and notification

delay_db <- cb_data %>% 
  
  mutate(delay_diag = as.numeric(date_of_diagnosis - date_of_onset)) %>%   #we create variables with difference between dates, we transform them in numeric to be able to then calculate measures of central tendency
  
  mutate(delay_not = as.numeric(date_of_notification - date_of_diagnosis))

summary(delay_db$delay_diag) #the summary will give us measures of central tendency and dispersion
summary(delay_db$delay_not)


delay_country <- delay_db %>% #here, we group by country and summarise the median to compare across countries
  
  group_by(country) %>% 
  
  summarise(median_delay_diag = median(delay_diag, na.rm = T),
            median_delay_not = median(delay_not, na.rm = T))

delay_country

# Line graph with the different dates 

dates_longer <- cb_data %>% # use the variables of the dates and make a longer dataset. In the pivot_longer() command we select the columns which we want to expand in long format and transform the dataset
  
  pivot_longer(
    
    cols=starts_with("date_"),         # all columns starting with "date_" will be taken 
    
    names_to = "indicator",            #the names of the columns will be placed in a single column called "indicator"
    
    values_to = "date")                # the values (which are dates in this case) will be placed in a column called "date"


dates_longer_week <- dates_longer  %>% 
  
  mutate(week_date = floor_date(date, unit = "week", week_start = "Monday")) %>%  # we create a week column
  
  group_by(indicator, week_date) %>% 
  
  summarise(n=n(), .groups="drop") %>%   # we group and summarise to have the number of cases by date type and week
  
  drop_na(week_date)                     # we drop the cases with no data on dates




plot_date_delay <-   ggplot(data = dates_longer_week,
                            aes(x = week_date, 
                                y = n, 
                                color=indicator)) +
  
  geom_line(linewidth = 1.5) +
  
  scale_x_date(breaks = "2 weeks")+
  
  theme_bw() +
  
  theme(legend.position = "bottom", 
        axis.text = element_text(size=9),
        axis.title = element_blank(),
        axis.text.x = element_text(angle=90),
        legend.title = element_blank()) +
  labs(title="Mpox cases reported in 2022, by date of onset, diagnosis and notification.")

plot_date_delay



## Step 5.2: Compare case-based and aggregated data ------------------------

# Create a data frame with the overall number of cases reported through the aggregated flux

agg_data_country <- agg_data %>% 
  
  group_by(country) %>% 
  
  filter(date_rep == max(date_rep)) %>% # as we have cumulative data, we keep only the last week (after grouping by country)
  
  select(-date_rep, -week_date) %>%     # remove unnecessary columns
  
  mutate(source = "aggregated")         # we create this column to distinguish the numbers from the case-based flux


# Create a data frame with the overall number of cases reported through the case-based flux

cb_data_country <- cb_data %>%
  
  group_by(country) %>% 
  
  summarise(cases = n(), .groups = "drop") %>% 
  
  mutate(source = "case_based")       # we create this column to distinguish the numbers from the


# We append both data frames. Remember this is different from merging

total_data <- bind_rows(cb_data_country, agg_data_country)


# We create a graph to compare the cases reported in both sources

graph_comp <- ggplot(data = total_data,
                     aes(x = source, 
                         y = cases, 
                         fill = source)) +
  
  geom_col(position = "dodge") +            #position dodge puts bars one next to each other, instead of "stacked"
  
  facet_wrap(~ country, scales = "free_y") +  # this command gives us one graph per country. The argument scales is used to allow each y axis scales to adjust to the data
  
  scale_fill_viridis_d(
    labels = c("Aggregated", "Case-based")  # this function changes the colours, but with the argument "labels" we can change the text of each fill.
  ) +
  
  
  labs(
    title = "Number of cases of Mpox reported in 2022 according to source of data",
    fill = "Source",
    x = "",
    y = "Total number of cases"
  ) + 
  
  theme_bw() +
  
  theme(axis.text.x = element_blank(),      # we remove the text of the x axis because it is already present in the legend
        axis.ticks.x = element_blank())     # we also remove the ticks for aesthetic purposes

graph_comp



## Step 5.3: Heat plot with number of cases by country and week of  --------


hp_epox <- cb_data %>% #we first group the data by country and week of notification
  
  group_by(country, week_date) %>% 
  
  summarise(n_cases = n(), .groups = "drop") %>% 
  
  #now we can use the pipe to directly plot the resulting data from the grouping
  
  ggplot(aes(x = week_date,
             y = country,           #we want the countries to be in the y axis
             fill = n_cases)) +     #the colour of the tiles should depend on the number of cases
  
  geom_tile(colour = "black") +   #this is the outline colour of each tile
  
  scale_fill_gradient(            #here we define the colours we want to use in the gradient
    low = "lightgreen",
    high = "red") +
  
  scale_x_date(breaks = "2 weeks") +             #set the x axis labels to two week intervals
  
  labs(
    title= "Mpox cases by country and week of notification",
    fill = "Number of cases"                               
  ) +
  
  theme_bw() +
  
  theme(legend.position = "bottom",             #legend position to the bottom
        axis.text = element_text(size=9),       #define the font size of the axis
        axis.title = element_blank(),           #remove the titles of the x and y 
        axis.text.x = element_text(angle=90))   #rotate the x axis text

hp_epox
