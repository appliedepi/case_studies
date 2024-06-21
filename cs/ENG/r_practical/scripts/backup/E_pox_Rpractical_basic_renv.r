############ "E-pox: Importing, cleaning and describing data"

install.packages("renv")
#renv::snapshot()
renv::restore()


library(tidyverse)
library(lubridate)
library(janitor)
library(glue)
library(knitr)


# 1. Importing

## Case-based data

CB_Data0<-read.csv("input_data/E_pox_case_based_data.csv")

skimr::skim(CB_Data0)

# Importing another file type (see ppt)
CB_Data0_json<-jsonlite::fromJSON("input_data/E_pox_case_based_data.json")

CB_Data0_xlsx<-readxl::read_excel(path = "input_data/E_pox_case_based_data.xlsx")


## Aggregated data

Agg_Data0<-read.csv("input_data/E_pox_aggregated_data.csv")

skimr::skim(CB_Data0)

Agg_Data0_json<-jsonlite::fromJSON("input_data/E_pox_aggregated_data.json")


Agg_Data0_xlsx<-readxl::read_excel(path = "input_data/E_pox_aggregated_data.xlsx")

# Remove the files we do not need 
# within rm() we ask for the objects containing 
# the pattern "json" or "xlsx" to be removed from the environment
rm(list = ls(pattern = "json|xlsx"))

# 2. Cleaning

## Case-based data

# Start working using the CB_Data0 dataset

str(CB_Data0)
summary(CB_Data0)

head(CB_Data0)
glimpse(CB_Data0)

# Working on the date variables

CB_Data <- CB_Data0 %>% 
  # convert one date variable
  mutate(DateOfNotification=ymd(DateOfNotification)) %>% 
  # Convert the date variables to date
  mutate(across(starts_with("Date"),
                ~ ymd(.x))) %>% 
  # Add a variable with the week
  mutate(WeekDate=floor_date(DateOfNotification, unit="week", week_start="Monday")) %>% 
  mutate(
    Gender = case_when(
      Gender == "F" ~ "Female",
      Gender == "M" ~ "Male",
      Gender == "O" ~ "Other",
      Gender == "UNK" ~ "Unknown",
      .default = Gender
    )
  )


head(CB_Data)
skimr::skim(CB_Data)
glimpse(CB_Data)

plot(CB_Data$DateOfNotification)
hist(CB_Data$DateOfDiagnosis, breaks="weeks")
hist(CB_Data$DateOfOnset, breaks="weeks")

skimr::skim(CB_Data)

# Cross-tabulate the country info and the date of notification
janitor::tabyl(dat = CB_Data, 
               WeekDate, 
               Country)

# Cross-tabulate the country info and the gender variable
janitor::tabyl(dat = CB_Data, 
               Gender, 
               Country)



## Aggregated data

# Working on the Agg_Data0 dataset

str(Agg_Data0)
names(Agg_Data0)

summary(Agg_Data0)

Agg_Data <- Agg_Data0 %>% 
  # Convert the date variables to date
  mutate(across(.cols= starts_with("Date"),
                .fns = ~ ymd(.x))) %>% 
  # Convert character columns to factors:
  mutate(across(.cols = where(is.character), 
                .fns = ~ as.factor(.x))) %>% 
  # Add a variable with the week
  mutate(WeekDate=floor_date(DateRep, unit="week", week_start="Monday")) 

str(Agg_Data)

summary(Agg_Data)



# 3. Basic descriptives of the case-based data


# nrow() used below gives the total number of rows in the dataset
# Getting the total number of cases 

nrow(CB_Data)

## Summary of the characteristics of the cases (tables and graphs)

### Summary by country

CB_table <- CB_Data %>%
  # Select columns starting with "Date"
  select(Country) %>%
  # Create summary table (attention to handling of missing values)
  gtsummary::tbl_summary(missing = "ifany") 
CB_table


### Time

# Checking the different variables

CB_table1 <- CB_Data %>%
  # Select columns starting with "Date"
  select(starts_with("Date")) %>%
  # Create summary table (attention to handling of missing values)
  gtsummary::tbl_summary(missing = "ifany") 

CB_table1


#### Epicurves (overall and by country)

# First we can nake an object to summarise the data we want to
# show on the epicurve
EpiCurve_object<- CB_Data %>% 
  group_by(WeekDate) %>% 
  summarise(n=n(), .groups = "drop") 

head(EpiCurve_object)

EpiCurve <- EpiCurve_object %>% 
  ggplot() +
  geom_col(aes(x = WeekDate, y = n), fill="darkgreen", color="white", alpha=0.8) +
  scale_fill_viridis_d() +
  scale_x_date(breaks = "2 weeks")+
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text = element_text(size=9),
        axis.title = element_blank(),
        axis.text.x = element_text(angle=90),
        legend.title = element_blank()) +
  labs(title="E-pox cases reported in 2022")

EpiCurve


# The dataset to be used for the epicurve (and in general 
# in ggplot()) can be created right before the plot and it 
# can be passed directly to the ggplot()

EpiCurve_byCountry<-CB_Data %>% 
  group_by(Country, WeekDate) %>% 
  summarise(n=n(), .groups = "drop") %>%  
  ungroup() %>% 
  ggplot(.) +
  geom_col(aes(x = WeekDate, y = n, fill = Country), color="white", alpha=0.8) +
  scale_fill_viridis_d() +
  scale_x_date(breaks = "2 weeks")+
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text = element_text(size=9),
        axis.title = element_blank(),
        axis.text.x = element_text(angle=90),
        legend.title = element_blank()) +
  labs(title="E-pox cases reported by country in 2022")

EpiCurve_byCountry

#### Plot the dates of onset, diagnosis, and notification

#In the plots below, we use the dates of onset, diagnosis and notification and put them together to see whether there are difference and visualize the information to check if there are large delays. First, we try with dates and the plot is a messy (not easy to interpret.

plot_dates_object <- CB_Data %>%
  # use the variables of the dates and make a longer dataset
  # In the pivot_longer() command we select the columns which 
  # we want to expand in long format and transform the dataset 
  pivot_longer(
    # all columns starting with "DateOf..." will be taken 
    cols=starts_with("DateOf"), 
    # the names of the variables will be placed in a single column called "Indicator"
    names_to = "Indicator", 
    # the values (which are dates in this case) will be placed in a 
    # column called "Date
    values_to = "Date") %>%
  # group by date and indicator (type of date, i.e., onset, diagnosis or notification) 
  group_by(Indicator, Date) %>% 
  summarise(n=n(), .groups="drop") %>% 
  # we add a filter to remove any rows where a date value might not exist
  filter(!is.na(Date))

# then we pass the object to the ggplot()
plot_dates <- plot_dates_object %>% 
  ggplot() +
  geom_line(aes(x = Date, y = n, color=Indicator)) +
  scale_x_date(breaks = "2 weeks")+
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text = element_text(size=9),
        axis.title = element_blank(),
        axis.text.x = element_text(angle=90),
        legend.title = element_blank()) +
  labs(title="E-pox cases reported in 2022, by date of onset, diagnosis and notification.")

plot_dates

# Then, we try to plot by week and there is some improvement in the visualization.

plot_date_w<- CB_Data %>%
  # use the variables of the dates and make a longer dataset
  pivot_longer(cols=starts_with("DateOf"), names_to = "Indicator", values_to = "Date") %>%
  mutate(Date=floor_date(Date, unit="week", week_start="Monday")) %>% 
  # group by date and indicator (type of date, i.e., onset, diagnosis or notification) 
  group_by(Indicator, Date) %>% 
  summarise(n=n(), .groups="drop") %>% 
  filter(!is.na(Date)) %>% 
  ggplot() +
  geom_line(aes(x = Date, y = n, color=Indicator)) +
  scale_x_date(breaks = "2 weeks")+
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text = element_text(size=9),
        axis.title = element_blank(),
        axis.text.x = element_text(angle=90),
        legend.title = element_blank()) +
  labs(title="E-pox cases reported in 2022, by date of onset, diagnosis and notification.")

plot_date_w


# We can create the same graph by country.


plot_date_w_country<- CB_Data %>%
  # use the variables of the dates and make a longer dataset
  pivot_longer(cols=starts_with("DateOf"), names_to = "Indicator", values_to = "Date") %>%
  mutate(Date=floor_date(Date, unit="week", week_start="Monday")) %>% 
  # group by date and indicator (type of date, i.e., onset, diagnosis or notification) 
  group_by(Country, Indicator, Date) %>% 
  summarise(n=n(), .groups="drop") %>% 
  filter(!is.na(Date)) %>% 
  ggplot() +
  geom_line(aes(x = Date, y = n, color=Indicator)) +
  # in this line we indicate that a graph should be created 
  # by country and that the axis should have different scales which
  # are adjusted by the range of values in each country
  facet_wrap(~Country, ncol = 1, scales = "free")+
  scale_x_date(breaks = "2 weeks")+
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text = element_text(size=9),
        axis.title = element_blank(),
        axis.text.x = element_text(angle=90),
        legend.title = element_blank()) +
  labs(title="E-pox cases reported in 2022, by date of onset, diagnosis and notification.")

plot_date_w_country

#### Graph on cases reported by day and country

hm_CB_Data<-CB_Data %>% 
  distinct(DateOfNotification, Country) %>% 
  mutate(n=1) %>%  
  ungroup() %>% 
  ggplot(.) +
  geom_tile(aes(x=DateOfNotification, y=as.factor(Country), fill = as.factor(Country)), alpha=0.7) +
  geom_hline(yintercept = seq(.5, nlevels(as.factor(CB_Data$Country)), 1), linewidth = .2) +
  geom_vline(xintercept = max(CB_Data$DateOfNotification, na.rm=T)-as.difftime(tim = 21, units = "days"), linetype=2) +
  geom_vline(xintercept = max(CB_Data$DateOfNotification, na.rm=T)-as.difftime(tim = 30, units = "days"), linetype=3) +
  geom_vline(xintercept = max(CB_Data$DateOfNotification, na.rm=T), linetype=1) +
  scale_fill_hue(l=4) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text = element_text(size=9),
        axis.title = element_blank()) +
  labs(caption="Vertical lines: 21 and 30 days before last case notification.",
       title="Date of reporting of E-pox cases by country.")

hm_CB_Data



### Demographics

CB_Data <- CB_Data %>% 
  mutate(Age_cat=cut(Age, 
                     breaks = c(0, 14, 17, 30, 40, 50, 60, 999), 
                     include.lowest = T))

CB_demo <- CB_Data %>%
  # Select demographics and other columns
  select("Age_cat") %>%
  # Create summary table (attention to handling of missing values)
  gtsummary::tbl_summary(missing = "ifany")

CB_demo


### Clinical description

clin_desc_table <- CB_Data %>%
  # Select demographics and other columns
  select("ClinicalSymptoms") %>%
  ## Create summary table (attention to handling of missing values)
  gtsummary::tbl_summary(missing = "ifany") 

clin_desc_table

clinical_desc <- CB_Data %>%  
  filter(ClinicalSymptoms!="") %>% 
  # Group by symptoms
  group_by(ClinicalSymptoms) %>% 
  # Count for each symptom:
  summarise(count = n(), .groups="drop") %>% 
  mutate(prop=(count/sum(count))*100) %>% 
  # Create plot:
  ggplot(aes(
    # ordering the symptoms by count
    x = reorder(ClinicalSymptoms, desc(prop), decreasing = TRUE), 
    y = prop)) +
  # Display bars as proportions
  geom_bar(stat = "identity") +
  #x axis label
  xlab("Symptoms") +
  #y axis label
  ylab("% E-pox cases") +
  # flip the axis
  coord_flip() +
  theme_minimal()

# Print plot:
clinical_desc

### Outcome, HIV status
Outcome_HIV <- CB_Data %>%
  # Select columns
  select("Outcome", "HIVStatus") %>%
  mutate(Outcome=case_when(Outcome=="UNK"~"Unknown",
                           Outcome=="A"~"Alive",
                           .default = Outcome))  %>%
  mutate(HIVStatus=case_when(HIVStatus=="POS"~"Positive",
                             HIVStatus=="NEG"~"Negative",
                             .default = HIVStatus)) %>% 
  # Create summary table (attention to handling of missing values)
  gtsummary::tbl_summary(by = Outcome) %>%
  gtsummary::add_overall() 

Outcome_HIV


### Sexual orientation

sexorient <- CB_Data %>%
  filter(!is.na(SexualOrientation)) %>% 
  # Select columns
  select("SexualOrientation") %>%
  gtsummary::tbl_summary() 

sexorient <- CB_Data %>%
  filter(!is.na(SexualOrientation)) %>% 
  # Select columns
  select("SexualOrientation") %>%
  mutate(SexualOrientation=case_when(SexualOrientation=="BISEXUAL"~"Bisexual",
                                     SexualOrientation=="HETERO"~"Heterosexual",
                                     SexualOrientation=="MSM"~"MSM/homo or bisexual male",
                                     SexualOrientation=="UNK"~"Unknown or undetermined",
                                     .default=SexualOrientation)) %>% 
  gtsummary::tbl_summary() 


sexorient

# Optional - Age and gender pyramid


# Creating the age and gender pyramid (first filtering out non M or F rows)
agegender_pyramid <- CB_Data %>%
  filter(Gender=="Female"|Gender=="Male" & !is.na(Age_cat)) %>% 
  # Create age gender pyramid
  apyramid::age_pyramid(
    # Specify column containing age groups
    age_group = "Age_cat",
    # Splitting by Gender
    split_by = "Gender", 
    # Don't show midpoint on the graph:
    show_midpoint = FALSE) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x="")


agegender_pyramid
