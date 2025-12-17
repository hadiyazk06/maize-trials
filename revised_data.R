library(readxl)
getwd()
list.files()

read_excel("Ndata2.0")
dfn <- read_excel("//home//schnablelab//Documents//N trials//data files//Ndata2.0.xlsx", sheet = "RD", skip = 2, na=".")

class(dfn)
library(dplyr)
dfn <- dfn%>% slice(-(1:2))

head(dfn) #see the first few rows
nrow(dfn)
class(dfn)
# Just to confirm sheet names
library(readxl)

excel_sheets("//home//schnablelab//Documents//N trials//data files//Ndata2.0.xlsx")

names(dfn)
#rename columns
dfn <- dfn %>% rename(year= "StudyYear", moisture_pct= "%", MNR = "Marginal Net Return")
names(dfn)
#year as factor 
dfn <- dfn %>% mutate(year= as.factor(year))

#selecting columns that I need 
nit <- dfn %>% select(n_rate, Rep, yield, StudyID, StudyID_Yr, year, MNR)
names(nit)

# cleaning the data 
nit_clean <- nit %>% filter(!is.na(yield), !is.na(n_rate), yield > 0, !(StudyID %in% c(216, 1713, 128)))

#looking for duplicates 
nit_clean %>% filter(duplicated(.))

# Count unique N rates per StudyID
Study <- nit_clean %>%
  group_by(StudyID) %>%
  summarise(n_Nrates = n_distinct(n_rate), .groups = "drop")

Study
# Filter dataset to keep only trials with 3 or more N rates
nit_filtered <- nit_clean %>%
  inner_join(Study %>% filter(n_Nrates >= 3), by = "StudyID")


#summarize
nit_summary <- nit_filtered %>%
  group_by(year, StudyID, StudyID_Yr, n_rate) %>%
  summarise(mean_yield = mean(yield, na.rm = TRUE), mean_MNR = mean(MNR, na.rm=TRUE),
            .groups = "drop")


str(nit_clean)
 
#which trials were filtered or kept 
dropped <- Study %>% filter(n_Nrates < 3)
kept    <- Study %>% filter(n_Nrates >= 3)


#plotting with clean data
ggplot(nit_summary, aes(x=n_rate, y= mean_yield, color=factor(StudyID_Yr))) + geom_point() + labs(title="Mean Yield Response to Nitrogen Rate", x= "Nitrogen rate(lb/A)", y = "Yield(bu/A)", color = "Trials", Shape =  "Replication") + theme_minimal()


#plotting with line 

library(ggplot2)

ggplot(nit_summary, aes(x = n_rate, y = mean_yield, color = factor(StudyID_Yr))) +
  geom_point() +
  geom_line(aes(group = StudyID)) +
  facet_wrap(~year) +
  labs(
    title = "Yield response to N rate",
    x = "N rate (lb/A)",
    y = "Mean Yield (bu/A)",
    color = "Study ID_Year"
  ) +
  theme_minimal()

ggsave("/home/schnablelab/Documents/N trials/scatterplots/mean_yieldnew.png",device = "png", dpi = 400,
       width = 12, height = 8, bg = "white")

#plotting the graph again 
library(ggplot2)

ggplot(nit_summary, aes(x = n_rate, y = mean_yield, color = factor(StudyID_Yr))) +
  geom_point() +
  geom_line(aes(group = StudyID)) +
  facet_wrap(~StudyID) +
  labs(
    title = "mean yield response to N rate",
    x = "N rate (lb/A)",
    y = "mean yield (bu/A)",
    color = "Study ID_Yr"
  ) +
  theme_minimal()

by_study
ggsave("/home/schnablelab/Documents/N trials/scatterplots/mean_yield_Study.png", device = "png", dpi = 400,
       width = 12, height = 8, bg = "white")


#separate plots by trial 


trials <- unique(nit_summary$StudyID)

for (tr in trials) {
  p <- nit_summary %>%
    filter(StudyID == tr) %>%
    ggplot(aes(x = n_rate, y = mean_yield, color = factor(year))) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = paste("Mean Yield at each N rate", tr),
         x = "Nitrogen rate (lb/A)",
         y = "Mean yield (bu/A)",
         color = "Year") +
    theme_minimal()
  
  print(p)
  ggsave(
    filename = paste0("/home/schnablelab/Documents/N trials/scatterplots/perStudyID_", tr, ".png"),
    plot = p, width = 10, height = 7, dpi = 400, bg="white"
  )
}
trials

###################################################################################################
#finding EONR 
##################################################################################################

library(dplyr)

EONR_table <- nit_summary %>%
  group_by(StudyID) %>%
  slice_max(mean_MNR, n = 1, with_ties = FALSE) %>%
  select(StudyID, year, n_rate, mean_MNR, mean_yield) %>%
  rename(EONR = n_rate, MaxMNR = mean_MNR, Yield_at_EONR = mean_yield)

# EONR with only StudyID_Yr

EONR_summary <- nit_summary %>% group_by (StudyID_Yr) %>% slice_max(mean_MNR, n = 1, with_ties = FALSE) %>% select(StudyID_Yr, EONR = n_rate)


#Merge ENOR back to nit_summary 

nit_eonr <- left_join(nit_summary, EONR_summary, by = "StudyID_Yr")

#Plot Yield response combined 

EONR_IDYr <- ggplot(nit_eonr,aes(x= n_rate, y= mean_yield, color = factor(StudyID_Yr))) +
  geom_point(size= 2)+
geom_line(aes(group= StudyID_Yr)) +
  geom_vline(aes(xintercept = EONR, color = factor(StudyID_Yr)), linetype = "dashed") +
  labs(title = "Yield Response and EONR by StudyID",
       subtitle = "Dashed line = Economic Optimum N Rate (EONR)",
       x = "N rate (lb N/ac)",
       y = "Mean yield (bu/ac)",
       color = "Study ID_Yr") + theme_minimal()


EONR_IDYr

#Plot Yield response by StudyID_yr 

EONR_year <- ggplot(nit_eonr,aes(x= n_rate, y= mean_yield, color = factor(StudyID_Yr))) +
  geom_point(size= 2)+
  geom_line(aes(group= StudyID_Yr)) +
  geom_vline(aes(xintercept = EONR, color = factor(StudyID_Yr)), linetype = "dashed") + facet_wrap(~StudyID_Yr, scales="fixed") +
  labs(title = "Yield Response and EONR by StudyID",
       subtitle = "Dashed line = Economic Optimum N Rate (EONR)",
       x = "N rate (lb N/ac)",
       y = "Mean yield (bu/ac)",
       color = "Study ID_Yr") + theme_minimal()
EONR_year

ggsave(
  filename = paste0("/home/schnablelab/Documents/N trials/graphs_output_final/", "EONR.png"),
  plot = EONR_year, width = 10, height = 7, dpi = 400, bg="white"
)


nit_summary %>%
  group_by(StudyID_Yr) %>%
  summarise(min_rate = min(n_rate), max_rate = max(n_rate), 
            n_levels = n_distinct(n_rate),
            MNR_range = range(mean_MNR, na.rm = TRUE))


names(nit_summary)


#don't use 

EONR_Ndata<- ggplot(nit_summary, aes(n_rate)) + geom_line(aes(y = mean_yield, color="Mean Yield (bu/ac)")) +
  geom_line(aes(y= mean_MNR/10, color = "Mean MNR (scaled)"), linetype = "dashed") +
  geom_vline(data = EONR_table, aes(xintercept = EONR), color = "red" , linetype = "dotted") +
  facet_wrap(~year) +
  scale_y_continuous(name = "Mean Yield (bu/ac)", sec.axis = sec_axis (~.*10, name = "Mean MNR ($/ac)")) +
  labs(title= "Means Yield and Mean Marginal Net Return vs N Rate", 
       x = "Nitrogen Rate (lb N/ac)", color="") + 
  theme_minimal()

EONR_Ndata
ggsave(
  filename = paste0("/home/schnablelab/Documents/N trials/graphs_output_final/", "EONR.png"),
  plot = EONR_Ndata, width = 10, height = 7, dpi = 400, bg="white"
)
