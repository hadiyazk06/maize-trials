library(readxl)
dfn <- read_excel("//home//schnablelab//Documents//N trials//data files//Ndata2.0.xlsx", sheet = "RD", skip = 2, na=".")
class(dfn)
library(dplyr)
names(dfn)
dfn <- dfn%>% slice(-(1:2))

dfn<- dfn %>% mutate (StudyYear = as.factor(StudyYear))

# cleaning the data 
n_clean <- dfn %>% filter(!is.na(yield), !is.na(n_rate), yield > 0, !(StudyID_Yr %in% c(216_2017, 1713_2022, 128_2016)))
         
unique(n_clean$StudyID_Yr)

 #after failing to remove these study's; try again 
n_clean <- dfn %>%
  mutate(StudyID_Yr = as.character(StudyID_Yr)) %>%  # ensure text
  filter(
    !is.na(yield),
    !is.na(n_rate),
    yield > 0,
    !(trimws(StudyID_Yr) %in% c("216_2017", "1713_2022", "128_2016"))
  )

                   
# Count unique N rates per StudyID
Study <- n_clean %>%
  group_by(StudyID_Yr) %>%
  summarise(n_Nrates = n_distinct(n_rate), .groups = "drop")

library(ggplot2)
total_trials <-ggplot(Study, aes(x= StudyYear, Y= StudyID_Yr, fill= StudyID_Yr)) + geom_bar(width = 0.6) + labs(title = "Total Number of Trials", x="Study Year", y="Number of Trials") + theme_minimal() +  theme(
  panel.background = element_rect(fill = "white", color = NA),
  plot.background = element_rect(fill = "white", color = NA),
  legend.background = element_rect(fill = "white", color = NA),
  legend.box.background = element_rect(fill = "white", color = NA),
  panel.grid = element_blank())   # optional: remove grid lines


ggsave(ggsave("/home/schnablelab/Documents/N trials/scatterplots/total_no_of_trials.png", plot= total_trials, device = "png", dpi = 400,
              width = 12, height = 8, bg = "white"))


# Filter to keep only trials with 3 or more N rates
nit_filtered <- Study %>%
  filter(n_Nrates >= 3)

#Add column counting number of unique N rates per StudyID_Yr
nit_filtered <- n_clean %>%
  group_by(StudyID_Yr) %>%
  mutate(n_Nrates = n_distinct(n_rate)) %>%
  ungroup() %>%
  filter(n_Nrates >= 3)

colnames(nit_filtered)

nit_summary <- nit_filtered %>%
  group_by(StudyID_Yr, n_rate) 

n_trials <-ggplot(nit_summary, aes(x= StudyYear, Y= StudyID_Yr, fill= StudyID_Yr)) + geom_bar(width = 0.6) + labs(title = "Number of Trials", x="Study Year", y="Number of Trials") + theme_minimal() +theme(
  panel.background = element_rect(fill = "white", color = NA),  # inside
  plot.background = element_rect(fill = "white", color = NA),   # outside
  legend.background = element_rect(fill = "white", color = NA),
  legend.box.background = element_rect(fill = "white", color = NA),
  panel.grid = element_blank()
)
n_trials


ggsave(ggsave("/home/schnablelab/Documents/N trials/scatterplots/no_of_trials.png", plot= n_trials, device = "png", dpi = 400,
              width = 12, height = 8, bg = "white"))


