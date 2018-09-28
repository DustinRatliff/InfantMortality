#import data

birth <- read_csv("data/birth/RestrictedOhioLiveBirths_Export.csv")

birth_total <- birth %>%
  filter(`Birth Year` == 2016) %>%
  filter(`Residence County` == "Warren") %>%
  # select(`Birth Year`) %>%
  summarise(Total = n()) %>%
  pull()

preterm <- birth %>%
  filter(`Residence County` == "Warren") %>%
  select(`Birth Year`, Gestation) %>%
  mutate(term = ifelse(Gestation < 23, "Less than 23 Weeks Gestation Birth", 
                          ifelse(Gestation >= 23 & Gestation < 32, "Very-Preterm Birth", 
                                 ifelse(Gestation >=23 & Gestation <37, "Preterm Birth", "Normal Term Birth")))) %>%

  mutate(term = factor(term, levels = c("Less than 23 Weeks Gestation Birth",
                                        "Very-Preterm Birth",
                                        "Preterm Birth",
                                        "Normal Term Birth")))   %>%
  group_by(`Birth Year`, term) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count)) %>%
  mutate(pct = freq * 100)

