#import data

birth_2016 <- read_csv("data/birth/RestrictedOhioLiveBirths_2016.csv")

birth_total <- birth_2016 %>%
  filter(`Residence County` == "Warren") %>%
  # select(`Birth Year`) %>%
  summarise(Total = n()) %>%
  pull()