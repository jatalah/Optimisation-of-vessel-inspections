library(tidyverse)
library(readxl)
library(janitor)


caw17sum <- read_excel("data/Cawthron 2017/summary4_DM.xlsx")

names(caw17sum)

tabyl(caw17sum, MPI_RS)
tabyl(caw17sum, MPI_RS1)
tabyl(caw17sum, Type)


caw17sum_risk <-
  caw17sum %>%
  mutate(
    Type = if_else(Name == "SOOCHOW", "Container", Type),
    portVisitClass = if_else(
      Ports_12m >= 35 & Type == c("Bulk",  "General Cargo"),
      1,
      if_else(
        Ports_12m >= 30 & Type == "Tanker",
        1,
        if_else(
          Tonnage_DWT <= 5000 & Type == "Passenger",
          1,
          if_else(
            Ports_12m >= 50 & Type == "Container",
            1,
            if_else(
              Ports_12m >= 5 & Type == "Fishing",
              1,
              if_else(Ports_12m >= 45 & Type == "RoRo", 1, 0)
            )
          )
        )
      )
    ),
    layupsClass = if_else(Max_LU_12m > 30,
                          1,
                          if_else(`Laid-up_more_10d` > 1, 1, 0)),
    AFclass = if_else(
      Time_since_AF > 548 & Type == "Passenger",
      1,
      if_else(
        Time_since_AF > 250 & Type == "Fishing",
        2,
        if_else(Time_since_AF > 1369, 1, 0)
      )
    ),
    C_class = (AFclass * 4) + layupsClass * 2 + portVisitClass,
    M_class  = AFclass + layupsClass + portVisitClass
  )



tabyl(caw17sum_risk, M_class)
tabyl(caw17sum_risk, C_class)


caw17sum_risk %>% select(
  Type,
  Ports_12m,
  Tonnage_DWT,
  Time_since_AF,
  `Laid-up_more_10d`,
  Max_LU_12m,
  tail(names(.), 5)
) %>% View

ggplot(caw17sum_risk, aes(MPI_RS, M_class)) +
  geom_point(position = position_jitter(width = 0.1))

tabyl(caw17sum_risk, CRMS_Sh_fail )
