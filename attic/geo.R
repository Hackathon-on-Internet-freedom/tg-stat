library(tidyverse)

df <- read_csv("/tmp/uniq-cities.csv") %>%
  mutate(lev1 = str_sub(okato, 0,2),
         lev2 = str_sub(okato, 3,5),
         lev3 = str_sub(okato, 6,8),
         lev4 = str_sub(okato, 9,11),
         name = settlementLabel,
         settlementLabel = NULL)

df1 <- df %>%
  filter(lev2 == "000" & lev3 == "000" & lev4 == "") %>%
  select(okato, name, lat, lon, lev1)

df2 <- df %>%
  filter(lev3 == "000" & lev4 == "") %>%
  select(okato, name, lat, lon, lev1, lev2)

df3 <- df %>%
  filter(lev4 == "") %>%
  select(okato, name, lat, lon, lev1, lev2, lev3)

df4 <- df %>%
  filter(lev4 != "") %>%
  select(okato, name, lat, lon, lev1, lev2, lev3, lev4)

md <- df4 %>%
  left_join(df3, by=c("lev1","lev2","lev3"), suffix=c(".4",".3")) %>%
  left_join(df2, by=c("lev1","lev2"), suffix=c(".3",".2")) %>%
  left_join(df1, by=c("lev1"), suffix=c(".2",".1")) %>%
  mutate(okato = okato.4) %>%
  select(okato, name.4,lat.4,lon.4, name.3,lat.3,lon.3, name.2,lat.2,lon.2, name.1,lat.1,lon.1) %>%
  arrange(okato)

write_csv(md, "/tmp/levels.csv")
