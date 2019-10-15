library(readr)
library(dplyr)

totals = data.frame()
per100 = data.frame()
for(year in 2015:2019) {
  totals = rbind(totals, read_csv(paste0("nba_totals/nba_", year, "_totals.csv")) %>% 
                   mutate(year = year,
                          fp = PTS + TRB + AST * 2 + STL * 3,
                          fp.game = fp/G,
                          fp.min = fp/MP
                          ) %>%
                   select(-Rk, -`FG%`, -`3P%`, -`2P%`, -`FT%`) %>%
                   select(Player, year, everything()) %>%
                   arrange(Player, year, desc(Tm)) %>%
                   select(-Tm) %>%
                   distinct(Player, year, .keep_all = T)
                   )
  per100 = rbind(per100, read_csv(paste0("nba_per100/nba_", year, "_per100.csv")) %>% 
                   mutate(year = year, MP = round(MP/G, 1)) %>%
                   select(-Rk, -Pos, -X30, -Age, -G, -GS, -ORtg, -DRtg) %>%
                   select(Player, year, everything()) %>%
                   arrange(Player, year, desc(Tm)) %>%
                   select(-Tm) %>%
                   distinct(Player, year, .keep_all = T)
  )
}

colnames(totals)[7:(length(colnames(totals)) - 2)] = paste(colnames(totals)[7:(length(colnames(totals)) - 2)], "total", sep = "_")
colnames(per100)[3:length(colnames(per100))] = paste(colnames(per100)[3:length(colnames(per100))], "pergame", sep = "_")

df = per100 %>%
  left_join(., totals, by = c("year", "Player")) %>%
  mutate(year_plus_1 = year + 1) %>%
  select(Player, year, Pos, Age, G, GS, everything())

df[is.na(df)] = 0

df_yy = df %>%
  inner_join(., df, by = c("Player", "year_plus_1"="year")) %>%
  setNames(gsub("\\.x", "", names(.))) %>%
  setNames(gsub("\\.y", "_yplus1", names(.))) %>%
  ungroup()

full_yy = df_yy %>%
  select_if(is.numeric) %>%
  select(-year, -year_plus_1_yplus1, -year_plus_1, -Age, -Age_yplus1)

cor_yy = full_yy %>% cor() %>% round(3)

test_df = cbind(df_yy[,c(1:51)], df_yy[,'fp_total_yplus1'])

model1 = step(lm(fp_total_yplus1 ~ . -Player -Pos -year -year_plus_1 -Age -`eFG%_total`, data = test_df), direction = "both")
summary(model1)

df_2019 = df %>% filter(year == 2019) %>% mutate(pred2020 = predict(model1, .))
