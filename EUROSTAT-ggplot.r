library(eurostat)
library(dplyr)
library(ggplot2)

d <- get_eurostat(id = "prc_hicp_manr")

# Dodanie kolumny z nazwami krajow, filtrowanie po koszyku, usuniecie wartosci NULL, zawezenie przedzialu czasowego
df <- d %>%
  mutate(country = case_when(
    geo == "AT" ~ "Austria",
    geo == "BE" ~ "Belgium",
    geo == "BG" ~ "Bulgaria",
    geo == "HR" ~ "Croatia",
    geo == "CY" ~ "Cyprus",
    geo == "CZ" ~ "Czechia",
    geo == "DK" ~ "Denmark",
    geo == "EE" ~ "Estonia",
    geo == "FI" ~ "Finland",
    geo == "FR" ~ "France",
    geo == "DE" ~ "Germany",
    geo == "EL" ~ "Greece",
    geo == "HU" ~ "Hungary",
    geo == "IE" ~ "Ireland",
    geo == "IT" ~ "Italy",
    geo == "LV" ~ "Latvia",
    geo == "LT" ~ "Lithuania",
    geo == "LU" ~ "Luxembourg",
    geo == "MT" ~ "Malta",
    geo == "NL" ~ "Netherlands",
    geo == "PL" ~ "Poland",
    geo == "PT" ~ "Portugal",
    geo == "RO" ~ "Romania",
    geo == "SK" ~ "Slovakia",
    geo == "SI" ~ "Slovenia",
    geo == "ES" ~ "Spain",
    geo == "SE" ~ "Sweden")) %>%
  filter(coicop == "CP00", !is.na(country), between(time, as.Date("2000-01-01"), as.Date("2022-10-01"))) %>%
  group_by(time, country) %>%
  summarize(values = sum(values))

# Rysowanie wykresu
ggplot(df, aes(x=time, y=values, col=country)) +
  geom_line() +
  ggtitle("Zharmonizowany indeks cen 2000-02 do 2022-10")

# Hierarchiczne grupowanie, zmiana formatu ramki danych
df_wide <- reshape(as.data.frame(df), idvar="country", timevar="time", direction="wide")

# Obliczanie odleglosci za pomoca metody Minkowskiego
mink <- hclust(dist(df_wide, method="minkowski", p=1.5))
mink$labels <- df_wide$country
plot(mink, xlab="", ylab="", main="Clustering countries based on HICP")
rect.hclust(mink, 4)
