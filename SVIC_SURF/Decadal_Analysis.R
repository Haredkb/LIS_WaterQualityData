### Decadal Analysis
### 
library(tidyverse)
library(lubridate)
library(viridis)
#https://stackoverflow.com/questions/53722679/function-to-remove-outliers-by-group-from-dataframe
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

WQ_Display <- readRDS("data/WQ_SVIC_Display.RDS")

#remove outliers
df <- WQ_Display %>%
  # dplyr::filter(!(CharacteristicName == "Nitrate" & Value >= 10)) %>%
  # dplyr::filter(!(CharacteristicName == "Ammonia and ammonium" & Value >= 10)) %>%
  # dplyr::filter(!(CharacteristicName == "Orthophosphate" & Value >= 10)) %>%
  # dplyr::filter(!(CharacteristicName == "Specific conductance" & Value >= 1500)) %>%
  mutate(decade = year(Date) - year(Date) %% 10)%>%
  dplyr::filter(Value > 0)

df_bf <- df %>%
  dplyr::filter(BaseflowConditions == "Baseflow")%>%
  mutate(label_fac = paste0(CharacteristicName, " (",Unit,")"))%>%
  group_by(site_id, CharacteristicName, decade)%>%
  mutate(val_avg = mean(Value))%>%
  distinct(val_avg, .keep_all=TRUE)%>%
  dplyr::filter(!(CharacteristicName == "Nitrate" & val_avg >= 10)) %>%
  dplyr::filter(!(CharacteristicName == "Ammonia and ammonium" & val_avg >= 2)) %>%
  dplyr::filter(!(CharacteristicName == "Orthophosphate" & val_avg >= 10))%>%
  dplyr::filter(!(CharacteristicName == "Specific conductance" & val_avg >= 5000)) 

# df_decade_quan <- df_bf %>%
#   group_by(CharacteristicName, decade)%>%
#   mutate(Value_out = remove_outliers(Value))


ggplot(df_bf)+
  geom_boxplot(aes(decade, val_avg, group = decade, fill = label_fac))+
  facet_wrap(~label_fac, scales = "free_y")+
  theme_bw(base_size = 12)+
  labs(x = "")+
  #scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  scale_y_continuous(
    # Features of the first axis
    name = paste0("Concentration")#bquote("Stream Temperature"~degree*C),
    # Add a second axis and specify its features
    #sec.axis = sec_axis(~.*coeff, name= paste0("Litter Input" , "\\\\n", "(kgC day-1)"))
  ) +
  scale_fill_viridis_d()+
  #facet_grid(rows = "regime")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),

  )+
  guides(color=guide_legend(ncol=1))
