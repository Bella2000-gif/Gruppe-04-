#install.packages("mosaic")
library(mosaic)
#install.packages("readxl")
library(readxl)
data<- read_excel("data_SoSe2024_BFM_Gruppe_04 (4).xlsx")
dimensions <- dim(data)
num_variables <- ncol(data)
str(data)
names(data)
data$schlaf_weeks <- as.numeric(data$schlaf_weeks)
data$Vertraglichkeit<- as.numeric(data$Vertraglichkeit)
data$Gewissenhaftigkeit<- as.numeric(data$Gewissenhaftigkeit)
inspect(data)
summary(data)

Arbeitspaket 3 sozio demographische Daten

tally(~geschlecht, data=data)
gf_bar(~geschlecht, data=data)
tally( ~ geschlecht, data=data, format="proportion")
gf_bar( ~ geschlecht, data=data,
        color="black", fill="red", # rote Säulen mit schwarzem Rand
        xlab="Geschlecht", # Beschriftung der x-Achse
        ylab="Anzahl") + # Beschriftung der y-Achse
  theme_bw()

favstats(~alter, data=data)
bwplot(~alter, data=data)
tally(~beschaftigung, data=data)
gf_bar(~beschaftigung, data=data)

tally(~arbeitszeit, data=data)
gf_bar(~arbeitszeit, data=data)

tally(~berufsausbildung, data=data)
gf_bar(~berufsausbildung, data=data)

tally(~bildung, data=data)
gf_bar(~bildung, data=data)



Aufgabenpaket 4:

Kategoriale Variabeln:

tally(~geschlecht, data=data)
tally(~geschlecht, format = "proportion", data=data)
gf_bar(~geschlecht, data=data)
ggsave("saeulendiagrammgeschlecht.pdf")

Kreisdiagramm

data <- data.frame(
  geschlecht = c(rep("männlich", 43), rep("weiblich", 157))
)
library(ggplot2)
library(dplyr)

geschlecht_data <- data %>%
  group_by(geschlecht) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count) * 100,
         ypos = cumsum(prop) - 0.5 * prop)


ggplot(geschlecht_data, aes(x = "", y = prop, fill = geschlecht)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  geom_text(aes(y = ypos, label = paste0(round(prop, 1), "%")), color = "white") +
  labs(title = "Verteilung nach Geschlecht")
ggsave("kreisdiagramm_geschlecht.png", width = 8, height = 6)



tally(~bildung, data=data)
tally(~bildung, format = "proportion", data=data)
gf_bar(~bildung, data=data)
ggsave("saeulendiagrammbildung.pdf")

tally(~berufsausbildung, data=data)
tally(~berufsausbildung, format = "proportion", data=data)
gf_bar(~berufsausbildung, data=data)
ggsave("saeulendiagrammberufsausbildung.pdf")

tally(~arbeitszeit, data=data)
tally(~arbeitszeit, format = "proportion", data=data)
gf_bar(~arbeitszeit, data=data)
ggsave("saeulendiagrammarbeitszeit.pdf")

tally(~beschaftigung, data=data)
tally(~beschaftigung, format = "proportion", data=data)
gf_bar(~beschaftigung, data=data)
ggsave("saeulendiagrammbeschaftigung.pdf")

tally(~biking, data=data)
tally(~biking, format = "proportion", data=data)
gf_bar(~biking, data=data)
ggsave("saeulendiagrammbiking.pdf")


tally(~dancing, data=data)
tally(~dancing, format = "proportion", data=data)
gf_bar(~dancing, data=data)
ggsave("saeulendiagrammdancing.pdf")

tally(~sport_haufigkeit, data=data)
tally(~sport_haufigkeit, format = "proportion", data=data)
gf_bar(~sport_haufigkeit, data=data)
ggsave("saeulendiagrammsport_haufigkeit.pdf")


tally(~triathlon, data=data)
tally(~triathlon, format = "proportion", data=data)
gf_bar(~triathlon, data=data)
ggsave("saeulendiagrammtriathlon.pdf")

tally(~kaffeetrinker, data=data)
tally(~kaffeetrinker, format = "proportion", data=data)
gf_bar(~kaffeetrinker, data=data)
ggsave("saeulendiagrammkaffeetrinker.pdf")

tally(~yoga, data=data)
tally(~yoga, format = "proportion", data=data)
gf_bar(~yoga, data=data)
ggsave("saeulendiagrammyoga.pdf")

tally(~rudern, data=data)
tally(~rudern, format = "proportion", data=data)
gf_bar(~rudern, data=data)
ggsave("saeulendiagrammrudern.pdf")

tally(~cardio, data=data)
tally(~cardio, format = "proportion", data=data)
gf_bar(~cardio, data=data)
ggsave("saeulendiagrammcardio.pdf")

tally(~ kraftsport, data=data)
tally(~kraftsport, format = "proportion", data=data)
gf_bar(~ kraftsport, data=data)
ggsave("saeulendiagrammkraftsport.pdf")

tally(~ tennis_badm_squash, data=data)
tally(~tennis_badm_squash, format = "proportion", data=data)
gf_bar(~ tennis_badm_squash, data=data)
ggsave("saeulendiagrammtennis_badm_squash.pdf")


tally(~ schwimmen, data=data)
tally(~schwimmen, format = "proportion", data=data)
gf_bar(~ schwimmen, data=data)
ggsave("saeulendiagrammschwimmen.pdf")


Numerische Variabeln:

favstats(~alter, data=data)
bwplot(~alter, data=data)
ggsave("boxplotalter.pdf")
gf_histogram( ~ alter, data = data)

favstats(~dankbarkeit, data=data)
bwplot(~dankbarkeit, data=data)
ggsave("boxplotdankbarkeit.pdf")
gf_histogram( ~ dankbarkeit, data = data)

favstats(~psq_freude, data=data)
bwplot(~psq_freude, data=data)
ggsave("boxplotfreude.pdf")
gf_histogram( ~ psq_freude, data = data)

favstats(~anzahl_sportarten, data=data)
bwplot(~anzahl_sportarten, data=data)
ggsave("boxplotanzahlsportarten.pdf")
gf_histogram( ~ anzahl_sportarten, data = data)


favstats(~achtsamkeit, data=data)
bwplot(~achtsamkeit, data=data)
ggsave("boxplotachtsamkeit.pdf")
gf_histogram( ~ achtsamkeit, data = data)


favstats(~Gewissenhaftigkeit, data=data)
bwplot(~Gewissenhaftigkeit, data=data)
ggsave("saeulendiagrammGewissenhaftigkeit.pdf")
gf_histogram( ~ Gewissenhaftigkeit, data = data)


favstats(~allg_swe, data=data)
bwplot(~allg_swe, data=data)
ggsave("boxplotallg_swe.pdf")
gf_histogram( ~ allg_swe, data = data)


favstats(~schuh, data=data)
bwplot(~schuh, data=data)
ggsave("boxplotschuh.pdf")
gf_histogram( ~ schuh, data = data)


favstats(~psq_anspannung, data=data)
bwplot(~psq_anspannung, data=data)
ggsave("boxplotanspannung.pdf")
gf_histogram( ~ psq_anspannung, data = data)


favstats(~kgroesse, data=data)
bwplot(~kgroesse , data=data)
ggsave("boxplotkgroesse.pdf")
gf_histogram( ~ kgroesse, data = data)

favstats(~Vertraglichkeit, data=data)
bwplot(~Vertraglichkeit, data=data)
ggsave("boxplotVertraglichkeit.pdf")
gf_histogram( ~ Vertraglichkeit, data = data)


favstats(~SMSC_sensitivity, data=data)
bwplot(~SMSC_sensitivity, data=data)
ggsave("boxplotSMSC_sensitivity.pdf")
gf_histogram( ~ SMSC_sensitivity, data = data)

favstats(~kgewicht, data=data)
bwplot(~kgewicht, data=data)
ggsave("boxplotgewicht.pdf")
gf_histogram( ~ kgewicht, data = data)

favstats(~schlaf_lastnight, data=data)
bwplot(~schlaf_lastnight, data=data)
ggsave("boxplotschlaf_lastnight.pdf")
gf_histogram( ~ schlaf_lastnight, data = data)


bwplot(alter~geschlecht, data=data)


# Berechnung der Standardabweichung für andere numerische Variablen
sd(data$dankbarkeit)
sd(data$psq_freude)
sd(data$anzahl_sportarten)
sd(data$achtsamkeit)
sd(data$allg_swe)
sd(data$schuh)
sd(data$psq_anspannung)
sd(data$kgroesse)
sd(data$Vertraglichkeit, na.rm = TRUE)
sd(data$SMSC_sensitivity)
sd(data$kgewicht)
sd(data$schlaf_lastnight)
sd(data$Gewissenhaftigkeit)



  
install.packages("corrplot")
library(corrplot)

relevant <- data[, c("dankbarkeit", "psq_freude", "anzahl_sportarten", "achtsamkeit", "allg_swe", "schuh", "psq_anspannung", "kgroesse", "SMSC_sensitivity", "kgewicht", "schlaf_lastnight")]
                                         
                                                     
korrelationsmatrix <- cor(relevant) 
corrplot (korrelationsmatrix, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 100)


favstats(~psq_anspannung, data=data)
histogram(~psq_anspannung, data=data)

cor( alter ~ psq_anspannung, data=data)
#cor.test(alter ~ psq_anspannung, data=data)
gf_point(alter ~ psq_anspannung, data=data)


bwplot(psq_anspannung ~ arbeitszeit, data=data)

gf_histogram(psq_anspannung ~ arbeitszeit, data=data)



bwplot(psq_anspannung ~ biking, data=data)

gf_histogram(psq_anspannung ~ biking, data=data)








diffmean(psq_anspannung~ geschlecht, data=data)   

t.test(psq_anspannung~ geschlecht, data=data)

set.seed(1896)
#Bootstrapping
Bootvtlg <- do(10000)* diffmean(psq_anspannung~ geschlecht, data=resample(data))
gf_histogram(~ diffmean, data=Bootvtlg)
quantile(~ diffmean, probs= c(0.025, 0.975), data=Bootvtlg)
gf_histogram(~ diffmean, data=Bootvtlg)
ggsave("diffmeangeschlecht.pdf")

gf_vline(xintercept=c(8.77,23.01)%>%
           gf_vline(xintercept= ~ (15.96))
         
set.seed(1896)
est.diff <- diffmean(psq_anspannung ~ geschlecht,
                     data = data)
Nullvtlg <- do(10000) *
  diffmean(psq_anspannung ~ shuffle(geschlecht),
           data = data)
gf_histogram( ~ diffmean, data=Nullvtlg) %>%
  gf_vline(xintercept = ~ (15.96))
Nullvtlg <- Nullvtlg %>%
  mutate(effect0 =abs(diffmean))

prop(~ (effect0>=15.96), data=Nullvtlg)

        


t.test(psq_anspannung ~ berufsausbildung, data=data)
Bootvtlg <- do(10000)* diffmean(psq_anspannung~ berufsausbildung, data=resample(data))
gf_histogram(~ diffmean, data=Bootvtlg)
quantile(~ diffmean, probs= c(0.025, 0.975), data=Bootvtlg)





Regression: Hat die Dankbarkeit einen positiven Einfluss auf die Anspannung(abhängige Variabel)
Regression1HA <-lm(psq_anspannung~dankbarkeit, data=data)
summary(Regression1HA)
plotModel(Regression1HA)
ggsave("regressionh2.pdf")


Regression1HA <-lm(psq_anspannung~achtsamkeit, data=data)
summary(Regression1HA)
plotModel(Regression1HA)
ggsave("regressionh2.pdf")

Regression1HA <-lm(psq_anspannung~psq_freude, data=data)
summary(Regression1HA)
plotModel(Regression1HA)
ggsave("regressionh3.pdf")

Regression1HA <-lm(psq_anspannung~anzahl_sportarten, data=data)
summary(Regression1HA)
plotModel(Regression1HA)
ggsave("regressionh4.pdf")






#Zusammenhang

cor(dankbarkeit~psq_anspannung, data=data)
gf_point(dankbarkeit~psq_anspannung, data=data)
ggsave("korrelationh1.pdf")
cor(achtsamkeit~psq_anspannung, data=data)
gf_point(achtsamkeit~psq_anspannung, data=data)
ggsave("korrelationh2.pdf")
cor(psq_freude~psq_anspannung, data=data)
gf_point(psq_freude~psq_anspannung, data=data)
ggsave("korrelationh3.pdf")
cor(psq_anspannung~anzahl_sportarten, data=data)
gf_point(psq_anspannung~anzahl_sportarten, data=data)
ggsave("korrelationh4.pdf")

df_stats(psq_anspannung ~ geschlecht + alter, data = data, mean, sd)
subset_cor <- subset(data, select=c(achtsamkeit, alter, psq_anspannung))
korr_tab <- cor(subset_cor)
korr_tab

model <- lm(alter~psq_anspannung+psq_freude, data=data)
summary(model)

#Unterschied

favstats(achtsamkeit~yoga, data=data)
boxplot(achtsamkeit~yoga, data=data)
favstats(psq_anspannung~yoga, data=data)
boxplot(psq_anspannung~yoga, data=data)
favstats(psq_anspannung~psq_freude, data=data)
boxplot(psq_anspannung~psq_freude, data=data)        
         


set.seed(1896)

Regression1HA <-lm(psq_anspannung~dankbarkeit, data=data)
summary(Regression1HA)
plotModel(Regression1HA)


Nullverteilung1 <-do(10000)*lm(psq_anspannung~ shuffle(dankbarkeit), data=data)
gf_histogram(~dankbarkeit, data=Nullverteilung1)%>%
  gf_vline(xintercept = 7.740)

Nullverteilung1 <- Nullverteilung1 %>%
  mutate(effekt0 = abs(dankbarkeit))
prop( ~ (effekt0 >= 7.740), data=Nullverteilung1)


--> gibt signifikanten Einfluss


BootstrappingVerteilung <-do(10000) *lm(psq_anspannung ~ dankbarkeit, data=resample(data))
BootstrappingVerteilung

gf_histogram(~ dankbarkeit, data=BootstrappingVerteilung)

quantile(~dankbarkeit, data=BootstrappingVerteilung,probs=c(0.025,0.975))

gf_histogram(~ dankbarkeit, data=BootstrappingVerteilung)%>%
 gf_vline(xintercept = c(-10,420152, -4.781289)%>%
 gf_vline(xintercept = (7.740)
 
 b=-7.740  liegt im KI und ist somit ein plausibler wert
 
 
 
 
 Hypothese mit kategorialer Variabel:
 
 t_test_result <- t.test(psq_anspannung ~ schwimmen, data = data)
 t_test_result
 
 anova_result <- aov(psq_anspannung ~ schwimmen, data = data)
 summary(anova_result)
 
 install.packages("lsr")
 library(lsr)
 cohensD(psq_anspannung ~ schwimmen, data=data)
 
 
 library(ggplot2)
 boxplot <- ggplot(data, aes(x = schwimmen, y = psq_anspannung)) +
   geom_boxplot(aes(fill = schwimmen)) +
   scale_fill_manual(values = c("skyblue", "orange")) +
   labs(title = "Verteilung des psq_anspannung nach Schwimmen",
        x = "Schwimmen",
        y = "psq_anspannung") +
   theme_minimal()
 print(boxplot)
 
 data$schwimmen <- as.factor(data$schwimmen)
 data <- data %>%
   mutate(psq_anspannung_kat = cut(psq_anspannung, 
                                   breaks = quantile(psq_anspannung, probs = seq(0, 1, 0.25), na.rm = TRUE),
                                   include.lowest = TRUE, 
                                   labels = c("Niedrig", "Mittel", "Hoch", "Sehr hoch")))
 table <- table(data$schwimmen, data$psq_anspannung_kat)
 
 chi_test <- chisq.test(table)
 chi_test
  
 
 Permutation:
 
 diff.stipro <- diffprop(psq_anspannung ~ schwimmen, data = data)
 set.seed(1896)
 NullvtlgDiffProp <- replicate(10000, {
   diffprop(psq_anspannung ~ shuffle(schwimmen), data = data)
 })
 gf_histogram(~ diffprop, data = data.frame(diffprop = NullvtlgDiffProp)) %>%
   gf_vline(xintercept = mean(NullvtlgDiffProp))
 prop(abs(NullvtlgDiffProp) >= abs(diff.stipro))
 
 
 
 
 
 Anspannung nach Geschlecht:
 
 
 
 t_test_result <- t.test(psq_anspannung ~ geschlecht, data = data)
 t_test_result
 anova_result <- aov(psq_anspannung ~ geschlecht, data = data)
 summary(anova_result)

 library(ggplot2)
 

 boxplot <- ggplot(data, aes(x = geschlecht, y = psq_anspannung)) +
   geom_boxplot(aes(fill = geschlecht)) +
   scale_fill_manual(values = c("skyblue", "orange")) +
   labs(title = "Verteilung des psq_anspannung nach Geschlecht",
        x = "Geschlecht",
        y = "psq_anspannung") +
   theme_minimal()
 
 print(boxplot)
 
 
