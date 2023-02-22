
library(dplyr)
mecha_data <- read.csv('MechaCar_mpg.csv', stringsAsFactors = F)

head(mecha_data)

lm(mpg ~ vehicle_weight + spoiler_angle + ground_clearance + AWD + vehicle_length, data=mecha_data)

summary(lm(mpg ~ vehicle_weight + spoiler_angle + ground_clearance + AWD + vehicle_length, data=mecha_data))

coil_data <- read.csv('Suspension_Coil.csv', stringsAsFactors = F)

total_summary <- coil_data %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')

lot_summary <- coil_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')

sample_data_allLots <- coil_data %>% sample_n(50)

t.test((sample_data_allLots$PSI), mu=1500)

lot1_sample <- coil_data %>% subset(Manufacturing_Lot=="Lot1") %>% sample_n(25)

lot2_sample <- coil_data %>% subset(Manufacturing_Lot=="Lot2") %>% sample_n(25)

lot3_sample <- coil_data %>% subset(Manufacturing_Lot=="Lot3") %>% sample_n(25)

t.test((lot1_sample$PSI), mu=1500)

t.test((lot2_sample$PSI), mu=1500)

t.test((lot3_sample$PSI), mu=1500)