# Load CSV into environment
survey <- read.csv('Stat 251/survey.csv', head = T, sep = ',', colClasses = rep('factor', 250))

# Set respondent as rowname
rownames(survey) <- survey[,1]; survey <- survey[,-1]

# Set interval columns
survey$TIME <- as.numeric(survey$TIME)        # Time consumed
survey$R2 <- as.numeric(survey$R2)            # Age of respondent
survey$R3 <- as.numeric(survey$R3)            # Bilang ng nakatirang tao sa bahay (kasamang natutulog at kumakain sa bahay)
survey$R4_1 <- as.numeric(survey$R4_1)        # Bilang ng nakatirang tao sa bahay na mas mababa sa 18 taong gulang
survey$D2_d <- as.numeric(survey$D2_d)        # Ilang buwan nang tumigil?
survey$D3 <- as.numeric(survey$D3)            # Ilang taon ka noong una kang uminom ng alak?
survey$S8 <- as.numeric(survey$S8)            # Magkano ang ginagastos mo sa paninigarilyo sa loob ng isang lingo?
survey$CD10_1 <- as.numeric(survey$CD10_1)    # Kung oo, ilan ang iyong nakilalang nakaranas nito sa nakalipas na taon?
survey$CD11_1 <- as.numeric(survey$CD11_1)    # Kung oo, ilan ang iyong nakilalang nakaranas nito sa nakalipas na taon?
survey$R6_3 <- as.numeric(survey$R6_3)        # Kung estudyante, magkano ang iyong allowance sa isang linggo?
survey$R6_4 <- as.numeric(survey$R6_4)        # Kung retirado, magkano ang iyong natatanggap na pension sa isang buwan, kung meron?
survey$R6_5 <- as.numeric(survey$R6_5)        # Magkano ang iyong natatanggap na tulong/regalo/donasyon sa isang buwan, kung meron?

# Recode missing values
survey[survey == '-2'] <- NA
survey[survey == '-1'] <- NA
survey[survey == '#NULL!'] <- NA

# Sub populations
survey_CD <- subset(survey, D == 'CD')                 # Current drinker
survey_CS <- subset(survey, S == 'CS')                 # Current smoker
survey_CDCS <- subset(survey, D == 'CD' & S == 'CS')   # Current drinker and smoker

survey_CD <- subset(survey, D != 'CD')                 # Current non-drinker
survey_CS <- subset(survey, S != 'CS')                 # Current non-smoker
survey_NDNS <- subset(survey, D != 'CD' & S != 'CS')   # Current non-drinker and non-smoker