admissions_df<-read.csv("adm2020.csv")
admissions_df[1337,c('ENRLW')]

length(admissions_df[is.na(admissions_df$ADMSSNM) | is.na(admissions_df$ADMSSNW), ])

mean(admissions_df[!is.na(admissions_df$ADMSSNM), c('ADMSSNM') ])
