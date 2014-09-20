
# global.R to share the df object with both ui.R and server.R.
# http://stackoverflow.com/questions/25321462/how-to-use-shiny-conditional-panel/25321866#25321866http://stackoverflow.com/questions/25321462/how-to-use-shiny-conditional-panel/25321866#25321866






##### FETCH DATA:
# # Importing "tidy" data
sh <- read.csv(file="data_coursera_noised.csv", sep=",", skip=0, header=TRUE, stringsAsFactors=FALSE);

sh$timestamp <- as.POSIXct(sh$timestamp, tz="UCT");
sh_colnames <- colnames(sh)


##### Fetch flags which indicates which client/MAC address is a boiler/PV/etc.:
File_Flags_csv <- "Flags_coursera.csv"
Flags_csv <- read.csv(File_Flags_csv)









Flag <- rep("undefined", length(sh_colnames))
Flag[1] <- "Date"





###### merge infos:


## PV
messPunkt_PV <- as.character(Flags_csv[Flags_csv$Flag=="PV",1])
sh_columns_PV <- c();
for(i in 1:length(messPunkt_PV)){
  sh_columns_PV <- c(sh_columns_PV, which(sh_colnames == messPunkt_PV[i]))
}
Flag[sh_columns_PV] <- "PV"



##House_WP
messPunkt_House_WP <- as.character(Flags_csv[Flags_csv$Flag=="House-WP",1])
sh_columns_House_WP <- c();
for(i in 1:length(messPunkt_House_WP)){
  sh_columns_House_WP <- c(sh_columns_House_WP, which(sh_colnames == messPunkt_House_WP[i]))
}
Flag[sh_columns_House_WP] <- "House_WP"

##House_WWWP
messPunkt_House_WWWP <- as.character(Flags_csv[Flags_csv$Flag=="House-WWWP",1])
sh_columns_House_WWWP <- c();
for(i in 1:length(messPunkt_House_WWWP)){
  sh_columns_House_WWWP <- c(sh_columns_House_WWWP, which(sh_colnames == messPunkt_House_WWWP[i]))
}
Flag[sh_columns_House_WWWP] <- "House_WWWP"


##House
messPunkt_House <- as.character(Flags_csv[Flags_csv$Flag=="House",1])
sh_columns_House <- c();
for(i in 1:length(messPunkt_House)){
  sh_columns_House <- c(sh_columns_House, which(sh_colnames == messPunkt_House[i]))
}
Flag[sh_columns_House] <- "House"


Flag[9] <- "Null"


# remove the first entry which is the date
sh_colnames <- sh_colnames[-1]
Flag <- Flag[-1]

df <- data.frame(Flag = Flag, MessPunkt = sh_colnames)

Flag_choices <- as.list(df$Flag)
names(Flag_choices) <- df$Flag









