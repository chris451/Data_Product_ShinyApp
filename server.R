library(shiny)
library(ggplot2)
Sys.setlocale("LC_TIME", "English")







# Define server logic
shinyServer(function(input, output, session) {

  #http://stackoverflow.com/questions/25321462/how-to-use-shiny-conditional-panel/25321866#25321866http://stackoverflow.com/questions/25321462/how-to-use-shiny-conditional-panel/25321866#25321866
  observe({
    if (input$select_Flag != "") {
      MessPunkt_choices <- as.list(df$MessPunkt[df$Flag == input$select_Flag])
      names(MessPunkt_choices) <- df$MessPunkt[df$Flag == input$select_Flag]   
      updateSelectInput(session, "select_MessPunkt", choices=MessPunkt_choices)     
    }


    if (input$select_Flag2 != "") {
      MessPunkt_choices2 <- as.list(df$MessPunkt[df$Flag == input$select_Flag2])
      names(MessPunkt_choices2) <- df$MessPunkt[df$Flag == input$select_Flag2]
      updateSelectInput(session, "select_MessPunkt2", choices=MessPunkt_choices2)        
    }

    if (input$select_Flag3 != "") {
      MessPunkt_choices3 <- as.list(df$MessPunkt[df$Flag == input$select_Flag3])
      names(MessPunkt_choices3) <- df$MessPunkt[df$Flag == input$select_Flag3]      
      updateSelectInput(session, "select_MessPunkt3", choices=MessPunkt_choices3)      
    }

  })
  
  

  
 output$Plot <- renderPlot({
   
   # set default value to zero:
   messPunktColumn <- 0
   messPunktColumn2 <- 0
   messPunktColumn3 <- 0
   

   mymessPunkt <- input$select_MessPunkt
   messPunktColumn <- which(colnames(sh[1,])==mymessPunkt);

   mymessPunkt2 <- input$select_MessPunkt2
   messPunktColumn2 <- max(0,which(colnames(sh[1,])==mymessPunkt2));
   
   mymessPunkt3 <- input$select_MessPunkt3
   messPunktColumn3 <- max(0,which(colnames(sh[1,])==mymessPunkt3));
   

   
#sum the total Energy consumption of MessPunkt & MessPunkt2 and compute the rescaling factor for the total consumption:   
# note: the tricky part here is to get sth that make sense as many entries are NA's! (for example, we need to take into 
#        account if have data for only half a year) 
#   
  tot_energy_messPunkt <- as.numeric(sum(sh[,messPunktColumn],na.rm=TRUE))# take it as numeric to avoid integer oveflow in the multiplication below
  nb_non_NA_entries_messPunkt <- sum(is.na(sh[,messPunktColumn])==0)
  expected_tot_nb_entries <- length(sh[,messPunktColumn])
  expected_tot_energy_messPunkt <- (tot_energy_messPunkt*expected_tot_nb_entries)/nb_non_NA_entries_messPunkt
  nb_entry_a_year <- 365*24*4
  expected_yearly_kWh_messPunk <- expected_tot_energy_messPunkt/(expected_tot_nb_entries/nb_entry_a_year)/1000 # divid by 1000 to get kWh
  

    rescaling_factor_consumption <- input$input_kWh/expected_yearly_kWh_messPunk
    if(input$input_kWh==0){rescaling_factor_consumption=1} # no scaling if user put zero





# search for the maximum power produced by the PV. This maxiumum is considered to be 100% kWp of the PV installation
#     use this maximum power (=kWp of the installation) to rescale the profile according to the user's input!:
rescaling_factor_PV <- 0
   if(messPunktColumn3 !=0){
        rescaling_factor_PV <- input$input_PV_kWp/(max(sh[,messPunktColumn3],na.rm=TRUE)*4);# factor 4 because we measure energy every 15min        
   } 

if(input$input_PV_kWp==0){rescaling_factor_PV=1} # no scaling if user put zero




   
day_in_year <- input$day_in_year;  

# return the first day that doesn't contains NAs:
mydate_tmp1 <- as.Date(sh[which(is.na(sh[,messPunktColumn])==0)[1],1]) # return the date of the first non-NA entry for the MessPunkt

non_NA <- which(is.na(sh[,mymessPunkt])==0)
max_nb_days <<- as.Date(sh[non_NA[length(non_NA)],1]) # return the date of the last non-NA entry for the MessPunkt

 mydate <- mydate_tmp1 + day_in_year; # start at first non NA entry of load 1
#mydate <- as.Date(paste("2013-02-24"))+day_in_year;


index_mydate_start <- which(as.Date(sh[,1])==mydate)[1] # get the index of the row corresponding to mydate 00:00 (midnight)
index_mydate_end <- index_mydate_start + 24*60/15 -1






MessPunktList <- colnames(sh)[-1] # general case, no filters






##### PLOT


# initialisation: set to zero if have no data: (avoid error in the plot)
energy <- rep(0, 96)
energy2 <- rep(0, 96)
energy3 <- rep(0, 96)

if(is.na(index_mydate_end)==0){ #check if we are trying to access an index which is beyond the data table...
      
      if(messPunktColumn != 0){
        energy <-  rescaling_factor_consumption * sh[index_mydate_start:index_mydate_end,messPunktColumn]
      }else{energy <- rep(0, 96)}
      
      if(messPunktColumn2 != 0){
        energy2 <-  sh[index_mydate_start:index_mydate_end,messPunktColumn2]
      }else{energy2 <- rep(0, 96)}
      
      if(messPunktColumn3 != 0){
        energy3 <-  rescaling_factor_PV * sh[index_mydate_start:index_mydate_end,messPunktColumn3]
      }else{energy3 <- rep(0, 96)}

}





energy[is.na(energy)] <- 0
energy2[is.na(energy2)] <- 0
energy3[is.na(energy3)] <- 0



# Comupte the Supply Cover Factor:
compute_EVO_rate <- 0
for(i in 1:length(energy)){
  compute_EVO_rate <- compute_EVO_rate + min(energy3[i], (energy[i] + energy2[i]))
}  
compute_EVO_rate <- compute_EVO_rate /sum(energy3)
if(is.nan(compute_EVO_rate)==1){compute_EVO_rate <- 0}



if(is.na(index_mydate_end)==0){
    atx <- sh[index_mydate_start:index_mydate_end,1];
}else{atx <- sh[1:96,1]} # case if we are trying to access days where there is no data (avoid error in plot), or days after the maximum nb of days
atx <- format(atx, "%H:%M");








Meter1 <- data.frame(Time=atx, Energy=energy)
Meter2 <- data.frame(Time=atx, Energy=energy2)
Meter3 <- data.frame(Time=atx, Energy=energy3)



if(max(max(energy),max(energy2), max(energy3))>5000){ymax=10000}else{ymax=5000}

p<- ggplot(NULL, aes(Time, Energy)) +
  geom_bar(aes(fill = "Meter1"), data = Meter1, alpha = 0.5, stat="identity") +
  geom_bar(aes(fill = "Meter2"), data = Meter2, alpha = 0.5, stat="identity") +
  geom_bar(aes(fill = "Meter3"), data = Meter3, alpha = 0.5, stat="identity") +
  
  
  
  scale_y_continuous(limits=c(0, ymax), expand = c(0, 0))+
  
  scale_x_discrete( expand = c(0, 0), breaks=c("00:00","02:00","04:00","06:00","08:00","10:00","12:00","14:00","16:00","18:00","20:00","22:00"))+ 
  #scale_x_discrete( expand = c(0, 0), breaks=c("00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00"))+ 
  
  labs(x = "Time", size=48) +
  labs(y = "Energy [Wh]", size=48) +
  labs(title = paste("Energy distribution\n", "Date = ", mydate, " (", weekdays(mydate) ,"), Supply Cover Factor = ", format(compute_EVO_rate*100, digits=1, nsmall=1), "%" ,  sep=""), size=66) +
  
  scale_fill_manual(values=c("#999999", "red", "#E69F00"),
                    name="Energy meters:",          
                    labels=c(input$select_Flag, input$select_Flag2, input$select_Flag3))+

  
  
  theme(axis.title.x = element_text(size = 48))+
  theme(axis.title.y = element_text(size = 48))+
  theme(title = element_text(size = 38))+
  theme(axis.text = element_text(size = 48))+
  theme_bw()
  #theme(legend.position="none") # This removes all legends 

print(p) # allows shiny to actually print ggplot! 




 }, height=function() { session$clientData$output_Plot_width * 0.7 }) #You can modify "1.0" in the last line to get a different aspect ratio. https://groups.google.com/forum/#!topic/shiny-discuss/ZmuZbQfJstg


# end of plot
#######################################################






output$PV_kWp <- renderPrint({ input$input_PV_kWp })
output$kWh <- renderPrint({ input$input_kWh })




output$textEVO <- renderText({

  
    paste("The plot show representative PV production and electricity consumption measurements [as the original data is proprietary data, this data subset is here quite heavily noised!] as well as the Supply Cover Factor (rate of self-consumption of the photovoltaic energy) for the day, for a photovoltaic installation of ", input$input_PV_kWp, "[kWp] and a yearly energy consumption (without heating and hot water) of ",input$input_kWh, " [kWh].", sep="")
          

})




}) # end of: shinyServer(function(input, output, session)