detections <- read.csv("data/All_detections.csv")

# Filtering out low-confidence identifications
detections <- detections[detections$Confidence > 0.55, ]

#Next to identify the species with enough calls to do the analysis - technically 3 would do
bird_frequency <- table(detections$Common.name)
Species_to_analyse <- unlist(labels(bird_frequency[bird_frequency>2]))
filtered_detections <- detections[detections$Common.name %in% Species_to_analyse,]

# Test graph
goldfinch <- filtered_detections[filtered_detections$Common.name == "European Goldfinch",]

# Want a bin table of the entire dataset - timestamps/time bins column 1, then rest of columns are counts for each species. Each species gets a column.
start_time <- min(filtered_detections$Detection.start)
end_time <- max(filtered_detections$Detection.start)

#Converting to strptime with minute bins
filtered_detections$Detection.start_strptime <- strptime(filtered_detections$Detection.start, "%Y-%m-%d %H:%M:%OS") #Converts to strptime
filtered_detections$Detection.start_strptime <- cut(filtered_detections$Detection.start_strptime, breaks = "min")  #cut converts it to minutes

#Creating the basic df to attach results to
timestamps <- unique(unlist(labels(table(filtered_detections$Detection.start_strptime))))
frequency_table <- data.frame("Timestamps" = as.POSIXct(timestamps, tz = "GMT", "%Y-%m-%d %H:%M:%OS"))

# Next, to run a count for each species for each timestamp.
for(Species in Species_to_analyse){
  species_table <- table(filtered_detections[filtered_detections$Common.name == Species,]$Detection.start_strptime)
  frequency_table <- cbind(frequency_table, species_table)
  colnames(frequency_table)[colnames(frequency_table) == 'Freq'] <- Species
  frequency_table <- subset(frequency_table, select = -Var1)
}

colnames(frequency_table) <- chartr(" ", "_", colnames(frequency_table))

#goldfinch <- frequency_table[,c("Timestamps", "European Goldfinch")]
#plot(goldfinch, type = "h")

#Graph is odd - showing a peak at midnight for Goldfinch calls. but thats what the data in shows, 
# so its not a weird formatting error or discrapancy in this code. Maybe spooked?

# Should:
# Make it so I merge multiple files. (An upgrade I can sort out later.)
# Fix midnight issue 
# Pick a species
# Isolate start and end time calls for each day in series
# Plot individual days on a pgram

NA_timestamps <- which(is.na(frequency_table$Timestamps))

for(x in NA_timestamps){
  frequency_table[x, "Timestamps"] <- frequency_table[x+1, "Timestamps"] - 60
}

rm(NA_timestamps)

days_covered <- unique(format(frequency_table$Timestamps, "%d/%m/%Y"))

Daily_dataset <- data.frame("Day_Minutes" = seq(0, 1439, 1)) #Creates a base dataset to merge data to

for(day in days_covered){

  #day <- days_covered[1]
  day_data <- frequency_table[format(frequency_table$Timestamps, "%d/%m/%Y") == day,] #subsets the data to a particular day

  Day_call_totals <- data.frame(t(colSums(day_data[,2:ncol(day_data)]))) # Calculates the total calls for that day
  Selected_species <- colnames(Day_call_totals[, Day_call_totals > 100]) # Filters to only birds with over 100 calls in the day
  Selected_species <- chartr(".", "_", Selected_species) # Replaces the full stops with underscores in the names



  Selected_day_data <- day_data[, c("Timestamps", Selected_species)] #Trims data down to only the selected species

  for(Species in Species_to_analyse){
    if(exists(paste0(Species, "_call_dataset")) == FALSE){
      if(file.exists(paste0("\"data/species_call_data/", Species, "_call_dataset.csv\"")) == TRUE){
        eval(paste0(Species, "_call_dataset <- read.csv(\"data/species_call_data/", Species, "_call_dataset.csv\""))
    }
  }

  }


-------


Test_dataset_goldfinch <- Selected_day_data[, c("Timestamps", "European_Goldfinch")]
spec.pgram(Test_dataset_goldfinch)

Test_dataset_goldfinch_Minutes <- Test_dataset_goldfinch
Test_dataset_goldfinch_Minutes$Minutes <- (as.numeric(format(Test_dataset_goldfinch$Timestamps, "%H"))*60)+(as.numeric(format(Test_dataset_goldfinch$Timestamps, "%M")))
Test_dataset_goldfinch_Minutes <- Test_dataset_goldfinch_Minutes[, c("Minutes", "European_Goldfinch")]
spec.pgram(Test_dataset_goldfinch_Minutes)

Test_dataset_goldfinch_Hours <- Test_dataset_goldfinch
Test_dataset_goldfinch_Hours$Hours <- (as.numeric(format(Test_dataset_goldfinch$Timestamps, "%H")))+((as.numeric(format(Test_dataset_goldfinch$Timestamps, "%M")))/60)
Test_dataset_goldfinch_Hours <- Test_dataset_goldfinch_Hours[, c("Hours", "European_Goldfinch")]
spec.pgram(Test_dataset_goldfinch_Hours)

ts_test_data <- ts(Test_dataset_goldfinch[, c("Timestamps", "European_Goldfinch")])
spectrum(ts_test_data, method = "pgram") 
'''
Time_1 <- Sys.time()

for(i in 1:100){
  print(i)
  spectrum(ts_test_data, method = "pgram") 
}

Time_2 <- Sys.time()

for(i in 1:100){
  print(i)
  spec.pgram(Test_dataset_goldfinch)
}

Time_3 <- Sys.time()

print(cat("Spectrum method time: ", Time_2-Time_1, ". spec.pgram method time: ", Time_3-Time_2))
'''
# Results: Spectrum method time:  1.145629 . spec.pgram method time:  1.106783

Test_dataset_goldfinch <- Selected_day_data[, c("Timestamps", "European_Goldfinch")]
plot_object <- spec.pgram(Test_dataset_goldfinch, demean = TRUE)

# This periodogram works. Time is minutes, so frequency is 1/the optimum time... so peak at ~0.115 means peak revisit time is 1/0.115 = 8.7 minutes
# Can I get multiple plots just by including more columns?

Test_dataset_goldfinch_bluetit <- Selected_day_data[, c("Timestamps", "European_Goldfinch", "Eurasian_Blue_Tit")]
plot_object <- spec.pgram(Test_dataset_goldfinch_bluetit, demean = TRUE)

# Looks like it works! 
# So just need to stack the data for each species so that timestamp is Day Minutes (which seems to work)
# And each column is a different day
# So need an idealistic dataset which is 1440 minutes, and then left merge the other columns
# If making this a long-term project, could keep the species datasets as files, open one up as needed, add to it, then save


