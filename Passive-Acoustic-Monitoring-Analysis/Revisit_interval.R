detections <- read.csv("data\\All_detections.csv")

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

goldfinch <- frequency_table[,c("Timestamps", "European Goldfinch")]
plot(goldfinch, type = "h")

#Graph is odd - showing a peak at midnight for Goldfinch calls. but thats what the data in shows, so its not a weird formatting error or discrapancy in this code.