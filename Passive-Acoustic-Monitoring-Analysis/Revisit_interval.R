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

filtered_detections$Detection.start_strptime <- strptime(filtered_detections$Detection.start, "%Y-%m-%d %I:%M:%OS")
time_cuts <- cut(filtered_detections$Detection.start_strptime, breaks = "min") 

#Need to fill gaps