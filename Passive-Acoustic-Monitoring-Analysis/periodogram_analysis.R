library(here)
library(ggplot2)
library(gridExtra)

import_files <- list.files(here("data", "species_call_data"))

for(file in import_files){
  species <- chartr("_", " ", gsub("_call_dataset.csv", "", x = file))
  
  
  df <- read.csv(here("data", "species_call_data", file), row.names = NULL)
  daytime_df <- df[360:1200, 2:ncol(df)]
  daytime_df <- daytime_df[ , colSums(is.na(daytime_df))<660] # Means at least 3 hours of recordings


  # plot(1,1, type = 'l', xlim = c(0.0167, 0.5), ylim = c(1, 1000), lwd = 2, xlab = 'frequency', ylab = 'power', main = paste0(species, " revisit Power Spectrum"))

  col_list <- colnames(daytime_df)

  for(i in 2:length(col_list)){
    daytime_df_subset <- na.omit(daytime_df[,c(1, i)])
    column_name <- gsub("X", "", colnames(daytime_df_subset[2]))

    x <- daytime_df_subset[,1]
    y <- daytime_df_subset[,2]

    if(sum(y)==0){
      next
    }

    # All of this stuff is the same as what the pgram does, the results are the same. 
    dx <- ((max(x)-min(x))/length(x))
    xout <- (min(x)+((0:length(x))*dx))

    y.new <- approx(x,y,xout)
    results_2 <- spec.pgram(ts(y.new$y, deltat = dx), demean = TRUE, detrend = TRUE, plot = TRUE, spans = 10, main = paste0(species, " revisit periodogram (", column_name, ")"))

    acf(y, demean = TRUE, main = paste0(species, " revisit Autocorrelation function (", column_name, ")"))

    fft_result <- fft(y)
    plot(Mod(fft_result), main = paste0(species, " revisit FFT plot (", column_name, ")"))

  }
}

# First for gildfinches

European_goldfinch_df <- read.csv(here("data", "species_call_data", "European_Goldfinch_call_dataset.csv"))


European_goldfinch_df_daylight_March <- European_goldfinch_df[360:1200, 2:3]
European_goldfinch_df_daylight_April <- European_goldfinch_df[360:1200, c(2,5)]


fft_result <- fft(European_goldfinch_df_daylight_March[,2])
March_fft <- plot(Mod(fft_result), main = "European Goldfinch revisit FFT plot")

fft_result_April <- fft(European_goldfinch_df_daylight_April[,2])
fft_experiment <- Mod(fft_result_April)
April_fft <- plot(Mod(fft_result_April), main = "European Goldfinch revisit FFT plot (April)")


x <- European_goldfinch_df_daylight_March[,1]
y <- European_goldfinch_df_daylight_March[,2]
dx <- ((max(x)-min(x))/length(x))
xout <- (min(x)+((0:length(x))*dx))
y.new <- approx(x,y,xout)

March_pgram <- spec.pgram(ts(y.new$y, deltat = dx), demean = TRUE, detrend = TRUE, plot = TRUE, spans = 10, main = paste0(species, " revisit periodogram"))
March_pgram_xy <- data.frame(x = March_pgram$freq, y = March_pgram$spec)
March_pgram_ggplot <- ggplot(March_pgram_xy)

x <- European_goldfinch_df_daylight_April[,1]
y <- European_goldfinch_df_daylight_April[,2]
dx <- ((max(x)-min(x))/length(x))
xout <- (min(x)+((0:length(x))*dx))
y.new <- approx(x,y,xout)

April_pgram <- spec.pgram(ts(y.new$y, deltat = dx), demean = TRUE, detrend = TRUE, plot = TRUE, spans = 10, main = paste0(species, " revisit periodogram"))
April_pgram_xy <- data.frame(x = April_pgram$freq, y = April_pgram$spec)
April_pgram_ggplot <- ggplot(April_pgram_xy)

March_acf <- acf(European_goldfinch_df_daylight_March[,2], demean = TRUE, main = "European Goldfinch revisit Autocorrelation function")
March_acf_xy <- data.frame(x = March_acf$lag, y = March_acf$acf)
March_acf_ggplot <- ggplot(March_acf_xy)

April_acf <- acf(European_goldfinch_df_daylight_April[,2], demean = TRUE, main = "European Goldfinch revisit Autocorrelation function")
April_acf_xy <- data.frame(x = April_acf$lag, y = April_acf$acf)
April_acf_ggplot <- ggplot(April_acf_xy)

grid.arrange(March_acf, April_acf, March_pgram, April_pgram, March_fft, April_fft, cols = 2)
