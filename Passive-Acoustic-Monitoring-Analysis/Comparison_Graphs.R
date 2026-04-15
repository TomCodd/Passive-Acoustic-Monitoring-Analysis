library(ggplot2)
library(gridExtra)

European_goldfinch_df <- read.csv(here("data", "species_call_data", "European_Goldfinch_call_dataset.csv"))


European_goldfinch_df_daylight_March <- European_goldfinch_df[360:1200, 2:3]
European_goldfinch_df_daylight_April <- European_goldfinch_df[360:1200, c(2,5)]


fft_result_March <- fft(European_goldfinch_df_daylight_March[,2])
#March_fft <- plot(Mod(fft_result_March), main = "European Goldfinch revisit FFT plot")
fft_int_March <- data.frame(x = 1:length(Mod(fft_result_March)), y = Mod(fft_result_March))
March_fft_ggplot = ggplot(fft_int_March, aes(x=x, y=y)) + geom_point() +
  labs(title = "European Goldfinch revisit timings - FFT Plot (March)")

fft_result_April <- fft(European_goldfinch_df_daylight_April[,2])
#April_fft <- plot(Mod(fft_result_April), main = "European Goldfinch revisit FFT plot (April)")
fft_int_April <- data.frame(x = 1:length(Mod(fft_result_April)), y = Mod(fft_result_April))
April_fft_ggplot = ggplot(fft_int_April, aes(x=x, y=y)) + geom_point() +
  labs(title = "European Goldfinch revisit timings - FFT Plot (April)")

x <- European_goldfinch_df_daylight_March[,1]
y <- European_goldfinch_df_daylight_March[,2]
dx <- ((max(x)-min(x))/length(x))
xout <- (min(x)+((0:length(x))*dx))
y.new <- approx(x,y,xout)

March_pgram <- spec.pgram(ts(y.new$y, deltat = dx), demean = TRUE, detrend = TRUE, plot = FALSE, spans = 20, main = paste0("European Goldfinch revisit periodogram"))
March_pgram_xy <- data.frame(x = March_pgram$freq, y = March_pgram$spec)
March_pgram_ggplot = ggplot(March_pgram_xy, aes(x=x, y=y)) + 
  geom_line() +
  scale_y_continuous(trans='log10') +
  labs(title = "European Goldfinch revisit timings - periodogram (March)")


x <- European_goldfinch_df_daylight_April[,1]
y <- European_goldfinch_df_daylight_April[,2]
dx <- ((max(x)-min(x))/length(x))
xout <- (min(x)+((0:length(x))*dx))
y.new <- approx(x,y,xout)

April_pgram <- spec.pgram(ts(y.new$y, deltat = dx), demean = TRUE, detrend = TRUE, plot = FALSE, spans = 20, main = paste0("European Goldfinch revisit periodogram"))
April_pgram_xy <- data.frame(x = April_pgram$freq, y = April_pgram$spec)
April_pgram_ggplot = ggplot(April_pgram_xy, aes(x=x, y=y)) + 
  geom_line() +
  scale_y_continuous(trans='log10') +
  labs(title = "European Goldfinch revisit timings - periodogram (April)")


March_acf <- acf(European_goldfinch_df_daylight_March[,2], demean = TRUE, main = "European Goldfinch revisit Autocorrelation function", plot = FALSE)
March_acf_xy <- data.frame(x = March_acf$lag, y = March_acf$acf)
March_acf_ggplot = ggplot(March_acf_xy, aes(x=x, y=y))+geom_segment(mapping = aes(xend = x, yend = 0))+labs(title = "European Goldfinch revisit timings - Autocorrelation function (March)")


April_acf <- acf(European_goldfinch_df_daylight_April[,2], demean = TRUE, main = "European Goldfinch revisit Autocorrelation function", plot = FALSE)
April_acf_xy <- data.frame(x = April_acf$lag, y = April_acf$acf)
April_acf_ggplot = ggplot(April_acf_xy, aes(x=x, y=y))+geom_segment(mapping = aes(xend = x, yend = 0))+labs(title = "European Goldfinch revisit timings - Autocorrelation function (April)")

grid.arrange(grobs = list(March_acf_ggplot, April_acf_ggplot, March_pgram_ggplot, April_pgram_ggplot, March_fft_ggplot, April_fft_ggplot), cols = 2)
# grid.arrange(grobs = list(March_acf_ggplot, April_acf_ggplot, March_fft_ggplot, April_fft_ggplot), cols = 2)
