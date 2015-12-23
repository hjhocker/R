
#Now with ggplot2 library
library(ggplot2)


calculate_ci <- function(stuff) {
  n=length(stuff)
  s=sd(stuff)
  ci = qt(0.975, df=n-1)*s/sqrt(n)
  return(ci)
}

old_times = read.table('old_query.dat', header = TRUE)
new_times = read.table('new_query.dat', header = TRUE)

old_times = old_times[,1]*1000
new_times = new_times[,1]*1000

old_mean = mean(old_times)
new_mean = mean(new_times)

time = c(old_mean, new_mean)
query = c("Original", "Updated")
sd = c(sd(old_times), sd(new_times))
ci = c(calculate_ci(old_times), calculate_ci(new_times))

df = data.frame(query, time, sd, ci)

labels = paste(time, "ms")
errorBarColor = "darkseagreen4"

ggplot(data=df, aes(x=query, y=time, fill=time)) +
  geom_bar(stat="identity") +
  geom_errorbar(position=position_dodge(.9), linetype=2, width=.25, aes(ymin=time-ci, ymax=time+ci), col=errorBarColor) +
  geom_point(data=df, mapping=aes(x=query, y=time), size=3, shape=23, fill=errorBarColor) +
  ggtitle("Title") +
  xlab("Variable") +
  ylab("Time (milliseconds)") +
  #geom_text(aes(label = labels, y = time-1.6*ci), size = 5, col="white") +
  guides(fill=FALSE)

ggsave(filename = "image.jpg")
