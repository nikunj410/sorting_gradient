library(readr)
library(png)
library(jpeg)
library(swfscMisc)
library(patchwork) 
library(ggplot2)

set.seed(234)
img = readPNG("data/images/storm.png",native = TRUE)
data <- read_csv("data/Spatial_Sorting_Comerford_Soapberrybugs.csv",show_col_types = FALSE)
data = na.omit(data)
data=data[data$Site!="Myerland",]
data = data[order(data$Step),]
data$Step = data$Step/2
# data = data[-which(data$Step<=-6.5 & data$Type =="Unflooded" ),]

mean_p_macro <- function(i, type, breaks) {
  dat = data$Wing[which(data$Step>=i & data$Step<(i+breaks) & data$Type == type)]
  N = length(dat)
  n = sum(dat == "Macropter")
  alpha = 1+n
  beta = 1+N-n
  # return(n/N)
  if(n==0){return(NA)} else {return(alpha/(alpha+beta))}
  
}

upper_p_macro <- function(i, type, breaks, CI) {
  dat = data$Wing[which(data$Step>=i & data$Step<(i+breaks) & data$Type == type)]
  N = length(dat)
  n = sum(dat == "Macropter")
  alpha = 1+n
  beta = 1+N-n
  if(n==0){return(NA)} else {return(as.numeric(quantile(rbeta(1e4, alpha,beta),1-(CI/2))))}
}

lower_p_macro <- function(i, type, breaks, CI) {
  dat = data$Wing[which(data$Step>=i & data$Step<(i+breaks) & data$Type == type)]
  N = length(dat)
  n = sum(dat == "Macropter")
  alpha = 1+n
  beta = 1+N-n
  if(n==0){return(NA)} else {return(as.numeric(quantile(rbeta(1e4, alpha,beta),CI/2)))}
  
}


c_unflooded <- rgb(173,216,230,max = 255, alpha = 200, names = "lt.blue")
c_flooded <- rgb(255,192,203, max = 255, alpha = 200, names = "lt.pink")

breaks  = 0.75
step_min = 5
CI = 0.33
steps = c(rev(seq(-breaks,-step_min,-breaks)),0,seq(breaks,step_min,breaks))

male_data = data.frame(avg_forewing_length_male_flooded = sapply(steps, mean_p_macro, type = "Flooded", breaks = breaks))
male_data$avg_forewing_length_male_unflooded = sapply(steps, mean_p_macro, type = "Unflooded", breaks = breaks)


male_data$upper_forewing_length_male_flooded = sapply(steps, upper_p_macro, type = "Flooded", breaks = breaks, CI)
male_data$upper_forewing_length_male_unflooded = sapply(steps, upper_p_macro, type = "Unflooded", breaks = breaks, CI)

male_data$lower_forewing_length_male_flooded = sapply(steps, lower_p_macro, type = "Flooded", breaks = breaks, CI)
male_data$lower_forewing_length_male_unflooded = sapply(steps, lower_p_macro, type = "Unflooded", breaks = breaks, CI)

male_data$time = c(steps[steps<0],0,steps[steps>0])
male_data = male_data[male_data$time!=0,]
male_data$group= male_data$time<0

cairo_pdf("Figures/Fig2/output.pdf")
# Male plot
p_male = ggplot(male_data)+ ylim(0,1) +
  geom_errorbar(aes(x = time, y = avg_forewing_length_male_flooded,
                    ymax = upper_forewing_length_male_flooded,
                    ymin = lower_forewing_length_male_flooded), col = c_flooded, size = 1,width=0)+
  geom_line(aes(x = time, y = avg_forewing_length_male_flooded, group=group), col = c_flooded, size = 0.75)+
  geom_errorbar(aes(x= time, y = avg_forewing_length_male_unflooded,
                    ymax = upper_forewing_length_male_unflooded,
                    ymin = lower_forewing_length_male_unflooded), col = c_unflooded, size = 1,width=0)+
  geom_line(aes(x = time, y = avg_forewing_length_male_unflooded, group=group), col = c_unflooded, size = 0.75)+
  geom_point(aes(x  = time, y = avg_forewing_length_male_unflooded, group=group), col = c_unflooded, size = 3)+
  geom_point(aes(x = time, y = avg_forewing_length_male_flooded, group=group), col = c_flooded, size = 3)+
  geom_rect(aes(xmin = -0.5, xmax = 0.5, ymin = -Inf, ymax = Inf), fill = "grey")+
  ylab("Proportion of \n Macropterous insects") + xlab("\nTime (4 weeks)")+
  geom_point(aes(x = step_min/4, y = 0.15), pch = 21, col = "white", fill = c_unflooded, size = 4)+
  annotate("text", x = step_min/4 + 1.75, y = 0.15, label = "Unflooded", size = 4)+
  geom_point(aes(x = step_min/4, y = 0.06), pch = 21, col = "white", fill = c_flooded, size = 4)+
  annotate("text", x = step_min/4 + 1.6, y = 0.06, label = "Flooded", size = 4)+
  scale_x_continuous(breaks = -5:5)+
  theme(plot.title = element_text(face = "bold", size = 15,hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  inset_element(p = img,left = 0.45, bottom = -.02, right = 0.55, top = -.11)


plot(p_male)
# dev.off()
ggsave("Figures/Fig2/selection.pdf",p_male, width = 9, height = 8 , units = c("cm"))




