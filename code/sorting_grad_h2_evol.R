library(cmdstanr)
library(readr)


Meta_kingsolver <- suppressWarnings(read_csv("data/Kingsolver_selection_gradients.csv",show_col_types = FALSE))

##### gradient
sig_linear_grd = abs(as.numeric(Meta_kingsolver$Grad.linear.value))
sig_linear_grd_sd = abs(as.numeric(Meta_kingsolver$Grad.linear.StErr))

index = which(!is.na(sig_linear_grd_sd) & !is.na(sig_linear_grd))
sig_linear_grd = sig_linear_grd[index]
sig_linear_grd_sd = sig_linear_grd_sd[index]


##### differential
sig_linear_diff = abs(as.numeric(Meta_kingsolver$Diff.linear.value))
sig_linear_diff_sd = as.numeric(Meta_kingsolver$Diff.linear.StErr)

index = which(!is.na(sig_linear_diff_sd))
sig_linear_diff = sig_linear_diff[index]
sig_linear_diff_sd = abs(sig_linear_diff_sd[index])

#### calculating parameters of spatial sorting
data <- read_csv("data/Spatial_Sorting_Comerford_Soapberrybugs.csv",show_col_types = FALSE)
data = na.omit(data)
data=data[data$Site!="Myerland",]

cutt_off_flooded = 3
cutt_off_unflooded = 3


morph = list(unflooded = data$Wing[which(data$Selection == "After" & data$Type == "Unflooded"  & data$Step <=cutt_off_unflooded)],
                  flooded = data$Wing[which(data$Selection == "After" & data$Type == "Flooded"  & data$Step <=cutt_off_flooded)])

morph$unflooded = as.numeric(morph$unflooded=="Macropter")
morph$flooded = as.numeric(morph$flooded=="Macropter")

data = list(N_unflooded = length(morph$unflooded),
            N_unflooded_macro = sum(morph$unflooded))

file <- file.path("code/sorting_differential.stan")
mod <- cmdstan_model(file, stanc_options = list("O1"))

fit_sorting <- mod$sample(
  data = data,
  chains = 4,
  seed = 343,
  parallel_chains = 8,
  refresh = 5000,
  iter_warmup = 1000,
  iter_sampling = 250,
  show_messages  = F
)

std_sorting_diff_samples = as.numeric(fit_sorting$draws(variables = "std_sorting_differential", format = "draws_matrix"))
percentile = sapply(1:1000, function (i) 100*(length(which(abs(rnorm(length(sig_linear_grd), sig_linear_grd,sig_linear_grd_sd))>abs(std_sorting_diff_samples[i])))/length(sig_linear_grd)))
percentile_diff = sapply(1:1000, function (i) 100*(length(which(abs(rnorm(length(sig_linear_diff), sig_linear_diff,sig_linear_diff_sd))>abs(std_sorting_diff_samples[i])))/length(sig_linear_diff)))

library(MCMCglmm)

cross_data = na.omit(readr::read_csv('data/genetic_crosses_Comerford.csv',show_col_types = FALSE))
# assigning unique ids to offspring, fathers (sire) and mothers (dam) 
ID = data.frame(name = c(unique(cross_data$Dad.ID),unique(cross_data$Mom.ID),cross_data$ID))
ID$number = 1:length(ID$name) 

# A data frame of all the relationships
ped = data.frame(animal = ID$number,
                 sire = c(rep(NA,length(unique(cross_data$Dad.ID))+length(unique(cross_data$Mom.ID))),match(cross_data$Dad.ID,ID$name)),
                 dam = c(rep(NA,length(unique(cross_data$Dad.ID))+length(unique(cross_data$Mom.ID))),match(cross_data$Mom.ID,ID$name)))

# Linking individual id to morphology
morphology = data.frame(animal = factor(ped$animal),
                        sex = c(rep("M",length(unique(cross_data$Dad.ID))),rep("F",length(unique(cross_data$Mom.ID))),cross_data$Sex),
                        morph = c(cross_data$Dad.Morph[match(unique(cross_data$Dad.ID),cross_data$Dad.ID)],
                                  cross_data$Mom.Morph[match(unique(cross_data$Mom.ID),cross_data$Mom.ID)],cross_data$Morph))
morphology$morph = as.numeric(morphology$morph == "Macro")


prior <- list(R = list(V = 1, fix = 1),
               G = list(G1 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1)))
fit_heritibility <- suppressWarnings(
                   MCMCglmm(morph ~ 1,
                     random = ~ animal ,
                     family = "threshold",
                     prior = prior,
                     pedigree = ped,
                     data = morphology,
                     nitt = 110000,
                     burnin = 10000,
                     thin = 100,
                     verbose = F))


heritibility_samples <- fit_heritibility[["VCV"]][ , "animal"] / rowSums(fit_heritibility[["VCV"]])
std_evolution = heritibility_samples*std_sorting_diff_samples

print(paste("std. s_vw or beta_vw =", round(as.numeric(mean(std_sorting_diff_samples)),2) ,"±", round(sd(std_sorting_diff_samples),2)))
print(paste("std. beta_vw percentile =", round(mean(percentile),2),"±", round(sd(percentile),2)))
print(paste("std. s_vw percentile =", round(mean(percentile_diff),2),"±", round(sd(percentile_diff),2)))
print(paste("h^2 =", round(as.numeric(mean(heritibility_samples)),2) ,"±", round(sd(heritibility_samples),2)))
print(paste("std. evolution =", round(as.numeric(mean(heritibility_samples*std_sorting_diff_samples)),2) ,"±", round(sd(heritibility_samples*std_sorting_diff_samples),2)))

