library(haven)
library(tidyverse)
library(ggplot2)
#We only need the following two libraries if we're displaying the correlation plot
#library(reshape)
#library(corrplot)
library(psych)
library(rstanarm)

#Reading the data in, so all the code works:

NSL_data_no_missing<- read.csv("NSL_data_no_missing.csv", header=TRUE)
SL_data<-read.csv("SL_data.csv", header=TRUE)
data_of_int<- read.csv("data_of_int.csv", header=TRUE)

####EDA##########

###Plot1
# Combine the three data frames with different groups and values
affordability_accuracy <- rbind(
    data.frame(Group = "SL(Session)", Value = SL_data$SL_session_affordability_a, Verbal = SL_data$Groups_Minimal_Verbal),
    data.frame(Group = "SL(Material)", Value = SL_data$SL_material_affordability_a, Verbal = SL_data$Groups_Minimal_Verbal),
    data.frame(Group = "NSL", Value = NSL_data_no_missing$NSL_affordable_a, Verbal = NSL_data_no_missing$Groups_Minimal_Verbal))

affordability_accuracy$Verbal <- ifelse(affordability_accuracy$Verbal == 1, "Verbal", "Minimally Verbal")

ggplot(affordability_accuracy,aes(x = Value, fill = Group), color = Group) +
    geom_bar(stat = "count", position = "dodge") +
    geom_vline(aes(xintercept = 3.5), linetype = "dashed", color = "black", size = 0.5)+
    annotate("text", x = 4.5, y = 15, label = "Neutral", vjust = -0.5, color = "black", size = 3) +
    facet_wrap(~Verbal)+
    scale_fill_manual(values = c("SL(Material)" = "#A90E50", "SL(Session)" = "#DCA3AC", "NSL" = "#0B4088")) +
    labs(title = "Histogram of Whether \nFamilies agree Speech Language Therapy is Affordable",
         x = "Payment Ability", y = "Frequency")+
    guides(fill = guide_legend(title = "Experiemnt Groups"))+
    scale_x_continuous(breaks = seq(1:7), labels = c("Strongly\nAgree", "Agree", "Somewhat\nAgree", "Somewhat\nDisagree", "Disagree", "Strongly\nDisagree", "Missing"))+
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, face = "italic"),
        strip.text = element_text(face = "bold"))

###Plot2
# Combine the two data frames
service_quality <- rbind(
    data.frame(Group = "SL", Value = SL_data$SL_service_quality_q, Verbal = SL_data$Groups_Minimal_Verbal),
    data.frame(Group = "NSL", Value = NSL_data_no_missing$NSL_therapy_good_q, Verbal = NSL_data_no_missing$Groups_Minimal_Verbal)
)
service_quality$Value[service_quality$Value ==0] <- 7
service_quality$Verbal <- ifelse(service_quality$Verbal == 1, "Verbal", "Minimally Verbal")

ggplot(service_quality,aes(x = Value, fill = Group), color = Group) +
    geom_bar(stat = "count", position = "dodge") +
    geom_vline(aes(xintercept = 3.5), linetype = "dashed", color = "black", size = 0.5)+
    annotate("text", x = 4.5, y = 15, label = "Neutral", vjust = -0.5, color = "black", size = 3) +
    facet_wrap(~Verbal)+
    scale_fill_manual(values = c("SL" = "#A90E50", "NSL" = "#0B4088")) +
    labs(title = "Histogram of Whether Families Agree that\n available/received Speech Therapy Quality is good",
         x = "Satisfaction", y = "Frequency")+
    guides(fill = guide_legend(title = "Experiment Groups"))+
    scale_x_continuous(breaks = seq(1:7), labels = c("Strongly\nAgree", "Agree", "Somewhat\nAgree", "Somewhat\nDisagree", "Disagree", "Strongly\nDisagree", "Missing"))+
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, face = "italic"),
        strip.text = element_text(face = "bold"))

###Plot3
days_perweek <- rbind(
    data.frame(Group = "SL", Value = SL_data$SL_days_per_week_f, Verbal = SL_data$Groups_Minimal_Verbal),
    data.frame(Group = "NSL", Value = NSL_data_no_missing$NSL_pre_cov_daysperwk_f,  Verbal = NSL_data_no_missing$Groups_Minimal_Verbal)
)
days_perweek$Verbal <- ifelse(days_perweek$Verbal == 1, "Verbal", "Minimally Verbal")

ggplot(days_perweek, aes(x = Value, fill = Group)) +
    geom_bar(stat = "count", position = "dodge") +
    geom_vline(aes(xintercept = 3.5), linetype = "dashed", color = "black", size = 0.5)+
    annotate("text", x = 4.5, y = 11, label = "Neutral", vjust = -0.5, color = "black", size = 3) +
    labs(title = "Histogram of Attendance Frequency(days/wk)",
         x = "Days", y = "Frequency") +
    facet_wrap(~Verbal)+
    scale_fill_manual(values = c("SL" = "#A90E50", "NSL" = "#0B4088")) + 
    scale_x_continuous(breaks = seq(1, 8), labels = c("0","1", "2", "3", "4", "5", "6", "N/A")) +
    guides(fill = guide_legend(title = "Experiment Groups")) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, face = "italic"),
        strip.text = element_text(face = "bold"))

######PCA#########

print("PCA for SL")

# Load necessary libraries

# Load the dataset SL
sl_df <- SL_data

# Filter columns and handle missing values
filtered_sl_df <- sl_df %>% select(
    SL_service_quality_q,
    SL_parents_can_observe_q,
    SL_receive_home_activities_q,
    SL_material_affordability_a)
cleaned_data<- filtered_sl_df

# Standardize the data
scaled_cleaned_data <- scale(cleaned_data)
# Remove columns where all values are NaN
scaled_cleaned_data <- scaled_cleaned_data[, !apply(is.nan(scaled_cleaned_data), 2, all)]

#dim(scaled_cleaned_data)
MCov <- cov(scaled_cleaned_data)
#eigen(MCov)$value

# Conduct PCA
pca_result <- prcomp(scaled_cleaned_data)

# Extract explained variance
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# If you want to display the first few rows for brevity
# scores <- pca_result$x
# head(scores)  

first_eigenvalue <- pca_result$sdev[1]^2
#print(first_eigenvalue)
all_eigenvalue <- pca_result$sdev^2
print("Eigenvalues:")
print(all_eigenvalue)

# Extract the loadings
loadings <- pca_result$rotation

# Select the first four principal components
loadings_4 <- loadings[, 1:2]
print("Loadings")
print(loadings_4)
# Convert to long format for easier plotting with ggplot
loadings_long <- as.data.frame(as.table(loadings_4))

# Plot the loadings
#ggplot(loadings_long, aes(x = Var2, y = Freq, fill = Var1)) +
# geom_bar(stat = "identity", position = "dodge") +
#labs(title = "Variable Loadings for First Four Principal Components",
#    x = "Variables",
#   y = "Loadings Value") +
#  theme_minimal() +
# theme(axis.text.x = element_text(angle = 90, hjust = 1))


print("PCA for NSL") 
# Load necessary libraries

# Load the dataset NSL
nsl_df <- NSL_data_no_missing

# Filter columns and handle missing values
filtered_nsl_df <- nsl_df %>% select(
    NSL_pre_cov_daysperwk_f,
    NSL_affordable_a,
    NSL_distance_a,
    NSL_helping_q,
    NSL_therapy_good_q,
    NSL_SL_unavailable_child_old_a
)

cleaned_data <- filtered_nsl_df 


# Standardize the data
scaled_cleaned_data <- scale(cleaned_data)
# Remove columns where all values are NaN
scaled_cleaned_data <- scaled_cleaned_data[, !apply(is.nan(scaled_cleaned_data), 2, all)]

#dim(scaled_cleaned_data)
MCov <- cov(scaled_cleaned_data)
#eigen(MCov)$value


# Conduct PCA
pca_result <- prcomp(scaled_cleaned_data)

# Extract explained variance
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Extract the loadings
loadings <- pca_result$rotation


# If you wannaDisplay the first few rows for brevity
# scores <- pca_result$x
# head(scores)  

first_eigenvalue <- pca_result$sdev[1]^2
#print(first_eigenvalue)
all_eigenvalue <- pca_result$sdev^2
print("Eigenvalues for No Speech Language")
print(all_eigenvalue)

# Select the first four principal components
loadings_4 <- loadings[, 1:2]
print("Loadings")
print(loadings_4)
# Convert to long format for easier plotting with ggplot
loadings_long <- as.data.frame(as.table(loadings_4))

# Plot the loadings
#ggplot(loadings_long, aes(x = Var2, y = Freq, fill = Var1)) +
# geom_bar(stat = "identity", position = "dodge") +
#labs(title = "Variable Loadings for First Four Principal Components",
#     x = "Variables",
#     y = "Loadings Value") +
#  theme_minimal() +
# theme(axis.text.x = element_text(angle = 90, hjust = 1))


#####Factor Analysis#########
SL_factor_data<- SL_data %>% select(SL_goals_awareness_q,
                                    SL_service_quality_q,
                                    SL_session_affordability_a,
                                    SL_parents_can_observe_q,
                                    SL_receive_home_activities_q,
                                    SL_material_affordability_a,
                                    SL_duration_f,
                                    SL_days_per_week_f
)

#KMO(SL_factor_data)


ev<- eigen(cor(SL_factor_data))

Nfacs<-4
fit <- factanal(SL_factor_data, Nfacs, rotation="varimax")

print(fit)

#FA for NSL Data

NSL_factor_data<- NSL_data_no_missing %>% select(
    NSL_pre_cov_daysperwk_f,
    NSL_affordable_a,
    NSL_distance_a,
    NSL_helping_q,
    NSL_therapy_good_q,
    NSL_SL_unavailable_child_old_a
)

#KMO(NSL_factor_data)

ev<- eigen(cor(SL_factor_data))

Nfacs<-3
fit <- factanal(NSL_factor_data, Nfacs, rotation="varimax")
print(fit)