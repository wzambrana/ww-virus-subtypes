######'SCRIPT DESCRIPTION ----
#'
#'This script is used to determine temporal and spatial variations in the predominant RSV subtype detected in wastewater samples, 
#'and to conduct a comparative analysis against clinical data.
#'
#'Six analysis are being conducted in this script:
#'
#'Main analyses: 
  #'1. Temporal comparison: Compare the proportion of subtype A between groups in different seasons.
  #'2. Spatial comparison: Compare the proportion of subtype A between groups in different areas.
  #'3. Clinical comparison: Compare the proportion of subtype A between groups from different sample types, i.e., wastewater vs clinical specimens. 

#'Supplementary analyses: 
  #'4. RSV storage impact assessment: Compare measurements of total RSV in samples with storage time and samples with no-storage time
  #'5. PMMoV storage impact assessment: Compare measurements of PPMoV in samples with storage time and samples with no-storage time
  #'6. Proportion vs total RSV: Asses correlation between proportion of total RSV and proportion of subtype A.

######LOADING LIBRARIES ----
library(openxlsx)
library(writexl)
library(rstatix)
library(conover.test)
library(here)

here::i_am("main_ww_virus_subtypes.R") 

######FUNCTIONS ----

  compute_summary_stats <- function(df, y, x) {
   
    ##'FUNCTION: Summary measurements ---
    #'
    #'This function computes a specific set of descriptive statistics.
    #' 
    #'  Args:
    #'    df: A dataframe.
    #'    y:A specified numeric column in df.
    #'    x: A grouping column in df. 
    #'    
    #'  Returns:
    #'    result: Dataframe containing maximum, minimum, median, standard deviation, 
    #'            first and third quartiles, interquartile range (IQR), 
    #'            and the count of values for each group.
    
    max_y <- aggregate(y ~ x, data = df, FUN = max)
    min_y <- aggregate(y ~ x, data = df, FUN = min)
    med_y <- aggregate(y ~ x, data = df, FUN = median)
    sd_y <- aggregate(y ~ x, data = df, FUN = sd)
    first_quant <- aggregate(y ~ x, data = df, FUN = 'quantile', probs=c(0.25))
    third_quant <- aggregate(y ~ x, data = df, FUN = 'quantile', probs=c(0.75))
    IQR <- third_quant[, 2] - first_quant[, 2]
    n_y <- aggregate(y ~ x, data = df, FUN = length)
    result <- data.frame(max_y, min_y[, 2], med_y[, 2], sd_y[, 2],first_quant[, 2],third_quant[, 2],IQR, n_y[, 2])
    colnames(result) <- c("x", "max_y", "min_y", "med_y", "sd_y", "1st_y", "3rd_y" ,"IQR" ,"n_y")
    return(result)
  }

######DATA PREPARATION ----

###Import wastewater data results for main and supplementary analyses ---
xlsx_source <- here("data_ww_virus_subtypes.xlsx")  

all_data<-read.xlsx(xlsx_source, sheet = "all_data")
all_data$Collection_Date <- as.Date(all_data$Collection_Date, origin = "1899-12-30") #correct date format

all_data_pmmov<-read.xlsx(xlsx_source, sheet = "pmmov_data")

###Prepare data for analysis --- 

  #Calculate proportion of subtype A to total RSV:
  all_data$Proportion <-all_data$RSV_A_gc_g_dry_weight/(all_data$RSV_A_gc_g_dry_weight + all_data$RSV_B_gc_g_dry_weight)
  
  #Label seasons
  all_data$season <- ifelse(all_data$Collection_Date >= as.Date("2021-11-15") & 
                            all_data$Collection_Date <= as.Date("2022-02-28"), "S1",
                     ifelse(all_data$Collection_Date >= as.Date("2022-11-1") & 
                            all_data$Collection_Date <= as.Date("2023-02-28"), "S2",
                            "Other"))
  
  #Label wastewater treatment plants
  all_data$wwtp <- ifelse(all_data$Plant == "Palo Alto Regional Water Quality Control Plant", "PA",
                   ifelse(all_data$Plant == "City of Sunnyvale Water Pollution Control Plant", "SV",
                   ifelse(all_data$Plant == "City of Garland Rowlett Creek WWTP", "GR",  
                   ifelse(all_data$Plant == "Duck Creek WWTP", "DC",   
                   ifelse(all_data$Plant == "RM Clayton Water Reclamation Center", "RM",  
                   ifelse(all_data$Plant == "South River Water Reclamation Center", "SR",                           
                                   "Other"))))))
  #Label wwtp + season
  all_data$wwtp_season <- paste(all_data$wwtp, all_data$season, sep = "_")
  
###Define variables for temporal and spatial comparison --- 

  #Temporal comparison (samples from Santa Clara County only)
  data_seasonal<-subset(all_data, (Area == "Santa Clara County, CA")) #seasonal = temporal
  
  #Spatial comparison (samples between 1 November 2022 and 28 February 2023)
  data_spatial <- subset(all_data, !(season == "S1"))
  
###Generate clinical data matrix for clinical comparison ---

  ##Clinical results ---
  total_n_clinical <- 593 #Total number of clinical samples
  a_clinical_percent <-0.79 #Percent of clinical samples positive for A subtype
  b_clinical_percent <- 0.21 #Percent of clinical samples positive for B subtype
  
  a_samples<-round(total_n_clinical*a_clinical_percent) #Total number of clinical samples positive for A subtype
  b_samples<-total_n_clinical-a_samples #Total number of clinical samples positive for B subtype
  
  ##Create a 0 and 1 matrix based on the clinical results ---
  clinical_data <- data.frame(sample.number = 1:total_n_clinical, 
                            result = (1:total_n_clinical)*0)
  clinical_data$result[1:a_samples]<-1
  
  ##Prepare bootstrapping for clinical analysis ---
  
    #Prepare bootstrap - define bootstrap number
    BootStrapNumber = 1000 #number of times the bootstrap is happening
    
    subsample_number = total_n_clinical #number of subsamples from the original clinical df
                                        #max number of samples (but with replacement)
    
    #List to store the results
    clinical_bootstrap <- list()
    
    #Randomly select a subset of samples from created clinical df
    for (i in 1:BootStrapNumber) {
      random_sample <- sample(clinical_data$result, subsample_number, replace = TRUE)
      random_sample_result <- sum(random_sample)/subsample_number
      clinical_bootstrap[[i]] <- random_sample_result
    }
    
    clinical_bootstrap<-as.numeric(clinical_bootstrap)
    
    clinical_bootstrap_subsample <- sample(clinical_bootstrap, 80, replace = TRUE)
    
    
##Combine clinical and wastewater data for clinical comparison ---
  clinical_df<-data.frame(sample_type = 'clinical',
                          Proportion = clinical_bootstrap_subsample)
    
  wastewater_s2_df<- subset(data_seasonal, !(season == "S1"))

  wastewater_df<-data.frame(sample_type = 'wastewater',
                            Proportion = wastewater_s2_df$Proportion)
    
  clinical_wastewater_df<-rbind(clinical_df, wastewater_df)
  

###Create dataframes for storage impact assessments ---

  #RSV measurements - storage vs no-storage results
  storage_data_rsv_fresh <- data.frame(sample_type = 'fresh', 
                                        result = all_data$Total.RSV_gc_g_dry_weight_fresh,
                                        log = log10(all_data$Total.RSV_gc_g_dry_weight_fresh))
  storage_data_rsv_stored <- data.frame(sample_type = 'stored', 
                                          result = Total.RSV_Sum_A_B_gc_g_dry_weight,
                                          log = log10(all_data$Total.RSV_Sum_A_B_gc_g_dry_weight))
  
  storage_data_rsv <-rbind(storage_data_rsv_fresh,storage_data_rsv_stored) #concatenate
  
  #PMMoV measurements - storage vs no-storage results
  storage_data_pmmov_fresh <- data.frame(sample_type = 'fresh', 
                                         result = all_data_pmmov$PMMoV_gc_g_dry_weight_fresh,
                                         log = log10(all_data_pmmov$PMMoV_gc_g_dry_weight_fresh))
  
  storage_data_pmmov_stored <- data.frame(sample_type = 'stored', 
                                          result = all_data_pmmov$PMMoV_gc_g_dry_weight_stored,
                                          log = log10(all_data_pmmov$PMMoV_gc_g_dry_weight_stored))
  
  storage_data_pmmov <-rbind(storage_data_pmmov_fresh,storage_data_pmmov_stored) #concatenate

  
####STATISTICAL ANALYSIS ----

###Descriptive statistics ---
descriptive_seasonal <-compute_summary_stats(data_seasonal, data_seasonal$Proportion,data_seasonal$wwtp_season)
descriptive_spatial <-compute_summary_stats(data_spatial, data_spatial$Proportion,data_spatial$Area)
descriptive_clinical <-compute_summary_stats(clinical_wastewater_df, clinical_wastewater_df$Proportion,clinical_wastewater_df$sample_type)
  
overall_summary_A <-summary(all_data$RSV_A_gc_g_dry_weight)
overall_summary_B <-summary(all_data$RSV_B_gc_g_dry_weight)
overall_summary_proportion <-summary(all_data$Proportion)
  
###Define the significance level ---
alpha <- 0.05
number_of_tests <-9 
  
bonferroni_alpha <- alpha / number_of_tests #Adjusted significance level using the Bonferroni correction 
  
####Check normality using Shapiro-Wilk test ---
all_shapiro<- shapiro.test(all_data$Proportion)

###Comparative analysis between groups using Kruskal-Wallis test ---

##Main analyses
  #SEASONAL COMPARISON
  seasonal_kruskal<- kruskal.test(Proportion ~ wwtp_season, data = data_seasonal)
  seasonal_kruskal_result<-seasonal_kruskal$p.value
  
  #SPATIAL COMPARISON
  spatial_kruskal<- kruskal.test(Proportion ~ Area, data = data_spatial)
  spatial_kruskal_result<-spatial_kruskal$p.value
  
  #CLINICAL COMPARISON
  clinical_kruskal<- kruskal.test(Proportion ~ sample_type, data = clinical_wastewater_df)
  clinical_kruskal_result<-clinical_kruskal$p.value


  #Main analyses summary: seasonal, spatial and clinical 
  kruskaltest<-data.frame(names = c("Seasonal", "Spatial","Clinical"), 
                          pvalues = c(seasonal_kruskal_result, spatial_kruskal_result,clinical_kruskal_result),
                          nvalues= c(length(data_seasonal$wwtp_season),length(data_spatial$Area), length(clinical_wastewater_df$Proportion)))
                          
  kruskaltest$results <- ifelse(kruskaltest$pvalues < alpha, "Reject H0", "Fail to Reject H0")
  kruskaltest$resultsBonferroni <- ifelse(kruskaltest$pvalues < bonferroni_alpha, "Reject H0", "Fail to Reject H0")

##Supplementary analyses
  #STORAGE IMPACT ASSESMENT - total RSV
  storage_kruskal_rsv<- kruskal.test(log ~ sample_type, data = storage_data_rsv)
  storage_kruskal_rsv_result<-storage_kruskal_rsv$p.value
  
  storage_kruskal_rsv_result_eta<-(storage_kruskal_rsv$statistic - 2 + 1)/(length(storage_data_rsv$result)-2)
  
  #STORAGE IMPACT ASSESMENT - PMMOV
  storage_kruskal_pmmov<- kruskal.test(log ~ sample_type, data = storage_data_pmmov)
  storage_kruskal_pmmov_result<-storage_kruskal_pmmov$p.value
  
  storage_kruskal_pmmov_result_eta<-(storage_kruskal_pmmov$statistic - 2 + 1)/(length(storage_data_pmmov$result)-2)


###Post-hoc analysis between groups using Conover-Iman test ---

##Main analyses
  #SEASONAL COMPARISON
  seasonal_conover <- conover.test(data_seasonal$Proportion, data_seasonal$wwtp_season, method="bonferroni", kw=TRUE, label=TRUE, 
                                     wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
    
  #SPATIAL COMPARISON
  spatial_conover <- conover.test(data_spatial$Proportion, data_spatial$Area, method="bonferroni", kw=TRUE, label=TRUE, 
                                   wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
    
  #CLINICAL COMPARISON
  clinical_conover <- conover.test(clinical_wastewater_df$Proportion, clinical_wastewater_df$sample_type, method="bonferroni", kw=TRUE, label=TRUE, 
                                    wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
##Supplementary analyses
  #STORAGE IMPACT ASSESMENT - total RSV
  storage_conover_rsv <- conover.test(storage_data_rsv$log, storage_data_rsv$sample_type, method="bonferroni", kw=TRUE, label=TRUE, 
                                   wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
  #STORAGE IMPACT ASSESMENT - PMMOV
  storage_conover_rsv <- conover.test(storage_data_pmmov$log, storage_data_pmmov$sample_type, method="bonferroni", kw=TRUE, label=TRUE, 
                                      wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)


#Assessing correlation between concentrations of total RSV and proportion of subtype A using Kendall Tau test ---
Taus <-cor.test(all_data$Total.RSV_Sum_A_B_gc_g_dry_weight, all_data$Proportion,  method = "kendall")
Taus_logged<-cor.test(log(all_data$Total.RSV_Sum_A_B_gc_g_dry_weight), all_data$Proportion,  method = "kendall")
