README for Data and R code for a project investigating variations in the predominant RSV subtype detected in wastewater and clinical samples.

Author: WZ
Associated publication: https://doi.org/10.1021/acsestwater.2c00050

FILES
1)	ww_virus_subtypes.Rproj: 		R project file needed to run main R script
2)	main_ww_virus_subtypes.R:		Main R script to run
3)	data_ww_virus_subtypes.xlsx: 		Data used in main R script 
4)	raw_visual_ww_virus_subtypes.ipynb	Python script used to process original raw data and creating visuals for project


1) ww_virus_subtypes.Rproj
Necessary to run main R script. Save all files (includes this one) in the same directory before running main_handwashing_efficacy.R.

2) main_ww_virus_subtypes.R
The main script for the analysis. This script is used to determine temporal and spatial variations in the predominant RSV subtype detected in wastewater samples, and to conduct a comparative analysis against clinical data.

3) data_ww_sampling_scales.xlsx
[Original data source: https://doi.org/10.25740/vm787sj6177]

Collection_Date:			Date of sample collection
Plant:					Wastewater treatment plant name
Area:					Nearest metropolitan area
RSV_A_gc_g_dry_weight:			RSV A gene concentrations in gene copies per gram dry weight in stored samples
RSV_A_gc_g_dry_weight_UCI:		Upper 68% confidence bound of RSV A gene concentrations in gene copies per gram dry weight in stored samples
RSV_A_gc_g_dry_weight_LCI:		Lower 68% confidence bound of RSV A gene concentrations in gene copies per gram dry weight in stored samples
RSV_B_gc_g_dry_weight:			RSV B gene concentrations in gene copies per gram dry weight in stored samples
RSV_B_gc_g_dry_weight_UCI:		Upper 68% confidence bound of RSV B gene concentrations in gene copies per gram dry weight in stored samples
RSV_B_gc_g_dry_weight_LCI:		Lower 68% confidence bound of RSV B gene concentrations in gene copies per gram dry weight in stored samples
Total RSV_Sum_A_B_gc_g_dry_weight: 	Sum of RSV A  and RSV B gene concentrations in gene copies per gram dry weight in stored samples
Total RSV_gc_g_dry_weight_fresh:	Total RSV  gene concentrations(not subtyped) in gene copies per gram dry weight measured in fresh samples
PMMoV_gc_g_dry_weight_fresh:		PMMoV gene concentrations in gene copies per gram dry weight in fresh samples
BCoV_Recovery_fresh:			Recovery of BCoV in fresh samples
PMMoV_gc_g_dry_weight_stored:		PMMoV gene concentrations in gene copies per gram dry weight in stored samples
PMMoV_gc_g_dry_weight_UCI_stored:	Upper 68% confidence bound of  PMMoV gene concentrations in gene copies per gram dry weight in stored samples
PMMoV_gc_g_dry_weight_LCI_stored:	Lower 68% confidence bound of PMMoV gene concentrations in gene copies per gram dry weight in stored samples

--
all_data:				RSV A and RSV B data
pmmov_data:				PMMoV data


4) raw_visual_ww_virus_subtypes.ipynb
Python script used to process raw data and create visuals for the project. Originally created in google collab. Provided for illustratiative purposes.

