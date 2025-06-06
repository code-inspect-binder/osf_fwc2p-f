#############################################################################################
# 			A review on the use of ultra-high field MRI to visualize the subcortex							#
# 											Script by M.C. Keuken																								#
#													mckeuken@gmail.com 																								#
#############################################################################################

#############################################################################################
# The goal of this code is to enable others to replicate the results and figures as
#		reported in the manuscript. The manuscript itself is a review on the use of UHF
#		MRI to visualze the subcortex. There will be another document with code where the
#		reader can generate summaries of their structures of interest. This second piece of
#		code will be a condensed version of this document.
#
#	Layout of the code:
#		1) Description of the literature search and the corresponding category labels
#		2) The R code to generate the results as reported in the paper
#		3) The R code to generate the figures as reported in the paper
#
# Code was tested on an apple computer using the following software and packages:
# 	R (3.4.1)
#		ggplot2 (2.2.1)
# 	grid (3.4.1)
#		cowplot (0.9.1)
#############################################################################################

#############################################################################################
# 1) Literature search description
#############################################################################################
#
#	Search date:	01/12/2017
#	search range:	past-december 2017
# 	Search database:	pubmed
# 	Search tools:	Python API

# Literature search results
# 	Total hits:	5818
#		Potential abstracts:	388
#		Excluded based on double check of abstract:	31
#		Exclude based on it being a review:	58
#		Exclude based on reading full text:	145
#		Crossref review abstracts: 5252
#		Extra studies via crossref: 13
#	Remaining studies used as input:	154

#	How many papers we are currently missing is unknown.
# Based on our own work we missed 2 out of 27 of our own papers.
#		Conservative estimate is that our literature search missed 7.5% of the papers
#
# Total papers included: 	154 (emperical search) +
#													13 (cross ref) +
#													2 (missing own):
#													169 total

###############################################
# Labels
#	Nr sequence mapping
#		T1 weighted / maps		1
#		T2 weighted / maps		2
#		T2* weighted / maps		3
#		functional EPI				4
#		DWI										5
#		SWI / QSM							6
#		MT										7
#		PD										8
#		Other									9
#		multiple							10

# Disease mapping
# 	Controls							0
#		PD										1
#		scleroris							2
#		huntington						3
#		wilson's disease			4
#		epilepsie							5
#		Friedreich's ataxia		6
#		fetal 								7
#		other									8

# Segmentation mapping
#		Identification				0
#		Manual seg						1
#		Manual ROI in visual ind.	2
#		Semi automatic				3
#		Automatic							4
#		Functional localizer	5

# In vivo versus post mortem
#		In vivo								1
#		Post mortem						2
#############################################################################################

#############################################################################################
#	2) The R code to generate the results as reported in the paper
#############################################################################################
# To ensure that no old variables in R are still lingering on,
#		I first cleanup the workspace:
rm(list=ls())

###############################################
# Loading in the R packages for plotting the figures:
###############################################
library(ggplot2)
library(grid)
library(cowplot)
###############################################

###############################################
# Loading and organizing the data:
###############################################
# 	The data is a comma seperate text file where per line the extract information
#			per included paper is noted. To be able to run the R code you need to have
#			a copy of this csv file which can be downloaded from the OSF page.
#
#	Change the workind path directory to where the "review_data.csv" is saved:
	setwd("/Users/mckeuken/Dropbox/Documents/werk/projects/Subcortical_7T_review/analysis/")
	raw_data 				<- read.csv("Keuken_etal_review_data.csv", header=T,sep = ",")
	data 						<- raw_data
###############################################

###################
# Some notes regarding the dataset and the missing values.
# In the review_data file there are two relevant lables:
#		"NA" 	== there was no relevant data in the original paper.
#			An example would be a paper only used a T1 based contrast. This would
#				then result in "NA" values for all the other MRI contrast columns.
#
# 	"9999"  == the original authors did not provide this information.
#			An example would be a paper where they used a T1 based contrast but then
#				did not provide the voxel dimensions.
#
#	In both cases they are missing data and we dont want them to interfere with
#		either the summary statistics or when plotting the data. Therefore I replace
#		both these values with NA (not a number).
# Replacement of "NA" and "9999" with NA:
	data[data==NA] 				<- NA
	data[data==9999] 			<- NA
###################

####################
# For ease of data manipulation we will use a data frame:
	data	<- as.data.frame(data)

# For summarizing and plotitng the data it makes sense to make certain columns
#		factors:
	data$invivo_exvivo_nr		<- as.factor(data$invivo_exvivo_nr)
	data$structures_nr 			<- as.factor(data$structures_nr)
	data$ident_seg_nr 			<- as.factor(data$ident_seg_nr)
	data$manseg_auto_seg_nr <- as.factor(data$manseg_auto_seg_nr)
	data$sequence_nr 				<- as.factor(data$sequence_nr)
	data$Field_strength_nr  <- as.factor(data$Field_strength_nr)
	data$control_patient  	<- as.factor(data$control_patient)
	data$type_of_patient  	<- as.factor(data$type_of_patient)
###################

####################
# One of the questions that we answer in the review is whether the original papers
#		used (near) isotropic or anisotropic voxels. With near isotropic I mean that the
# 	voxel dimensions of the x,y,z lenghts are within 10% of the isotropic voxel give
#		the reported voxel volume. To be able to answer this question I need to calculate
#		a few things:
# Step 1) I calculate what the dimension of an isotropic voxel would be given the
#		the reported voxel volumes:
	data$iso_voxel_T1				<- data$T1_voxel_volume_cubic_mm^(1/3)
	data$iso_voxel_T2				<- data$T2_voxel_volume_cubic_mm^(1/3)
	data$iso_voxel_T2s			<- data$T2s_voxel_volume_cubic_mm^(1/3)
	data$iso_voxel_fT2s			<- data$fT2s_voxel_volume_cubic_mm^(1/3)
	data$iso_voxel_dwi			<- data$dwi_voxel_volume_cubic_mm^(1/3)
	data$iso_voxel_swi			<- data$swi_voxel_volume_cubic_mm^(1/3)
	data$iso_voxel_mt				<- data$mt_voxel_volume_cubic_mm^(1/3)
	data$iso_voxel_pd				<- data$pd_voxel_volume_cubic_mm^(1/3)
	data$iso_voxel_other		<- data$other_voxel_volume_cubic_mm^(1/3)

# Step 2) What is 10% of the calculated isotropic voxel length:
	data$iso_voxel_T1_10		<- data$iso_voxel_T1/10
	data$iso_voxel_T2_10		<- data$iso_voxel_T2/10
	data$iso_voxel_T2s_10		<- data$iso_voxel_T2s/10
	data$iso_voxel_fT2s_10	<- data$iso_voxel_fT2s/10
	data$iso_voxel_dwi_10		<- data$iso_voxel_dwi/10
	data$iso_voxel_swi_10		<- data$iso_voxel_swi/10
	data$iso_voxel_mt_10		<- data$iso_voxel_mt/10
	data$iso_voxel_pd_10		<- data$iso_voxel_pd/10
	data$iso_voxel_other_10	<- data$iso_voxel_other/10

# Step 3: The final step is that I subtract the calculated isotropic voxel length
#		from the reported voxel size in the x dimension. If the reported voxel is isotropic,
#		the difference is zero; if the voxel size is different from the calculated
#		isotropic voxel length, this difference will differ from zero.
	data$x_voxel_T1_dif_iso 	<- data$T1_x - data$iso_voxel_T1
	data$x_voxel_T2_dif_iso 	<- data$T2_x - data$iso_voxel_T2
	data$x_voxel_T2s_dif_iso	<- data$T2s_x - data$iso_voxel_T2s
	data$x_voxel_fT2s_dif_iso <- data$fT2s_x - data$iso_voxel_fT2s
	data$x_voxel_dwi_dif_iso 	<- data$dwi_x - data$iso_voxel_dwi
	data$x_voxel_swi_dif_iso 	<- data$swi_x - data$iso_voxel_swi
	data$x_voxel_mt_dif_iso 	<- data$mt_x - data$iso_voxel_mt
	data$x_voxel_pd_dif_iso 	<- data$pd_x - data$iso_voxel_pd
	data$x_voxel_other_dif_iso<- data$other_x - data$iso_voxel_other

# Finally I want to make a dataframe that will be used to summarize the isotropic
#		and anisotropic data
# Select the necessary data from the main dataframe and provide a column name:
	t1_voxel 					<- as.data.frame(data$T1_voxel_volume_cubic_mm)
	names(t1_voxel)		<-	c("mm")
	t2_voxel 					<- as.data.frame(data$T2_voxel_volume_cubic_mm)
	names(t2_voxel)		<-	c("mm")
	t2s_voxel 				<- as.data.frame(data$T2s_voxel_volume_cubic_mm)
	names(t2s_voxel)	<-	c("mm")
	ft2s_voxel 				<- as.data.frame(data$fT2s_voxel_volume_cubic_mm)
	names(ft2s_voxel)	<-	c("mm")
	dwi_voxel 				<- as.data.frame(data$dwi_voxel_volume_cubic_mm)
	names(dwi_voxel)	<-	c("mm")
	swi_voxel 				<- as.data.frame(data$swi_voxel_volume_cubic_mm)
	names(swi_voxel)	<-	c("mm")
	mt_voxel 					<- as.data.frame(data$mt_voxel_volume_cubic_mm)
	names(mt_voxel)		<-	c("mm")
	pd_voxel 					<- as.data.frame(data$pd_voxel_volume_cubic_mm)
	names(pd_voxel)		<-	c("mm")
	other_voxel 			<- as.data.frame(data$other_voxel_volume_cubic_mm)
	names(other_voxel)<-	c("mm")
	invivo 						<- as.data.frame(data$invivo_exvivo_nr)
	names(invivo)			<-	c("invivo")

# Combine the different dataframes into a single one. This dataset (voxel_list)
#		will be used to answer the questions regarding isotropic voxels:
	voxel_list				<-	rbind(t1_voxel,t2_voxel,t2s_voxel,ft2s_voxel,dwi_voxel,swi_voxel,mt_voxel,pd_voxel,other_voxel)
	invivo 						<-	rbind(invivo,invivo,invivo,invivo,invivo,invivo,invivo,invivo,invivo)
	voxel_list$seq		<-	rep(c(1:9), each=length(data[,1]))
	voxel_list				<-	cbind(voxel_list,invivo)
###################

###############################################
# Summarizing the data
###############################################

####################
# Publication information
####################
# 	Range publication years
			range(data$pubyear)

# 	Number of papers
			max(data$study_nr)

# 	Sample sizes
# 	Across all studies
			mean(data$Total_n[!duplicated(data$study_nr)],na.rm=T)
			median(data$Total_n[!duplicated(data$study_nr)],na.rm=T)
			sd(data$Total_n[!duplicated(data$study_nr)],na.rm=T)
			range(data$Total_n[!duplicated(data$study_nr)],na.rm=T)

# 	Sample sizes in vivo
			with(data=data[data$invivo_exvivo_nr==1,],mean(Total_n[!duplicated(study_nr)],na.rm=T))
			with(data=data[data$invivo_exvivo_nr==1,],median(Total_n[!duplicated(study_nr)],na.rm=T))
			with(data=data[data$invivo_exvivo_nr==1,],sd(Total_n[!duplicated(study_nr)],na.rm=T))
			with(data=data[data$invivo_exvivo_nr==1,],range(Total_n[!duplicated(study_nr)],na.rm=T))

# 	Sample sizes post mortem
			with(data=data[data$invivo_exvivo_nr==2,],mean(Total_n[!duplicated(study_nr)],na.rm=T))
			with(data=data[data$invivo_exvivo_nr==2,],median(Total_n[!duplicated(study_nr)],na.rm=T))
			with(data=data[data$invivo_exvivo_nr==2,],sd(Total_n[!duplicated(study_nr)],na.rm=T))
			with(data=data[data$invivo_exvivo_nr==2,],range(Total_n[!duplicated(study_nr)],na.rm=T))

# 	Patient versus controls
			table(data$control_patient[!duplicated(data$study_nr)], useNA="ifany")

# 	Type of patients
			table(data$type_of_patient[!duplicated(data$study_nr)], useNA="ifany")

####################
# MRI Field strength and sequence information
####################
# 	How often are certain field strengths used?
			subset_data <- subset(data, subset=!duplicated(data[c("study_nr","Field_strength")]))
			table(subset_data$Field_strength)

# 	Across all the field strengths and structures, which sequence type is most frequently
#			used?
			table(data$sequence_nr)
####################
# What is the ratio of isotropic voxels versus anisotropic voxel
####################
# 	Check whether _10 is larger or equal to from _dif_iso
#		This means that I check whether the difference between the reported voxel size
#		and the calculated isotropic voxel size is larger than 10% of the calculated
#		In the following "true" means that the reported voxel is (near) isotropic
			table(abs(data$iso_voxel_T1_10) >= abs(data$x_voxel_T1_dif_iso))
			table(abs(data$iso_voxel_T2_10) >= abs(data$x_voxel_T2_dif_iso))
			table(abs(data$iso_voxel_T2s_10) >= abs(data$x_voxel_T2s_dif_iso))
			table(abs(data$iso_voxel_fT2s_10) >= abs(data$x_voxel_fT2s_dif_iso))
			table(abs(data$iso_voxel_dwi_10) >= abs(data$x_voxel_dwi_dif_iso))
			table(abs(data$iso_voxel_swi_10) >= abs(data$x_voxel_swi_dif_iso))
			table(abs(data$iso_voxel_mt_10) >= abs(data$x_voxel_mt_dif_iso))
			table(abs(data$iso_voxel_pd_10) >= abs(data$x_voxel_pd_dif_iso))
			table(abs(data$iso_voxel_other_10) >= abs(data$x_voxel_other_dif_iso))

# 	Summary statistics on voxel volume
# 	To be able to calculate the summary statistics I need to make the values in
#		the voxel list numeric
			voxel_list$mm<- as.numeric(voxel_list$mm)

# 	I provide the voxel volumes separately for structural and functional scans.
# 	In vivo structural
			mean(voxel_list$mm[voxel_list$invivo==1&voxel_list$seq!=4],na.rm=T)
			sd(voxel_list$mm[voxel_list$invivo==1&voxel_list$seq!=4],na.rm=T)
			median(voxel_list$mm[voxel_list$invivo==1&voxel_list$seq!=4],na.rm=T)
			range(voxel_list$mm[voxel_list$invivo==1&voxel_list$seq!=4],na.rm=T)

# 	Post mortem structural
			mean(voxel_list$mm[voxel_list$invivo==2&voxel_list$seq!=4],na.rm=T)
			sd(voxel_list$mm[voxel_list$invivo==2&voxel_list$seq!=4],na.rm=T)
			median(voxel_list$mm[voxel_list$invivo==2&voxel_list$seq!=4],na.rm=T)
			range(voxel_list$mm[voxel_list$invivo==2&voxel_list$seq!=4],na.rm=T)

# 	In vivo fMRI scans
			mean(voxel_list$mm[voxel_list$invivo==1&voxel_list$seq==4],na.rm=T)
			sd(voxel_list$mm[voxel_list$invivo==1&voxel_list$seq==4],na.rm=T)
			median(voxel_list$mm[voxel_list$invivo==1&voxel_list$seq==4],na.rm=T)
			range(voxel_list$mm[voxel_list$invivo==1&voxel_list$seq==4],na.rm=T)

########################
# Anatomy information
########################
# 	How many unique structures have been reported (either visualized or parcellated)
			max(as.numeric(levels(data$structures_nr)))

# 	Summary statistics on the reported frequence of the structures
			data_struc_names <- as.data.frame(sort(table(data$structures_names)))
			data_struc_names
			range(table(data$structures_names))
			mean(table(data$structures_names))
			sd(table(data$structures_names))
			median(table(data$structures_names))
			sort(table(data$structures_names))

# In general all data related to a given structure
	data[data$structures_names=='lateral geniculate thalamic nucleus magnocellular part',]
# Of the structures reported, how many unique structures are there in the in
# 	vivo versus post mortem data?
# 	In vivo
			table(table(as.data.frame(sort(unique(data$structures_nr[data$invivo_exvivo_nr==1])),useNA="ifany")))

# 	Post mortem
			table(table(as.data.frame(sort(unique(data$structures_nr[data$invivo_exvivo_nr==2])),useNA="ifany")))

# 	What is the overlap in visualizing the structures between in vivo and post mortem data O
			length(intersect(data$structures_nr[data$invivo_exvivo_nr==2],data$structures_nr[data$invivo_exvivo_nr==1]))

########################
# Identification and parcellation information
########################
# 	Of the structures reported, how where they identified?
#	 	Were the structures only identified or also parcellated?
			table(data$ident_seg_nr,useNA="ifany")
			table(data$manseg_auto_seg_nr,useNA="ifany")

# 	Which structures are visualised most frequently?
# 		Either using identification
			as.data.frame(sort(table(data$structures_names[data$ident_seg_nr==1])))

# or using a form of parcellation?
# 	Overall, regardless of method
			as.data.frame(sort(table(data$structures_names[data$ident_seg_nr==2])))
# 	Manual parcellation
			as.data.frame(sort(table(data$structures_names[data$ident_seg_nr==2&data$manseg_auto_seg_nr==1])))

# Of the structures parcellated, how many unique structures?
#		Where 0 indicates that the structure is identified, 1 indicates that the structure is segmented
# 	In vivo
			table(table(as.data.frame(sort(unique(data$structures_nr[data$ident_seg_nr==2&data$invivo_exvivo_nr==1])),useNA="ifany")))

# 	Post mortem
			table(table(as.data.frame(sort(unique(data$structures_nr[data$ident_seg_nr==2&data$invivo_exvivo_nr==2])),useNA="ifany")))

# Overlap between in vivo and post mortem?
			length(intersect(data$structures_nr[data$ident_seg_nr==2&data$invivo_exvivo_nr==1],data$structures_nr[data$ident_seg_nr==2&data$invivo_exvivo_nr==2]))

# What are the unique structures that have been reported only with post mortem data?

####################

#############################################################################################
#		3) The R code to generate the summary figures as reported in the paper
#############################################################################################

####################
# "Summary plot"
# Note this figure is NOT reported in the paper as it is a complete visual overkill :)
#		I have added it here as I use it for a quick visual check to see if the data is loaded in
#		correctly.
#		Color coding corresponds to in vivo ~ post mortem; shape corresponds to Identification
#			method; size of shape corresponds to field strenght used.
with(data=data, plot(as.numeric(sequence_nr),structures_nr, col=invivo_exvivo_nr, cex=as.numeric(Field_strength_nr), pch=as.numeric(ident_seg_nr)))
with(data=data, text (sequence_nr, structures_nr, structures_names, pos=4, offset=0.5, cex=0.5))
####################

###############################################
# Fig 3. Visualisation of all data
###############################################
# "Bar plot field strength type"
	freq_bo_strength 			<- 	table(data$Field_strength_nr[!duplicated(data$study_nr)] , useNA="ifany")
	label_bo_strength 		<- 	as.data.frame(c("7.0", "8.0", "9.4",">9.4"))
	dat_bo 								<- 	data.frame(freq_bo_strength, label_bo_strength)
	dat_bo[1] 						<- 	NULL
	names(dat_bo) 				<- 	c("freq_bo_strength","label_bo_strength")
	dat_bo$order 					<- 	c(1:4)

	barplot_fieldstength 	<- 	ggplot(dat_bo, aes(reorder(label_bo_strength,order), freq_bo_strength, fill=label_bo_strength)) +
	  													geom_bar(stat="identity", position="dodge", show.legend=FALSE)+
	  														xlab("B0 field strength")+ylab("Frequency")+
	  															theme(axis.text.x = element_text(angle = 45, hjust = 1))+
																		scale_fill_grey()
####################
# "Bar plot sequence type"
	freq_seq						<-	table(data$sequence_nr, useNA="ifany")
	label_seq 					<-	as.data.frame(c("T1", "T2", "T2s","fT2s","DWI","SWI","MT","PD","Other","Multiple","N.s."))
	dat_seq 						<-	data.frame(freq_seq, label_seq)
	dat_seq[1] 					<- 	NULL
	names(dat_seq) 			<- 	c("Freq","seq")
	dat_seq$order 			<- 	c(1:11)

	barplot_sequence 		<- 	ggplot(dat_seq, aes(reorder(seq,order), Freq, fill=seq)) +
	  												geom_bar(stat="identity", position="dodge", show.legend=FALSE)+
	 						 								xlab("Sequence type")+ylab("Frequency")+
	  														theme(axis.text.x = element_text(angle = 45, hjust = 1))+
																	scale_fill_grey()

####################
# "Bar plot structure count"
	freq_struc_names 		<- table(data$structures_names, useNA="ifany")
	label_struc_names 	<- as.data.frame(levels(data$structures_names))
	datstruc_names 			<- data.frame(freq_struc_names, label_struc_names)
	names(datstruc_names) 		<- c("Var1","Freq","structure")
	dat_struc_names_selection	<- datstruc_names[datstruc_names$Freq >= 10,]
	dat_struc_names_selection	<- droplevels(dat_struc_names_selection)

	barplot_structure 	<- 	ggplot(dat_struc_names_selection, aes(structure, Freq, fill=structure)) +
	  												geom_bar(stat="identity", position="dodge", show.legend=FALSE)+
	  													xlab("Structure name")+ylab("Frequency")+
	  														theme(axis.text.x = element_text(angle = 45, hjust = 1))+
																	scale_fill_grey()

####################
# "Bar plot tissue type"
	freq_tissue_type			<-	table(data$invivo_exvivo_nr, useNA="ifany")
	label_tissue_type 		<-	as.data.frame(c("Invivo", "Post mortem"))
	dat_tissue_type 			<-	data.frame(freq_tissue_type, label_tissue_type)
	dat_tissue_type[1] 		<- 	NULL
	names(dat_tissue_type) 	<- 	c("Freq","tissue_type")
	dat_tissue_type$order <- 	c(1:2)

	barplot_tissue_type		<- 	ggplot(dat_tissue_type, aes(reorder(tissue_type,order), Freq, fill=tissue_type)) +
	  													geom_bar(stat="identity", position="dodge", show.legend=FALSE)+
	 						 									xlab("Tissue type")+ylab("Frequency")+
	  															theme(axis.text.x = element_text(angle = 45, hjust = 1))+
																		scale_fill_grey()

####################
# Plotting the figures
	barplot_fieldstength
	barplot_sequence
	barplot_tissue_type
	barplot_structure

	r_output_summary_figure3 <- plot_grid(barplot_fieldstength,barplot_sequence,barplot_tissue_type,barplot_structure)
	ggsave("r_output_summary_figure3.pdf", width=50, height = 50, units="cm")

# Note that the next step is that I use the saved pdf in adobe illustrator where I do the following:
#		- align the x axis of the sub panels
#		- add labels to the panels
#		- change font size to reduce the crowding of the labels and make stuff subscript where needed
#		- move the title to the top of the subplot
#		- save it as a .tiff
###############################################

###############################################
# Fig 5. The difference in voxel volume for the different contrasts and samples
###############################################
# Plotting voxel sizes
	r_output_summary_figure4	<- ggplot(voxel_list, aes(factor(seq),mm,colour = factor(invivo)))+
																	geom_point(size = 10, alpha=0.2,na.rm=T)+
																		scale_y_continuous(trans = "log",breaks = c(0,0.001,0.01,0.1,1,10))+
																			theme_bw()+xlab("Sequence type")+
																				ylab("log(mm3)")+
																					theme(axis.text.x = element_text(angle = 45, hjust = 1))
	ggsave("r_output_summary_figure5.pdf", width=50, height = 50, units="cm")

# Note that the next step is that I use the saved pdf in adobe illustrator where I do the following:
#		- replace the sequence type numbers in the x-asis with the labels as used in figure 3.
#		- change font size to reduce the crowding of the labels and make stuff subscript where needed
#		- replace the y-axis values from 1e03 -> 0.001 etc as I find it more readable
# 	- add a legend
#		- save it as a .tiff
###############################################

###############################################
# Fig 6-8. A summary figure for a given structure
###############################################

	####################
	# summary per structure
	# To generate a summary figure of the substantia nigra (fig. 6 of the manuscript)
	#   run the following code:
	  structure="thalamus"
	  summary <- with(data=data, structures_nr[structures_names==structure])
	  summary <- data[data$structures_nr==unique(summary),]
	  summary <- as.data.frame(summary)
	# If you want to generate a summary figure of the subthalamic nucleus or thalamus
	#		you need to replace "substantia nigra" with "subthalamic nucleus" or "thalamus"
	####################

	####################
	# To view the list of structures that were reported in the literature more than
	#   once, run the following code in R:
	  as.data.frame(sort(table(data$structures_names)))

	# To change for another structure:
	#	1) select one of the unique names
	#	2) replace the "subthalamic nucleus" with that name
	#	3) Run all code again
	####################

	####################
	# creating variable names
	freq_bo_strength         <- vector()
	freq_tissue_type         <-	vector()
	freq_subject_type        <- vector()
	freq_patient_type	       <- vector()
	freq_visual_type         <- vector()
	freq_seq_type            <- vector()
	###################

	###################
	# Creating the figures
	###################

	####################
	# "Bar plot field strength type"
	  freq_bo_strength[1]    <-  sum(summary$Field_strength_nr==1)  # 7T
	  freq_bo_strength[2]    <-  sum(summary$Field_strength_nr==2)  # 8T
	  freq_bo_strength[3]    <-  sum(summary$Field_strength_nr==3)  # 9.4T
	  freq_bo_strength[4]    <-  sum(summary$Field_strength_nr==4)  # >9.4
	  freq_bo_strength       <-  as.matrix(freq_bo_strength)

		label_bo_strength 		 <- 	as.data.frame(c("7.0", "8.0", "9.4",">9.4"))
		dat_bo 					       <- 	data.frame(freq_bo_strength[,1], label_bo_strength)
		names(dat_bo) 			   <- 	c("freq_bo_strength","label_bo_strength")
		dat_bo$order 			     <- 	c(1:4)

		barplot_fieldstength 	 <- 	ggplot(dat_bo, aes(reorder(label_bo_strength,order), freq_bo_strength, fill=label_bo_strength)) +
		  							                geom_bar(stat="identity", position="dodge", show.legend=FALSE)+
		  							                        scale_y_continuous(limits = c(0,45))+
		  							                                xlab("B0 field strength")+ylab("Frequency")+
		  							                                        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
																																scale_fill_grey()
	###################

	###################
	# "Bar plot tissue type"
	  freq_tissue_type[1]   <-  sum(summary$invivo_exvivo_nr==1)  # In vivo
	  freq_tissue_type[2]   <-  sum(summary$invivo_exvivo_nr==2)  # Post mortem
	  freq_tissue_type      <-  as.matrix(freq_tissue_type)

	  label_tissue_type 		<- 	as.data.frame(c("In vivo", "Post mortem"))
	  dat_tissue_type 			<- 	data.frame(freq_tissue_type, label_tissue_type)
	  names(dat_tissue_type)<- 	c("Freq","tissue_type")
	  dat_tissue_type$order <- 	c(1:2)

		barplot_tissue_type		<- 	ggplot(dat_tissue_type, aes(reorder(tissue_type,order), Freq, fill=tissue_type)) +
		  							              geom_bar(stat="identity", position="dodge", show.legend=FALSE)+
		  							                      scale_y_continuous(limits = c(0,45))+
		 						 	                              xlab("Tissue type")+ylab("Frequency")+
		  							                                    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
																														scale_fill_grey()
	###################

	###################
	# "Bar plot subject type"
	  freq_subject_type[1]    <-  sum(summary$control_patient==1,na.rm=T) # Controls
	  freq_subject_type[2]    <-  sum(summary$control_patient==2,na.rm=T) # Patients
	  freq_subject_type[3]    <-  sum(summary$control_patient==3,na.rm=T) # Both
	  freq_subject_type[4]    <-  sum(is.na(summary$control_patient))     # Not stated
	  freq_subject_type       <-  as.matrix(freq_subject_type)

	  label_subject_type 		  <- 	as.data.frame(c("Control", "Patient", "Both","N.s."))
	  dat_subject_type 			  <- 	data.frame(freq_subject_type, label_subject_type)
	  names(dat_subject_type) <- 	c("Freq","subject_type")
	  dat_subject_type$order  <- 	c(1:4)

		barplot_subject_type	  <- 	ggplot(dat_subject_type, aes(reorder(subject_type,order), Freq, fill=subject_type)) +
		  							                geom_bar(stat="identity", position="dodge", show.legend=FALSE)+
		  							                      scale_y_continuous(limits = c(0,45))+
		 						 	                              xlab("Subject type")+ylab("Frequency")+
		  							                                    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
																														scale_fill_grey()
	###################

	###################
	# "Bar plot patient type"
	  freq_patient_type[1]    <-  sum(summary$type_of_patient==0,na.rm=T) # Controls
	  freq_patient_type[2]    <-  sum(summary$type_of_patient==1,na.rm=T) # Parkinson
	  freq_patient_type[3]    <-  sum(summary$type_of_patient==2,na.rm=T) # Multiple Sclerosis
	  freq_patient_type[4]    <-  sum(summary$type_of_patient==3,na.rm=T) # Huntington
	  freq_patient_type[5]    <-  sum(summary$type_of_patient==4,na.rm=T) # Wilson
	  freq_patient_type[6]    <-  sum(summary$type_of_patient==5,na.rm=T) # Epilepsie
	  freq_patient_type[7]    <-  sum(summary$type_of_patient==6,na.rm=T) # Ataxia
	  freq_patient_type[8]    <-  sum(summary$type_of_patient==7,na.rm=T) # Fetal
	  freq_patient_type[9]    <-  sum(summary$type_of_patient==8,na.rm=T) # Other
	  freq_patient_type[10]   <-  sum(is.na(summary$type_of_patient))     # not stated
		label_patient_type 		  <-	as.data.frame(c("Control","PD","MS","Huntington","Wilson's", "Epilepsie","Ataxia", "Fetal","Other","N.s."))
		dat_patient_type 		    <-	data.frame(freq_patient_type, label_patient_type)

		names(dat_patient_type) <- 	c("Freq","patient_type")
		dat_patient_type$order 	<- 	c(1:10)

		barplot_patient_type	  <- 	ggplot(dat_patient_type, aes(reorder(patient_type,order), Freq, fill=patient_type)) +
		  							                geom_bar(stat="identity", position="dodge", show.legend=FALSE)+
		  							                        scale_y_continuous(limits = c(0,45))+
		 						 	                                xlab("Patient type")+ylab("Frequency")+
		  							                                      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
																															scale_fill_grey()
	###################

	###################
	# "Bar plot visualisation type"
		freq_visual_type[1]		  <-	sum(summary$ident_seg_nr==1,na.rm=T) # Identification
	  freq_visual_type[2]	    <-	sum(summary$ident_seg_nr==2,na.rm=T) # Segmentation
		label_visual_type 		  <-	as.data.frame(c("Identification","Segmentation"))
		dat_visual_type 		    <-	data.frame(freq_visual_type, label_visual_type)

		names(dat_visual_type) 	<- 	c("Freq","visual_type")
		dat_visual_type$order 	<- 	c(1:2)

		barplot_visual_type	    <- 	ggplot(dat_visual_type, aes(reorder(visual_type,order), Freq, fill=visual_type)) +
		  							                geom_bar(stat="identity", position="dodge", show.legend=FALSE)+
		  							                        scale_y_continuous(limits = c(0,45))+
		 						 	                                xlab("Visualisation type")+ylab("Frequency")+
		  							                                      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
																															scale_fill_grey()
	###################

	###################
	# "Bar plot sequence type"
	  freq_seq_type[1]    <-  sum(summary$sequence_nr==1,na.rm=T) # T1
	  freq_seq_type[2]    <-  sum(summary$sequence_nr==2,na.rm=T) # T2
	  freq_seq_type[3]    <-  sum(summary$sequence_nr==3,na.rm=T) # T2s
	  freq_seq_type[4]    <-  sum(summary$sequence_nr==4,na.rm=T) # Functional
	  freq_seq_type[5]    <-  sum(summary$sequence_nr==5,na.rm=T) # DWI
	  freq_seq_type[6]    <-  sum(summary$sequence_nr==6,na.rm=T) # SWI
	  freq_seq_type[7]    <-  sum(summary$sequence_nr==7,na.rm=T) # MT
	  freq_seq_type[8]    <-  sum(summary$sequence_nr==8,na.rm=T) # PD
	  freq_seq_type[9]    <-  sum(summary$sequence_nr==9,na.rm=T) # Other
	  freq_seq_type[10]   <-  sum(summary$sequence_nr==10,na.rm=T)     # multiple
	  freq_seq_type[11]   <-  sum(is.na(summary$sequence_nr))    # not stated

		label_seq 				  <-	as.data.frame(c("T1", "T2", "T2s","Funct","DWI","SWI","MT","PD","Other","Multiple","N.s."))
		dat_seq 				    <-	data.frame(freq_seq_type, label_seq)

		names(dat_seq) 			<- 	c("Freq","seq")
		dat_seq$order 			<- 	c(1:11)

		barplot_sequence 		<- 	ggplot(dat_seq, aes(reorder(seq,order), Freq, fill=seq)) +
		  							            geom_bar(stat="identity", position="dodge", show.legend=FALSE)+
		  							                    scale_y_continuous(limits = c(0,45))+
		 						 	                            xlab("Sequence type")+ylab("Frequency")+
		  							                                  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
																													scale_fill_grey()
	###################

	###################
	# Plot all data in single figure
	#	Note that I only plot the sequence, tissue, subject, and patient type as the
	#		field strength and type of identification is a bit redundant. The other
	#		piece of code to generate the figures interactively will have these extra boxplots
	  plot_grid(barplot_tissue_type, barplot_subject_type,barplot_patient_type,barplot_sequence)
		ggsave("r_output_summary_figure8.pdf", width=50, height = 50, units="cm")
	###################
