#############################################################################################
# 			A review on the use of ultra-high field MRI to visualize the subcortex			        #
# 								          R. Script by M.C. Keuken									                      #
#############################################################################################

#############################################################################################
# The goal of this code is to enable others to generate figures such as fig. 6,7, and 8
#		reported in the manuscript. The manuscript itself is a review on the use of UHF
#		MRI to visualze the subcortex.
#
#	Layout of the code:
#		1) Description of the data set
#		2) The R code to generate the figures as reported in the paper for a given structure of
#       interest.
#   3) Additional pieces of code to explore the dataset
#
# Code was tested on an apple computer using the following software and packages:
# 	R (3.4.1)
#		ggplot2 (2.2.1)
# 	grid (3.4.1)
#		cowplot (0.9.1)
#############################################################################################

#############################################################################################
# To get this code to work, there are a few things you need to do:
#   - make sure that all R libraries are installed
#   - change the working directory to the folder where the "Keuken_etal_review_data.csv" file
#       is saved
#############################################################################################

#############################################################################################
# 1) Description of the dataset and used labels
#############################################################################################
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
#		2) The R code to generate the figures as reported in the paper for a given structure of
#       interest.
#############################################################################################
###################
# clean up the R environment
  rm(list=ls())
###################

###################
# Set the required packages
  library(ggplot2)
  library(reshape2)
  library(grid)
  library(cowplot)
###################

###################
# Load in the data
# Change path to where the "Keuken_etal_summary_data.csv" is saved:
	setwd("/Users/mckeuken/Dropbox/Documents/werk/projects/Subcortical_7T_review/analysis/")
	raw_data 				<- read.csv("Keuken_etal_review_data.csv", header=T,sep = ",")
	data 						<- raw_data
###################

####################
# So which subcortical structures have been visualized using UHF-MRI?
# To view this list of structures run the following code:
  as.data.frame(sort(table(data$structures_names)))
# The first column corresponds to the name of the structure, the second
#   column corresponds to the frequency of which this structure has been
#   reported.
# So now you can pick a structure of interest and generate a summary figure using
#   the following code.
####################

####################
# Summary per structure of interest
# To change for another structure:
#	1) select one of the unique names that you just created above (line 99)
#	2) replace the "subthalamic nucleus" with that name
#	3) Run all code again
####################

####################
# To generate a summary figure of the subthalamic nucleus (fig. 7 of the manuscript)
#   run the following code:
  structure="subthalamic nucleus"
  summary <- with(data=data, structures_nr[structures_names==structure])
  summary <- data[data$structures_nr==unique(summary),]
  summary <- as.data.frame(summary)
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
	  							                                        theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
	  							                                    theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
	  							                                    theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
	  							                                      theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
	  							                                      theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
  freq_seq_type[11]   <-  sum(is.na(summary$type_of_patient))    # not stated

	label_seq 				  <-	as.data.frame(c("T1", "T2", "T2s","Funct","DWI","SWI","MT","PD","Other","Multiple","N.s."))
	dat_seq 				    <-	data.frame(freq_seq_type, label_seq)

	names(dat_seq) 			<- 	c("Freq","seq")
	dat_seq$order 			<- 	c(1:11)

	barplot_sequence 		<- 	ggplot(dat_seq, aes(reorder(seq,order), Freq, fill=seq)) +
	  							            geom_bar(stat="identity", position="dodge", show.legend=FALSE)+
	  							                    scale_y_continuous(limits = c(0,45))+
	 						 	                            xlab("Sequence type")+ylab("Frequency")+
	  							                                  theme(axis.text.x = element_text(angle = 45, hjust = 1))
###################

###################
# Plot all data in single figure
  plot_grid(barplot_fieldstength,barplot_sequence,barplot_visual_type, barplot_tissue_type, barplot_subject_type,barplot_patient_type)
###################
#############################################################################################


#############################################################################################
#   3) Additional pieces of code to explore the dataset
#############################################################################################

####################
# For a given structure, which papers report it?
# To change for another structure:
#	1) select one of the unique names that you just created above (line 99)
#	2) replace the "subthalamic nucleus" with that name
#	3) Run the following code:
  which_papers <- data[data$structures_names=='subthalamic nucleus',1:4]
  unique(which_papers)
# Note that if you want to view the entire dataset per structure, remove the "1:4" part
#   of the code above.
####################
