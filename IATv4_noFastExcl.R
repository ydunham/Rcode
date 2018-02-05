## R function to read in IAT data and output standard analyzed statistics
##
## NOTE: This is a draft function; no results are guaranteed and all users should take full responsibility to ensure 
##       that this is doing what they want/expect
##
##
## This algorithm is based on Greenwald et al. 2003 revised algorithm but departs from it in a few ways
## Primarily in that it treats compatible and incompatible as a single block each ("5-block" IAT rather than "7-block" IAT)
## It performs the following steps:
## 1. Reads data, drops all trials  > 10000 ms
## 2. Computes mean difference between first and second critical block, divides by pooled SD to create "D-as-is"
## 3. Also computes proportion of error trials and proportion of fast (< 300 ms) trials for each block and in total
##
## USAGE NOTE 1: Algorithm treats blocks in the order of the sort function. Users must manually confirm that positive/negative scores are as intended!
##
## USAGE NOTE 2: Requires reshape2 package to be local for the user
##
## Version 3, 5.29.14
## Changelog: 
## -added output of percent error and percent < 400 ms to the output file, and drops subjects with > 10% fast trials
## -made edits to address issue where a subject with 100% fast trials would break the later merges by being dropped completely
## --all exclusions for missing data are now left in and appear as NA for missing data.
## Version 4, 2.25.15
## Changelog:
## -Now allows user to specify variable names in the call rather than requiring fixed variable names
## -Also rather than dropping participants with lots of fast trials it now marks them for manual exclusion
## 
## Usage: 
## the first item should be the dataframe with the IAT data; users can then specify column names
## corresponding to the two-level block name identifying compatible vs incompatible blocks, the latencies, the subject identifier, and the variable indicating whether the trial was correct (1) or error (0)
##
## Examples:
## IAT(somedata)    # This call requires that columns called subject, latency, block, and correct are present in somedata
## IAT(somedata,subject=subj,block=blockname,latency=rt,correct=cor)   # This call identifies subj, blockname, rt, and cor as the relevant columns for analysis
## D_Scores <- IAT(somedata)  # assigns the output of the scoring algorithm to a new dataframe for subsequent processing


IAT <- function(data,subject=data$subject,block=data$block,latency=data$latency,correct=data$correct)
{
  require("reshape2")  #load reshape2 package. May output an error if this package is not local!
  attach(data)
  data$fast=ifelse(latency<300,1,0)                          #code for fast trials, RT < 300
  latency = ifelse(latency>10000,NA,latency)    					#drop trials with latency > 1000 or < 400
  outmeans=aggregate(latency,by=list(subject,block), FUN=mean,na.rm=TRUE) 	#create block means by subject and block, for 5 block IAT
  outmeans_melt=melt(outmeans,id=c("Group.1","Group.2"))					#melt means dataset for transposing
  subjmeans=dcast(outmeans_melt,Group.1~Group.2+variable)					#caste dataset with separate column for each block mean, one row = 1 subject
  subjmeans$diff=(subjmeans[,3]-subjmeans[,2])							      #create variable for difference in block means (incompatible - compatible, B6-B3)
  subjsd=aggregate(latency,by=list(subject), FUN=sd, na.rm=TRUE) 				#create pooled SD over all trials
  outerr=aggregate(correct,by=list(subject,block), FUN=mean,na.rm=TRUE)   #create mean rate of correct responding by subject and block
  outerr_melt=melt(outerr,id=c("Group.1","Group.2")) 				      #melt err dataset for transposing)
  errmeans=dcast(outerr_melt,Group.1~Group.2+variable)            #caste dataset with separate column for each correct mean, one row = 1 subject
  subjmeans$err.B3 = 1-errmeans[,3]                              #copy over compatible error rate to main output datafile
  subjmeans$err.B6 = 1-errmeans[,2]                              #copy over incompatible error rate to main output datafile
  subjmeans$errmean = (subjmeans$err.B3+subjmeans$err.B6)/2       #create variable for mean overall error rate
  outfast=aggregate(data$fast,by=list(subject,block), FUN=mean,na.rm=TRUE)   #create mean rate of fast trials by subject and block
  outfast_melt=melt(outfast,id=c("Group.1","Group.2")) 				      #melt fasttrial dataset for transposing)
  fastmeans=dcast(outfast_melt,Group.1~Group.2+variable)            #caste dataset with separate column for each correct fastmean, one row = 1 subject
  subjmeans$fast.B3 = fastmeans[,3]                              #copy over compatible error rate to main output datafile
  subjmeans$fast.B6 = fastmeans[,2]                             #copy over incompatible error rate to main output datafilesubjsd=aggregate(reduced$latency,by=list(reduced$subject), FUN=sd) 				#create pooled SD over all trials
  subjmeans$fastmean=(subjmeans$fast.B3+subjmeans$fast.B6)/2    #create variable for mean overall fast trial proportion
  subjmeans$sd=subjsd$x										                       #copy variable over to subjmeans dataframe
  subjmeans$IAT=subjmeans$diff/subjmeans$sd								            #calculate IAT D
  subjmeans$excl=ifelse(subjmeans$fastmean>.1,1,0)                    #mark subjects with > 40% fast trials for exclusion by tagging with excl=1
  names(subjmeans)[names(subjmeans)=="Group.1"] <- "subject"					#rename columns
  colnames(subjmeans)[2] <- "mean.B6"                                 #rename columns
  colnames(subjmeans)[3] <- "mean.B3"                                 #rename columns
  subjmeans											#output results
}