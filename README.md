# williams-kukona-kamide_2018_competition
This repository contains the data and analysis code necessary to reproduce the analyses described in Williams, Kukona, &amp; Kamide (2018) - *Spatial narrative context modulates semantic (but not visual) competition during discourse processing*

Here, you can find a **summary of the results**, including the analysis code in your browser for [Experiment 1](https://github.com/gpwilliams/williams-kukona-kamide_2018_competition/blob/master/output/02_semantic-results.md) and [Experiment 2](https://github.com/gpwilliams/williams-kukona-kamide_2018_competition/blob/master/output/03_visual-results.md).

There are 3 folders for this analysis:

## data: contains 4 data files;

1. **semantic_data.RData** - an R binary data file containing 4 objects associated with the data collected for experiment 1 (semantic competition):

   * **semantic_data**: a data.frame of 22 columns and 6,721,920 rows providing eye-movement data in 5ms time bins for every experimental trial in the experiment. The columns are as follows:
      * *subject*: a factor of unique subject (i.e. participant) IDs.  
      * *trial*: an integer of the unique trial ID (i.e. the order in which trials were played). These start from 5 since we exclude the first 4 practice trials.  
      * *item*: an integer of the unique item ID (i.e. associated with images and scenes).  
      * *condition*: a factor identifying the discourse condition (apart or together).  
      * *play_sound_a*: a numeric variable providing the onset of the first audio file in a given trial (e.g. ‘The piano and the trumpet are in the bar. The carrot and the lantern are in the gallery.’).  
      * *play_sound_b*: a numeric variable providing the onset of the second audio file in a given trial (e.g. 'Supposedly, the piano is exceptionally rare.').  
      * *crit_det*: a numeric variable providing the onset of the determiner prior to the critical noun in play_sound_b (e.g. the).  
      * *crit_noun_on*: a numeric variable providing the onset of the critical noun in play_sound_b (e.g. piano).  
      * *crit_noun_off*: a numeric variable providing the offset of the critical noun in play_sound_b (e.g. piano).  
      * *adverb_on*: a numeric variable providing the onset of the adverb in play_sound_b (e.g. exceptionally).  
      * *adjective_on*: a numeric variable providing the onset of the adjective in play_sound_b (e.g. rare).  
      * *s3_end*: a numeric variable providing the offset of sentence 3 (i.e. the offset of play_sound_b).  
      * *time*: an integer identifying the time in the trial. This increments in 5ms bins.  
      * *sample*: an integer used as a unique sample identifier for the time variable.  
      * *current_fix_index*: an integer used as a unique sample identifier for the current fixation.  
      * *fix_start*: an integer providing the onset of the a given fixation identified in current_fix_index.  
      * *fix_end*: an integer providing the offset of the a given fixation identified in current_fix_index.  
      * *IA*: a factor for the interest area in which a given fixation lands. Note that fixations outside of the interest areas of the target (e.g. piano), distractor (e.g. trumpet), distractor 1 (e.g. carrot), and distractor 2 (e.g. lantern) (labelled t, c, d1, and d2 respectively) are not given a specific label. Here, fixations are labelled `NA` if no fixation has been made, and `.` if it lands outside of the relevant interest areas.  
      * *t*: an binomial integer defining whether or not the target was being fixated on (1) or not(0).  
      * *c*: an binomial integer defining whether or not the competitor was being fixated on (1) or not(0).  
      * *d1*: an binomial integer defining whether or not distractor 1 was being fixated on (1) or not(0).  
      * *d2*: an binomial integer defining whether or not distractor 2 was being fixated on (1) or not(0). 
      
   * **ias**: a vector of characters outlining the interest areas which are coded in the data file (i.e. t, c, d1 and d2 corresponding to the target, competitor, distractor 1, and distractor 2 respectively).  
    
   * **sr**: a number outlining the sample rate with which eye-movements are stored (5ms).  
    
   * **timeM**: Named numbers outlining the average timings for each of the recorded time windows stored in the data (i.e. from play_sound_a to s3_end).  

2. **semantic_demographics.csv** - a csv containing the demographic information associated with the subjects in experiment 1 (semantic competition). The columns are as follows:
 
   * *participant_number*: integers of unique subject (i.e. participant) IDs ranging from 1-66. These map onto the unique IDs in the eye-movement data set.  
   * *gender*: characters indicating the gender of the participant. The unique values here are M (male) and F (female).  
   * *age*: integers providing the age of the participants in years.  
   * *included*: a statement identifying whether (yes) or not (no) the data were included in the analyses. Note that any participants who are not included in the analyses (i.e. participants with included = no) are not saved in the eye-movement data set.  
   * *notes*: a short statement on why any excluded participants were not included in the analyses.  
 
3. **visual_data.RData** - an R binary data file containing 4 objects associated with the data collected for experiment 2 (visual competition). This has the same columns and meaning associated with the column values as in the semantic_data.RData file.

4. **visual_demographics.csv** - a csv containing the demographic information associated with the subjects in experiment 2 (visual competition). This has the same columns and meaning associated with the column values as in the semantic_demographics.csv file.

## output: contains 2 html files and 2 markdown files outlining the analyses in each experiment;

   1. **02_semantic-results.html**: an annotated html file outlining the analyses reported in Experiment 1 (semantic competition), including all R code and output.  
   2. **02_semantic-results.md**: an annotated markdown file outlining the analyses reported in Experiment 1 (semantic competition), including all R code and output. This is used for previewing on GitHub.  
   3. **03_visual-results.html**: an annotated html file outlining the analyses reported in Experiment 2 (visual competition), including all R code and output.  
   4. **03_visual-results.md**: an annotated markdown file outlining the analyses reported in Experiment 2 (visual competition), including all R code and output. This is used for previewing on GitHub.  

## R-files: contains 4 R files used in producing the analyses for Experiment 1 and 2;

1. **01_helper-functions.R**: an R file containing a number of user-defined functions to aid with conducting and reporting analyses in both experiments. These are all commented with a description of what the functions do, what they expect as input, and what they return.
  
2. **02_semantic-results.Rmd**: an Rmd file which describes and performs the analyses for Experiment 1 (semantic competition). Note that the semantic_data.RData file is loaded here.
  
3. **03_visual-results.Rmd**: an Rmd file which describes and performs the analyses for Experiment 2 (visual competition). Note that the visual_data.RData file is loaded here.
  
4. **99_render-all.R**: An R file which loads the required packages and user-defined functions from 01_helper-functions.R and renders the 02_semantic-results.Rmd and 02_visual-results.Rmd files as html output. The packages assigned to required_packages must be installed on the user's machine prior to running the analyses.

Please note that while the loading and saving of external files is executed using a relative file-path system, the ordering of files within each folder is necessary for the R scripts to run (i.e. do not move the data files from the data folder, or rename the folders and files). 
