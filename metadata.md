# Metadata

There are 3 folders for this analysis:

## data: contains 5 data files;

1. **semantic_data.RData** - an R binary data file containing 4 objects associated with the data collected for experiment 1 (semantic competition):

   * **semantic_data**: a data.frame of 22 columns and 6,721,920 rows providing eye-movement data in 5ms time bins for every experimental trial in the experiment. The columns are as follows:
      * *subject*: a factor of unique subject (i.e. participant) IDs.  
      * *trial*: integers of the unique trial IDs (i.e. the order in which trials were played). These start from 5 since we exclude the first 4 practice trials.  
      * *item*: integers of the unique item IDs (i.e. associated with images and scenes).  
      * *condition*: a factor identifying the discourse condition (apart or together).  
      * *play_sound_a*: a numeric variable providing the onset of the first audio file in a given trial (e.g. ‘The piano and the trumpet are in the bar. The carrot and the lantern are in the gallery.’).  
      * *play_sound_b*: a numeric variable providing the onset of the second audio file in a given trial (e.g. 'Supposedly, the piano is exceptionally rare.').  
      * *crit_det*: a numeric variable providing the onset of the determiner prior to the critical noun in play_sound_b (e.g. the).  
      * *crit_noun_on*: a numeric variable providing the onset of the critical noun in play_sound_b (e.g. piano).  
      * *crit_noun_off*: a numeric variable providing the offset of the critical noun in play_sound_b (e.g. piano). 
      * *adverb_on*: a numeric variable providing the onset of the adverb in play_sound_b (e.g. exceptionally).  
      * *adjective_on*: a numeric variable providing the onset of the adjective in play_sound_b (e.g. rare).  
      * *s3_end*: a numeric variable providing the offset of sentence 3 (i.e. the offset of play_sound_b).  
      * *time*: integers identifying the time in the trial. This increments in 5ms bins.  
      * *sample*: integers used as a unique sample identifier for the time variable.  
      * *current_fix_index*: integers used as a unique sample identifier for the current fixation.  
      * *fix_start*: integers providing the onset of the a given fixation identified in current_fix_index.  
      * *fix_end*: integers providing the offset of the a given fixation identified in current_fix_index.  
      * *IA*: a factor for the interest area in which a given fixation lands. Note that fixations outside of the interest areas of the target (e.g. piano), distractor (e.g. trumpet), distractor 1 (e.g. carrot), and distractor 2 (e.g. lantern) (labelled t, c, d1, and d2 respectively) are not given a specific label. Here, fixations are labelled `NA` if no fixation has been made, and `.` if it lands outside of the relevant interest areas.  
      * *t*: binomial integers defining whether or not the target was being fixated on (1) or not(0).  
      * *c*: binomial integers defining whether or not the competitor was being fixated on (1) or not(0).  
      * *d1*: binomial integers defining whether or not distractor 1 was being fixated on (1) or not(0).  
      * *d2*: binomial integers defining whether or not distractor 2 was being fixated on (1) or not(0). 
      
   * **ias**: a vector of characters outlining the interest areas which are coded in the data file (i.e. t, c, d1 and d2 corresponding to the target, competitor, distractor 1, and distractor 2 respectively).  
    
   * **sr**: a number outlining the sample rate with which eye-movements are stored (5ms).  
    
   * **timeM**: Named numbers outlining the average timings for each of the recorded time windows stored in the data (i.e. from play_sound_a to s3_end).  

2. **semantic_demographics.csv** - a csv containing the demographic information associated with the subjects in experiment 1 (semantic competition). The columns are as follows:
 
   * *subject*: integers of unique subject (i.e. participant) IDs ranging from 1-66. These map onto the unique IDs in the eye-movement data set.  
   * *list*: integers indicating the list used in presentation for the items in the Apart condition. This is related to how many objects are mentioned between the target and competitor in the Apart condition, and which distractor is in the same location as the target in the Apart condition. This is used for cross-referencing with the semantic_item_checks.csv data for further analyses requested by reviewers.
   * *gender*: characters indicating the gender of the participant. The unique values here are M (male) and F (female).  
   * *age*: integers providing the age of the participants in years.  
   * *included*: a character vector identifying whether (yes) or not (no) the data were included in the analyses. Note that any participants who are not included in the analyses (i.e. participants with included = no) are not saved in the eye-movement data set.  
   * *notes*: a character vector of short statements on why any excluded participants were not included in the analyses.  

3. **semantic_item_checks.csv** - a csv containing an index of lists for items in the Apart condition in Experiment 1 (semantic competition) used for further analyses. The columns are as follows:
    
    * *item*: integers indicating the unique item ID (i.e. associated with images and scenes).
    * *list*: integers indicating the list used in presentation for the items in the Apart condition. This is cross-referenced with the list assigned to subjects in the semantic_demographics.csv file.
    * *objects_between_mention*: integers indicating how many objects (between 0-2) are mentioned in the time between the mention of the target and competitor (i.e. the surface proximity of the target and competitor in the discourse).
    * *distractor_with_target*: characters indicating which of the two distractors is in the same location as the target (this is always d1 or d2 in the Apart condition as the competitor (c) cannot be in the same location as the target in this condition).
 
4. **visual_data.RData** - an R binary data file containing 4 objects associated with the data collected for experiment 2 (visual competition). This has the same columns and meaning associated with the column values as in the semantic_data.RData file.

5. **visual_demographics.csv** - a csv containing the demographic information associated with the subjects in experiment 2 (visual competition). This has the same columns and meaning associated with the column values as in the semantic_demographics.csv file, excluding the list column (which is only required for additional analyses in Experiment 1(semantic competition)).

## output: contains 3 html files and 3 related markdown files outlining the analyses in each experiment;

   1. **01_semantic-results.html**: an annotated html file outlining the analyses reported in Experiment 1 (semantic competition), including all R code and output.  
   2. **01_semantic-results.md**: an annotated markdown file outlining the analyses reported in Experiment 1 (semantic competition), including all R code and output. This is used for previewing on GitHub.  
   3. **02_visual-results.html**: an annotated html file outlining the analyses reported in Experiment 2 (visual competition), including all R code and output.  
   4. **02_visual-results.md**: an annotated markdown file outlining the analyses reported in Experiment 2 (visual competition), including all R code and output. This is used for previewing on GitHub.  
   5. **03_additional-analyses.html**: an annotated html file outlining the addititional reviewer-requested analyses reported for Experiment 1 (semantic competition), including all R code and output.  
   6. **03_additional-analyses.md**: an annotated markdown file outlining the addititional reviewer-requested analyses reported for Experiment 1 (semantic competition), including all R code and output. This is used for previewing on GitHub. 

## R-files: contains 5 R files used in producing the analyses for Experiment 1 and 2;

1. **00_helper-functions.R**: an R file containing a number of user-defined functions to aid with conducting and reporting analyses in both experiments. These are all commented with a description of what the functions do, what they expect as input, and what they return.
  
2. **01_semantic-results.Rmd**: an Rmd file which describes and performs the analyses for Experiment 1 (semantic competition). Note that the semantic_data.RData file is loaded here.
  
3. **02_visual-results.Rmd**: an Rmd file which describes and performs the analyses for Experiment 2 (visual competition). Note that the visual_data.RData file is loaded here.

4. **03_additional-analyses.Rmd**: an Rmd file which describes and performs the additional reviewer-requested analyses for Experiment 1 (semantic competition). Note that the semantic_data.RData file is loaded here.
  
5. **99_render-all.R**: an R file which loads the required packages and user-defined functions from 01_helper-functions.R and renders the 02_semantic-results.Rmd, 03_visual-results.Rmd, and 04_additional-analyses.Rmd files as html output. The packages assigned to required_packages must be installed on the user's machine prior to running the analyses.

Please note that while the loading and saving of external files is executed using a relative file-path system, the ordering of files within each folder is necessary for the R scripts to run (i.e. do not move the data files from the data folder, or rename the folders and files). 