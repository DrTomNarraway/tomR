
#' @title Return Rounded Seconds from Milliseconds
#'
#' @description Divide milliseconds by 1000 and then round to specified decimal places.
#' @param milliseconds Data to convert to seconds. Expects numeric.
#' @param dp Optional. Number of decimal places to round to. Defaults to 3.
#' @returns A data frame of rounded seconds of the same type as the input data.
return_rounded_seconds<-function(milliseconds, dp=3){
  return(round(milliseconds/1000, dp))
}

#' @title Jatos to Data
#'
#' @description Convert the messy JATOS csv file to a standardized data frame.
#' @param path Path to the folder where the JATOS file is stored. Used to find the first available file with 'jatos_results' in the title.
#' @param subject_col The current name of the subject column.
#' @param outcome_col The current name of the outcome / accuracy column.
#' @param rt_col The current name of the response time column.
#' @param block_col Optional. The current name of the block number or id column.
#' @param deadline_col Optional. The Current name of the deadline column.
#' @param difficulty_col Optional. The current name of the difficulty column.
#' @param group_col Optional. The current name of the between-subjects ID column.
#' @param sat_col Optional. The current name of the speed-accuracy within-subjects ID column.
#' @param trial_col Optional. The current name of the trial number column.
#' @param sat_conditions Optional. List to rename SAT conditions from their internal names to more useful ones. Must be listed in format 'current'='new'.
#' @returns A data frame with columns Subject, Outcome, and RT, which optimally includes Group, Block, Trial, SAT, Deadline, Difficulty.
return_data_from_jatos<-function(path, subject_col, outcome_col, rt_col, block_col='', deadline_col='', difficulty_col='', group_col='', sat_col='', sat_conditions=list(), trial_col=''){
  if (substr(path, nchar(path)-3, nchar(path)) %in% c('.csv','.txt')) {
    p <- path
  } else if (substr(path, nchar(path), nchar(path)) == '/') {
    files <- list.files(path)[grep('results', list.files(path))]
    target <- files[grep('jatos', files)]
    p <- paste0(path,target)
    message(paste0('Your path lead to a folder so I am assuming you want this file: ',target))
  } else {
    stop('The path you passed did not lead to a .csv or .txt file, or a folder.')
  }
  r<-utils::read.csv(file=p)
  d<-as.data.frame(r)
  # rename required columns
  colnames(d)[colnames(d)==subject_col]<-'Subject'
  d$Subject<-as.character(d$Subject)
  colnames(d)[colnames(d)==outcome_col]<-'Outcome'
  d$Outcome<-as.numeric(d$Outcome)
  colnames(d)[colnames(d)==rt_col]<-'RT'
  d$RT<-as.numeric(d$RT)
  # rename provided optional columns
  if (group_col != '') {
    message(paste0('Changing the column name ',group_col,' to Group.'))
    colnames(d)[colnames(d)==group_col]<-'Group'
    d$Group<-as.character(d$Group)
  }
  if (block_col != '') {
    message(paste0('Changing the column name ',block_col,' to Block.'))
    colnames(d)[colnames(d)==block_col]<-'Block'
    d$Block<-as.numeric(d$Block)
  }
  if (sat_col != '') {
    message(paste0('Changing the column name ',sat_col,' to SAT.'))
    colnames(d)[colnames(d)==sat_col]<-'SAT'
    d$SAT<-as.character(d$SAT)
  }
  if (deadline_col != '') {
    message(paste0('Changing the column name ',deadline_col,' to Deadline.'))
    colnames(d)[colnames(d)==deadline_col]<-'Deadline'
    d$Deadline<-round(as.numeric(d$Deadline),3)
  }
  if (difficulty_col != '') {
    message(paste0('Changing the column name ',difficulty_col,' to Difficulty.'))
    colnames(d)[colnames(d)==difficulty_col]<-'Difficulty'
    d$Difficulty<-as.numeric(d$Difficulty)
  }
  if (trial_col != '') {
    message(paste0('Changing the column name ',trial_col,' to Trial.'))
    colnames(d)[colnames(d)==trial_col]<-'Trial'
    d$Trial<-as.numeric(d$Trial)
  }
  # process optional renaming of sat conditions
  if (length(sat_conditions) > 0) {
    for(i in 1:length(sat_conditions)) {
      old_cond<-names(sat_conditions[i])
      new_cond<-sat_conditions[[i]]
      d$SAT[d$SAT == old_cond]<-new_cond
    }
  } else { d$SAT<-as.character(d$SAT) }
  # return final data frame
  d<-d[d$Subject != subject_col, colnames(d) %in% c('Subject', 'Group', 'Block', 'Trial', 'SAT', 'Deadline', 'Difficulty', 'Outcome', 'RT')]
  return(d)
}

#' @title Meanify
#'
#' @description Group and summarise a data.frame into a meanified version.
#' @param data The data.frame to meanify.
#' @param ... The columns to group by.
#' @param rt.col The current name of the RT column. Will be renamed 'RT'.
#' @param score.col The current name of the score column. Will become the 'PC' (percentage correct) column.
meanify <- function(data, ..., rt.col='RT', score.col='Score') {
  out <- dplyr::group_by(.data=data, ...)
  colnames(out)[colnames(out)==rt.col] = 'RT'
  colnames(out)[colnames(out)==score.col] = 'Score'
  out <- dplyr::summarise(
    .data = out,
    .groups = 'keep',
    RT = mean(RT),
    PC = mean(Score)
  )
  return(out)
}
