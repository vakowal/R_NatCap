##########################
# Feature grouping script for Joanna
# Author: Ginger Kowal
# November 11 2014
##########################

data_dir <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Joanna'
out_dir <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Joanna'
features_file <- paste(data_dir, 'Impact_Results.txt', sep = '/')
threshold <- 0.35  # change the cutoff threshold here if desired

# I replaced spaces in the first column ('JOIN_ID') with * because R has 
# trouble reading spaces
features_original <- read.table(features_file, header = TRUE)

# make sure the data table is sorted in the way we expect, to choose the group 'anchors'
features <- features_original[order(features_original$Max_value), ] 

group_anchors <- unique(features[1:20, 'IMPACT'])  # unique values in the first 20 values in the data frame; 
                                                   # these are the features that we will use to
                                                   # 'anchor' group membership

selected_features <- c(1, 4, 5)  # for example: here is where we would add any 'cherry-picked' features you want to include
group_anchors <- unique(c(group_anchors, selected_features))

group_list = list()  # initialize a big list to contain the vectors for all anchors
for (anchor in group_anchors){  # for each anchor
  group <- c()  # initialize a vector to contain the group
  for (i in 1:dim(features)[1]){  # for each row in the features data frame
    if (features[i, 'IMPACT'] == anchor && features[i, 'Max_value'] <= threshold) {  # criteria for group membership
      group <- c(group, features[i, 'CONTROL'])  # add this row's 'CONTROL' value to the group
    }
  }
  group_list[[toString(anchor)]] <- group  # groups are collected in the group_list, indexed by their anchor
}

# now we must test each feature for membership in multiple groups, and remove 
# from all but the first group it appeared in
all_features <- c()
exclude_index = 2
for (anchor in names(group_list)[-length(names(group_list))]) {  # now we can use the indexes of the group_list to access each group individually
  for (member in group_list[[anchor]]) {
    for (anchor_index in exclude_index:length(group_list)) { # for all groups subsequent to this one
      subsequent_group <- group_list[[anchor_index]]
      if (member %in% subsequent_group) {
        group_list[[anchor_index]] <- group_list[[anchor_index]][-(match(member, subsequent_group))]  # remove the member from that group
        all_features <- c(all_features, anchor, group_list[[anchor_index]])
      }
    }
  }
  exclude_index <- exclude_index + 1
}
# delete groups whose anchor belongs to a previous group
prev_members <- c()
for (anchor in names(group_list)) {
  if (anchor %in% prev_members) {
    group_list[[anchor]] <- NULL
  }
  prev_members <- c(prev_members, group_list[[anchor]])
}
# delete groups that have no members remaining
for (anchor in names(group_list)) {
  if (length(group_list[[anchor]]) == 0) {
    group_list[[anchor]] <- NULL
  }
}

# create dataframe to export
export_df <- data.frame(matrix(ncol = 2))
colnames(export_df) <- c('feature', 'group_id')
for (anchor_index in 1:length(group_list)){
  for (member in c(names(group_list)[anchor_index], group_list[[anchor_index]])) {
    row <- c(member, anchor_index)
    export_df <- rbind(export_df, row)
  }
}
# remove rows of NA
export_df <- na.omit(export_df)

# write to csv file for joining in ArcMap
filename <- paste(out_dir, 'group_table.csv', sep = '/')
write.csv(export_df, filename, row.names = FALSE)
