# process Century documentation

# original file sources:
# folder C:\Users\Ginger\Dropbox\NatCap_backup\Forage_model\CENTURY4.6\Century46_PC_Jan-2014
# files crop.def, fix.def, graz.def, site.def

# intermediate processed files:
param_list_csv <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/GK_doc/Century_parameter_list_intermediate.csv"
param_def_csv <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/GK_doc/Century_parameter_definitions_intermediate.csv"
param_range_csv <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/GK_doc/Century_parameter_ranges_intermediate.csv"

pdef <- read.csv(param_def_csv, stringsAsFactors=FALSE)
cols <- colnames(pdef)[4:15]
pdef$definition <- do.call(paste, c(pdef[cols], sep=" "))
pdef$definition <- trimws(pdef$definition, which="right")
# concatenate consequetive comments
for(r in seq(nrow(pdef), 1, by=-1)){
  if(nchar(pdef[r, 'parameter']) == 0){
    pdef[r-1, 'definition'] <- paste(pdef[r-1, 'definition'],
                                     pdef[r, 'definition'], collapse=" ")
    }
}
keep_cols <- c('file', 'parameter', 'definition', 'row.index')
pdef_processed <- pdef[(nchar(pdef$parameter) > 0),
                        keep_cols]
pdef_processed <- pdef_processed[pdef_processed$parameter != '***', ]
write.csv(pdef_processed,
           "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/GK_doc/Century_parameter_definitions.csv",
           row.names=FALSE)

# join parameter files together
param_list <- read.csv(param_list_csv, stringsAsFactors=FALSE)
param_list$param_upper <- param_list$parameter
param_list$parameter <- tolower(param_list$param_upper)
param_list <- subset(param_list, select=-c(param_upper, row.index))
pdef_processed[pdef_processed$parameter == "som2ci(1.2)", 'parameter'] <- "som2ci(1,2)"
pdef_processed[pdef_processed$parameter == "som2ci(2.2)", 'parameter'] <- "som2ci(2,2)"
param_list[param_list$parameter == "4-dec", 'parameter'] <- "dec4*"
params <- merge(pdef_processed, param_list)

# parameters to be removed marked in this file
params_to_remove <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/GK_doc/Century_parameters_to_remove.csv")
params <- merge(params, params_to_remove)
params_necc <- params[params$fix_remove != 'yes', ]

# make tables for inclusion in design doc
p_ranges <- read.csv(param_range_csv, stringsAsFactors=FALSE)
params_w_ranges <- merge(params_necc, p_ranges)
params_w_ranges <- subset(params_w_ranges, select=-c(fix_remove))
params_w_ranges$file <- factor(params_w_ranges$file,
                               levels=c("fix.100", "site.100", "crop.100", "graz.100"))
params_w_ranges$range <- as.character(do.call(paste, c(params_w_ranges[c('range.lower.bound', 'range.upper.bound')],
                                                       sep=" - ")))
init_table <- params_w_ranges[params_w_ranges$derivation == 'spin-up', ]
init_cols <- c('parameter', 'definition', 'range',
              'example.value', 'example.value.source')
init_table <- init_table[order(init_table$row.index), init_cols]
colnames(init_table) <- c("Variable name", "Definition", "Valid range",
                         "Example value", "Example value source")
write.csv(init_table, "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/GK_doc/Initialization_variables.csv",
          row.names=FALSE)

p_table <- params_w_ranges[params_w_ranges$derivation == 'parameter', ]
# p_table$possible.values <- gsub(";", ",", p_table$possible.values, fixed=TRUE)
for(r in 1:nrow(p_table)){
  if(nchar(p_table[r, 'possible.values']) > 0){
    p_table[r, 'valid_values'] <- p_table[r, 'possible.values']
  }
  else{
    p_table[r, 'valid_values'] <- p_table[r, 'range']
  }
}
p_cols <- c('parameter', 'definition', 'valid_values', 'example.value',
            'example.value.source', 'file')
p_table <- p_table[order(p_table$file, p_table$row.index), p_cols]
p_table[p_table$parameter=='dec4*', 'parameter'] <- 'dec4'  # mistake in Century docs
# add column for rangeland production model parameter names
p_table$rp_name <- p_table$parameter
p_table$rp_name <- gsub("\\(", "_", p_table$rp_name)
p_table$rp_name <- gsub("\\,", "_", p_table$rp_name)
p_table$rp_name <- gsub("\\)", "", p_table$rp_name)
p_table$char_of <- NA
p_table[p_table$file == 'fix.100', 'char_of'] <- 'site'
p_table[p_table$file == 'site.100', 'char_of'] <- 'site'
p_table[p_table$file == 'crop.100', 'char_of'] <- 'PFT'
colnames(p_table) <- c("Century parameter name", "Definition", "Valid values",
                       "Example value", "Example value source", "Century input file",
                       "Rangeland production model name", "Property of")
write.csv(p_table, "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/GK_doc/Century_parameter_table.csv",
          row.names=FALSE)
