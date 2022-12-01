##### create allele frequency data for the IGHV website
### projects P1 and P11
## TODO: add datasets for biomed-2 primers

### Run on the server
## get the files of the pre-processing. containing the mutation count and the 3' length filter
# setwd("/work/jenkins/vdjbase_runs_group/naive_final_germline_set")
# setwd("/work/jenkins/vdjbase_runs_group/naive_extended")
files <- list.files(".", "genotype_pre_process.tsv.gz", recursive = T, full.names = T)

## there is a subject column in the files. no need to add another.

columns_ <- c(
  "subject",
  "sequence_id",
  "v_germline_end",
  "sequence_alignment",
  "v_call",
  "j_call",
  "productive",
  "c_call",
  #"upper_range",
  "mut_v"
)

dbs <-
  data.table::rbindlist(
    lapply(
      files,
      data.table::fread,
      stringsAsFactors = F,
      select = columns_
    ),
    fill = T
  )

##### filter the dataset to match the number of mutations
### max number is 3 and minimum is 0
### filter to complete sequence
### filter to sequences that are meeting the 3' threshold
### filter multiple gene and allele assignments

dbs_filter <- dbs[mut_v <= 3]
dbs_filter <- dbs_filter[v_germline_end >= 312]
dbs_filter <- dbs_filter[!grepl("^[.]", sequence_alignment)]
dbs_filter[, v_gene := alakazam::getGene(v_call,
                                    first = F,
                                    collapse = T,
                                    strip_d = F)]
data_filter <- dbs_filter[!grepl(",", v_gene)]
data_filter[, v_allele := gsub(
  "(IG[HKL][VDJADEGMC]|TR[ABDG])[A-Z0-9\\(\\)]+[-/\\w]*[*]",
  "",
  v_call,
  perl = T
)]
data_filter <- data_filter[!(grepl(",", v_allele))]
data_filter <- data_filter[!is.na(v_allele)]
data_filter[, v_allele2 := v_allele]


tmp <- lapply(0:3, function(mut_val) {
  #alleles_db <- alleles_dbs$IGH$functional$nonsingle$all$`318`$complete$`95`
  genotypes <- data_filter[mut_v <= mut_val]
  
  genotypes[, n_row_sub := .N, by = subject]
  genotypes[, n_row := .N, by = list(subject, v_gene)]
  # genotypes <-
  #   genotypes[, .(v_allele = unlist(tstrsplit(
  #     v_allele, ",", type.convert = FALSE
  #   ))), by = setdiff(names(genotypes), "v_allele")]
  genotypes[, frac := 1 / 1]
  
  genotypes_fraction <-
    genotypes %>% ungroup() %>% dplyr::group_by(subject, v_gene, v_allele) %>%
    dplyr::summarise(
      count = round(sum(frac), 3),
      freq = round(sum(frac) / unique(n_row), 8),
      freq2 = round(sum(frac) / unique(n_row_sub), 8)
    ) %>% ungroup() %>%  dplyr::arrange(subject, v_gene, desc(freq)) %>%
    dplyr::group_by(subject, v_gene) %>% dplyr::mutate(loc = 1:dplyr::n(),
                                                       project = strsplit(subject, "_")[[1]][1])
  
  hetro_samples <-
    genotypes %>% dplyr::filter(grepl("J6", j_call),!grepl(",", j_call)) %>%
    dplyr::select(subject, j_call) %>% dplyr::group_by(subject) %>% dplyr::mutate(nrow = dplyr::n()) %>% dplyr::ungroup() %>%
    dplyr::group_by(subject, j_call) %>% dplyr::summarise(frac = dplyr::n() /
                                                            nrow) %>% dplyr::slice(1) %>%
    dplyr::group_by(subject) %>% dplyr::summarise(
      frac_J6 = frac[which(frac >= 0.2 & frac <= 0.8)],
      A = sum(frac >=
                0.2 & frac <= 0.8) >= 2,
      J_A = paste0(j_call[which(frac >=
                                  0.2 & frac <= 0.8)])
    )  %>%
    dplyr::filter(A)
  
  genotypes_fraction$J6 <-
    ifelse(genotypes_fraction$subject %in% hetro_samples$subject,
           1,
           2)
  
  genotypes_fraction$J6_TAG <- ""
  
  # style_str <- "white-space: nowrap; border: 1px solid white; background: steelblue; height: 15px;"
  
  for (sub in unique(hetro_samples$subject)) {
    no_check <- "❌ "
    check <- "✅"
    j02 <-
      paste0(
        "</b>J6*02: ",
        ifelse(
          hetro_samples %>% filter(subject == sub, J_A == "IGHJ6*02") %>% pull(frac_J6) %>% length(),
          check,
          no_check
        )
      ) #hetro_samples %>% filter(subject==sub, J_A == "IGHJ6*02") %>% pull(frac_J6), 0)*1000,"px")
    j03 <-
      paste0(
        "</b>J6*03: ",
        ifelse(
          hetro_samples %>% filter(subject == sub, J_A == "IGHJ6*03") %>% pull(frac_J6) %>% length(),
          check,
          no_check
        )
      ) #hetro_samples %>% filter(subject==sub, J_A == "IGHJ6*03") %>% pull(frac_J6), 0)*1000,"px")
    j04 <-
      paste0(
        "</b>J6*04: ",
        ifelse(
          hetro_samples %>% filter(subject == sub, J_A == "IGHJ6*04") %>% pull(frac_J6) %>% length(),
          check,
          no_check
        )
      ) #hetro_samples %>% filter(subject==sub, J_A == "IGHJ6*04") %>% pull(frac_J6), 0)*1000,"px")
    genotypes_fraction$J6_TAG[genotypes_fraction$subject == sub] <-
      paste0(j02, j03, j04)
  }
  
  genotypes_fraction_j6 <-
    genotypes %>% ungroup() %>% filter(subject %in% hetro_samples$subject) %>%
    group_by(subject) %>%
    filter(j_call %in% hetro_samples$J_A[hetro_samples$subject %in% subject]) %>%
    filter(grepl("J6", j_call),!grepl(",", j_call)) %>% ungroup() %>%
    dplyr::group_by(subject, v_gene) %>%
    dplyr::mutate(n_row = n()) %>% ungroup() %>%
    dplyr::group_by(subject, v_gene, v_allele, j_call) %>%
    dplyr::summarise(
      count = round(sum(frac), 3),
      freq = round(sum(frac) / unique(n_row), 8),
      freq2 = round(sum(frac) / unique(n_row_sub), 8)
    ) %>% ungroup() %>%
    dplyr::arrange(subject, v_gene, desc(freq)) %>%
    dplyr::group_by(subject, v_gene) %>%
    dplyr::mutate(loc = 1:dplyr::n(),
                  project = strsplit(subject, "_")[[1]][1])
  
  genotypes_fraction_comb <-
    rbind(genotypes_fraction_j6, genotypes_fraction)
  
  genotypes_fraction_comb$mut <- mut_val
  return(genotypes_fraction_comb)
})

genotypes_fraction_comb <- rbindlist(tmp)

genotypes_fraction_comb$call <- paste0(genotypes_fraction_comb$v_gene, "*", genotypes_fraction_comb$v_allele)
genotypes_fraction_comb$v_allele <- as.character(genotypes_fraction_comb$v_allele)
write.table(genotypes_fraction_comb, file = gzfile("~/data_allele_fraction_for_website_extended.tsv.gz"), 
            sep = "\t", row.names = F)

alleles_db <- data.table::fread("~/Dropbox (BIU)/processpipeline/scripts_docker/alleles_db.csv", data.table = F)

## collapse imgt genes


merge_group <- setNames(c("IGHVF8-G36_37_40","IGHVF8-G36_37_40","IGHVF8-G36_37_40",
                          "IGHVF8-G38_39", "IGHVF8-G38_39",
                          "IGHVF8-G42_43", "IGHVF8-G42_43"),
                        c("IGHVF8-G36","IGHVF8-G37","IGHVF8-G40",
                          "IGHVF8-G38", "IGHVF8-G39",
                          "IGHVF8-G42", "IGHVF8-G43"))
alleles_db$func_group_original <- alleles_db$func_group

alleles_db$func_group <- sapply(alleles_db$func_group, function(x){
  if(x %in% names(merge_group)) merge_group[x] else x
})




alleles_db <- data.table::fread("~/Dropbox (BIU)/processpipeline/scripts_docker/alleles_db.csv", data.table = F)#read.delim("data/alleles_db_merged.csv", stringsAsFactors = F, sep = "\t")
alleles_db$func_group_original <- alleles_db$func_group
# collapse genes that are split into multiple groups
alleles_db$gene <- sapply(strsplit(alleles_db$imgt_allele,"[*]"),"[[",1)

merge_asc_groups <- function(alleles_db, genes=c("IGHV4-4","IGHV4-59")){
  groups <- unique(alleles_db$func_group_original[alleles_db$gene %in% genes])
  groups <- sort(groups)
  fam <- strsplit(groups[1],"G[0-9]")[[1]][1]
  groups_merged <- paste0(gsub(fam,"",groups), collapse = "_")
  groups_merged <- paste0(fam,groups_merged)
  alleles_db$func_group[alleles_db$func_group_original %in% groups] <- groups_merged
  return(alleles_db)
}
alleles_db <- merge_asc_groups(alleles_db, c("IGHV4-4","IGHV4-59"))
alleles_db <- merge_asc_groups(alleles_db, "IGHV4-30-4")
alleles_db <- merge_asc_groups(alleles_db, "IGHV4-34")

write.table(alleles_db, "data/alleles_db_merged.csv", sep = "\t", row.names = F)

data_frac <- data.table::fread("~/Dropbox (BIU)/IGHV_reference_book/data/data_allele_fraction_for_website_extended.tsv.gz", colClasses = "character")
imgt_alleles <- setNames(alleles_db$imgt_allele, alleles_db$new_allele)
data_frac$imgt_call <- imgt_alleles[data_frac$call]

## add novel alleles

missing_calls <- which(is.na(data_frac$imgt_call))
data_frac[missing_calls]$imgt_call <- sapply(missing_calls, function(i){
  gene <- data_frac$v_gene[i]
  allele <- data_frac$v_allele[i]
  base <- strsplit(allele, "_")[[1]][1]
  add <- gsub(base, "", allele)
  iuis_call <- imgt_alleles[paste0(gene,"*",base)]
  paste0(iuis_call,add)
})


data_frac$func_group_original <- data_frac$v_gene
data_frac$group <- data_frac$v_gene
data_frac$gene <- sapply(strsplit(data_frac$imgt_call,"[*]"),"[[",1)


merge_asc_groups2 <- function(alleles_db, data_frac, genes=c("IGHV4-4","IGHV4-59")){
  groups <- unique(alleles_db$func_group_original[alleles_db$gene %in% genes])
  groups <- sort(groups)
  fam <- strsplit(groups[1],"G[0-9]")[[1]][1]
  groups_merged <- paste0(gsub(fam,"",groups), collapse = "_")
  groups_merged <- paste0(fam,groups_merged)
  data_frac$group[data_frac$func_group_original %in% groups] <- groups_merged
  return(data_frac)
}

data_frac <- merge_asc_groups2(alleles_db, data_frac, genes = c("IGHV4-4","IGHV4-59"))
data_frac <- merge_asc_groups2(alleles_db, data_frac, "IGHV4-30-4")
data_frac <- merge_asc_groups2(alleles_db, data_frac, "IGHV4-34")



# data_frac$group <- sapply(data_frac$v_gene, function(x){
#   if(x %in% names(merge_group)) merge_group[x] else x
# })

data_frac <- setDT(data_frac)
save(data_frac, file = "~/Dropbox (BIU)/IGHV_reference_book/data/data_frac.rda")



