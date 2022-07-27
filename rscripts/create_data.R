##### create allele frequency data for the IGHV website
### projects P1 and P11
## TODO: add datasets for biomed-2 primers

### Run on the server
## get the files of the pre-processing. containing the mutation count and the 3' length filter
# setwd("/work/jenkins/vdjbase_runs_group/naive_final_germline_set")
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
  "upper_range",
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
dbs_filter <- dbs_filter[v_germline_end >= upper_range]
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
write.table(genotypes_fraction_comb, file = gzfile("~/data_allele_fraction_for_website.tsv.gz"), 
            sep = "\t", row.names = F)

alleles_db <- read.delim("data/alleles_db.csv", sep = "\t")

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

write.table(alleles_db, "data/alleles_db_merged.csv", sep = "\t", row.names = F)

data_frac <- data.table::fread("data/data_allele_fraction_for_website.tsv.gz", colClasses = "character")
imgt_alleles <- setNames(alleles_db$or_allele, alleles_db$new_allele)
data_frac$imgt_call <- imgt_alleles[data_frac$call]

data_frac$group <- sapply(data_frac$v_gene, function(x){
  if(x %in% names(merge_group)) merge_group[x] else x
})

data_frac <- setDT(data_frac)
save(data_frac, file = "data/data_frac.rda")



