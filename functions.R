pacman::p_load('dplyr', 'tidyr', 'htmltools', 'bbplot', 'scales',
               'ggplot2', 'rdrop2', 'shiny', 'BiocManager', 'DECIPHER',
               'dendextend', 'data.table', 'Biostrings', 'alakazam', "unikn", 
               'plotly', "jcolors", 'ggdendro', "RColorBrewer","kmer","heatmaply", install = F)

load("data/data_frac.rda")
data <- data_frac
allele_db <- read.delim("data/alleles_db_merged.csv", stringsAsFactors = F, sep = "\t")
absolute_thresholds_dict <- read.delim("data/alleles_db_merged.csv", stringsAsFactors = F, sep = "\t")


absolute_thresholds_dict <- sapply(unique(absolute_thresholds_dict$func_group), function(x){
  tmp <- absolute_thresholds_dict[absolute_thresholds_dict$func_group==x,]
  setNames(tmp$thresh,gsub("IGH","",tmp$or_allele))
})

vgerms <- tigger::readIgFasta("data/reference.fasta")

### count the allele appearance in both cohorts. 
allele_appearance <- function(data_, g_group, chain = "IGH") {
  
  data_ <- data_[grepl(g_group, group) & mut == 3 & is.na(j_call),]
  height = (length(unique(data_$imgt_call)))
  height = ifelse(length(height)<20, 25, height)
  height = height*30
  
  p <- ggplot(data_, aes(imgt_call, text = paste0("Group Call: ",call))) + 
    geom_bar() + coord_flip() + facet_wrap(. ~ project, nrow = 3) +
    labs(x = "allele", y = "# Individuals", fill = "") + theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(
        size = 12
      ),
      axis.text.y = element_text(
        size = 12
      ), axis.title = element_text(size = 14)
    )
  ggplotly(p, height = height, width = height*1.25)
}

## show the sequence depth of each sample per group
sequence_depth <- function(data_, g_group, allele_db) {
  
  data_ <- data_[grepl(g_group, group) & mut == 3 & is.na(j_call),]
  data_[, text := paste(
    '</br>Project: ',
    project,
    '</br>Subject: ',
    subject,
    '</br>Alleles: ',
    imgt_call,
    '</br>Group Call: ',
    call,
    '</br># assignments: ',
    count,
    '</br>Relative freq.: ',
    round(as.numeric(freq), 4),
    '</br>Relative Rep. freq.: ',
    round(as.numeric(freq2), 4)
  )]
  
  width = (length(unique(data_$imgt_call)))
  width = ifelse(length(width)<20, 25, width)
  width = width*30
  
  pp <- ggplot(data_, aes(x = imgt_call, y = as.numeric(count), text = text)) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_jitter(alpha = 0.9, color = "gray", fill = "yellow", shape = 21) + 
    facet_grid(project~.) + 
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
    labs(y = "# Sequences", x = "")
  p_list <- ggplotly(pp, tooltip = "text", height = 600, width = width)
  return(p_list)
}

##### plot the dendogram and the alignment

seq_align <-
  function(v_calls,
           allele_db,
           vgerms,
           g_group) {
    alleles <-
      allele_db %>% dplyr::filter(new_allele %in% v_calls) %>% dplyr::pull(or_allele)
    alleles <- unlist(strsplit(alleles,"/"))
    
    new_alleles <- sapply(alleles, function(x){
      allele_db$new_allele[grepl(paste0(gsub("[*]","[*]",x),"($|/)"),allele_db$or_allele)]
    })
    
    sequences <- vgerms[alleles]
    names(sequences) <- paste0(names(sequences),"(",new_alleles[names(sequences)],")")
    
    vgerm_dist <- gsub("[.]", "-", sequences)
    vgerm_dnaset <- DNAStringSet(vgerm_dist)
    mat_sub <-
      DistanceMatrix(
        vgerm_dnaset,
        includeTerminalGaps = FALSE,
        penalizeGapGapMatches = FALSE,
        penalizeGapLetterMatches = T,
        verbose = F
      )
    
    if(length(mat_sub)==1) return(NULL)
    colnames(mat_sub) <-  gsub("IGH", "", colnames(mat_sub))
    rownames(mat_sub) <-  gsub("IGH", "", rownames(mat_sub))
    
    matrix_sequences <-
      as.data.frame(sapply(sequences, seqinr::s2c), stringsAsFactors = F)

    nucs <-
       nrow(matrix_sequences) - sum(apply(matrix_sequences, 1, function(x)
         all(x == ".")))
    if (length(alleles) < 3) {
      hc <- hclust(as.dist(mat_sub))
    } else{
      hc <- ape::nj(as.dist(mat_sub))
      hc <- ape::ladderize(hc)
    }
    dend <- as.dendrogram(hc)
    dend <- dendextend::set(dend, "labels_cex", 0.5)
    ggd1 <- as.ggdend(dend)
    ggd1$labels$y <- ggd1$labels$y-0.01

    p_dend <- ggplot(ggd1, horiz = T,  theme = NULL)  +
      theme(
        axis.line = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),axis.ticks.x = element_line(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        panel.grid.minor = element_line(color = "gray"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none"
      ) +
      scale_y_continuous(limits = c(-0.035, NA), sec.axis = sec_axis(~ . * nucs, name = "Mutations")) +
      ylab("Ratio")
    
    pg <- ggplotly(p_dend)%>% layout(margin = list(l = 75))
    return(pg)
  }

seq_align2 <-
  function(v_calls,
           allele_db,
           vgerms,
           g_group) {
    alleles <-
      allele_db %>% dplyr::filter(new_allele %in% v_calls) %>% dplyr::pull(or_allele)
    alleles <- unlist(strsplit(alleles,"/"))
    new_alleles <- sapply(alleles, function(x){
      allele_db$new_allele[grepl(paste0(gsub("[*]","[*]",x),"($|/)"),allele_db$or_allele)]
    })
    
    sequences <- vgerms[alleles]
    names(sequences) <- paste0(names(sequences),"(",new_alleles[names(sequences)],")")
    
    matrix_sequences <-
      as.data.frame(sapply(sequences, seqinr::s2c), stringsAsFactors = F)
    
    nucs <-
      nrow(matrix_sequences) - sum(apply(matrix_sequences, 1, function(x)
        all(x == ".")))
    
    snps <-
      which(apply(matrix_sequences, 1, function(x)
        length(unique(x)) != 1))
    
    matrix_sequences$annot <-
      apply(matrix_sequences, 1, function(x)
        length(unique(x)) != 1)
    matrix_sequences$pos <- 1:318
    matrix_sequences_plot <-
      reshape2::melt(matrix_sequences, id.vars = c("pos", "annot"))
    matrix_sequences_plot$id <- matrix_sequences_plot$pos
    matrix_sequences_plot$allele <-
      gsub("IGH", "", matrix_sequences_plot$variable)
    matrix_sequences_plot$allele <-
      factor(matrix_sequences_plot$allele,
             levels = unique(matrix_sequences_plot$allele))
    # matrix_sequences_plot$value[matrix_sequences_plot$value == "."] <-
    #   NA
    matrix_sequences_plot$annot_text <-
      sapply(1:nrow(matrix_sequences_plot), function(i)
        ifelse(
          matrix_sequences_plot$annot[i],
          matrix_sequences_plot$value[i],
          ""
        ))
    
    
    
    hotspot <- c()
    if (length(snps) != 0) {
      for (s in snps) {
        ht_snp <- c()
        for (i in 1:(ncol(matrix_sequences) - 2)) {
          if (s > 3)
            ht_snp <- c(ht_snp,
                        grepl("G[TC][AT]",
                              paste0(matrix_sequences[(s - 1):(s + 2), i], collapse = "")) |
                          grepl("[AT][AG]C",
                                paste0(matrix_sequences[(s -
                                                           2):(s + 1), i], collapse = "")))
        }
        if (any(ht_snp))
          hotspot <- c(hotspot, s)
      }
    }
    plot_align <-
      function(dt,
               low_bound = 1,
               upper_boud = 80,
               hotspot) {
        if (length(hotspot) != 0)
          ht <-
            hotspot[which(as.numeric(hotspot) %in% low_bound:upper_boud)]
        else
          ht <- NULL
        p <- ggplot(dt[dt$id >= low_bound &
                           dt$id < upper_boud, ]) +
          geom_tile(aes(
            x = (pos),
            y = (allele),
            fill = value
          ), colour = "white") +
          geom_text(aes(
            x = (pos),
            y = (allele),
            label = annot_text
          ), color = "black") +
          #coord_equal(expand = F, xlim = c(low_bound, upper_boud), ratio = 9/5, clip = "off") +
          #bbplot::bbc_style() +
          scale_fill_manual(values = setNames(
            c(unname(jcolors("pal2")[c(1, 3, 4, 5)]), "gray50"),
            c("A","C","G","T",".")
            )) +
          theme_align # "#1380A1", "#FAAB18", "#990000", "#588300"
        
        if (!is.null(ht)) {
          for (h in ht) {
            df_rect <- data.frame(ymin = 1,
                                  ymax = length(unique(dt$allele)),
                                  x = h)
            
            
            p <- p + geom_rect(
              data = df_rect,
              size = 1,
              fill = NA,
              linejoin = "bevel",
              lty = 1,
              colour = jcolors("pal2")[2],
              aes(
                xmin = x - 0.5,
                xmax = x + 0.5,
                ymin = ymin - 0.5,
                ymax = ymax + 0.5
              )
            )
          }
        }
        
        p <- ggplotly(p, tooltip = "none")
        
        if (low_bound == 1)
          p <- p %>% layout(legend = list(orientation = "h", x = 0, y = 1.1))
        return(p)
      }
    
    theme_align <- theme(
      axis.line = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(
        size = 16
        # angle = 0,
        # hjust = 0.5,
        # vjust = 0.5
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      legend.position = "none"
    )
    
    
    
    p_list <- apply(data.frame(
      low_bound = seq(1, 318, by = 80),
      upper_bound = c(seq(81, 318, by = 80), 319)
    ),
    1, function(x) {
      plot_align(matrix_sequences_plot, x[1], x[2], hotspot)
    })
    
    
    #subplot(p_list, nrows = 4)
    p_list
    # p1 <- cowplot::plot_grid(plotlist = p_list,
    #                          nrow = 4,
    #                          align = "v")
    # return(plot(p1))
    #p_list_plotly <- lapply(p_list, ggplotly)
    
  }

rect.dendrogram2 <-
  function (tree,
            k = NULL,
            which = NULL,
            x = NULL,
            h = NULL,
            border = 2,
            cluster = NULL,
            horiz = FALSE,
            density = NULL,
            angle = 45,
            text = NULL,
            text_cex = 1,
            text_col = 1,
            xpd = TRUE,
            lower_rect,
            upper_rect = 0,
            prop_k_height = 0.5,
            stop_if_out = FALSE,
            ...)
  {
    if (!is.dendrogram(tree))
      stop("x is not a dendrogram object.")
    if (length(h) > 1L | length(k) > 1L) {
      stop("'k' and 'h' must be a scalar(i.e.: of length 1)")
    }
    tree_heights <- heights_per_k.dendrogram(tree)[-1]
    tree_order <- order.dendrogram(tree)
    if (!is.null(h)) {
      if (!is.null(k)) {
        stop("specify exactly one of 'k' and 'h'")
      }
      ss_ks <- tree_heights < h
      k <- min(as.numeric(names(ss_ks))[ss_ks])
      k <- max(k, 2)
    }
    else if (is.null(k)) {
      stop("specify exactly one of 'k' and 'h'")
    }
    if (k < 2 | k > length(tree_heights)) {
      if (stop_if_out) {
        stop(gettextf("k must be between 2 and %d", length(tree_heights)),
             domain = NA)
      }
      else {
        warning(gettextf("k must be between 2 and %d", length(tree_heights)),
                domain = NA)
      }
    }
    if (is.null(cluster)) {
      cluster <- cutree(tree, k = k)
    }
    clustab <- table(cluster)[unique(cluster[tree_order])]
    m <- c(0, cumsum(clustab))
    if (!is.null(x)) {
      if (!is.null(which)) {
        stop("specify exactly one of 'which' and 'x'")
      }
      which <- x
      for (n in seq_along(x))
        which[n] <- max(which(m < x[n]))
    }
    else if (is.null(which)) {
      which <- 1L:k
    }
    if (any(which > k)) {
      stop(gettextf("all elements of 'which' must be between 1 and %d",
                    k),
           domain = NA)
    }
    border <- rep_len(border, length(which))
    retval <- list()
    old_xpd <- par()["xpd"]
    par(xpd = xpd)
    for (n in seq_along(which)) {
      next_k_height <- tree_heights[names(tree_heights) ==
                                      k + 1]
      if (length(next_k_height) == 0) {
        next_k_height <- 0
        prop_k_height <- 1
      }
      if (!horiz) {
        xleft <- m[which[n]] + 0.66
        if (missing(lower_rect)) {
          lower_rect <- -max(strheight2(labels(tree)))
          dLeaf <- -0.75 * strheight("x")
          extra_space <- -strheight2("_")
          lower_rect <- lower_rect + dLeaf + extra_space
        }
        ybottom <- lower_rect
        xright <- m[which[n] + 1] + 0.33
        ytop <- tree_heights[names(tree_heights) == k] *
          prop_k_height + next_k_height * (1 - prop_k_height) +
          upper_rect
      }
      else {
        ybottom <- m[which[n]] + 0.66
        if (missing(lower_rect)) {
          lower_rect <- min(strwidth(labels(tree)))
          dLeaf <- 0.75 * strwidth("w")
          extra_space <- strwidth("_")
          lower_rect <- lower_rect + dLeaf + extra_space
        }
        xright <- lower_rect
        ytop <- m[which[n] + 1] + 0.33
        xleft <- tree_heights[names(tree_heights) == k] *
          prop_k_height + next_k_height * (1 - prop_k_height) +
          upper_rect
      }
      rect(
        xleft,
        ybottom,
        xright,
        ytop,
        border = border[n],
        density = density,
        angle = angle,
        ...
      )
      if (!is.null(text)) {
        text((m[which[n]] + m[which[n] + 1] + 1) / 2,
             ytop + 0.01,
             text[n],
             cex = text_cex,
             col = text_col)
      }
      retval[[n]] <-
        which(cluster == as.integer(names(clustab)[which[n]]))
    }
    par(xpd = old_xpd)
    invisible(retval)
  }


source_haplo_usage <- function(g_group, allele_thresh) {
  cat(
    '<style>

#scroll-box {
  overflow-y: scroll;
  overflow-x: scroll !important;
}
</style>\n\n',
'<div class="container">',
'<iframe id="scroll-box"
  src=', paste0("https://peresay.shinyapps.io/relative_usage_haplo_new/?g_group=%22",
                g_group,
                "%22"),
' scrolling="yes" border="0" frameborder="0" style="border-style:none;box-shadow:0px 0px 2px 2px rgba(0,0,0,0.2); width: 100%;height:800px;overflow-x: scroll !important;overflow: scroll;" cellspacing="0">
  </iframe>\n\n</div>', sep = ""
  )
}

source_haplo_usage_specific <- function(g_group, allele_thresh) {
  cat(
    '<style>

#scroll-box {
  overflow-y: scroll;
  overflow-x: scroll !important;
}
</style>\n\n',
'<div class="container">',
'<iframe id="scroll-box"
  src=', paste0(
    " https://peresay.shinyapps.io/absolute_usage_app/?g_group=%22",
    g_group,
    "%22&allele_thresh=%22",
    allele_thresh,
    "%22"
  ),
' scrolling="yes" border="0" frameborder="0" style="border-style:none;box-shadow:0px 0px 2px 2px rgba(0,0,0,0.2); width: 100%;height:800px;overflow-x: scroll !important;overflow: scroll;" cellspacing="0">
  </iframe>\n\n</div>', sep = ""
  )
}

heatmap_alleles <-
  function(data_, g_group = "IGHVF5-G30", allele_db) {
    
    
    data_ <- data_[grepl(g_group, group) & mut == 0 & is.na(j_call),]
    
    threhsolds <- setNames(allele_db$thresh[allele_db$func_group==g_group],
                           allele_db$or_allele[allele_db$func_group==g_group])
    
    alleles_db_l <- setNames(allele_db$thresh, allele_db$new_allele)
    data_ <-
      data_[, absolute_thresh := alleles_db_l[call]]
    
    data_cluster <- data_[as.numeric(freq2) >= absolute_thresh,]
    
    n_alleles <- data_cluster$imgt_call %>% unique() %>% length()#allele_db %>% filter(func_group == g_group) %>% pull(or_allele) %>% unique() %>% length()
    alleles <- data_cluster$imgt_call %>% unique()#allele_db %>% filter(func_group == g_group) %>% pull(or_allele) %>% unique()
    subjects <- unique(data_cluster$subject)
    allele_show <-
      matrix(
        0,
        nrow = length(subjects),
        ncol = n_alleles,
        dimnames = list(subjects, alleles)
      )
    
    for (samp in subjects) {
      tmp <- data_cluster[subject == samp]
      alleles <- unique(tmp$imgt_call)
      allele_show[samp, alleles] <- 1
    }
    
    p <- heatmaply(
      allele_show,
      dendrogram = ifelse(n_alleles>1,"row","none"),
      xlab = "",
      ylab = "",
      main = "",
      colors = c("white", "black"),
      grid_color = "gray",
      grid_width = 0.0001,
      titleX = FALSE,
      hide_colorbar = TRUE,
      branches_lwd = 0.5,
      fontsize_row = 12,
      fontsize_col = 12,
      label_names = c("Subject", "Allele", "Found"),
      labCol = colnames(allele_show),
      labRow = rownames(allele_show),
      heatmap_layers = theme(axis.line = element_blank())
    )
    
    hline <- function(y = 0,
                      color = "red",
                      x0 = 0,
                      x1 = 1) {
      list(
        type = "line",
        x0 = x0,
        x1 = x1,
        xref = "xaxis",
        y0 = y,
        y1 = y,
        line = list(color = color, dash = "dot"),
        text = formatC(as.numeric(y),format = "e")
      )
    }
    
    
    data_$v_allele_axis2 <-
      factor(data_$imgt_call, unique(data_$imgt_call))
    data_$v_allele_axis3 <-
      as.numeric(data_$v_allele_axis2)
    
    data_ <- data_ %>% dplyr::arrange(desc(as.numeric(freq2))) %>%
      dplyr::group_by(subject) %>% 
      dplyr::mutate(zygousity_state = n()) %>% 
      arrange(subject) %>% rowwise() %>% 
      mutate(
        pass = as.numeric(freq2)>=absolute_thresh,
        freq3 = formatC(as.numeric(freq2),format = "e"))
    
    ticktext <- levels(data_$v_allele_axis2)
    tickvals <- 1:length(ticktext)
    
    plotly2 <-
      data_ %>%
      highlight_key(., ~ subject) %>%
      plotly::plot_ly() %>%
      plotly::add_trace(
        type = "scatter",
        x = ~ jitter(v_allele_axis3),
        y = ~ as.numeric(freq2),
        symbol = ~ pass,
        mode = 'markers',
        color = ~ as.factor(project),
        showlegend = TRUE,
        opacity = 0.9,
        hoverinfo = 'text',
        hovertext = paste0("Allele fraction: ",data_$freq3)
        #legendgroup = ~ project
      ) %>%
      plotly::add_trace(
        x = ~ v_allele_axis3,
        y = ~ as.numeric(freq2),
        type = "box",
        hoverinfo = "none",
        fillcolor = "transparent",
        showlegend = FALSE
      ) %>%
      plotly::layout(
        hovermode = 'closest',
        shapes = lapply(1:length(ticktext), function(ia) {
          a = ticktext[ia]
          xx = tickvals
          hline(
            ifelse(is.na(as.numeric(threhsolds[a])), 0.0001, as.numeric(threhsolds[a])),
            x0 = xx[ia] -
              0.25,
            x1 = xx[ia] + 0.25,
            color = "red"
          )
        }
        ),
        legend = list(orientation = "h", x = 0, y = 1.1),
        xaxis = list(
          title = paste0("Alleles"),
          tickfont = list(size = 16, color = "rgba(77,77,77,1)"),
          tickangle = -90,
          range = c(.5, n_alleles + 0.5),
          autotick = F,
          tickmode = "array",
          tickvals = tickvals,
          ticktext = ticktext
        ),
        yaxis = list(title = "Rep.\nnormalization", range = c(0, NULL))
        #,range = c(0, 0.5))
      ) %>%
      plotly::highlight(
        on = "plotly_click",
        opacityDim = 0.3,
        off = "plotly_doubleclick",
        selected = attrs_selected(showlegend = T),
        persistent = F
      )
    # %>% add_annotations(x = tickvals-0.2, y = 0.001, 
    #                             text = as.character(threhsolds), 
    #                             textfont = list(color = '#000000'))
    # plotly2$x$data <- lapply(plotly2$x$data, FUN = function(x){
    #   if(x$marker$line$color=="rgba(0,0,0,1)") x$marker = list(opacity = 0)
    #   return(x)
    # })
    subplot(
      subplot(
        plotly2,
        plotly_empty(),
        widths = c(0.82, 0.18),
        which_layout = 1
      ),
      plotly_empty(),
      p, heights = c(1/5, 1/5, 3/5),
      nrows = 3,
      margin = 0.04,
      which_layout = 1
    )
    
    #return(list(plotly2, p))
  }
