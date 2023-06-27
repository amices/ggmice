#' (Clustered) missingness proportion per variable
#'
#' With this function, you can quickly check the number op subjects per level for 
#' categorical variables and in total for continuous data. It creates a heat map 
#' in which red means little subjects per group and blue numerous subjects per group.
#'
#' @param data A dataset of class `data.frame`, `tibble`, or `matrix`.
#' @param vrb String, vector, or unquoted expression with variable name(s), default is "all".
#' @param label Logical indicating whether correlation values should be displayed.
#' @param square Logical indicating whether the plot tiles should be squares.
#' @param diagonal Logical indicating whether the correlation of each variable with itself should be displayed.
#' @param rotate Logical indicating whether the variable name labels should be rotated 90 degrees.
#' @param useNA String, indicating whether to include a separate level for missing values. Options are "no" to exclude them from the plot, "both" to include both variables and missings and "only" to only include the 'NA' category on the y-axis.
#'
#' @return An object of class [ggplot2::ggplot].
#'
#' @examples
#' plot_groups(mice::boys, rotate = T, useNA = "no")
#' plot_groups(mice::boys, rotate = T, label = TRUE, useNA = "only")
#' plot_groups(mice::boys, rotate = T, useNA = "both")
#' @export

plot_groups <- function(data,
                        vrb = "all",
                        label = FALSE,
                        square = TRUE,
                        diagonal = FALSE,
                        rotate = FALSE,
                        useNA = "no") {
  if (!is.data.frame(data) & !is.matrix(data)) {
    stop("Dataset should be a 'data.frame' or 'matrix'.")
  }
  if (vrb[1] == "all") {
    vrb <- names(data)
  }
  
  groups <- data.frame(vrb=NA, lvl=NA)
  for(i in vrb){
    if("factor" %in% class(data[,i])){
      tmp <- data.frame(vrb=rep(i,times=length(levels(data[,i]))),lvl=levels(data[,i]))
    } else{
      tmp <- data.frame(vrb=i,lvl=NA)
    }
    tmp <- rbind(tmp, cbind(vrb=i, lvl="NA"))
    groups <- rbind(groups,tmp)
  }
  groups<-groups[-1,]
  
  groups_compl <- data.frame(groups[rep(seq_len(nrow(groups)), each = nrow(groups)), ],
                             groups, compl=NA, x=NA, y=NA)
  for(i in 1:nrow(groups_compl)){
    if(groups_compl[i,"vrb"]==groups_compl[i,"vrb.1"]){
      groups_compl[i,"compl"] <- NA
    }else if(is.na(groups_compl[i,"lvl"]) & is.na(groups_compl[i,"lvl.1"])){
      groups_compl[i,"compl"] <- nrow(data[!is.na(data[,groups_compl[i,"vrb"]]) & !is.na(data[,groups_compl[i,"vrb.1"]]),])
    }else if (is.na(groups_compl[i,"lvl"])&groups_compl[i,"lvl.1"]=="NA"){
      groups_compl[i,"compl"] <- nrow(data[!is.na(data[,groups_compl[i,"vrb"]]) & is.na(data[,groups_compl[i,"vrb.1"]]),])
    }else if (groups_compl[i,"lvl"]=="NA" & is.na(groups_compl[i,"lvl.1"])){
      groups_compl[i,"compl"] <- nrow(data[is.na(data[,groups_compl[i,"vrb"]]) & !is.na(data[,groups_compl[i,"vrb.1"]]),])
    }else if(is.na(groups_compl[i,"lvl"])){
      groups_compl[i,"compl"] <- nrow(data[!is.na(data[,groups_compl[i,"vrb"]]) & data[,groups_compl[i,"vrb.1"]]%in%groups_compl[i,"lvl.1"],])
    }else if(is.na(groups_compl[i,"lvl.1"])){
      groups_compl[i,"compl"]<- nrow(data[data[,groups_compl[i,"vrb"]]%in%groups_compl[i,"lvl"] & !is.na(data[,groups_compl[i,"vrb.1"]]),])
    }else if(groups_compl[i,"lvl"]=="NA"){
      groups_compl[i,"compl"]<- nrow(data[is.na(data[,groups_compl[i,"vrb"]]) & data[,groups_compl[i,"vrb.1"]]%in%groups_compl[i,"lvl.1"],])
    }else if(groups_compl[i,"lvl.1"]=="NA"){
      groups_compl[i,"compl"]<- nrow(data[data[,groups_compl[i,"vrb"]]%in%groups_compl[i,"lvl"] & is.na(data[,groups_compl[i,"vrb.1"]]) ,])
    }else{
      groups_compl[i,"compl"]<- nrow(data[data[,groups_compl[i,"vrb"]]%in%groups_compl[i,"lvl"] & data[,groups_compl[i,"vrb.1"]]%in%groups_compl[i,"lvl.1"],])
    }
    groups_compl[i,"y"]<-ifelse(is.na(groups_compl[i,"lvl.1"]), paste0(groups_compl[i,"vrb.1"]),paste0(groups_compl[i,"vrb.1"], " | ", groups_compl[i,"lvl.1"]))
    groups_compl[i,"x"]<-ifelse(is.na(groups_compl[i,"lvl"]), paste0(groups_compl[i,"vrb"]),paste0(groups_compl[i,"vrb"], " | ", groups_compl[i,"lvl"]))
  }
  groups_compl[groups_compl$compl%in%0,"compl"]<-0.1
  if(useNA=="both"){
    groups_plot<-groups_compl
  } else if(useNA=="only"){
    groups_plot <- groups_compl[groups_compl$lvl%in%"NA",]
  } else{
    groups_plot <- groups_compl[(is.na(groups_compl$lvl)|groups_compl$lvl!="NA")&(is.na(groups_compl$lvl.1)|groups_compl$lvl.1!="NA"),]
  }
  
  gg <- ggplot2::ggplot(groups_plot, ggplot2::aes(x = .data$x, y = .data$y, label = .data$compl, fill = .data$compl)) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::scale_x_discrete(limits = unique(groups_plot$x), position = "top") +
    ggplot2::scale_y_discrete(limits = rev(unique(groups_plot$y))) +
    # ggplot2::scale_fill_viridis_c("number\nof\ncases", option = "viridis", na.value = "white", direction = -1, trans = "log10")+
    # ggplot2::scale_fill_viridis_b("number\nof\ncases", option = "viridis", na.value = "white", direction = -1, trans = "log10", breaks = c(10,100,1000))+
    ggplot2::scale_fill_gradient2("number\nof\ncases", low = "orangered", mid = "lightyellow", high = "deepskyblue", midpoint = log10(10), na.value = "grey50", trans = "log10") +
    ggplot2::labs(title = "Number of cases per category")
  
  if (label) {
    gg <- gg + ggplot2::geom_text(color = "black", show.legend = FALSE, na.rm = TRUE)
  }
  if (square) {
    gg <- gg + ggplot2::coord_fixed(expand = FALSE)
  } else {
    gg <- gg + ggplot2::coord_cartesian(expand = FALSE)
  }
  if (rotate) {
    gg <- gg + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  }
  return(gg)
}