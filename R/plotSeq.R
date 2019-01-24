#' Sequence plot
#'
#' Returns a plot visualizing sequences along with the sequence code
#' @param seq A dataframe or datatable with each row representing a sequence
#' @param x.title A character vector to be displayed as the title of x axis
#' @param y.title A character vector to be displayed as the title of y axis
#' @param title A character vector to be displayed as the title of the plot
#' @param text.width An integer denoting the size of the sequence code
#' @import tidyr data.table ggplot2
#' @return Plot visualizing the sequences along with the sequence code
#'
#' @examples
#' plotSeq(seq = data.frame(1:10,2:11,3:12,4:13,5:14),x.title = "X Axis",y.title = "Y Axis",title = "Plot Title",text.width = 5)
#'
#' @export




plotSeq <- function(seq,x.title = "",y.title = "",title = "",text.width = 2)
{
  syn_dt <- as.data.table(seq)
  colnames(syn_dt) <- gsub("X","",colnames(syn_dt))
  syn_dt[,id := 1:nrow(syn_dt)]
  seq2 <- as.data.table(gather(syn_dt,key = "variable",value= "value",-id))


  p <- ggplot(data = seq2, aes(x = as.factor(as.numeric(variable)),y = as.factor(id), fill = as.factor(value), label = value)) +
    geom_tile(colour = "white",height=0.8) +
    geom_text(colour = "white", fontface = "bold", size = text.width) +
    xlab(x.title) + ylab(y.title) +
    ggtitle(title) +
    scale_fill_discrete(na.value="transparent") +
    theme_bw() +
    theme(legend.position="none",
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

  return(p)
}
