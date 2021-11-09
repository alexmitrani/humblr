#' @name digitizr
#' @title a wrapper for the magick package, focussed on extracting data from images containing graphs
#'
#' @import tidyverse
#' @import magick
#'
#' @param inputfilename Name of the input image file. This file should contain only the graph area, not the axes, titles or legends.
#' @param outputfilename Name to be used for the output image file (PNG format).
#' @param xvarname Name to be used for the x variable.
#' @param yvarname Name to be used for the y variable.
#' @param yscalemax Maximum value to be used for the y-scale.
#' @param xmin Minimum x value.
#' @param xmax Maximum x value.
#' @param ymin Minimum y value.
#' @param ymax Maximum y value.
#' @param threshold The threshold is a percentage written as a character string, e.g. "70%".  Some experimentation might be needed to find the value that works best.
#' @param mygraphsize The graph size in pixels.
#'
#' @return
#' @export
#'
#' @examples
#' myinputfile <- system.file("extdata", "ari.png", package = "humblr")
#' extracted_data <- digitizr(inputfilename=myinputfile, outputfilename="extracted_data_graph.png", xvarname="año", yvarname="millones de pasajeros", xmin=2020, xmax=2050, ymin=0.928253, ymax=4.159696, yscalemax=5, threshold="70%")
#'
digitizr <- function(inputfilename=NULL, outputfilename=NULL, xvarname=NULL, yvarname=NULL, xmin=NULL, xmax=NULL, ymin=NULL, ymax=NULL, yscalemax=NULL, threshold=NULL, mygraphsize=1000){

  # source: https://www.r-bloggers.com/2019/06/extracting-the-data-from-static-images-of-graphs-with-magick/

  im <- image_read(inputfilename)

  im_proc <- im %>%
    image_channel("saturation")
  im_proc

  im_proc2 <- im_proc %>%
    image_threshold("white", threshold)
  im_proc2

  im_proc3 <- im_proc2 %>%
    image_negate()

  im_proc3

  dat <- image_data(im_proc3)[1,,] %>%
    as.data.frame() %>%
    mutate(Row = 1:nrow(.)) %>%
    select(Row, everything()) %>%
    mutate_all(as.character) %>%
    gather(key = Column, value = value, 2:ncol(.)) %>%
    mutate(Column = as.numeric(gsub("V", "", Column)),
           Row = as.numeric(Row),
           value = ifelse(value == "00", NA, 1)) %>%
    filter(!is.na(value))

  dat <- dat %>%
    group_by(Row) %>%
    summarise(Column = mean(Column), value = mean(value)) %>%
    ungroup()

  cmin <- min(dat$Column)
  cmax <- max(dat$Column)

  rmin <- min(dat$Row)
  rmax <- max(dat$Row)

  # transform scales to interval 0,1

  dat <- dat %>%
    mutate(Column2 = (cmax - Column)/(cmax-cmin))

  dat <- dat %>%
    mutate(Row2 = (Row - rmin)/(rmax - rmin))

  # transform scale to fit required maximum and minimum values

  dat <- dat %>%
    mutate(y = (Column2*(ymax-ymin)+ymin))

  dat <- dat %>%
    mutate(x = (Row2*(xmax-xmin)+xmin))

  png(outputfilename, width = mygraphsize, height = mygraphsize)

    print({
      p <- ggplot(data = dat, aes(x = x, y = y)) +
        geom_point(size = 1) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, yscalemax)) +
        scale_colour_manual(values = c("red4", "blue4")) +
        theme(legend.position = "off") +
        labs(y= yvarname, x = xvarname)
    })

  dev.off()

  return(dat)

}
