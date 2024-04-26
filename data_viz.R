setwd ("D:\\R_folder")


load.lib <- c("reshape2", "scales", "ggplot2", "dplyr", "plyr", "data.table")

install.lib<-load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                        position = "dodge", trim = TRUE, scale = "area",
                        show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            

            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
          },
          
          draw_group = function(data, panel_scales, coord) {

            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
         
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
)


raincloud_theme = theme(
text = element_text(size = 10),
axis.title.x = element_text(size = 16),
axis.title.y = element_text(size = 16),
axis.text = element_text(size = 14),
axis.text.x = element_text(angle = 45, vjust = 0.5),
legend.title=element_text(size=16),
legend.text=element_text(size=16),
legend.position = "right",
plot.title = element_text(lineheight=.8, face="bold", size = 16),
panel.border = element_blank(),
panel.grid.minor = element_blank(),
panel.grid.major = element_blank(),
axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))



setwd ("D:\\R_folder")

data <- read.csv ("max_COVID3.csv")

data$PC[data$PC == "0"] <- "High PC Income"
data$PC[data$PC == "1"] <- "Low PC Income"

data$HonorsCollegeMember <- as.factor (as.character (data$HonorsCollegeMember))

ggplot(data = data, aes(y = cases_normalized_for_pop, x = population_density, fill = PC )) +
	geom_smooth(method = "lm", alpha = 0.8, color = "Black") +
	geom_point(color = "grey") +
	xlab ("Population Density") +
	ylab ("Cases per 1000 population") +
	theme(axis.text.x = element_text(face = "bold", size = 16, angle = 0, hjust = 1), panel.grid.major = element_blank(), 
		axis.title.y = element_text(face = "bold", size = 18), axis.text.y = element_text(face = "bold", size = 16),
		axis.title.x = element_text(face = "bold", size = 18),
		legend.text = element_text(face = "bold", size = 16), legend.title = element_text(face = "bold", size = 18),
		panel.grid.minor = element_blank(),
		panel.background = element_blank(), axis.line = element_line(colour = "black")) +
	scale_fill_grey(start = .2, end = .7)
