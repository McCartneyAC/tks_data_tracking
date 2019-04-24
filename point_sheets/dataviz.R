library(dplyr)
library(tibble)
library(ggplot2)
library(lubridate)
library(reshape2)
library(forcats)

inova<-c("#004b8d", "#d52b1e", "#6caddf", "#4d4f53", "#a5a5a9")
pal_inova <- function(palette = c("inova"), alpha = 1) {
    palette <- match.arg(palette)
    if (alpha > 1L | alpha <= 0L) stop("alpha must be in (0, 1]")
        raw_cols <- inova
        raw_cols_rgb <- col2rgb(raw_cols)
        alpha_cols <- rgb(
            raw_cols_rgb[1L, ], raw_cols_rgb[2L, ], raw_cols_rgb[3L, ],
            alpha = alpha * 255L, names = names(raw_cols),
            maxColorValue = 255L
        )
        scales::manual_pal(unname(alpha_cols))
        }
#' Scale Colors as inova healthsystems
#'
#' @export scale_color_inova
scale_color_inova <- function(palette = c("inova"), alpha = 1, ...) {
    palette <- match.arg(palette)
    ggplot2::discrete_scale("colour", "inova", pal_inova(palette, alpha), ...)
}
scale_colour_inova<-scale_color_inova


## Example workflow for first student: 

dat<-tribble(
  ~date, ~day, ~Respect, ~OnTask, ~Focus, ~mean, ~week,
"08/28/18","Tuesday",2.00,2.00,2.00,2.00,1,
"08/29/18","Wednesday",2.25,2.25,2.25,2.25,1,
"08/30/18","Thursday",2.00,2.50,2.00,2.17,1,
"08/31/18","Friday",1.75,1.75,2.50,2.00,1,
"09/04/18","Tuesday",1.75,1.75,2.00,1.83,2,
"09/05/18","Wednesday",2.00,2.00,2.33,2.11,2,
"09/06/18","Thursday",1.75,1.50,2.25,1.83,2,
"09/07/18","Friday",1.75,1.50,2.25,1.83,2,
"09/10/18","Monday",2.00,2.33,2.33,2.22,3,
"09/11/18","Tuesday",1.25,1.25,1.50,1.33,3,
"09/12/18","Wednesday",1.25,1.50,1.25,1.33,3,
"09/13/18","Thursday",1.00,1.25,1.25,1.17,3,
"09/14/18","Friday",1.75,1.75,1.75,1.75,3,
"09/17/18","Monday",2.00,1.50,1.50,1.67,4,
"09/18/18","Tuesday",1.25,1.25,2.00,1.50,4,
"09/19/18","Wednesday",2.25,2.25,1.75,2.08,4,
"09/20/18","Thursday",1.25,1.50,1.50,1.42,4,
"09/21/18","Friday",2.50,2.25,2.75,2.50,4,
"09/24/18","Monday",NA,NA,NA,NA,5,
"09/25/18","Tuesday",1.75,1.75,1.75,1.75,5,
"09/26/18","Wednesday",1.25,1.25,1.75,1.42,5,
"09/27/18","Thursday",2.33,3.00,3.00,2.78,5,
"09/28/18","Friday",2.00,2.25,2.25,2.17,5,
"10/01/18","Monday",1.67,1.67,1.67,1.67,6,
"10/02/18","Tuesday",2.25,2.50,3.00,2.58,6,
"10/03/18","Wednesday",NA,NA,NA,NA,6,
"10/04/18","Thursday",2.00,2.33,2.67,2.33,6,
"10/05/18","Friday",NA,NA,NA,NA,6,
"10/09/18","Tuesday",2.25,2.50,3.00,2.58,7,
"10/10/18","Wednesday",1.25,1.25,1.25,1.25,7,
"10/11/18","Thursday",NA,NA,NA,NA,7,
"10/12/18","Friday",2.25,2.50,3.00,2.58,7,
"10/15/18","Monday",1.33,1.33,2.00,1.56,8,
"10/16/18","Tuesday",0.50,0.50,0.75,0.58,8,
"10/17/18","Wednesday",NA,NA,NA,NA,8,
"10/18/18","Thursday",2.50,2.25,3.00,2.58,8,
"10/19/18","Friday",0.67,0.67,1.00,0.78,8,
"10/22/18","Monday",NA,NA,NA,NA,9,
"10/23/18","Tuesday",2.25,2.50,2.75,2.50,9,
"10/24/18","Wednesday",1.50,1.13,1.75,1.46,9,
"10/25/18","Thursday",2.00,2.00,2.50,2.17,9,
"10/26/18","Friday",1.50,2.00,2.00,1.83,9,
"10/29/18","Monday",2.50,2.00,2.75,2.42,10,
"10/30/18","Tuesday",2.75,2.75,2.75,2.75,10,
"10/31/18","Wednesday",2.00,2.00,2.00,2.00,10,
"11/01/18","Thursday",2.00,2.00,3.00,2.33,10,
"11/02/18","Friday",2.00,2.00,2.00,2.00,10,
"11/06/18","Tuesday",NA,NA,NA,NA,11,
"11/07/18","Wednesday",1.67,2.00,2.00,1.89,11,
"11/08/18","Thursday",2.50,2.50,2.50,2.50,11,
"11/09/18","Friday",2.00,2.50,2.50,2.33,11,
"11/12/18","Monday",NA,NA,NA,NA,12,
"11/13/18","Tuesday",2.00,2.66,3.00,2.55,12,
"11/14/18","Wednesday",2.33,2.66,3.00,2.66,12,
"11/15/18","Thursday",NA,NA,NA,NA,12,
"11/16/18","Friday",NA,NA,NA,NA,12,
"11/19/18","Monday",2.00,1.00,2.66,1.89,13,
"11/20/18","Tuesday",NA,NA,NA,NA,13,
"11/21/18","Wednesday",0.00,0.00,0.00,0.00,13,
"11/26/18","Monday",2.00,2.00,2.67,2.22,14,
"11/27/18","Tuesday",2.00,1.33,1.67,1.67,14,
"11/28/18","Wednesday",NA,NA,NA,NA,14,
"11/29/18","Thursday",1.50,2.00,2.00,1.83,14,
"11/30/18","Friday",NA,NA,NA,NA,14,
"12/03/18","Monday",NA,NA,NA,NA,15,
"12/04/18","Tuesday",1.67,2.00,1.50,1.72,15,
"12/05/18","Wednesday",2.00,2.00,1.00,1.67,15,
"12/06/18","Thursday",NA,NA,NA,NA,15,
"12/07/18","Friday",NA,NA,NA,NA,15,
"12/10/18","Monday",NA,NA,NA,NA,16,
"12/11/18","Tuesday",NA,NA,NA,NA,16,
"12/12/18","Wednesday",NA,NA,NA,NA,16,
"12/13/18","Thursday",NA,NA,NA,NA,16,
"12/14/18","Friday",NA,NA,NA,NA,16,
"12/17/18","Monday",NA,NA,NA,NA,17,
"12/18/18","Tuesday",NA,NA,NA,NA,17,
"12/19/18","Wednesday",NA,NA,NA,NA,17,
"12/20/18","Thursday",NA,NA,NA,NA,17,
"12/21/18","Friday",NA,NA,NA,NA,17,
"01/03/19","Thursday",2.00,2.00,3.00,2.33,18,
"01/04/19","Friday",0.67,0.33,1.00,0.67,18,
"01/07/19","Monday",1.67,1.67,1.50,1.61,19,
"01/08/19","Tuesday",NA,NA,NA,NA,19,
"01/09/19","Wednesday",1.67,1.67,2.00,1.78,19,
"01/10/19","Thursday",1.67,1.67,2.00,1.78,19,
"01/11/19","Friday",NA,NA,NA,NA,19,
"01/14/19","Monday",NA,NA,NA,NA,20,
"01/15/19","Tuesday",NA,NA,NA,NA,20,
"01/16/19","Wednesday",2.00,1.00,1.00,1.33,20,
"01/17/19","Thursday",NA,NA,NA,NA,20,
"01/18/19","Friday",NA,NA,NA,NA,20,
"01/21/19","Monday",NA,NA,NA,NA,21,
"01/22/19","Tuesday",NA,NA,NA,NA,21,
"01/23/19","Wednesday",NA,NA,NA,NA,21,
"01/24/19","Thursday",NA,NA,NA,NA,21,
"01/25/19","Friday",NA,NA,NA,NA,21,
"01/28/19","Monday",1.67,1.67,1.67,1.67,22,
"01/29/19","Tuesday",0.00,0.00,0.00,0.00,22,
"02/04/19","Monday",1.67,2.00,2.33,2.00,23,
"02/05/19","Tuesday",0.00,0.00,0.00,0.00,23,
"02/06/19","Wednesday",2.00,2.33,2.67,2.33,23,
"02/07/19","Thursday",2.67,2.67,2.67,2.67,23,
"02/08/19","Friday",2.00,2.00,2.00,2.00,23,
"02/12/19","Tuesday",2.33,2.33,2.33,2.33,24,
"02/13/19","Wednesday",2.33,2.67,3.00,2.67,24,
"02/14/19","Thursday",2.33,2.33,2.67,2.44,24,
"02/15/19","Friday",NA,NA,NA,NA,24,
"02/18/19","Monday",NA,NA,NA,NA,25,
"02/19/19","Tuesday",2.00,2.00,2.00,2.00,25,
"02/20/19","Wednesday",NA,NA,NA,NA,25,
"02/21/19","Thursday",NA,NA,NA,NA,25,
"02/22/19","Friday",2.67,2.67,3.00,2.78,25,
"02/25/19","Monday",NA,NA,NA,NA,26,
"02/26/19","Tuesday",1.33,1.33,1.00,1.22,26,
"02/27/19","Wednesday",1.67,1.67,1.67,1.67,26,
"02/28/19","Thursday",1.50,1.50,NA,1.50,26,
"03/01/19","Friday",NA,NA,NA,NA,26,
"03/04/19","Monday",2.00,2.00,2.00,2.00,27,
"03/05/19","Tuesday",1.50,1.50,NA,1.50,27,
"03/06/19","Wednesday",2.00,2.33,2.33,2.22,27,
"03/07/19","Thursday",NA,NA,NA,NA,27,
"03/08/19","Friday",NA,NA,NA,NA,27,
"03/11/19","Monday",2.00,2.00,2.33,2.11,28,
"03/12/19","Tuesday",NA,NA,NA,NA,28,
"03/13/19","Wednesday",0.00,0.00,0.00,0.00,28,
"03/14/19","Thursday",NA,NA,NA,NA,28,
"03/15/19","Friday",2.00,2.00,2.00,2.00,28,
"03/18/19","Monday",1.67,1.67,2.33,1.89,29,
"03/19/19","Tuesday",NA,NA,NA,NA,29,
"03/20/19","Wednesday",0.00,0.00,0.00,0.00,29,
"03/21/19","Thursday",0.00,0.00,0.00,0.00,29,
"03/22/19","Friday",NA,NA,NA,NA,29,
"03/25/19","Monday",1.67,2.00,1.67,1.78,30,
"03/26/19","Tuesday",2.33,2.00,3.00,2.44,30,
"03/27/19","Wednesday",1.50,1.50,2.00,1.67,30,
"03/28/19","Thursday",1.00,1.00,1.00,1.00,30,
"03/29/19","Friday",0.67,1.00,0.67,0.78,30,
"04/01/19","Monday",0.00,0.00,0.00,0.00,31,
"04/02/19","Tuesday",2.00,2.33,2.67,2.33,31,
"04/03/19","Wednesday",2.33,2.33,2.67,2.44,31,
"04/04/19","Thursday",NA,NA,NA,NA,31,
"04/05/19","Friday",2.00,2.00,2.67,2.22,31,
"04/08/19","Monday",2.00,2.00,2.67,2.22,32,
"04/09/19","Tuesday",2.00,2.33,2.67,2.33,32,
"04/10/19","Wednesday",0.33,0.33,0.33,0.33,32,
"04/11/19","Thursday",0.00,0.00,0.00,0.00,32,
"04/22/19","Monday",0.00,0.00,0.00,0.00,33,
"04/23/19","Tuesday",2.00,2.00,2.00,2.00,33
)





dat  %>% 
mutate(date = mdy(date))  %>% 
mutate(day = factor(day))  %>% 
melt(id.vars = c("date", "day", "week"))  %>% 
filter(variable != "mean") %>% 
ggplot(aes(x = date, y = value, color = variable))  + 
geom_jitter(alpha = 0.4, width = 0.25, height = 0.25)  +
geom_smooth() +
scale_color_inova() + 
theme_light() +
labs(title= "Trajectory of _______'s points", 
     subtitle = "2018-2019 Academic Year",
     y = "Average Daily Points"
    ) + 
scale_y_continuous(limits = c(0.0, 3.0)) 
)


dat  %>% 
mutate(date = mdy(date))  %>%
mutate(day = fct_relevel(day, "Monday","Tuesday", "Wednesday","Thursday","Friday"))  %>% 
mutate(day = factor(day))  %>% 
melt(id.vars = c("date", "day", "week"))  %>% 
filter(variable == "mean") %>%  
ggplot(aes(x = day, y = week, fill = value))  + 
geom_tile(color = "black") + 
# geom_text() +
scale_fill_gradient(low="#d52b1e",  high="#6caddf", limits = c(0,3)) +
scale_y_reverse() + 
theme_light() + 
geom_text(label = dat$mean, stat = "identity")+
labs(
    title = " [lastname] [firstname] Average Daily Points",
    subtitle = "2018-2019 School Year",
    x = "Day"
) + 
scale_x_discrete(position = "top") +
NULL






 
