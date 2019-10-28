# libraries
library(ggplot2)
library(dplyr)
library(ggrepel)
library(tibble)



# data frame structure (THIS MUST BE PRECISE)
{studentname}<-tribble(
    ~student,~value,~variable,~y_val,~grade_level,
    "{studentname}",102,"word_reading",0.01,9.6,
    "{studentname}",101,"sentence_reading",0.01,10.9,
    "{studentname}",89,"spelling",0.01,5.9,
    "{studentname}",82,"math",0.01,4.8
    )



# run iteratively? 
wrat_plot<-function(student, version){
    #gen normal dist data 
    basis <- rnorm(1000000)
    dat <- as.data.frame(basis)%>% 
        mutate(stand = basis * 15 + 100) # Standardized Dist. 


    i<-student
    insert_layer <- function(P, after=0, ...) {
          if (after < 0)
            after <- after + length(P$layers)
          if (!length(P$layers))
            P$layers <- list(...)
          else 
            P$layers <- append(P$layers, list(...), after)
          return(P)
        }
          
    p <- ggplot() + 
        theme_classic()+
        scale_x_continuous(
            limits = c(50,150), 
            breaks = seq(55, 145, by = 15)) + 
        geom_vline(xintercept = i$value[1]) +
        geom_vline(xintercept = i$value[2]) +
        geom_vline(xintercept = i$value[3]) +
        geom_vline(xintercept = i$value[4]) + 
        geom_label_repel(data = i, force = 3, 
                         aes(label = paste0(variable, ": ", value), x = value, y = y_val)
                        )+
        labs(title = paste0("WRAT Standard Scores for ", i$student[1]),
             subtitle = "October 2019",
            x = "Standard Score", 
            y = "", 
            caption = paste0("Assessment Used: ", version, " version")) + 
        # annotate("rect",xmin = 115, xmax = 150, ymin = 0.02, ymax = 0.03, color = "white")
        annotate("text", label = "Grade Equivalents:", x = 130, y = 0.028) + 
        annotate("text", label = paste0(i$variable[1], ": ",i$grade_level[1]), x = 130, y = 0.027) + 
        annotate("text", label = paste0(i$variable[2], ": ",i$grade_level[2]), x = 130, y = 0.026) + 
        annotate("text", label = paste0(i$variable[3], ": ",i$grade_level[3]), x = 130, y = 0.025) + 
        annotate("text", label = paste0(i$variable[4], ": ",i$grade_level[4]), x = 130, y = 0.024) + 
        NULL 
    
    if (version == "blue") {
        insert_layer(p, after = 0, geom_density(data = dat, aes(x = stand), fill = "#6caddf") )

    } else if (version == "green") {
        insert_layer(p, after = 0,geom_density(data = dat, aes(x = stand), fill = "#98E093") )
    }
    

}
