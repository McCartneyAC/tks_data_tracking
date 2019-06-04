library(tidyverse)
library(readxl)
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



dat<-tribble(
~admin,~test,~core,~scaled_score,~date,
"2015_2016", "EOC Writing ", "eng", 403, "3/4/2016", 
"2015_2016", "EOC Writing ", "eng", 596, "3/4/2016", 
"2015_2016", "EOC Writing ", "eng", 541, "3/4/2016", 
"2015_2016", "EOC Writing ", "eng", 448, "3/4/2016", 
"2015_2016", "Gr 8 Writing", "eng", 373, "3/8/2016", 
"2015_2016", "Gr 8 Writing", "eng", 276, "3/8/2016", 
"2015_2016", "Gr 8 Writing", "eng", 374, "3/8/2016", 
"2015_2016", "EOC Writing ", "eng", 392, "3/10/2016", 
"2015_2016", "EOC Writing ", "eng", 442, "3/10/2016", 
"2015_2016", "Algebra I ", "math", 392, "5/16/2016", 
"2015_2016", "Gr 8 Science", "sci", 421, "5/16/2016", 
"2015_2016", "Gr 8 Science", "sci", 350, "5/16/2016", 
"2015_2016", "Gr 8 Science", "sci", 357, "5/16/2016", 
"2015_2016", "Biology ", "sci", 378, "5/17/2016", 
"2015_2016", "Gr 8 Reading ", "eng", 422, "5/18/2016", 
"2015_2016", "Gr 8 Reading ", "eng", 339, "5/18/2016", 
"2015_2016", "Gr 8 Reading ", "eng", 321, "5/18/2016", 
"2015_2016", "W Hist I ", "histss", 392, "5/18/2016", 
"2015_2016", "W Hist II ", "histss", 320, "5/18/2016", 
"2015_2016", "Algebra I ", "math", 399, "5/19/2016", 
"2015_2016", "Algebra I ", "math", 350, "5/19/2016", 
"2015_2016", "Gr 8 Math  CAT", "math", 397, "5/20/2016", 
"2015_2016", "Gr 8 Math  CAT", "math", 308, "5/20/2016", 
"2015_2016", "Algebra I ", "math", 364, "5/23/2016", 
"2015_2016", "Algebra I ", "math", 367, "5/23/2016", 
"2015_2016", "Algebra I ", "math", 351, "5/23/2016", 
"2015_2016", "Chemistry ", "sci", 501, "5/23/2016", 
"2015_2016", "Chemistry ", "sci", 445, "5/23/2016", 
"2015_2016", "Chemistry ", "sci", 432, "5/23/2016", 
"2015_2016", "PE Algebra I ", "math", 341, "5/23/2016", 
"2015_2016", "PE Algebra I ", "math", 356, "5/23/2016", 
"2015_2016", "Geometry ", "math", 351, "5/24/2016", 
"2015_2016", "Geometry ", "math", 368, "5/24/2016", 
"2015_2016", "Geometry ", "math", 337, "5/24/2016", 
"2015_2016", "Geometry ", "math", 429, "5/24/2016", 
"2015_2016", "Algebra II ", "math", 337, "5/25/2016", 
"2015_2016", "Algebra II ", "math", 318, "5/25/2016", 
"2015_2016", "Algebra II ", "math", 457, "5/25/2016", 
"2015_2016", "Algebra II ", "math", 415, "5/25/2016", 
"2015_2016", "W Hist I ", "histss", 430, "5/25/2016", 
"2015_2016", "W Hist I ", "histss", 434, "5/25/2016", 
"2015_2016", "W Hist I ", "histss", 367, "5/25/2016", 
"2015_2016", "W Hist I ", "histss", 345, "5/25/2016", 
"2015_2016", "EOC Reading ", "eng", 395, "5/26/2016", 
"2015_2016", "EOC Reading ", "eng", 415, "5/26/2016", 
"2015_2016", "EOC Reading ", "eng", 509, "5/26/2016", 
"2015_2016", "EOC Reading ", "eng", 469, "5/26/2016", 
"2015_2016", "EOC Reading ", "eng", 462, "5/26/2016", 
"2015_2016", "W Hist II ", "histss", 433, "5/26/2016", 
"2015_2016", "W Hist II ", "histss", 368, "5/26/2016", 
"2015_2016", "W Hist II ", "histss", 409, "5/26/2016", 
"2015_2016", "W Hist II ", "histss", 335, "5/26/2016", 
"2015_2016", "W Hist II ", "histss", 474, "5/26/2016", 
"2015_2016", "Biology ", "sci", 395, "5/27/2016", 
"2015_2016", "Biology ", "sci", 395, "5/27/2016", 
"2015_2016", "VA&US Hist ", "histss", 354, "5/27/2016", 
"2015_2016", "VA&US Hist ", "histss", 384, "5/27/2016", 
"2015_2016", "VA&US Hist ", "histss", 565, "5/27/2016", 
"2015_2016", "VA&US Hist ", "histss", 437, "5/27/2016", 
"2015_2016", "VA&US Hist ", "histss", 425, "5/27/2016", 
"2015_2016", "VA&US Hist ", "histss", 444, "5/27/2016", 
"2015_2016", "VA&US Hist ", "histss", 455, "5/27/2016", 
"2015_2016", "Earth Sci ", "sci", 408, "5/31/2016", 
"2015_2016", "Earth Sci ", "sci", 396, "5/31/2016", 
"2015_2016", "Earth Sci ", "sci", 357, "5/31/2016", 
"2015_2016", "EOC Reading ", "eng", 411, "6/1/2016", 
"2015_2016", "EOC Reading ", "eng", 489, "6/1/2016", 
"2015_2016", "Biology ", "sci", 414, "6/2/2016", 
"2015_2016", "Earth Sci ", "sci", 414, "6/2/2016", 
"2015_2016", "Earth Sci ", "sci", 375, "6/2/2016", 
"2015_2016", "VA&US Hist ", "histss", 376, "6/2/2016", 
"2015_2016", "VA&US Hist ", "histss", 390, "6/2/2016", 
"2015_2016", "Biology ", "sci", 374, "6/3/2016", 
"2015_2016", "W Hist II ", "histss", 353, "6/3/2016", 
"2015_2016", "W Hist II ", "histss", 376, "6/3/2016", 
"2015_2016", "W Hist II ", "histss", 335, "6/3/2016", 
"2015_2016", "Biology ", "sci", 407, "6/6/2016", 
"2015_2016", "W Hist II ", "histss", 394, "6/7/2016", 
"2015_2016", "W Hist I ", "histss", 383, "6/8/2016", 
"2015_2016", "Geometry ", "math", 429, "6/9/2016", 
"2016_2017", "Biology ", "sci", 389, "11/30/2016", 
"2016_2017", "Biology ", "sci", 397, "11/30/2016", 
"2016_2017", "VA&US Hist ", "histss", 356, "12/1/2016", 
"2016_2017", "VA&US Hist ", "histss", 364, "12/1/2016", 
"2016_2017", "W Hist II ", "histss", 423, "12/1/2016", 
"2016_2017", "W Hist II ", "histss", 386, "12/1/2016", 
"2016_2017", "Algebra I ", "math", 330, "12/2/2016", 
"2016_2017", "Algebra I ", "math", 368, "12/2/2016", 
"2016_2017", "Algebra I ", "math", 356, "12/2/2016", 
"2016_2017", "PE Algebra I ", "math", 338, "12/2/2016", 
"2016_2017", "EOC Writing", "eng", 401, "12/6/2016", 
"2016_2017", "EOC Writing", "eng", 442, "12/6/2016", 
"2016_2017", "Biology ", "sci", 389, "12/7/2016", 
"2016_2017", "Biology ", "sci", 372, "12/7/2016", 
"2016_2017", "Earth Sci ", "sci", 440, "12/7/2016", 
"2016_2017", "VA&US Hist ", "histss", 386, "12/8/2016", 
"2016_2017", "Algebra I ", "math", 356, "12/9/2016", 
"2016_2017", "PE Algebra I ", "math", 364, "12/9/2016", 
"2016_2017", "EOC Reading ", "eng", 347, "12/13/2016", 
"2016_2017", "Algebra I ", "math", 372, "12/15/2016", 
"2016_2017", "Algebra I ", "math", 368, "12/15/2016", 
"2016_2017", "Algebra I ", "math", 372, "12/20/2016", 
"2016_2017", "Algebra I ", "math", 364, "12/20/2016", 
"2016_2017", "EOC Writing", "eng", 485, "3/15/2017", 
"2016_2017", "EOC Writing", "eng", 334, "3/15/2017", 
"2016_2017", "EOC Writing", "eng", 289, "3/15/2017", 
"2016_2017", "EOC Writing", "eng", 470, "3/15/2017", 
"2016_2017", "EOC Writing", "eng", 600, "3/15/2017", 
"2016_2017", "EOC Writing", "eng", 391, "3/16/2017", 
"2016_2017", "EOC Writing", "eng", 415, "3/16/2017", 
"2016_2017", "EOC Writing", "eng", 323, "3/16/2017", 
"2016_2017", "EOC Writing", "eng", 421, "3/16/2017", 
"2016_2017", "Algebra I ", "math", 360, "5/8/2017", 
"2016_2017", "PE Algebra I ", "math", 372, "5/8/2017", 
"2016_2017", "Gr 8 Reading ", "eng", 308, "5/9/2017", 
"2016_2017", "PE Algebra I ", "math", 376, "5/10/2017", 
"2016_2017", "Algebra I ", "math", 369, "5/15/2017", 
"2016_2017", "Algebra I ", "math", 367, "5/15/2017", 
"2016_2017", "Algebra I ", "math", 376, "5/15/2017", 
"2016_2017", "Algebra I ", "math", 359, "5/15/2017", 
"2016_2017", "Algebra I ", "math", 350, "5/15/2017", 
"2016_2017", "VMAST Algebra I", "math", 305, "5/15/2017", 
"2016_2017", "VM EOC Reading", "eng", 369, "5/16/2017", 
"2016_2017", "Gr 8 Reading ", "eng", 352, "5/17/2017", 
"2016_2017", "W Hist II ", "histss", 413, "5/18/2017", 
"2016_2017", "W Hist II ", "histss", 413, "5/18/2017", 
"2016_2017", "Biology ", "sci", 381, "5/19/2017", 
"2016_2017", "Earth Sci ", "sci", 369, "5/19/2017", 
"2016_2017", "Geometry ", "math", 373, "5/19/2017", 
"2016_2017", "Algebra I ", "math", 341, "5/22/2017", 
"2016_2017", "Algebra I ", "math", 363, "5/22/2017", 
"2016_2017", "Algebra I ", "math", 387, "5/22/2017", 
"2016_2017", "Chemistry ", "sci", 373, "5/22/2017", 
"2016_2017", "Geometry ", "math", 357, "5/23/2017", 
"2016_2017", "Geometry ", "math", 356, "5/23/2017", 
"2016_2017", "Geometry ", "math", 315, "5/23/2017", 
"2016_2017", "Geometry ", "math", 392, "5/23/2017", 
"2016_2017", "Geometry ", "math", 330, "5/23/2017", 
"2016_2017", "Geometry ", "math", 427, "5/23/2017", 
"2016_2017", "Geometry ", "math", 397, "5/23/2017", 
"2016_2017", "Algebra II ", "math", 370, "5/24/2017", 
"2016_2017", "W Hist I ", "histss", 399, "5/24/2017", 
"2016_2017", "W Hist I ", "histss", 459, "5/24/2017", 
"2016_2017", "Biology ", "sci", 400, "5/25/2017", 
"2016_2017", "Biology ", "sci", 432, "5/25/2017", 
"2016_2017", "EOC Reading ", "eng", 423, "5/25/2017", 
"2016_2017", "EOC Reading ", "eng", 368, "5/25/2017", 
"2016_2017", "EOC Reading ", "eng", 355, "5/25/2017", 
"2016_2017", "EOC Reading ", "eng", 349, "5/25/2017", 
"2016_2017", "EOC Reading ", "eng", 410, "5/25/2017", 
"2016_2017", "EOC Reading ", "eng", 475, "5/25/2017", 
"2016_2017", "EOC Reading ", "eng", 475, "5/25/2017", 
"2016_2017", "VA&US Hist ", "histss", 352, "5/26/2017", 
"2016_2017", "VA&US Hist ", "histss", 372, "5/26/2017", 
"2016_2017", "VA&US Hist ", "histss", 400, "5/26/2017", 
"2016_2017", "VA&US Hist ", "histss", 411, "5/26/2017", 
"2016_2017", "VA&US Hist ", "histss", 432, "5/26/2017", 
"2016_2017", "VA&US Hist ", "histss", 453, "5/26/2017", 
"2016_2017", "Biology ", "sci", 413, "5/30/2017", 
"2016_2017", "Chemistry ", "sci", 414, "5/30/2017", 
"2016_2017", "Earth Sci ", "sci", 367, "5/30/2017", 
"2016_2017", "Earth Sci ", "sci", 449, "5/30/2017", 
"2016_2017", "VA&US Hist ", "histss", 418, "5/31/2017", 
"2016_2017", "VA&US Hist ", "histss", 397, "5/31/2017", 
"2016_2017", "W Hist I ", "histss", 418, "5/31/2017", 
"2016_2017", "Algebra I ", "math", 383, "6/1/2017", 
"2016_2017", "Geometry ", "math", 375, "6/1/2017", 
"2016_2017", "Geometry ", "math", 384, "6/1/2017", 
"2016_2017", "Geometry ", "math", 438, "6/1/2017", 
"2016_2017", "Algebra I ", "math", 351, "6/9/2017", 
"2016_2017", "Algebra II ", "math", 380, "6/9/2017", 
"2016_2017", "Geometry ", "math", 361, "6/9/2017", 
"2017_2018", "Earth Sci ", "sci", 384, "12/15/2017", 
"2017_2018", "W Hist I ", "histss", 433, "12/18/2017", 
"2017_2018", "Algebra I ", "math", 337, "12/19/2017", 
"2017_2018", "Algebra I ", "math", 361, "12/19/2017", 
"2017_2018", "Algebra I ", "math", 361, "12/19/2017", 
"2017_2018", "EOC Reading ", "eng", 395, "12/20/2017", 
"2017_2018", "Geometry ", "math", 334, "1/15/2018", 
"2017_2018", "W Hist II ", "histss", 411, "1/15/2018", 
"2017_2018", "Algebra II ", "math", 426, "1/23/2018", 
"2017_2018", "Algebra I ", "math", 372, "2/1/2018", 
"2017_2018", "Algebra I ", "math", 387, "2/1/2018", 
"2017_2018", "Chemistry ", "sci", 449, "2/2/2018", 
"2017_2018", "EOC Reading ", "eng", 371, "2/15/2018", 
"2017_2018", "EOC Reading ", "eng", 415, "2/15/2018", 
"2017_2018", "EOC Reading ", "eng", 428, "2/15/2018", 
"2017_2018", "Algebra I ", "math", 380, "2/16/2018", 
"2017_2018", "EOC Writing ", "eng", 554, "3/20/2018", 
"2017_2018", "EOC Writing ", "eng", 551, "3/20/2018", 
"2017_2018", "EOC Writing ", "eng", 412, "3/20/2018", 
"2017_2018", "EOC Writing ", "eng", 425, "3/20/2018", 
"2017_2018", "Gr 8 Writing", "eng", 429, "3/20/2018", 
"2017_2018", "Algebra I ", "math", 395, "5/14/2018", 
"2017_2018", "Geometry ", "math", 337, "5/14/2018", 
"2017_2018", "EOC Reading ", "eng", 338, "5/15/2018", 
"2017_2018", "EOC Reading ", "eng", 427, "5/15/2018", 
"2017_2018", "VA&US Hist ", "histss", 403, "5/16/2018", 
"2017_2018", "W Hist II ", "histss", 412, "5/16/2018", 
"2017_2018", "W Hist II ", "histss", 345, "5/16/2018", 
"2017_2018", "Biology ", "sci", 370, "5/17/2018", 
"2017_2018", "Biology ", "sci", 376, "5/17/2018", 
"2017_2018", "Algebra I ", "math", 397, "5/21/2018", 
"2017_2018", "Algebra I ", "math", 379, "5/21/2018", 
"2017_2018", "Algebra I ", "math", 359, "5/21/2018", 
"2017_2018", "Algebra I ", "math", 397, "5/21/2018", 
"2017_2018", "Chemistry ", "sci", 402, "5/21/2018", 
"2017_2018", "Chemistry ", "sci", 414, "5/21/2018", 
"2017_2018", "Chemistry ", "sci", 435, "5/21/2018", 
"2017_2018", "Algebra I ", "math", 317, "5/22/2018", 
"2017_2018", "Algebra II ", "math", 319, "5/23/2018", 
"2017_2018", "Algebra II ", "math", 337, "5/23/2018", 
"2017_2018", "Algebra II ", "math", 305, "5/23/2018", 
"2017_2018", "Algebra II ", "math", 331, "5/23/2018", 
"2017_2018", "Algebra II ", "math", 279, "5/23/2018", 
"2017_2018", "Algebra II ", "math", 312, "5/23/2018", 
"2017_2018", "Algebra II ", "math", 322, "5/23/2018", 
"2017_2018", "Algebra II ", "math", 322, "5/23/2018", 
"2017_2018", "Algebra II ", "math", 340, "5/23/2018", 
"2017_2018", "W Hist I ", "histss", 376, "5/23/2018", 
"2017_2018", "W Hist I ", "histss", 406, "5/23/2018", 
"2017_2018", "W Hist I ", "histss", 380, "5/23/2018", 
"2017_2018", "W Hist I ", "histss", 417, "5/23/2018", 
"2017_2018", "EOC Reading ", "eng", 485, "5/24/2018", 
"2017_2018", "EOC Reading ", "eng", 477, "5/24/2018", 
"2017_2018", "EOC Reading ", "eng", 453, "5/24/2018", 
"2017_2018", "EOC Reading ", "eng", 427, "5/24/2018", 
"2017_2018", "W Hist II ", "histss", 394, "5/24/2018", 
"2017_2018", "W Hist II ", "histss", 379, "5/24/2018", 
"2017_2018", "W Hist II ", "histss", 448, "5/24/2018", 
"2017_2018", "W Hist II ", "histss", 443, "5/24/2018", 
"2017_2018", "W Hist II ", "histss", 397, "5/24/2018", 
"2017_2018", "VA&US Hist ", "histss", 432, "5/25/2018", 
"2017_2018", "VA&US Hist ", "histss", 463, "5/25/2018", 
"2017_2018", "VA&US Hist ", "histss", 428, "5/25/2018", 
"2017_2018", "Biology ", "sci", 423, "5/29/2018", 
"2017_2018", "Biology ", "sci", 484, "5/29/2018", 
"2017_2018", "Biology ", "sci", 416, "5/29/2018", 
"2017_2018", "Biology ", "sci", 339, "5/29/2018", 
"2017_2018", "Biology ", "sci", 368, "5/29/2018", 
"2017_2018", "Biology ", "sci", 324, "5/29/2018", 
"2017_2018", "Biology ", "sci", 363, "5/29/2018", 
"2017_2018", "Biology ", "sci", 452, "5/29/2018", 
"2017_2018", "Earth Sci ", "sci", 392, "5/30/2018", 
"2017_2018", "Earth Sci ", "sci", 411, "5/30/2018", 
"2017_2018", "Earth Sci ", "sci", 416, "5/30/2018", 
"2017_2018", "Earth Sci ", "sci", 437, "5/30/2018", 
"2017_2018", "Earth Sci ", "sci", 344, "5/30/2018", 
"2017_2018", "Earth Sci ", "sci", 379, "5/30/2018", 
"2017_2018", "Gr 7 Reading CAT", "eng", 415, "5/30/2018", 
"2017_2018", "Gr 7 Reading CAT", "eng", 289, "5/30/2018", 
"2017_2018", "Gr 7 Math CAT", "math", 378, "5/31/2018", 
"2017_2018", "Gr 7 Math CAT", "math", 205, "5/31/2018", 
"2017_2018", "Gr 8 Math  CAT", "math", 377, "5/31/2018", 
"2017_2018", "EOC Reading ", "eng", 419, "6/1/2018", 
"2017_2018", "Geometry ", "math", 315, "6/1/2018", 
"2017_2018", "Geometry ", "math", 383, "6/1/2018", 
"2017_2018", "Geometry ", "math", 424, "6/1/2018", 
"2017_2018", "Geometry ", "math", 346, "6/1/2018", 
"2017_2018", "Gr 8 Read  CAT", "eng", 398, "6/1/2018", 
"2017_2018", "Gr 8 Science", "sci", 415, "6/4/2018", 
"2017_2018", "VA&US Hist ", "histss", 392, "6/4/2018", 
"2017_2018", "W Hist I ", "histss", 361, "6/4/2018", 
"2017_2018", "W Hist II ", "histss", 365, "6/4/2018", 
"2017_2018", "W Hist II ", "histss", 398, "6/4/2018", 
"2017_2018", "W Hist II ", "histss", 357, "6/4/2018", 
"2017_2018", "W Hist II ", "histss", 444, "6/4/2018", 
"2017_2018", "Biology ", "sci", 365, "6/5/2018", 
"2017_2018", "Earth Sci ", "sci", 387, "6/5/2018", 
"2017_2018", "W Hist II ", "histss", 353, "6/5/2018", 
"2017_2018", "Algebra I ", "math", 359, "6/6/2018", 
"2017_2018", "Algebra I ", "math", 339, "6/6/2018", 
"2017_2018", "Algebra I ", "math", 420, "6/6/2018", 
"2017_2018", "Algebra II ", "math", 322, "6/6/2018", 
"2017_2018", "Civics & Economics", "histss", 452, "6/6/2018", 
"2017_2018", "Geometry ", "math", 357, "6/6/2018", 
"2017_2018", "VA&US Hist ", "histss", 368, "6/6/2018", 
"2017_2018", "W Hist I ", "histss", 424, "6/6/2018", 
"2017_2018", "W Hist II ", "histss", 306, "6/6/2018", 
"2017_2018", "Biology ", "sci", 324, "7/31/2018", 
"2017_2018", "Geometry ", "math", 351, "7/31/2018", 
"2018_2019", "EOC Writing ", "eng", 314, "11/2/2018", 
"2018_2019", "Earth Sci ", "sci", 396, "2/22/2019", 
"2018_2019", "EOC Reading ", "eng", 374, "2/22/2019", 
"2018_2019", "Biology ", "sci", 385, "2/25/2019", 
"2018_2019", "W Hist II ", "histss", 352, "2/25/2019", 
"2018_2019", "Algebra I ", "math", 341, "2/26/2019", 
"2018_2019", "EOC Writing ", "eng", 357, "3/15/2019", 
"2018_2019", "EOC Writing ", "eng", 357, "3/19/2019", 
"2018_2019", "EOC Writing ", "eng", 432, "3/20/2019", 
"2018_2019", "EOC Writing ", "eng", 314, "3/20/2019", 
"2018_2019", "EOC Writing ", "eng", 359, "3/20/2019", 
"2018_2019", "EOC Writing ", "eng", 473, "3/20/2019", 
"2018_2019", "EOC Writing ", "eng", 430, "3/20/2019", 
"2018_2019", "Gr 8 Writing", "eng", 385, "3/20/2019", 
"2018_2019", "EOC Writing ", "eng", 580, "3/25/2019", 
"2018_2019", "EOC Writing ", "eng", 473, "3/29/2019", 
"2018_2019", "EOC Reading ", "eng", 375, "5/13/2019", 
"2018_2019", "EOC Reading ", "eng", 419, "5/13/2019", 
"2018_2019", "EOC Reading ", "eng", 346, "5/13/2019", 
"2018_2019", "Gr 6 Reading CAT", "eng", 260, "5/14/2019", 
"2018_2019", "Gr 7 Reading CAT", "eng", 422, "5/14/2019", 
"2018_2019", "Gr 8 Read  CAT", "eng", 381, "5/14/2019", 
"2018_2019", "Gr 8 Science", "sci", 444, "5/16/2019", 
"2018_2019", "Earth Sci ", "sci", 472, "5/17/2019", 
"2018_2019", "Earth Sci ", "sci", 407, "5/17/2019", 
"2018_2019", "Earth Sci ", "sci", 423, "5/17/2019", 
"2018_2019", "Earth Sci ", "sci", 427, "5/17/2019", 
"2018_2019", "Chemistry ", "sci", 398, "5/20/2019", 
"2018_2019", "W Hist II ", "histss", 379, "5/21/2019", 
"2018_2019", "W Hist II ", "histss", 390, "5/21/2019",
"2018_2019", "W Hist I ", "histss", 398, "5/22/2019", 
"2018_2019", "W Hist I ", "histss", 348, "5/22/2019", 
"2018_2019", "Gr 6 Math CAT", "math", 308, "5/23/2019", 
"2018_2019", "Gr 7 Math CAT", "math", 416, "5/23/2019", 
"2018_2019", "Gr 8 Math  CAT", "math", 375, "5/23/2019", 
"2018_2019", "VA&US Hist ", "histss", 417, "5/24/2019", 
"2018_2019", "VA&US Hist ", "histss", 396, "5/24/2019", 
"2018_2019", "VA&US Hist ", "histss", 449, "5/24/2019", 
"2018_2019", "Algebra I ", "math", 350, "5/28/2019", 
"2018_2019", "Algebra II ", "math", 367, "5/29/2019", 
"2018_2019", "Geometry ", "math", 328, "5/29/2019", 
"2018_2019", "Geometry ", "math", 369, "5/29/2019", 
"2018_2019", "Earth Sci ", "sci", 354, "5/30/2019", 
"2018_2019", "VA&US Hist ", "histss", 509, "5/30/2019", 
"2018_2019", "Civics & Economics", "histss", 407, "5/31/2019", 
"2018_2019", "Civics & Economics", "histss", 407, "5/31/2019", 
"2018_2019", "Geometry ", "math", 393, "5/31/2019", 
"2018_2019", "EOC Reading ", "eng", 386, "6/3/2019", 
"2018_2019", "Gr 7 Reading CAT", "eng", 318, "6/3/2019", 
"2018_2019", "Gr 8 Read  CAT", "eng", 435, "6/3/2019")  %>% 
mutate(date = mdy(date))   %>% 
mutate(core = factor(core))  %>% 
mutate(admin = factor(admin))



# dat
dat  %>% 
  count(core)
psych::describeBy(dat, group = dat$core)


dat  %>% 
ggplot(aes(x = date, y = scaled_score, color = core))  + 
# geom_jitter(alpha = 0.4, width = 0.25, height = 0.25)  +
geom_point(alpha = 0.4)+
geom_smooth() +
scale_color_inova() + 
theme_light() +
labs(title= "History of SOL Scores", 
     subtitle = "K A's Tenure",
     y = "Score", 
     x = "date", 
     color = "Subject Group"
    ) + 
scale_y_continuous(limits = c(200, 600)) +
scale_x_date(date_breaks = "1 month") +
theme(axis.text.x = element_text(angle = 70, hjust = 1))

dat  %>% 
ggplot(aes(x = core, y = scaled_score, color = core))  + 
geom_boxplot() +
scale_color_inova() + 
theme_light() +
labs(title= "SOL Scores", 
     subtitle = "K.A.'s Tenure",
     y = "Score", 
     x = "Subject Group", 
     color = "Subject Group"
    ) 


# library(ggridges)
# library(ggjoy)

anova(lm(scaled_score ~ core, data = dat))
car::leveneTest(scaled_score ~ core, data = dat)
# need heteroskedasticity-robust standard errors here.

dat  %>% 
ggplot(aes(x = scaled_score, fill = core)) + 
geom_density(alpha = 0.4)+ 
# scale_fill_inova() + 
theme_light() + 
labs(title = "SOL scores") + 
geom_vline(xintercept = 400)


dat  %>% 
ggplot(aes(x = admin, y = scaled_score, color = admin))  + 
geom_boxplot() +
scale_color_inova() + 
geom_jitter(alpha = 0.4, width = 0.25, color = "#999999") +
theme_light() +
labs(title= "SOL Scores", 
     subtitle = "KA's Tenure",
     y = "Score", 
     x = "Academic Year", 
     color = "Subject Group"
    ) 
    
dat   %>% 
    lm(scaled_score ~ admin*core, data = .)  %>% 
    summary()   
    
    
    
    
