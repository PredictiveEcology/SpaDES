#install.packages("hexSticker")
#if (Sys.info()["sysname"] == "Darwin") install.packages("sysfonts")

library(hexSticker)
library(showtext)
library(sysfonts)

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Teko", "teko")
## Automatically use showtext to render text for future devices
showtext_auto()

imgurl <- "https://github.com/PredictiveEcology/SpaDES/raw/master/man/figures/SpaDES.png"

sticker(imgurl, package = "SpaDES",
        h_color = "#5bac47", h_fill = "#222222",
        p_color = "#cccccc", p_family = "teko", p_size = 28, p_x = 1, p_y = 1.5,
        s_x = 1, s_y = 0.75, s_width = 0.4, s_height = 0.4,
        #url = "http://spades.predictiveecology.org", u_color = "#cccccc", u_size = 4.5,
        filename = "stickers/hexsticker.png", spotlight = FALSE)
