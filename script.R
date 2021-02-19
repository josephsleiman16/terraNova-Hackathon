
smooth <- function(m, by) {
  z <- matrix(0, nrow = nrow(m) + 2 * by, ncol = ncol(m) + 2 * by)
  for (i in 0:(2 * by))
    for (j in 0:(2 * by))
      z[1:nrow(m) + i, 1:ncol(m) + j] <- z[1:nrow(m) + i, 1:ncol(m) + j] + m
    z <- z / (2 * by + 1) ^ 2
    
    return(z[1:nrow(m) + by, 1:ncol(m) + by])
}

c16 <- readPNG("c16.png")
c17 <- readPNG("c17.png")
c18 <- readPNG("c18.png")
c19 <- readPNG("c19.png")
c20 <- readPNG("c20.png")

u <- (c16 + c17 + c18 + c19 + c20) / 5
p <- u[nrow(u):1, 1:ncol(u), 1]

t <- (c16 + c17 + c18 + c19 + c20) / 5
c <- t[nrow(t):1, 1:ncol(t), 1]

cc <- smooth(c, 20)
image(1:ncol(c), 1:nrow(c), t(c), col = rev(heat.colors(50)), axes = FALSE)
image(1:ncol(cc), 1:nrow(cc), t(cc), col = rev(heat.colors(10)), axes = FALSE)

f16 <- readPNG("f16.png")
f17 <- readPNG("f17.png")
f18 <- readPNG("f18.png")
f19 <- readPNG("f19.png")
f20 <- readPNG("f20.png")

t <- (f16 + f17 + f18 + f19 + f20) / 5
f <- t[nrow(t):1, 1:ncol(t), 1]

ff <- smooth(f, 20)
image(1:ncol(c), 1:nrow(c), t(c), col = rev(heat.colors(50)), axes = FALSE)
image(1:ncol(f), 1:nrow(f), t(f), col = rev(heat.colors(50)), axes = FALSE)
image(1:ncol(cc), 1:nrow(cc), t(cc), col = rev(heat.colors(10)), axes = FALSE)
image(1:ncol(ff), 1:nrow(ff), t(ff), col = rev(heat.colors(10)), axes = FALSE)

u <- log(100 * c + 1) * log(100 * f + 1)
uu <- smooth(u, 10)
image(1:ncol(u), 1:nrow(u), t(u), col = rev(heat.colors(50)), axes = FALSE)
image(1:ncol(uu), 1:nrow(uu), t(uu), col = rev(heat.colors(10)), axes = FALSE, 
      xlab = "", ylab = "")
axis(1, labels = F)

c1 <- readPNG(paste0(c_txt, dates[[i + 1]]))
c2 <- readPNG(paste0(c_txt, dates[[i + 2]]))
c3 <- readPNG(paste0(c_txt, dates[[i + 3]]))
c4 <- readPNG(paste0(c_txt, dates[[i + 4]]))
c5 <- readPNG(paste0(c_txt, dates[[i + 5]]))
c6 <- readPNG(paste0(c_txt, dates[[i + 6]]))
c7 <- readPNG(paste0(c_txt, dates[[i + 7]]))
f1 <- readPNG(paste0(f_txt, dates[[i + 1]]))
f2 <- readPNG(paste0(f_txt, dates[[i + 2]]))
f3 <- readPNG(paste0(f_txt, dates[[i + 3]]))
f4 <- readPNG(paste0(f_txt, dates[[i + 4]]))
f5 <- readPNG(paste0(f_txt, dates[[i + 5]]))
f6 <- readPNG(paste0(f_txt, dates[[i + 6]]))
f7 <- readPNG(paste0(f_txt, dates[[i + 7]]))






library(animation)
library(png)

setwd("Documents/trnh/data")
int <- 5


dates <- as.Date(as.Date("2019-10-07"):as.Date("2020-01-05"), 
                 origin = "1970-01-01")
is <- 0:(length(dates) %/% int - 1) * int

c_txt <- "Sentinel-5P CO from "
f_txt <- "Sentinel-5P HCHO from "
png   <- ".png"

sam  <- readPNG(paste0(c_txt, dates[[1]], png))
rows <- nrow(sam)
cols <- ncol(sam)
rm(sam)

con <- readPNG("contour.png")
con <- con[rows:1, 1:cols, 1]
con <- which(con > 0.1, arr.ind = TRUE)
con <- con[, 2] + 1i * con[, 1]


for (i in is) {
  c <- array(0, dim = c(rows, cols, 3))
  f <- array(0, dim = c(rows, cols, 3))
  for (j in 1:int) {
    eval(parse(text = paste0(
      "c <- c + readPNG(paste0(c_txt, dates[[i + ", j, "]], png))")))
    eval(parse(text = paste0(
      "f <- f + readPNG(paste0(f_txt, dates[[i + ", j, "]], png))")))
  }
  
  cc <- c / int
  cc <- cc[rows:1, 1:cols, 1]
  ff <- f / int
  ff <- ff[rows:1, 1:cols, 1]
  
  uu <- log(100 * cc + 1) * log(100 * ff + 1)
  
  save(uu, file = paste0("uu-", i, ".mtr"))
}

maxs <- numeric()
for (i in is) {
  load(file = paste0("uu-", i, ".mtr"))
  maxs <- c(maxs, max(uu))
}

cls <- 50
hm  <- rev(heat.colors(cls))
hmi <- round(maxs / max(maxs) * cls)

saveGIF({
  ani.options(interval = 0.5, nmax = length(is))
  for (i in is) {
    load(file = paste0("uu-", i, ".mtr"))
    image(1:ncol(uu), 1:nrow(uu), t(uu), col = hm[1:hmi[[i %/% int + 1]]], 
          axes = FALSE, xlab = "", ylab = "", main = dates[[i + 1]])
    points(con, pch = '.', cex = 0.01)
  }
}, movie.name = "australia.gif", ani.height = rows %/% 2, 
ani.width = cols %/% 2)



