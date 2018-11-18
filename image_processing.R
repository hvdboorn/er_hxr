library(hgutils)
startup()
library("BMS")
library("stringr")
library("crayon")

linewidth = 228 #height=55
key = "FACADE" %>% hex2bin %>% paste0(collapse = "")

generate_text = function(key, linewidth) {
  ok = FALSE
  while(!ok) {
    img = load.image("mssg.png")
    width = width(img); height=height(img)
    img %<>% resize(size_x=linewidth, size_y=round(linewidth/width*height), size_c = 1) %>% {.>0.5} %>% as.matrix
    img = apply(t(img), c(1,2), function(x) ifelse(x,ifelse(runif(1)>0.9,"1","0"),"1"))
    text = img %>% apply(1, function(x) paste0(x,collapse = "")) 
    
    ok = !any(sapply(text, function(x) str_detect(x,key)))
  }
  text %<>% sapply(function(x) {r = floor(runif(1,0,nchar(x))); paste0(str_sub(x,1,r), key, str_sub(x,r+1,nchar(x)))})
  text %<>% paste0(collapse = "")
}

splt = function(str, length = 180) {
  acc = c()
  while(nchar(str) > length) {
    acc = c(acc, substr(str, 1, length))
    str = substr(str,length+1,nchar(str))
  }
  acc
}

display_text = function(text, linewidth, pswd =  "X") {
  txt = splt(text %>% str_replace_all(pswd, ""), linewidth) %>% str_replace_all("1", green("1"))
  cat(paste0(txt, collapse = "\n"))
}

#library("imager")
#text = generate_text(key, linewidth)
#saveRDS(text, "message.RDS")

client <- function(){
  text = readRDS("message.RDS")
  while(TRUE){
    readline("Connecting to server: first run the server and then press enter to continue...")
    con <- socketConnection(host="192.168.2.3", port = 7337, blocking=TRUE,
                            server=FALSE, open="r+", timeout=15)
    writeLines("REQUEST_CONNECT", con)
    data = readLines(con, 1)
    if(data=="CONNECT_SUCCESS") {
      while(TRUE) {
        cat("Waiting for password...\n")
        pswd = readLines(con, 1)
        if (length(pswd)>0) {
          if(pswd=="QUIT") {
            close(con)
            return()
          } else {
            display_text(text, linewidth, pswd)
            cat("\n")
          }
        }
      }
    }
    close(con)
  }
}
#writeLines(text, "image_output.txt")
#regex = key %>% str_replace_all("1","[5-9]") %>% str_replace_all("0","[0-4]")
#remove all regex occurences of: 11111.1.11..1.1.11.1111.
#set all [0,2-9] occurrences to " "

#code = "111110101100101011011110"
#code="111110101100101011011110"
#rotate <- function(x) t(apply(x, 2, rev))
#img = str_replace_all(text, code,"") %>% {str_split(., "")[[1]]} %>% {stfu(matrix(., ncol=linewidth, byrow = TRUE))} %>% rotate
#df = melt(img) %>% set_colnames(c("x","y","val"))
#ggplot(df, aes(x=x, y=y, fill=val)) + geom_raster() + cowplot::theme_nothing() + scale_fill_manual(values = c("white","black"))
