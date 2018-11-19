linewidth = 228 #height=55

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
