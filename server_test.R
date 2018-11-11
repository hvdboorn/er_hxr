server <- function(){
  while(TRUE){
    writeLines("Listening...")
    con <- socketConnection(host="192.168.2.1", port = 7337, blocking=TRUE,
                            server=TRUE, open="r+")
    data <- readLines(con, 1)
    print(data)
    close(con)
  }
}

client <- function(){
  while(TRUE){
    con <- socketConnection(host="192.168.2.1", port = 7337, blocking=TRUE,
                            server=FALSE, open="r+")
    sendme <- readline("Enter text to be upper-cased, q to quit: ")
    if(tolower(sendme)=="q"){
      break
    }
    write_resp <- writeLines(sendme, con)
    close(con)
  }
}
