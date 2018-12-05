library(hgutils)
library(crayon)
library(BMS)
library(stringr)

server <- function(){
  while(TRUE){
    cat("Waiting for client to connect @ 192.168.2.3:7337...\n")
    con <- socketConnection(host="192.168.2.3", port = 7337, blocking=TRUE,
                            server=TRUE, open="r+", timeout = 3600)
    data <- readLines(con, 1)
    if(data=="REQUEST_CONNECT") {
      cat(green("Client connected"), "\n")
      writeLines("CONNECT_SUCCESS", con)
      
      while(TRUE) {
        pswd = readline("Enter password (QUIT to close connection): ")
        if(pswd=="")
          pswd="X"
        pswd = trimws(pswd %>% str_replace_all(" ", "")) %>% toupper
        writeLines(pswd, con)
        
        if(pswd == "QUIT")
          break;
        
        cat(green("Encrypted message sent.\n"))
      }
    }
    close(con)
  }
}

server()
