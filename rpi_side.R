library(hgutils)
startup()
library("BMS")
library("stringr")
library("crayon")
source("image_util.R")

settings <<- c(bits="128", TZ="GMT+1", colorcoding="FALSE", encryption="TRUE")
text = "The hexadecimal numeral system, also known as just hex, is a numeral system made up of 16 symbols (base 16). 
The standard numeral system is called decimal (base 10) and uses ten symbols: 0,1,2,3,4,5,6,7,8,9. Hexadecimal uses the decimal numbers and includes six extra symbols. 
There are no symbols that mean ten, or eleven etc. so these symbols are letters taken " %+% red("f") %+% "rom the English alphabet: A, B, C, D, E and F. 
Hexadecimal A = decimal 10, and hexadecimal F = decimal 15.
Humans mostly use the decimal system. This is probably because humans have ten fingers (ten digits). 
Computers however, only have on and off, called a binary digit (or bit, for short). " %+% red("A") %+% " binary number is just a string of zeros and ones: 11011011, for example. 
For convenience, engineers working with computers tend to group bits together. In earlier days, such as the 1960's, 
they would group 3 bits at a time (much like large decimal numbers are grouped in threes, like the number 123,456,789). 
Three bits, each being on or off, can represent the eight numbers from 0 to 7: 000 = 0; 001 = 1; 010 = 2; 011 = 3; 100 = 4; 101 = 5; 110 = 6 and 111 = 7. 
This is called o"%+% red("c") %+% "tal. As computers got bigger, it was more convenient to group bits by four instead of three. 
This doubles the numbers that the symbol would represent; it can h"%+% red("a") %+% "ve 16 values instead of eight. 
Hex = 6 and Decimal = 10, so it is called hexa"%+% red("d") %+% "ecimal. 
Four bits is called a nibble (sometimes spelled nybble). A nibble is one hexadecimal digit, and is written using a symbol 0-9 or A-F. 
Two nibbles is a byte (8 bits). Most computer operations use the byte, or a multiple of the byte (16 bits, 24, 32, 64, etc.). 
Hexadecimal makes it easier to write these large binary numbers. To avoid confusion with decimal, octal or other numbering systems, 
hexadecimal numbers are sometim"%+% red("e") %+% "s written with a \"h\" after the number. For example, 63h means 63 hexadecimal. 
Software developers quite often use 0x before the number (0x63)."

clc = function() {cat("\014");  system("clear")}

display_enc = function(settings) {
  clc()
  if("colorcoding" %in% names(settings) && settings["colorcoding"] == "TRUE") {
    cat(text)
  } else {
    cat(strip_style(text))
  }
  cat("\n\n")
  readline("Press enter to continue...")
}
translate = function() {
  clc()
  input = toupper(readline("Enter phrase: "))
  if (str_detect(input, "^[0-9A-F]+$")) {
    output = paste0(hex2bin(input), collapse = "")
    output = paste0(strsplit(output,"(?<=\\G.{4})", perl = TRUE)[[1]], collapse = " ")
    cat("Output:", green(output))
  } else {
    cat(red("ERROR: input must be hexadecimal."))
  }
  
  cat("\n\n")
  readline("Press enter to continue...")
}
update_settings = function(settings) {
  while (TRUE) {
    clc()
    cat(cyan("Current settings: "),"\n")
    cat(paste0(names(settings), ": ", green(settings),collapse = "\n"), "\n\n\n")
    val = menu(names(settings), title = "Change value (0 to exit)")
    if(val != 0) {
      new_val = readline(paste0("New value for [", names(settings)[val],"]: "))
      settings[val] = new_val
    } else {
      break
    }
  }
  settings
}
client <- function(){
  clc()
  message = readRDS("message.RDS")
  while(TRUE){
    cat("Connecting to server @ " %+% cyan("192.168.2.3:7337" %+% black(".\n")))
    readline("First start the server and then press enter to continue...")
    con <- socketConnection(host="192.168.2.3", port = 7337, blocking=TRUE,
                            server=FALSE, open="r+", timeout=15)
    writeLines("REQUEST_CONNECT", con)
    data = readLines(con, 1)
    if(data=="CONNECT_SUCCESS") {
      display_text(message, linewidth, "X")
      cat("\n")
      once = FALSE
      while(TRUE) {
        pswd = readLines(con, 1)
        if (length(pswd)>0) {
          if(pswd=="QUIT") {
            close(con)
            return()
          } else {
            display_text(message, linewidth, pswd)
            cat("\n")
          }
        }
      }
    }
    close(con)
  }
}

main_menu = function() {
  while (TRUE) {
    clc()
    options = c("Encyclopaedia", "Translate", "Settings", "Start client")
    val = menu(options, title="Choose option")
    if(val == 1) {
      display_enc(settings)
    } else if (val == 2) {
      translate()
    } else if (val == 3) {
      settings <<- update_settings(settings)
    } else if (val == 4) {
      client()
    }
  }
}
main_menu()
