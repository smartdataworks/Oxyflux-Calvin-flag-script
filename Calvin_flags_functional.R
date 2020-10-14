#!/usr/bin/env R
### Written by Emanuel Blei, Edinburgh 2019, Georg-August University, Goettingen
### This file contains helper methods to automatically handle Calvin's error 
### flags and to translate them easily into hunman-readable form. 
### The functions distinguish between two types of files 
### as the length and meaning of the flags is different.
### "flag.messages(obj)" will print out the error messages for each row.
### "flag.codes(obj)" will give a more easily searchable output that is
### not convenient to read. Use the function flag.explanation() to see a map of 
### the flag codes given by flag.codes(obj). 
### 

###########################################################################

flag_explanation <- function() {
  writeLines("
             Flag codes and messages for 'DIG' and 'SEC' files:
             flag code | Message
             -------------------------------
             0         | 'All good'
             a         | 'P20 outside range'
             b         | 'P22 outside range'
             c         | 'FL20 outside range'
             d         | 'M20 - FL20 outside range'
             e         | 'T2 outside range'
             f         | 'T20 outside range'
             g         | 'T21 outside range'
             h         | 'T40 – T45 outside range'
             i         | 'Aspirated inlet fan has stopped'
             j         | 'Labjack error'
             
             Flag codes and messages for 'AIR', 'DEF', 'DIF', 'AIR', 'LSS', 'OTHERS', 'SPAN', 'TARGET', 'WSS', and 'ZERO' files. 
             flag code | Message
             -------------------------------             
             0         | 'All good'
             1         | 'DIG error (please see above)'
             2         | 'Bad Zero Cal'
             3         | 'Bad CO2 Cal''
             4         | 'Bad O2 Cal'
             5         | 'Bad Target CO2'
             6         | 'Bad Target O2''
             7         | 'Unrealistic CO2',
             8         | 'Unrealistic O2'
             ")
}



check_pattern <- function(string){
  check.pattern <- grepl(pattern="^\\[[0|A-Z]{8}\\]$|^\\[[0|A-Z]{10}\\]$", x=string, useBytes=TRUE)
  check.pattern <- all(check.pattern, na.rm=TRUE)
  if(check.pattern==FALSE) stop("This is not a valid flag code./n")
}


status_ok <- function(obj) {
  # This function simply states whether or not the status is ok.
  # TRUE, means no error messages, FALSE means that there is at least on error flag up. 
  check_pattern(obj)
  output <- grepl(pattern="\\[00000000\\]|\\[0000000000\\]", x=obj, useBytes=TRUE)
  return(output)
}


flag_codes <- function(obj){
  # This function is meant to give machine searchable flags.
  # The input is a calvin_flag object.
  # The output is a string of numbers and lower case letters.
  # Use the output with 'grepl(flag.code, data.frame$column)'.
  # You can see all the flag codes by typing 'flag.explanation()'.
  # ################################################################
  # Check that the input is valid.
  check_pattern(obj)
  # Remove the square brackets
  obj <- gsub(pattern="\\[|\\]", replacement ="", x=obj)
  # Convert the flags string into a vector of TRUE and FALSE
  flag.code <- lapply(obj, function(x) !unlist(strsplit(x, split="", fixed=TRUE)) %in% 0)
  # Apply the logical vector to the character vector with the warning messages.
  flags <- lapply(flag.code, function(x) switch(as.character(length(x)),
                                                "10"=(function(y) y[x])(letters[1:10]),
                                                "8"=(function(y) y[x])(1:8)
                                                )
                  )
  flags <- lapply(flags, unlist)
  # If there flag code string is empty set it to '0' instead.
  flags <- sapply(flags, function(x) if(length(x)==0) 0 else x, USE.NAMES=FALSE)
   # Combine the flag codes into one string.
  flags <- sapply(flags, function(x) paste0(x, collapse=""), USE.NAMES=FALSE)
  return(flags)
}


flag_messages <- function(obj){
  # This function is supposed to give human readable flag output.
  # The output will give one long output string separated by commas
  # and will fit into one column.
  # For more searchable output please use the function 'flag.codes'
  # instead.
  # ################################################################
  # First make a list of the messages.
  ft.1.messages <- c("P20 outside range",
                     "P22 outside range",
                     "FL20 outside range",
                     "M20 - FL20 outside range",
                     "T2 outside range",
                     "T20 outside range",
                     "T21 outside range",
                     "T40 – T45 outside range",
                     "Aspirated inlet fan has stopped",
                     "Labjack error")
  ft.2.messages <- c("DIG error",
                     "Bad Zero Cal",
                     "Bad CO2 Cal",
                     "Bad O2 Cal",
                     "Bad Target CO2",
                     "Bad Target O2",
                     "Unrealistic CO2",
                     "Unrealistic O2")
  # Check that the input is valid.
  check_pattern(obj)
  # Remove the square brackets
  obj <- gsub(pattern="\\[|\\]", replacement ="", x=obj)
  # Convert the flags string into a vector of TRUE and FALSE
  flags <- lapply(obj, function(x) !unlist(strsplit(x, split="", fixed=TRUE), use.names=FALSE) %in% 0)
  messages <- sapply(flags, function(x) switch(as.character(length(x)), 
                     "10"=ft.1.messages[x], 
                     "8"=ft.2.messages[x]
                     ), USE.NAMES=FALSE
  )
  messages <- sapply(messages, function(x) paste0(x, collapse=", "), USE.NAMES=FALSE)
  # If there is no warning message, write 'All good' instead.
  messages <- sapply(messages, function(x) if(nchar(x)==0) {"All good"} else {x}, USE.NAMES=FALSE)
  return(messages)
}
