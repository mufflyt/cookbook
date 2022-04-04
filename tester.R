#Bespoke functions
tm_write2pdf <- 
  function(object, filename) {  
    #pass filename and title with quotations, do not add .pdf
    print("Function Sanity Check: Creating Arsenal Table as a PDF")
    arsenal::write2pdf(object,(paste0(filename, ".pdf")), 
                       #puts the file into a subdirectory called tables
                       keep.md = TRUE,
                       quiet = TRUE) # passed to rmarkdown::render
    #pander::openFileInOS((paste0(filename, ".pdf"))) #sweet
  }