# MNIST load package
# 

download <- function(url, filename) {
  error_handling <- function(cond) {
    
  }
  
  ret <- tryCatch(
    {
      if(!dir.exists("data")) {
        dir.create("data");
      }
      out_file = paste(file.path("data"),filename,sep="/")
      
      if(!file.exists(out_file)) {
        temp_file <- tempfile()
        result <- download.file(url,temp_file)
        
        if( result != 0 | file.exists(temp_file) == FALSE ) {
          # filesize is not considered yet.
          warning(sprintf(" file download error, unable to locate."))
        }
        
        file.copy(temp_file,out_file)
        unlink(temp_file)
        return(0)
      }
    },#try-catch
    error = error_handling,
    warn = error_handling,
    finally = {
      if( file.exists(out_file)) {
        cat("Successfully download MNIST dataset in ",out_file,"\n",sep="")
        return(0)
      } else {
        if( file.exists(out_file)) file.remove(out_file)
        message("Something went wrong")
        return(1)
      }
    }
  )
  invisible(ret)
}

mnist <- function() {
  base_url = "http://yann.lecun.com/exdb/mnist/"
  
  parse_labels <- function(filename) {
    # [offset] [type] [value] [description]
    # 0000  32bit integer 0x000000801(2049) magic number (MSB first)
    # 0004  32bit integer 60000             number of items
    fcon <- gzfile(filename,"rb")
    meta <- readBin(fcon,what="int",n=2,endian="big")
    magic <- meta[1]
    num_data <- meta[2]
    dt <- readBin(fcon,what="raw",n = num_data,endian="big")
    close(fcon)
    
    return(dt)
  }
  
  parse_images <- function(filename) {
     
  }
  
  filenames <- c("train-images-idx3-ubyte.gz","train-labels-idx1-ubyte.gz","t10k-images-idx3-ubyte.gz","t10k-labels-idx1-ubyte.gz")
  for(filename in filenames) {
    download(paste(base_url,filename,sep=""),filename)
  }
}

load_mnist <- function() {
  
}

plot_images <- function() {
  
}

save_images <- function() {
  
}