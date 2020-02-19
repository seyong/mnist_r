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
  
  # [offset] [type] [value] [description]
  # 0000  32bit integer 0x000000801(2049) magic number (MSB first)
  # 0004  32bit integer 60000             number of items
  parse_labels <- function(filename) {
    fcon <- gzfile(filename,"rb")
    meta <- readBin(fcon,what="int",n=2,endian="big")
    magic <- meta[1]
    num_data <- meta[2]
    dt <- readBin(fcon,what="raw",n = num_data,endian="big")
    close(fcon)
    
    return(dt)
  }
  
  # [offset]  [type]  [valeu] [description]
  # 0000  32bit_integer 0x00000803(2051)  magic number
  # 0004  32bit_integer 10000 number of images
  # 0008  32bit_integer 28  number of rows
  # 0012  32bit_integer 28  number of columns
  parse_images <- function(filename) {
    fcon <- gzfile(filename,"rb")
    meta <- readBin(fcon,what="int",n=4,endian = "big") 
    magic <- meta[1]
    num_data <- meta[2]
    num_rows <- meta[3]
    num_cols <- meta[4]
    dt <- readBin(fcon,what="raw",n=num_data*num_rows*num_cols,endian="big") 
    close(fcon)
   
    return(array(data=dt,dim=c(num_data,num_rows,num_cols)))
  }
  
  filenames <- c("train-images-idx3-ubyte.gz","train-labels-idx1-ubyte.gz","t10k-images-idx3-ubyte.gz","t10k-labels-idx1-ubyte.gz")
  for(filename in filenames) {
    download(paste(base_url,filename,sep=""),filename)
  }
  
  train_images = parse_images('data/train-images-idx3-ubyte.gz')
  train_labels = parse_labels('data/train-labels-idx1-ubyte.gz')
  test_images = parse_images('data/t10k-images-idx3-ubyte.gz')
  test_labels = parse_labels('data/t10k-labels-idx1-ubyte.gz')
  
  return(list(train_images = train_images,train_labels = train_labels, test_images = test_images, test_labels = test_labels))
  
}

load_mnist <- function() {
  mnist_data <- mnist() 
  
}

plot_images <- function() {
  
}

save_images <- function() {
  
}