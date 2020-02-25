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
  
    #return with flattened images 
    return(matrix(data=dt,nrow=num_data,ncol=num_rows*num_cols,byrow=TRUE))
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

one_hot_encoding <- function(x, k) {
  # prototype:
  # lambda x, k: np.array(x[:,None] == np.arange(k)[None,:], dtype=int)
  # Input: 
  #   x: array 
  #   k: integer
  x <- sapply(x,as.integer)
  
  one_hot <- function(digit,class) {
    one_hot_vector <- rep(0,class)
    one_hot_vector[digit+1] <- 1 
    return(one_hot_vector)
  }
  return(t(sapply(x,one_hot,class=k)))
}

load_mnist <- function() {
  mnist_data <- mnist() 
  
  numData <- dim(train_ima)
}

plot_images <- function(images,images_per_row=5, padding=5, 
                        digit_dim = c(28,28),colorMap=hcl.colors(20,"gray",rev=TRUE) ) {
  # Images should be a (N_images x pixels) matrix. 
  numImages <- dim(images)[1] 
  numRow <- ceiling(numImages / images_per_row) # numbe of row in this plot
  init_val <- 0
  out_img_width <- ((digit_dim[2] + padding)* images_per_row ) + padding
  out_img_height <- ((digit_dim[1] + padding) * numRow ) + padding
  out_img <- matrix(data = init_val, nrow = out_img_height, ncol = out_img_width )
  
  for(i in 1:numImages){
    img <- as.integer(images[i,])
    img_mat <- matrix(img,nrow=digit_dim[1],ncol=digit_dim[2],byrow=TRUE)
    #img_mat <- apply(img_mat,MARGIN=2,rev)
    row_idx <- as.integer((i-1) / images_per_row)
    col_idx <- ((i-1) %% images_per_row)
   
    row_i <- padding + (row_idx * (digit_dim[1] + padding) + 1)
    col_i <- padding + (col_idx * (digit_dim[2] + padding) + 1)
    out_img[row_i: (row_i + digit_dim[1] - 1), 
            col_i: (col_i + digit_dim[2] - 1)] <- img_mat
  }
  
  # plot config.
  # image function interprets Z matrix as a table of f(x[i],y[i])
  # so that the value of the x axis corresponds to row number and 
  # the y axis to column number, with column 1 at the bottom.
  # i.e. a 90 degree counter-clockiwise rotation of the conventional 
  #   printed layout of a matrix

  out_img <- apply(out_img,MARGIN=2,rev)
  out_plot <- image(z = t(out_img),col = colorMap,useRaster = TRUE, xaxt = 'n',yaxt = 'n') 
  return(out_plot)
}

save_images <- function(images,plot_name,...) {
  colorMap <- hcl.colors(20,"gray",rev=TRUE)
  png(filename = plot_name)
  plot_images(images,...)
  dev.off()
}

mnist_data = mnist()
test_imgs <- mnist_data$test_image[1:10,]
test_labels <- mnist_data$test_label[1:10]
test_img <- apply(test_imgs,MARGIN = c(1,2),FUN = as.integer)
g <- plot_images(test_img)
g <- apply(g,MARGIN=2,rev)

image(z = t(g),useRaster = TRUE)
