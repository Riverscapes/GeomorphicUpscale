# ToDo:
# Ask NK what the purpose of this script is for.  At the bottom she's checking for 
# objects (e.g., "network.file" which aren't even declared in the Upscale.Wrapper.R that this is called from)

#This script builds the project directory

#Natalie Kramer (n.kramer.anderson@gmail.com)
#Last Updated JUne 4, 2019

# Make directory structure -------------------------------------------------------------------

database.dir = paste(repo.dir, "\\Database", sep="")

if(!file.exists(proj.dir)){dir.create(proj.dir)}
if(!file.exists(database.dir)){print(paste("Database not found, check", repo.dir, "for presence of Database subfolder", sep=" "))}
                                        

# create inputs folder if doesn't already exist
input.dir = file.path(proj.dir, "Inputs")
if(!file.exists(input.dir)){dir.create(input.dir)}


#copy file to input directory and re-declare variable 
copy.read.func = function(file){
  
  split_path = function(path) {
    if (dirname(path) %in% c(".", path)) return(basename(path))
    return(c(basename(path), split_path(dirname(path))))
  }
  
  if(!file.exists(paste(input.dir, split_path(file)[1], sep = "\\"))){
    file.copy(from = file, to = input.dir)
    newfileloc = paste(input.dir,split_path(file)[1], sep="\\")
    data = read.csv(newfileloc)
  }else{data = read.csv(file)}
  
  return(data)
}
  
# copies over network, selections and braid.index to inputs folder

if(exists("selections.file")){
  selections = copy.read.func(selections.file)
}else{print("selections file not found")}

if(exists("braid.index.file")){
  braid.index = copy.read.func(braid.index.file)
}else{print("braid.index file not specified")}

if(exists("network.file")){
  network=copy.read.func(network.file)
}else{print("network file not specified")}

