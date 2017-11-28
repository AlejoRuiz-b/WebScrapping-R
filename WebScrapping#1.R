# WebScrapping
# Name: Luis Alejandro Ruiz Bareno


# Import the library for loading web pages
library(rvest)


# Assignment 1.1 - Write a function to fetch and save all
# of the vispubdata pages found here: 
vispubdata_root <- 'https://www.lri.fr/~isenberg/VA/vispubdata'
vispubdata_first <- 'https://www.lri.fr/~isenberg/VA/vispubdata/pub_output_0.html'
#----------------------------------------------------------------------------------
#Create the directory and save the file there
#----------------------------------------------------------------------------------
fetch_and_save_vispub_page <- function(page_name){
  
  full_url  <- paste(vispubdata_root, '/', page_name,sep="")
  
  directory <- "Assignment_1"
  if (!file.exists(directory)){
    dir.create(file.path(directory))
  }
  
  # And save the file there
  file_name = paste('Assignment_1','/',page_name,'.html',sep="")
  
  url_open = tryCatch({
    download.file(full_url,file_name)
    
  }, warning = function(w){
    #print("a warning was issued when opening the website")  
    #print(w)
  }, error = function(e){
    print(e)
  }) 
  
  print(paste('saved ',file_name))
  
}


#-----------------------------------------------------------------------------------------------
#This function is used to create the page name and then call the function "fetch_and_save_vispub_page"
#------------------------------------------------------------------------------------------------
fetch_vispubdata_pages <- function(file_name){
  print('TODO: Fetch vispubdata pages and save them locally')
  
  for (i in 0:137){
    file_name2 = paste(file_name, i, sep = "")
    fetch_and_save_vispub_page (file_name2)
  }
  df_final <- data.frame(0)
  
}

#------------------------------------------------------------------------------------------------------

# Assignment 1.2 - Write a function that processe s all
#  of the saved vispubdata files and produces a CSV


# -------- Saving Results to a CSV file -------- #
#---------------------------------------------------------------------------------------------
# this functions works for read in file, find all the links in the document, get all links
# Build a data frame with the HTML DATA, and return this Data frame
#---------------------------------------------------------------------------------------------
process_vispub_csv <- function(file_name){
  # Read in the file and parse the html
  html <- read_html(file_name)
  
  # Find all the links in the document
  all_links <- html_nodes(html,'div.conference')
  all_year  <- html_nodes(html,'div.year')
  all_pap   <- html_nodes(html,'div.paper_title')
  all_doi   <- html_nodes(html,'div.DOI')
  all_link  <- html_nodes(html,'div.Link')
  all_page  <- html_nodes(html,'span.pagenumbers')
  all_abstr <- html_nodes(html,'div.Abstract')
  all_autho <- html_nodes(html,'div.Authors')
  all_refe  <- html_nodes(html,'div.References')
  all_key   <- html_nodes(html,'div.Keywords')
  
  #get all hrefs
  cnf <- html_attr(all_links,"conference")
  yer <- html_attr(all_year,"year")
  pap <- html_attr(all_year,"paper_title")
  doi <- html_attr(all_pap,"DOI")
  
  #get all link texts
  link_texts <- html_text(all_links)
  link_year  <- html_text(all_year)
  link_pap   <- html_text(all_pap)
  link_doi   <- html_text(all_doi)
  link_link  <- html_text(all_link)
  link_page  <- html_text(all_page)
  link_abstr <- html_text(all_abstr)
  link_autho <- html_text(all_autho)
  link_refe  <- html_text(all_refe)
  link_key   <- html_text(all_key)
  
  df <- data.frame(link_texts,
                   link_year,
                   link_pap,
                   link_doi,
                   link_link,
                   link_page,
                   link_abstr,
                   link_autho,
                   link_refe,
                   link_key,
                   stringsAsFactors=FALSE) 
  
  return(df)
}

#------------------------------------------------------------------------------------------------
#this function Creates the page name, call de funcion process_vispub_csv
#Then creates a data frame final where we will have the all information of all pages
#If the final data frame is 0 write the information of df_to_write
#If not 0 its gets a rbind and put the information in the final data frame
#then write or generate the CSV with all information of DF_FINAL
#------------------------------------------------------------------------------------------------
process_Vispub_save_csv<- function(file_name){
  
  #df_to_write<-process_vispub_csv('Assignment_1/pub_output_0.html')
  for (i in 0:137){
    name_page  <- paste(file_name, i, ".html",sep="")
    df_to_write<-process_vispub_csv(name_page)
    
    if(df_final == 0)
      df_final<-df_to_write
    
    else
      df_final<- rbind(df_final, df_to_write)
    
  }
  
  #-------------------------------------------------------------------------------------------------------
  #I had problems using this method because when the csv was created, its put double quotes in all rows
  #-------------------------------------------------------------------------------------------------------
  #View(df_to_write)
  #csv_file_name <- paste('Assignment_1/',"_links2.csv",sep="")
  
  #write.csv(df_to_write, csv_file_name)
  
  write.table(df_final, file = "Assignment_1/MyData.csv",row.names=FALSE, na="",col.names=TRUE, sep=";")
}

#------------------------------------------------------------------------------------------------------
# call the function Fetch_vispubdata_pages 
#------------------------------------------------------------------------------------------------------

fetch_vispubdata_pages('pub_output_')

#------------------------------------------------------------------------------------------------------
# call the function process_Vispub_save_csv
#------------------------------------------------------------------------------------------------------
process_Vispub_save_csv('Assignment_1/pub_output_')



