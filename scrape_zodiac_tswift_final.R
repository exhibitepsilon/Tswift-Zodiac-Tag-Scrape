## Dealing with strings to extract IDs
library(stringr)

## Dealing with the tumblr API
library(httr)

## API keys
hname <- "returnoftheobradinn.tumblr.com"
ckey <- ""
seckey <- ""

setwd("")
full_html_text <- read.table("all_notes_html.txt",sep="<")
post_urls <- c()

post_ids <- c()
blog_names <- c()

for(i in 1:length(full_html_text)){
    current_string <- full_html_text[1,i]
    ## Is a url present in the string?
    if(!(grepl("/post/",current_string))){
        next
    } else{
        ## If yes, nab the urls
        url_start <- str_locate(current_string,"https://")[1,1]
        url_end <- str_locate(current_string,"/post/")[1,2] + 18
        url_str <- substr(current_string,url_start,url_end)
        post_urls <- c(post_urls,url_str)
    }
}

for(i in 1:length(post_urls)){
    current_url <- post_urls[i]
    is_view <- grepl("/view/",current_url)
    if(is_view){
        namestart <- (as.numeric(str_locate(current_url,"/view/")[1,2]))+1
        nameend <- nchar(current_url)-19
        postname <- substr(current_url,namestart,nameend)

        blog_names <- c(blog_names,postname)

        idstart <- nchar(current_url)-17
        idend <- nchar(current_url)
        postid <- substr(current_url,idstart,idend)

        post_ids <- c(post_ids,postid)
    } else{
        namestart <- 9
        nameend <- (as.numeric(str_locate(current_url,".tumblr.com")[1,1]))-1
        postname <- substr(current_url,namestart,nameend)

        blog_names <- c(blog_names,postname)

        idstart <- (as.numeric(str_locate(current_url,"/post/")[1,2]))+1
        idend <- nchar(current_url)
        postid <- substr(current_url,idstart,idend)

        post_ids <- c(post_ids,postid)
    }
}

all_postid <- c()
all_blognames <- c()
temp_tags_string <- paste0()
all_tags_raw <- c()


for(i in 1:length(post_ids)){
    Sys.sleep(2)
    print(paste0(i," out of ",length(post_ids)))
    current_id <- post_ids[i]
    current_name <- blog_names[i]
    print(current_name)
    post_url <- paste0("https://api.tumblr.com/v2/blog/",current_name,"/posts?api_key=",ckey,"&id=",current_id,collapse=NULL)
    postcontent <- content(GET(post_url))

    all_postid <- c(all_postid,current_id)
    all_blognames <- c(all_blognames,current_name)
    temp_tags_string <- paste0(postcontent$response$posts[[1]]$tags,collapse=",")
    all_tags_raw <- c(all_tags_raw,temp_tags_string)
}

tag_data_raw <- cbind(all_postid,all_blognames,all_tags_raw)
colnames(tag_data_raw) <- c("Post_ID","Blog_Name","Tags_Raw")

write.table(tag_data_raw,"zodiac_tswift_tags_raw.txt",row.names=F)
