# image making

library(magick)
names = c("vikander", "reynolds", "george")


for(k in seq_along(names)){
    
    name = names[k]
    
    image1 <- image_read(paste("www/", name,"1.jpg", sep = ""))
    image2 <- image_read(paste("www/", name,"2.jpg", sep = ""))
    
    image_scale(image1, "x800")
    image_scale(image2, "x800")
    
    combined <- image_append( c(image_scale(image1, "x800"),image_scale(image2, "x800")))
    image_write(combined, path = paste("www/",name,".jpg", sep = ""), format = "jpg")
    
}


### male celebrities

men = list.files("www/male_celebs_original" )

for(j in 1:(length(men)/2) ){
    
    image1 <- image_read(paste("www/male_celebs_original/", men[(2*j -1)], sep = ""))
    image2 <- image_read(paste("www/male_celebs_original/", men[(2*j)], sep = ""))
    
    minheight = min(c(image_info(image1)$height, image_info(image2)$height ))
    
    combined <- image_append( c(image_scale(image1, paste("x", minheight, sep = "")),
                                image_scale(image2, paste("x", minheight, sep = "")))
                              )
    
    image_write(combined, path = paste("www/male_celebs/male",j,".jpg", sep = ""), format = "jpg")
}