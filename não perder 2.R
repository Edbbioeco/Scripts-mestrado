library(coveR)
image <- system.file('extdata','imagem1.JPEG',package='coveR')
image <- system.file('extdata','IMG1.JPG',package='coveR')
image1 <- open_blue(image,which.blue=3,exif=TRUE,crop=NULL)
plot(image1)
thd_blue(img1,method='Minimum', display=TRUE)
image1 |> as.data.frame(xy = T)


devtools::install_git("https://gitlab.com/fchianucci/hemispheR")
image<-system.file('extdata/circular_coolpix4500+FC-E8_chestnut.jpg',package='hemispheR')
image
img<-import_fisheye(image,
                    channel = 3,
                    circ.mask=list(xc=1136,yc=852,rc=754),
                    circular=TRUE,
                    gamma=2.2,
                    stretch=FALSE,
                    display=TRUE,
                    message=TRUE)

image2<-system.file('extdata/fullframe_D90_Nikkor-10.5_beech.jpg',package='hemispheR')
img2<-import_fisheye(image2,
                     circular=FALSE,
                     gamma=2.2,
                     display=TRUE,
                     message=TRUE)

img.bw<-binarize_fisheye(img,
                         method='Otsu',
                         zonal=FALSE,
                         manual=NULL,
                         display=TRUE,
                         export=FALSE)
binarize_fisheye(img,
                 method='Otsu',
                 zonal=FALSE,
                 manual=NULL,
                 display=TRUE,
                 export=FALSE)
img.bw %>% as.data.frame()





library(tidyv)
