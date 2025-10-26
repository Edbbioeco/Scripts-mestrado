# Instale as bibliotecas se ainda não estiverem instaladas
# install.packages("jpeg")
# install.packages("raster")

library(jpeg)
library(raster)


# Carrega a imagem JPEG
imagem <- readJPEG("testando.jpeg")

# Converte a imagem para um objeto raster
raster_imagem <- raster(imagem)

# Calcula a quantidade de pixels brancos na imagem
pixels_brancos <- sum(raster_imagem[] == 1)

# Calcula o índice de luz
indice_luz <- pixels_brancos / length(raster_imagem[])

# Exibe o índice de luz
print(indice_luz)

# Instale a biblioteca se ainda não estiver instalada
# install.packages("EBImage")

library(EBImage)

# Carrega a imagem JPEG
imagem <- readImage("img2.jpeg")

# Converte a imagem para escala de cinza
imagem_cinza <- channel(imagem, "gray")

# Calcula a quantidade de pixels brancos na imagem
pixels_brancos <- sum(imagem_cinza > 0)

# Calcula o índice de luz
indice_luz <- pixels_brancos / prod(dim(imagem_cinza))

# Exibe o índice de luz
print(indice_luz)

# Instale a biblioteca se ainda não estiver instalada
# install.packages("EBImage")

library(EBImage)

# Carrega a imagem JPEG
imagem <- readImage("img2.jpeg")

# Converte a imagem para escala de cinza
imagem_cinza <- channel(imagem, "gray")

# Calcula a quantidade de pixels brancos na imagem
pixels_brancos <- sum(imagem_cinza > 0)

# Calcula o índice de luz
indice_luz <- pixels_brancos / length(dim(imagem_cinza))

# Exibe o índice de luz
print(indice_luz)


# Instale a biblioteca se ainda não estiver instalada
# install.packages("jpeg")

library(jpeg)

# Carrega a imagem JPEG
imagem <- readJPEG("img.jpeg", native = T)

# Converte a imagem para escala de cinza
imagem_cinza <- RGB2gray(imagem)

# Calcula a quantidade de pixels brancos na imagem
pixels_brancos <- sum(imagem_cinza == 1)

# Calcula o índice de luz
indice_luz <- pixels_brancos / length(imagem_cinza)

# Exibe o índice de luz
print(indice_luz)

?rgb2gray



# Instale as bibliotecas se ainda não estiverem instaladas
# install.packages("jpeg")
# install.packages("raster")

library(jpeg)
library(raster)

# Carrega a imagem JPEG
imagem <- readJPEG("img2.jpeg", native = TRUE)

imagem_cinza <- channel(imagem, "gray")

# Calcula a quantidade de pixels brancos na imagem
pixels_brancos <- sum(imagem_cinza > 0)

# Calcula o índice de luz
indice_luz <- pixels_brancos / prod(dim(imagem_cinza))

# Exibe o índice de luz
print(indice_luz)

?readJPEG

hemispheR::canopy_fisheye(img)

png <- readJPEG("j.JPG")
png %>% plot

leafR::lai(png)

normlas.file = system.file("extdata", "lidar_example.laz", package="leafR")

normlas.file %>% class

# Calculate LAD from voxelization
VOXELS_LAD = lad.voxels(normlas.file,
                        grain.size = 2)

# Calculate the LAD profile
lad_profile = lad.profile(VOXELS_LAD)

lidar.lai = lai(lad_profile); lidar.lai

LAI::LAI_from_gf_at_57(png)

library(raster)

library(LAI)

test_image <-  system.file("extdata", "IMG_7595.JPG", package = "LAI")

# See the image
plotRGB(
  brick("imagem6.jpeg"),
  asp = 1
)

LAI::unimodal_threshold("imagem1.jpeg" |> raster())

# Calculate LAI
LAI::LAI_from_gf_at_57("imagem6.jpeg")

file_path = system.file("extdata", "IMG_7595.JPG", package = "LAI")

# View the original image
plotRGB(
  brick("imagem1.jpeg"),
  asp = 1
)

# View the binarized image
plot(unimodal_threshold(raster("imagem7.jpeg")),
     useRaster = TRUE,
     asp = 1
)

LAI::unimodal_threshold("imagem1.jpeg" |> raster()) |>
  as.data.frame(xy = TRUE)
