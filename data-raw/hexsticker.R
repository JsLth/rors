library(ggplot2)
library(hexSticker)

if (!any_mounted() && !grepl("rutland", get_instance()$extract$name)) {
  ors <- ors_instance(dir = tempdir())
  ors$set_name("ors-hexsticker")
  ors$set_port()
  ors$set_extract("Rutland")
}

set.seed(222)
src <- ors_sample(size = 50)
dst <- ors_sample(size = 50)

routes <- ors_pairwise(src, dst, radiuses = -1, geometry = TRUE)
routes <- st_transform(routes, "ESRI:102014")
geom <- st_geometry(routes)
cent <- st_centroid(geom)
geom <- (geom - cent[1]) * rot(25 * 2 * pi / 360) + cent[1]
st_geometry(routes) <- geom

rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

p <- ggplot(routes) +
  geom_sf(aes(color = distance), size = 0.5) +
  scale_color_distiller(palette = "OrRd") +
  guides(color = "none") +
  theme_void() +
  theme_transparent()

hexSticker::sticker(
  p,
  p_y = 1.5,
  p_family = "mono",
  package = "rors",
  p_size = 35,
  s_x = 1,
  s_y = 0.8,
  s_width = 1.5,
  s_height = 1.5,
  h_fill = "#181c1f",
  h_color = "#b5152b",
  p_color = "#fbfcfd",
  url = "https://jslth.github.io/rors/",
  u_color = "#fbfcfd",
  u_size = 4,
  u_family = "mono",
  filename = file.path("man/figures/rors_sticker.png"),
  dpi = 400
)

ors$purge()
