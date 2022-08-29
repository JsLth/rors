load("inst/figures/route_sample")

p <- ggplot2::ggplot(t) +
  ggplot2::geom_sf(ggplot2::aes(color = units::drop_units(distance)), size = 0.5) +
  ggplot2::scale_color_distiller(palette = "OrRd") +
  ggplot2::guides(color = "none") +
  ggplot2::theme_void() +
  hexSticker::theme_transparent()

hexSticker::sticker(
  p,
  package = "ORSRouting",
  p_size = 20,
  s_x = 1,
  s_y = 0.75,
  s_width = 1.5,
  s_height = 1.5,
  h_fill = "#181c1f",
  h_color = "#b5152b",
  p_color = "#fbfcfd",
  filename = file.path("inst/figures/orsr_sticker.png")
)