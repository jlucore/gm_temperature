# color scale from chat gpt
generate_shades <- function(base_color, n, light_factor = 0.6, dark_factor = 0.6) {
  if (!requireNamespace("colorspace", quietly = TRUE)) {
    stop("The 'colorspace' package is required. Please install it with install.packages('colorspace').")
  }
  
  half_n <- floor(n / 2)
  
  # Generate lighter and darker colors from the base
  light_color <- colorspace::lighten(base_color, amount = light_factor)
  dark_color  <- colorspace::darken(base_color, amount = dark_factor)
  
  # Create gradients
  light_to_base <- colorRampPalette(c(light_color, base_color))(half_n + 1)
  base_to_dark  <- colorRampPalette(c(base_color, dark_color))(half_n + 1)
  
  # Combine, avoiding duplication
  if (n %% 2 == 1) {
    c(light_to_base[-length(light_to_base)], base_color, base_to_dark[-1])
  } else {
    c(light_to_base[-length(light_to_base)], base_to_dark[-1])
  }
}