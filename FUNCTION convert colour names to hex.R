# converts colour name to hex code
# by Mikael Jagan https://stackoverflow.com/users/12685768/mikael-jagan
# posted at https://stackoverflow.com/questions/70121336/hexadecimal-codes-for-r-built-in-colours
col2hex <- function(x, alpha = FALSE) {
  args <- as.data.frame(t(col2rgb(x, alpha = alpha)))
  args <- c(args, list(names = x, maxColorValue = 255))
  do.call(rgb, args)
}