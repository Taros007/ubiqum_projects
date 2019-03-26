df <- readr::read_csv('./input/ElectronidexTransactions2017.csv', col_names = F, trim_ws = T)

laptops <- c(
  "LG Touchscreen Laptop",
  "Acer Aspire",
  "HP Laptop",
  "ASUS Chromebook",
  "Apple MacBook Pro",
  "Apple MacBook Air",
  "Dell Laptop",
  "Eluktronics Pro Gaming Laptop",
  "Alienware Laptop",
  "HP Notebook Touchscreen Laptop PC"
  )

desktops <- c(
  "Lenovo Desktop Computer",
  "iMac",
  "HP Desktop",
  "ASUS Desktop",
  "Dell Desktop",
  "Intel Desktop",
  "Acer Desktop",
  "CYBERPOWER Gamer Desktop"
  )

monitors <- c(
  "Acer Monitor",
  "LG Monitor",
  "ASUS Monitor",
  "ASUS 2 Monitor",
  "Dell Monitor",
  "Samsung Monitor",
  "Sceptre Monitor",
  "ViewSonic Monitor",
  "AOC Monitor",
  "HP Monitor"
  )

iodevices <- c(
  "3-Button Mouse",
  "Logitech Wireless Mouse",
  "Microsoft Basic Optical Mouse",
  "Logitech 3-button Mouse",
  "Redragon Gaming Mouse  ",
  "HP Wireless Mouse",
  "Generic Black 3-Button",
  "Wireless Portable Mouse",
  "Gaming Mouse Professional",
  "Slim Wireless Mouse",
  "HP USB Keyboard",
  "Logitech Wireless Keyboard",
  "Rii LED Keyboard",
  "Logitech Keyboard",
  "Backlit LED Gaming Keyboard",
  "Dell Wired Keyboard",
  "Apple Wired Keyboard",
  "Apple Wireless Keyboard",
  "Apple Magic Keyboard",
  "Logitech MK550 Wireless Wave Keyboard and Mouse Combo",
  "Logitech Desktop MK120 Mouse and keyboard Combo",
  "Logitech MK270 Wireless Keyboard and Mouse Combo",
  "Dell KM117 Wireless Keyboard & Mouse",
  "EagleTec Wireless Combo Keyboard and Mouse",
  "Microsoft Wireless Comfort Keyboard and Mouse",
  "Microsoft Wireless Desktop Keyboard and Mouse",
  "Rii LED Gaming Keyboard & Mouse Combo",
  "Logitech MK360 Wireless Keyboard and Mouse Combo"
)

for (i in seq_along(laptops)){
  df[df == laptops[i]] <- "Laptop"
}

for (i in seq_along(desktops)){
  df[df == desktops[i]] <- "Desktop"
}

for (i in seq_along(monitors)){
  df[df == monitors[i]] <- "Monitor"
}

for (i in seq_along(iodevices)){
  df[df == iodevices[i]] <- "I/O-device"
}

readr::write_csv(df, './input/replaced.csv', na="", col_names = F)
