type t = int

let reached = 0x59A89C

let other = 0xA559AA

let nothing = 0xF0C571

let timeout = 0xE02B35

let killed = 0x082A54

let lines = 0xCECECE

let dark = 0x000000

let white = 0xFFFFFF

let to_string n = Format.sprintf "#%06x" n

let to_rgb n = `Rgb (n lsr 16, (n land 0x00FF00) lsr 8, n land 0x0000FF)
