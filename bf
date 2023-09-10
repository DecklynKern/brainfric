byte a
a <- 99

while ?a
  write a
  write " bottles of beer on the wall, "
  write a
  write " bottles of beer."
  writeln
  write "Take one down, pass it around."
  writeln
  dec a
  write a
  write " bottles of beer on the wall."
  writeln
end