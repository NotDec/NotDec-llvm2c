define i32 @f(i512 %x) {
entry:
  %masked = and i512 %x, -1
  %trunc = trunc i512 %masked to i32
  ret i32 %trunc
}
