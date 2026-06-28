define i32 @f(ptr %p) {
entry:
  %x = load i512, ptr %p
  %trunc = trunc i512 %x to i32
  ret i32 %trunc
}
