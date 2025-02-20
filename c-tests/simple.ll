declare i8* @malloc(i32)
define i32 @main() {
main__entry:
  %a = bitcast i32 1 to i32
  %b = bitcast i32 2 to i32
  %temp1 = bitcast i32 %a to i32
  %temp2 = bitcast i32 %b to i32
  %c = add i32 %temp1, %temp2
  %temp4 = bitcast i32 %c to i32
  %temp5 = bitcast i32 3 to i32
  %temp3 = add i32 %temp4, %temp5
  ret i32 %temp3
label1:
  ret i32 0
}
