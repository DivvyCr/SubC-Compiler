; ModuleID = 'MiniC'
source_filename = "MiniC"

define i32 @arithmetic(i32 %a, i32 %b, i32 %c) {
entry:
  %a1 = alloca i32, align 4
  store i32 %a, ptr %a1, align 4
  %b2 = alloca i32, align 4
  store i32 %b, ptr %b2, align 4
  %c3 = alloca i32, align 4
  store i32 %c, ptr %c3, align 4
  %result = alloca i32, align 4
  %a4 = load i32, ptr %a1, align 4
  %b5 = load i32, ptr %b2, align 4
  %c6 = load i32, ptr %c3, align 4
  %0 = mul i32 %b5, %c6
  %1 = add i32 %a4, %0
  store i32 %1, ptr %result, align 4
  %result7 = load i32, ptr %result, align 4
  ret i32 %result7
}
