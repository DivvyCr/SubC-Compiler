; ModuleID = 'MiniC'
source_filename = "MiniC"

@glob = external global i32

define float @f(float %x) {
entry:
  %y = alloca i32, align 4
  %x1 = alloca float, align 4
  store float %x, ptr %x1, align 4
  %y2 = load i32, ptr %y, align 4
  %conv = sitofp i32 %y2 to float
  ret float %conv
}

define i1 @test(i1 %a, i1 %b, i1 %c, i1 %d, i1 %e) {
entry:
  %x = alloca i32, align 4
  %e5 = alloca i1, align 1
  %d4 = alloca i1, align 1
  %c3 = alloca i1, align 1
  %b2 = alloca i1, align 1
  %a1 = alloca i1, align 1
  store i1 %a, ptr %a1, align 1
  store i1 %b, ptr %b2, align 1
  store i1 %c, ptr %c3, align 1
  store i1 %d, ptr %d4, align 1
  store i1 %e, ptr %e5, align 1
  store i32 0, ptr %x, align 4
  %x6 = load i32, ptr %x, align 4
  %conv = sitofp i32 %x6 to float
  %0 = call float @f(float %conv)
  %a13 = load i1, ptr %a1, align 1
  br i1 %a13, label %logic.lhs11, label %logic.lhs9

lor.end:                                          ; preds = %logic.lhs11, %land.end
  %1 = phi i1 [ %2, %land.end ], [ true, %logic.lhs11 ]
  ret i1 %1

land.end:                                         ; preds = %logic.lhs9, %logic.lhs, %logic.rhs
  %2 = phi i1 [ %e7, %logic.rhs ], [ false, %logic.lhs ], [ false, %logic.lhs9 ]
  br label %lor.end

logic.rhs:                                        ; preds = %logic.lhs
  %e7 = load i1, ptr %e5, align 1
  br label %land.end

logic.lhs:                                        ; preds = %logic.lhs9
  %d8 = load i1, ptr %d4, align 1
  br i1 %d8, label %logic.rhs, label %land.end

logic.lhs9:                                       ; preds = %entry, %logic.lhs11
  %c10 = load i1, ptr %c3, align 1
  br i1 %c10, label %logic.lhs, label %land.end

logic.lhs11:                                      ; preds = %entry
  %b12 = load i1, ptr %b2, align 1
  br i1 %b12, label %lor.end, label %logic.lhs9
}
