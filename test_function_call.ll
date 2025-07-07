; COLANG Module: main
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

declare i32 @printf(i8*, ...)
declare void @exit(i32)
declare i8* @malloc(i64)
declare void @free(i8*)

define i32 @main() {
  %4 = add i32 0, 5
  %5 = add i32 0, 10
  %6 = call i32 @add(i32 %4, i32 %5)
  ret i32 %6
}

define i32 @add(i32 %0, i32 %1) {
  %2 = add i32 %0, %1
  ret i32 %2
}

