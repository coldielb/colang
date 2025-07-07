; COLANG Module: main
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

declare i32 @printf(i8*, ...)
declare void @exit(i32)
declare i8* @malloc(i64)
declare void @free(i8*)

define i32 @main() {
  %0 = add i32 0, 42
  %1 = add i32 0, 10
  %2 = add i32 %0, %1
  ret i32 %2
}

