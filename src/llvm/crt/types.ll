; ModuleID = 'types.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i8*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type { %struct._IO_marker*, %struct._IO_FILE*, i32 }
%struct._value_t = type { i8, i32 }

@stderr = external global %struct._IO_FILE*
@.str = private unnamed_addr constant [15 x i8] c"(p%p,t%d,v%d)\0A\00", align 1
@.str1 = private unnamed_addr constant [9 x i8] c"(undef)\0A\00", align 1

; Function Attrs: nounwind uwtable
define %struct._value_t* @new_value_int(i32 %val) #0 {
  %1 = alloca i32, align 4
  %new_val = alloca %struct._value_t*, align 8
  store i32 %val, i32* %1, align 4
  %2 = call noalias i8* @calloc(i64 1, i64 8) #3
  %3 = bitcast i8* %2 to %struct._value_t*
  store %struct._value_t* %3, %struct._value_t** %new_val, align 8
  %4 = load %struct._value_t** %new_val, align 8
  %5 = getelementptr inbounds %struct._value_t* %4, i32 0, i32 0
  store i8 1, i8* %5, align 1
  %6 = load i32* %1, align 4
  %7 = load %struct._value_t** %new_val, align 8
  %8 = getelementptr inbounds %struct._value_t* %7, i32 0, i32 1
  store i32 %6, i32* %8, align 4
  %9 = load %struct._value_t** %new_val, align 8
  ret %struct._value_t* %9
}

; Function Attrs: nounwind
declare noalias i8* @calloc(i64, i64) #1

; Function Attrs: nounwind uwtable
define void @free_value(%struct._value_t* %value) #0 {
  %1 = alloca %struct._value_t*, align 8
  store %struct._value_t* %value, %struct._value_t** %1, align 8
  %2 = load %struct._value_t** %1, align 8
  %3 = bitcast %struct._value_t* %2 to i8*
  call void @free(i8* %3) #3
  ret void
}

; Function Attrs: nounwind
declare void @free(i8*) #1

; Function Attrs: nounwind uwtable
define zeroext i8 @values_equal(%struct._value_t* %value1, %struct._value_t* %value2) #0 {
  %1 = alloca %struct._value_t*, align 8
  %2 = alloca %struct._value_t*, align 8
  %equal = alloca i8, align 1
  store %struct._value_t* %value1, %struct._value_t** %1, align 8
  store %struct._value_t* %value2, %struct._value_t** %2, align 8
  store i8 0, i8* %equal, align 1
  %3 = load %struct._value_t** %1, align 8
  %4 = getelementptr inbounds %struct._value_t* %3, i32 0, i32 0
  %5 = load i8* %4, align 1
  %6 = zext i8 %5 to i32
  %7 = load %struct._value_t** %2, align 8
  %8 = getelementptr inbounds %struct._value_t* %7, i32 0, i32 0
  %9 = load i8* %8, align 1
  %10 = zext i8 %9 to i32
  %11 = icmp eq i32 %6, %10
  br i1 %11, label %12, label %22

; <label>:12                                      ; preds = %0
  %13 = load %struct._value_t** %1, align 8
  %14 = getelementptr inbounds %struct._value_t* %13, i32 0, i32 1
  %15 = load i32* %14, align 4
  %16 = load %struct._value_t** %2, align 8
  %17 = getelementptr inbounds %struct._value_t* %16, i32 0, i32 1
  %18 = load i32* %17, align 4
  %19 = icmp eq i32 %15, %18
  %20 = zext i1 %19 to i32
  %21 = trunc i32 %20 to i8
  store i8 %21, i8* %equal, align 1
  br label %22

; <label>:22                                      ; preds = %12, %0
  %23 = load i8* %equal, align 1
  ret i8 %23
}

; Function Attrs: nounwind uwtable
define void @dump_value(%struct._value_t* %value) #0 {
  %1 = alloca %struct._value_t*, align 8
  store %struct._value_t* %value, %struct._value_t** %1, align 8
  %2 = load %struct._value_t** %1, align 8
  %3 = icmp ne %struct._value_t* %2, null
  br i1 %3, label %4, label %15

; <label>:4                                       ; preds = %0
  %5 = load %struct._IO_FILE** @stderr, align 8
  %6 = load %struct._value_t** %1, align 8
  %7 = load %struct._value_t** %1, align 8
  %8 = getelementptr inbounds %struct._value_t* %7, i32 0, i32 0
  %9 = load i8* %8, align 1
  %10 = zext i8 %9 to i32
  %11 = load %struct._value_t** %1, align 8
  %12 = getelementptr inbounds %struct._value_t* %11, i32 0, i32 1
  %13 = load i32* %12, align 4
  %14 = call i32 (%struct._IO_FILE*, i8*, ...)* @fprintf(%struct._IO_FILE* %5, i8* getelementptr inbounds ([15 x i8]* @.str, i32 0, i32 0), %struct._value_t* %6, i32 %10, i32 %13)
  br label %18

; <label>:15                                      ; preds = %0
  %16 = load %struct._IO_FILE** @stderr, align 8
  %17 = call i32 (%struct._IO_FILE*, i8*, ...)* @fprintf(%struct._IO_FILE* %16, i8* getelementptr inbounds ([9 x i8]* @.str1, i32 0, i32 0))
  br label %18

; <label>:18                                      ; preds = %15, %4
  ret void
}

declare i32 @fprintf(%struct._IO_FILE*, i8*, ...) #2

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4.1 (tags/RELEASE_34/dot1-final)"}
