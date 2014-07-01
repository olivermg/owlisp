; ModuleID = 'frame.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i8*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type { %struct._IO_marker*, %struct._IO_FILE*, i32 }
%struct._frame_t = type { %struct._frame_t*, [16 x %struct._value_t*] }
%struct._value_t = type { i8, i32 }

@.str = private unnamed_addr constant [27 x i8] c"binding not found in frame\00", align 1
@stderr = external global %struct._IO_FILE*
@.str1 = private unnamed_addr constant [11 x i8] c"FRAME: %p\0A\00", align 1
@global_frame = internal global %struct._frame_t* null, align 8
@.str2 = private unnamed_addr constant [16 x i8] c"frame not found\00", align 1

; Function Attrs: nounwind uwtable
define %struct._frame_t* @new_frame(%struct._frame_t* %parent) #0 {
  %1 = alloca %struct._frame_t*, align 8
  %newframe = alloca %struct._frame_t*, align 8
  store %struct._frame_t* %parent, %struct._frame_t** %1, align 8
  %2 = call noalias i8* @calloc(i64 1, i64 136) #3
  %3 = bitcast i8* %2 to %struct._frame_t*
  store %struct._frame_t* %3, %struct._frame_t** %newframe, align 8
  %4 = load %struct._frame_t** %1, align 8
  %5 = load %struct._frame_t** %newframe, align 8
  %6 = getelementptr inbounds %struct._frame_t* %5, i32 0, i32 0
  store %struct._frame_t* %4, %struct._frame_t** %6, align 8
  %7 = load %struct._frame_t** %newframe, align 8
  ret %struct._frame_t* %7
}

; Function Attrs: nounwind
declare noalias i8* @calloc(i64, i64) #1

; Function Attrs: nounwind uwtable
define %struct._frame_t* @free_frame(%struct._frame_t* %f) #0 {
  %1 = alloca %struct._frame_t*, align 8
  %parent = alloca %struct._frame_t*, align 8
  store %struct._frame_t* %f, %struct._frame_t** %1, align 8
  store %struct._frame_t* null, %struct._frame_t** %parent, align 8
  %2 = load %struct._frame_t** %1, align 8
  %3 = icmp ne %struct._frame_t* %2, null
  br i1 %3, label %4, label %10

; <label>:4                                       ; preds = %0
  %5 = load %struct._frame_t** %1, align 8
  %6 = getelementptr inbounds %struct._frame_t* %5, i32 0, i32 0
  %7 = load %struct._frame_t** %6, align 8
  store %struct._frame_t* %7, %struct._frame_t** %parent, align 8
  %8 = load %struct._frame_t** %1, align 8
  %9 = bitcast %struct._frame_t* %8 to i8*
  call void @free(i8* %9) #3
  br label %10

; <label>:10                                      ; preds = %4, %0
  %11 = load %struct._frame_t** %parent, align 8
  ret %struct._frame_t* %11
}

; Function Attrs: nounwind
declare void @free(i8*) #1

; Function Attrs: nounwind uwtable
define void @set_binding(%struct._frame_t* %f, i32 %frameindex, i32 %varindex, %struct._value_t* %value) #0 {
  %1 = alloca %struct._frame_t*, align 8
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca %struct._value_t*, align 8
  %correctframe = alloca %struct._frame_t*, align 8
  store %struct._frame_t* %f, %struct._frame_t** %1, align 8
  store i32 %frameindex, i32* %2, align 4
  store i32 %varindex, i32* %3, align 4
  store %struct._value_t* %value, %struct._value_t** %4, align 8
  %5 = load %struct._frame_t** %1, align 8
  %6 = load i32* %2, align 4
  %7 = call %struct._frame_t* @find_frame(%struct._frame_t* %5, i32 %6)
  store %struct._frame_t* %7, %struct._frame_t** %correctframe, align 8
  %8 = load i32* %3, align 4
  %9 = icmp slt i32 %8, 0
  br i1 %9, label %10, label %15

; <label>:10                                      ; preds = %0
  %11 = load i32* %3, align 4
  %12 = sext i32 %11 to i64
  %13 = icmp uge i64 %12, 128
  br i1 %13, label %14, label %15

; <label>:14                                      ; preds = %10
  call void @fatal_error(i8* getelementptr inbounds ([27 x i8]* @.str, i32 0, i32 0))
  br label %15

; <label>:15                                      ; preds = %14, %10, %0
  %16 = load %struct._value_t** %4, align 8
  %17 = load i32* %3, align 4
  %18 = sext i32 %17 to i64
  %19 = load %struct._frame_t** %correctframe, align 8
  %20 = getelementptr inbounds %struct._frame_t* %19, i32 0, i32 1
  %21 = getelementptr inbounds [16 x %struct._value_t*]* %20, i32 0, i64 %18
  store %struct._value_t* %16, %struct._value_t** %21, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define internal %struct._frame_t* @find_frame(%struct._frame_t* %f, i32 %frameindex) #0 {
  %1 = alloca %struct._frame_t*, align 8
  %2 = alloca i32, align 4
  %found = alloca %struct._frame_t*, align 8
  %i = alloca i32, align 4
  store %struct._frame_t* %f, %struct._frame_t** %1, align 8
  store i32 %frameindex, i32* %2, align 4
  %3 = load %struct._frame_t** %1, align 8
  store %struct._frame_t* %3, %struct._frame_t** %found, align 8
  store i32 0, i32* %i, align 4
  br label %4

; <label>:4                                       ; preds = %16, %0
  %5 = load i32* %i, align 4
  %6 = load i32* %2, align 4
  %7 = icmp slt i32 %5, %6
  br i1 %7, label %8, label %19

; <label>:8                                       ; preds = %4
  %9 = load %struct._frame_t** %found, align 8
  %10 = getelementptr inbounds %struct._frame_t* %9, i32 0, i32 0
  %11 = load %struct._frame_t** %10, align 8
  store %struct._frame_t* %11, %struct._frame_t** %found, align 8
  %12 = load %struct._frame_t** %found, align 8
  %13 = icmp eq %struct._frame_t* null, %12
  br i1 %13, label %14, label %15

; <label>:14                                      ; preds = %8
  call void @fatal_error(i8* getelementptr inbounds ([16 x i8]* @.str2, i32 0, i32 0))
  br label %15

; <label>:15                                      ; preds = %14, %8
  br label %16

; <label>:16                                      ; preds = %15
  %17 = load i32* %i, align 4
  %18 = add nsw i32 %17, 1
  store i32 %18, i32* %i, align 4
  br label %4

; <label>:19                                      ; preds = %4
  %20 = load %struct._frame_t** %found, align 8
  ret %struct._frame_t* %20
}

declare void @fatal_error(i8*) #2

; Function Attrs: nounwind uwtable
define %struct._value_t* @get_binding(%struct._frame_t* %f, i32 %frameindex, i32 %varindex) #0 {
  %1 = alloca %struct._frame_t*, align 8
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %correctframe = alloca %struct._frame_t*, align 8
  store %struct._frame_t* %f, %struct._frame_t** %1, align 8
  store i32 %frameindex, i32* %2, align 4
  store i32 %varindex, i32* %3, align 4
  %4 = load %struct._frame_t** %1, align 8
  %5 = load i32* %2, align 4
  %6 = call %struct._frame_t* @find_frame(%struct._frame_t* %4, i32 %5)
  store %struct._frame_t* %6, %struct._frame_t** %correctframe, align 8
  %7 = load i32* %3, align 4
  %8 = icmp slt i32 %7, 0
  br i1 %8, label %9, label %14

; <label>:9                                       ; preds = %0
  %10 = load i32* %3, align 4
  %11 = sext i32 %10 to i64
  %12 = icmp uge i64 %11, 128
  br i1 %12, label %13, label %14

; <label>:13                                      ; preds = %9
  call void @fatal_error(i8* getelementptr inbounds ([27 x i8]* @.str, i32 0, i32 0))
  br label %14

; <label>:14                                      ; preds = %13, %9, %0
  %15 = load i32* %3, align 4
  %16 = sext i32 %15 to i64
  %17 = load %struct._frame_t** %correctframe, align 8
  %18 = getelementptr inbounds %struct._frame_t* %17, i32 0, i32 1
  %19 = getelementptr inbounds [16 x %struct._value_t*]* %18, i32 0, i64 %16
  %20 = load %struct._value_t** %19, align 8
  ret %struct._value_t* %20
}

; Function Attrs: nounwind uwtable
define void @dump_frame(%struct._frame_t* %f) #0 {
  %1 = alloca %struct._frame_t*, align 8
  %i = alloca i32, align 4
  store %struct._frame_t* %f, %struct._frame_t** %1, align 8
  %2 = load %struct._frame_t** %1, align 8
  %3 = icmp ne %struct._frame_t* %2, null
  br i1 %3, label %4, label %25

; <label>:4                                       ; preds = %0
  %5 = load %struct._IO_FILE** @stderr, align 8
  %6 = load %struct._frame_t** %1, align 8
  %7 = call i32 (%struct._IO_FILE*, i8*, ...)* @fprintf(%struct._IO_FILE* %5, i8* getelementptr inbounds ([11 x i8]* @.str1, i32 0, i32 0), %struct._frame_t* %6)
  store i32 0, i32* %i, align 4
  br label %8

; <label>:8                                       ; preds = %18, %4
  %9 = load i32* %i, align 4
  %10 = icmp slt i32 %9, 16
  br i1 %10, label %11, label %21

; <label>:11                                      ; preds = %8
  %12 = load i32* %i, align 4
  %13 = sext i32 %12 to i64
  %14 = load %struct._frame_t** %1, align 8
  %15 = getelementptr inbounds %struct._frame_t* %14, i32 0, i32 1
  %16 = getelementptr inbounds [16 x %struct._value_t*]* %15, i32 0, i64 %13
  %17 = load %struct._value_t** %16, align 8
  call void @dump_value(%struct._value_t* %17)
  br label %18

; <label>:18                                      ; preds = %11
  %19 = load i32* %i, align 4
  %20 = add nsw i32 %19, 1
  store i32 %20, i32* %i, align 4
  br label %8

; <label>:21                                      ; preds = %8
  %22 = load %struct._frame_t** %1, align 8
  %23 = getelementptr inbounds %struct._frame_t* %22, i32 0, i32 0
  %24 = load %struct._frame_t** %23, align 8
  call void @dump_frame(%struct._frame_t* %24)
  br label %25

; <label>:25                                      ; preds = %21, %0
  ret void
}

declare i32 @fprintf(%struct._IO_FILE*, i8*, ...) #2

declare void @dump_value(%struct._value_t*) #2

; Function Attrs: nounwind uwtable
define void @init_global_frame() #0 {
  %1 = call %struct._frame_t* @new_frame(%struct._frame_t* null)
  store %struct._frame_t* %1, %struct._frame_t** @global_frame, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define %struct._frame_t* @get_global_frame() #0 {
  %1 = load %struct._frame_t** @global_frame, align 8
  ret %struct._frame_t* %1
}

; Function Attrs: nounwind uwtable
define %struct._frame_t* @extend_global_frame() #0 {
  %1 = load %struct._frame_t** @global_frame, align 8
  %2 = call %struct._frame_t* @new_frame(%struct._frame_t* %1)
  store %struct._frame_t* %2, %struct._frame_t** @global_frame, align 8
  %3 = load %struct._frame_t** @global_frame, align 8
  ret %struct._frame_t* %3
}

; Function Attrs: nounwind uwtable
define %struct._frame_t* @shrink_global_frame() #0 {
  %old_frame = alloca %struct._frame_t*, align 8
  %1 = load %struct._frame_t** @global_frame, align 8
  store %struct._frame_t* %1, %struct._frame_t** %old_frame, align 8
  %2 = load %struct._frame_t** @global_frame, align 8
  %3 = getelementptr inbounds %struct._frame_t* %2, i32 0, i32 0
  %4 = load %struct._frame_t** %3, align 8
  store %struct._frame_t* %4, %struct._frame_t** @global_frame, align 8
  %5 = load %struct._frame_t** %old_frame, align 8
  %6 = call %struct._frame_t* @free_frame(%struct._frame_t* %5)
  %7 = load %struct._frame_t** @global_frame, align 8
  ret %struct._frame_t* %7
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4.1 (tags/RELEASE_34/dot1-final)"}
