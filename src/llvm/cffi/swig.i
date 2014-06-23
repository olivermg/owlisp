%module llvm_api

%insert(lisphead) %{
(in-package :owlisp/llvm)
%}

%{
%}

%include <llvm-c/Core.h>

