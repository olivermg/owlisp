%module llvm_api_low

%insert(lisphead) %{
(in-package :owlisp/llvm)
%}

%{
%}

%include <llvm-c/Core.h>

