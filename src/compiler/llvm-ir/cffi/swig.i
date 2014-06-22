%module llvmcffi

%insert(lisphead) %{
(in-package :owlisp/llvm-ir)
%}

%feature( "export" );

%{
%}

%include <llvm-c/Core.h>

