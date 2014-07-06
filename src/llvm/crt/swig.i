%module runtime_api_low

%insert(lisphead) %{
(in-package :owlisp/llvm)
%}

%{
%}

%include "error.h";
%include "types.h";
%include "frame.h";

