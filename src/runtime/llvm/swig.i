%module runtime

%insert(lisphead) %{
(in-package :owlisp/runtime)
%}

%{
%}

%rename("RT_%(upper)s") "";
%include "runtime.h";

