# CL-MUSH

* Common Lisp MUSH Environment Engine
* Multi-User TCP Server

# Install
* install quicklisp
* (ql:quickload '(:usocket :cl-ppcre :bordeaux-threads :local-time :babel))
* (save-lisp-and-die "cl-mush.img")
* sbcl --core cl-mush.img --load "src/server.lisp"


# To Do
* fix pmake pools.sexp write dir problem

* dynamic reloading from T60, editing from bronx

* fix special character entry error


* easy edit/dynamic loading of definitions w/o runtimes
* shell/remote git ease
* unique user identification generate name/desc from ip/place+count other