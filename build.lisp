(load "/tmp/quicklisp.lisp")

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (if (probe-file quicklisp-init)
    (load quicklisp-init)
    (quicklisp-quickstart:install)))

(ql:quickload "iterate")
(ql:quickload "metatilities")
(ql:quickload "cl-json")
(ql:quickload "drakma")

(push (merge-pathnames "buildapp-1.5.5/" (format nil "~a/" (sb-posix:getcwd))) asdf:*central-registry*)
(asdf:load-system "buildapp")
(buildapp::main `(""
                  "--load" ,(format nil "~a" (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))) 
                  "--asdf-path" ,(format nil "~a" (merge-pathnames "game/" (format nil "~a/" (sb-posix:getcwd)))) 
                  "--load-system" "hextris" 
                  "--entry" "hextris:main"
                  "--output" "play_icfp2015"))

(quit)

