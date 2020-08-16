(require 'demo-it)

(defun my-demo-step/show-code ()
    "Helper demo function that displays some source code and
advances the presentation at the same time."
    (demo-it-load-side-window "example/example.py")
    (demo-it-presentation-advance))

;; Order the functions and forms for this presentation:
(demo-it-create (demo-it-presentation "/home/rigidus/src/rigidus.ru/org/lrn/crypto/wp.org")
                ;; my-demo-step/show-code
                ;; demo-it-presentation-return ; close file and advance
                ;; (demo-it-run-in-eshell "python example/example.py")
                )

(demo-it-start)
