;;; foo.el --- Foo  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Stolen from `helm'

;;; Code:


;;; Multi keys
;;
;;
;;;###autoload
(defun foo-define-multi-key (keymap key functions &optional delay)
  (define-key keymap key (foo-make-multi-command functions delay)))

;;;###autoload
(defmacro foo-multi-key-defun (name docstring funs &optional delay)
  "Define NAME as a multi-key command running FUNS.
After DELAY seconds the FUNS list is reinitialised.
See `foo-define-multi-key'."
  (declare (indent 2))
  (setq docstring (if docstring (concat docstring "\n\n")
                    "This is a fooish multi-key command."))
  `(defalias (quote ,name) (foo-make-multi-command ,funs ,delay) ,docstring))

(defun foo-make-multi-command (functions &optional delay)
  "Return an anonymous multi-key command running FUNCTIONS.
Run each function of FUNCTIONS list in turn when called within DELAY seconds."
  (declare (indent 1))
  (let ((funs functions)
        (iter (cl-gensym "foo-iter-key"))
        (timeout delay))
    (eval (list 'defvar iter nil))
    (lambda () (interactive) (foo-run-multi-key-command funs iter timeout))))

(defun foo-run-multi-key-command (functions iterator delay)
  (let ((fn (lambda ()
              (cl-loop for count from 1 to (length functions)
                       collect count)))
        next)
    (unless (and (symbol-value iterator)
                 ;; Reset iterator when another key is pressed.
                 (eq this-command real-last-command))
      (set iterator (foo-iter-list (funcall fn))))
    (setq next (foo-iter-next (symbol-value iterator)))
    (unless next
      (set iterator (foo-iter-list (funcall fn)))
      (setq next (foo-iter-next (symbol-value iterator))))
    (and next (symbol-value iterator) (call-interactively (nth (1- next) functions)))
    (when delay (run-with-idle-timer delay nil `(lambda ()
                                                  (setq ,iterator nil))))))

(defun foo-iter-list (seq)
  "Return an iterator object from SEQ."
  (let ((lis seq))
    (lambda ()
      (let ((elm (car lis)))
        (setq lis (cdr lis))
        elm))))

(defun foo-iter-next (iterator)
  "Return next elm of ITERATOR."
  (funcall iterator))

(foo-multi-key-defun foo-toggle-resplit-and-swap-windows
    "Multi key command to resplit and swap foo window.
First call run `foo-toggle-resplit-window',
second call within 0.5s run `foo-swap-windows'."
  '(foo-toggle-resplit-window foo-swap-windows) 1)

;;;###autoload
(defun foo-define-key-with-subkeys (map key subkey command
                                        &optional other-subkeys menu exit-fn)
  (declare (indent 1))
  (define-key map key
    (lambda ()
      (interactive)
      (unwind-protect
          (progn
            (call-interactively command)
            (while (let ((input (read-key menu))
                         other
                         kb
                         com)
                     (setq last-command-event input)
                     (cond
                      ((eq input subkey)
                       (call-interactively command)
                       t)
                      ((setq other (assoc input other-subkeys))
                       (call-interactively (cdr other))
                       t)
                      (t
                       (setq kb (vector last-command-event))
                       (setq com (lookup-key map kb))
                       (if (commandp com)
                           (call-interactively com)
                         (setq unread-command-events
                               (nconc (mapcar 'identity kb)
                                      unread-command-events)))
                       nil)))))
        (and exit-fn (funcall exit-fn))))))



;;; URL: https://gist.github.com/jordonbiondo/4918ac0043af4b3889fd
;;; Related Emacs SE: http://emacs.stackexchange.com/questions/10993/how-to-write-a-function-which-toggles-between-the-executions-of-two-other-functi

;;; Ways to do this kind stuff
;;
;; * separate variable
;; * lexical closure
;; * function symbol
;; * `set-transient-map', temporary (for key sequence only, see C-x C-+)
;; * `repeat'(for repeat command only, see its implementation)
;; * `loop' (for sub menu keys, see `helm-define-key-with-subkeys')

;;; lexical closure (simple)
(let (toggle)
  (setf fn4 (lambda ()
              (setf toggle (not toggle))
              (funcall (if toggle #'fn1 #'fn2)))))
(funcall fn4)                           ; => "Function 1"
(funcall fn4)                           ; => "Function 2"
(funcall fn4)                           ; => "Function 1"

;;; lexical closure
(defmacro cycle-forms (&rest forms)
  (let ((sym (make-symbol "sym"))
        (n 0)
        (l (length forms)))
    (set sym 0)
    `(case (prog1 ,sym
             (incf ,sym)
             (when (= ,sym ,l) (setq ,sym 0)))
       ,@(mapcar (lambda (form) (list (1- (incf n)) form)) forms))))

(defun my-func1 ()
  (message "in my func 1"))

(defun my-func2 ()
  (message "in my func 2"))

(defun my-func3 ()
  (message "in my func 3"))

(defun my-toggling-function ()
  (cycle-forms
   (my-func1)
   (my-func2)
   (my-func3)))

(my-toggling-function) ;; => "in my func 1"
(my-toggling-function) ;; => "in my func 2"
(my-toggling-function) ;; => "in my func 3"


;;; function symbol
(defun fn1 ()
  (message "Function 1"))

(defun fn2 ()
  (message "Function 2"))

(defun fn3 ()
  (put 'fn3 'fn1-p (not (get 'fn3 'fn1-p)))
  (funcall (if (get 'fn3 'fn1-p) #'fn1 #'fn2)))

(provide 'foo)

;;; foo.el ends here
