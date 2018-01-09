(require 'cl-lib)
(require 'time)
(require 'battery)
(require 's)
(require 'json)

;;* Custom
(defgroup exwm-blocks nil
  "Echo area status bar for exwm."
  :group 'exwm)

(defcustom exwm-blocks-adjust -2
  "Amount to adjust exwm-blocks status by."
  :group 'exwm-blocks
  :type 'integer)

(defcustom exwm-blocks-separator " "
  "String to separate blocks with."
  :group 'exwm-blocks
  :type 'string)

(defcustom exwm-blocks-format
  '(:eval
    (s-pad-left
     (+ (frame-width) exwm-blocks-adjust)
     " "
     (concat
      (format " %s" battery-mode-line-string)
      "  "
      (format " %s"
              display-time-string))))
  "Specification of the contens of exwm-blocks.
Uses the same format as `mode-line-format'"
  :group 'exwm-blocks
  :type 'sexp)

(defcustom exwm-blocks-script-format "%s"
  "A format string or function applied to the block name to
determine the script to call if :script and :elisp are omitted."
  :group 'exwm-blocks
  :type '(choice function string))

;;* Block def
(defvar exwm-blocks--saved-cmds (make-hash-table))

(defvar exwm-blocks--values (make-hash-table))

(defvar exwm-blocks--timers (make-hash-table))

(cl-defmacro exwm-blocks--make-filter (proc filter &optional block)
  (when (or filter block)
    `(set-process-filter
      ,proc
      ,(cond ((and filter (symbolp filter))
              (symbol-value filter))
             ((and (consp filter) (or (eq (car filter) 'function)
                                      (eq (car filter) 'quote)))
              (cadr filter))
             ((functionp filter)
              filter)
             (t
              `(lambda (<proc> <out>)
                 ,(if block
                      `(progn
                         (puthash ,block ,(or filter '(string-trim <out>)) exwm-blocks--values)
                         (exwm-blocks-update))
                    ,filter)))))))

(cl-defun exwm-blocks-exec (&key block name script filter)
  (declare (indent defun))
  (or (gethash name exwm-blocks--saved-cmds)
      (if (not block)
          `(lambda ()
             (interactive)
             (start-process-shell-command
              ,script
              nil
              ,script)))
      (let ((func
             `(lambda ()
                (interactive)
                (let ((proc
                       (start-process-shell-command
                        ,(if name
                             (concat "exwm-blocks: " (symbol-name name))
                           script)
                        nil
                        ,script)))
                  ,(if block
                       `(exwm-blocks--make-filter proc ,filter ',block)
                     `(exwm-blocks--make-filter proc ,filter))))))
        (when name
          (puthash name func exwm-blocks--saved-cmds))
        func)))

(cl-defmacro exwm-blocks-set (&rest blocks)
  `(progn
     (setq exwm-blocks-format
           '(:eval
             (s-pad-left
              (+ (frame-width) exwm-blocks-adjust)
              " "
              (string-join
               (list
                ,@(mapcar
                   (lambda (block)
                     (apply #'exwm-blocks-define-block block))
                   blocks))
               exwm-blocks-separator))))
     (when exwm-blocks-mode
       (exwm-blocks-update))))

(defun exwm-blocks--make-callable (x &optional block)
  (cond ((and (listp x) (eq (car x) 'exec))
         (let* ((args (cdr x))
                (script-pos (cl-position-if #'stringp args)))
           (when script-pos
             (setf (nthcdr script-pos args) (cons :script (nthcdr script-pos args))))
           (apply #'exwm-blocks-exec args)))
        ((and (listp x) (eq (car x) 'exec*))
         (let* ((args (cdr x))
                (script-pos (cl-position-if #'stringp args)))
           (when script-pos
             (setf (nthcdr script-pos args) (cons :script (nthcdr script-pos args))))
           (apply #'exwm-blocks-exec (append args `(:block ,block)))))
        ((symbolp x)
         (symbol-value x))
        ((functionp x)
         x)
        ((and (consp x) (or (eq (car x) 'function)
                            (eq (car x) 'quote)))
         (cadr x))
        (t
         `(lambda ()
            (interactive)
            ,x))))

(defun exwm-blocks--create-map (bindings block)
  (let ((map (make-sparse-keymap)))
    (cl-loop
     for (key func) on bindings by #'cddr
     do  (let* ((key (if (stringp key) (kbd key) key))
                (func (exwm-blocks--make-callable func block)))
           (define-key map key func)))
    map))

(defmacro exwm-blocks-define-keys (&rest bindings)
  "Define exwm keys using exwm-input-set-key.

FUNC can be:

- var, quoted symbol, or sharp quoted symbol
- lambda
- arbitrary sexp, which is wrapped in an interactive lambda
- Sexp of form (exec CMD &optional ASYNC), where CMD is a shell
  command. This gets converted into an interactive lambda wrapper
  around `start-process-shell-command' if ASYNC is nil or
  omitted, or `call-process-shell-command' if ASYNC is non-nil.

\(fn [KEY FUNC]...)"
  (cl-loop for (key func) on bindings
           by #'cddr
           do (let* ((key (if (stringp key) (kbd key) key))
                     (func (exwm-blocks--make-callable func)))
                (exwm-input-set-key key func))))

(defun exwm-block-value (value)
  (gethash value exwm-blocks--values))

(defun exwm-blocks--handle-shell-process (name script &optional filter)
  (let ((timer (gethash name exwm-blocks--timers)))
    (when timer
      (cancel-timer timer)))
  (let* ((update-fn `(lambda ()
                       (let ((proc
                              (start-process-shell-command
                               ,(concat "exwm-blocks: " (symbol-name name))
                               nil
                               ,script)))
                         (exwm-blocks--make-filter proc ,filter ',name)))))
    (puthash name
             (run-at-time 0 interval update-fn)
             exwm-blocks--timers)))

(defun exwm-blocks--determine-script (name script args elisp)
  (unless (member name '(battery-emacs time-emacs))
    (let ((script-default
           (if (functionp exwm-blocks-script-format)
               (funcall exwm-blocks-script-format (symbol-name name))
             (format exwm-blocks-script-format (symbol-name name)))))
      (cond (elisp
             nil)
            ((stringp script)
             script)
            (args
             (concat script-default " " args))
            (t script-default)))))

(cl-defun exwm-blocks-define-block (name
                                    &key
                                    icon
                                    script
                                    args
                                    filter
                                    interval
                                    fmt
                                    face
                                    foreground
                                    color
                                    background
                                    bindings
                                    elisp)
  (let* ((script (exwm-blocks--determine-script name script args elisp))
         (foreground (or foreground color))
         (face (or face
                   (cond ((and foreground background)
                          (list :foreground foreground
                                :background background))
                         (foreground
                          (list :foreground foreground))
                         (background
                          (list :background background)))))
         (map (exwm-blocks--create-map bindings name)))
    `(propertize
      (format
       "%s%s%s"
       ,(or icon "")
       ,(if icon " " "")
       ,(cond
         (script
          (exwm-blocks--handle-shell-process name script filter)
          `(gethash ',name exwm-blocks--values))
         ((eq name 'battery-emacs)
          (display-battery-mode)
          (setq battery-mode-line-format (or fmt battery-mode-line-format))
          'battery-mode-line-string)
         ((eq name 'time-emacs)
          (display-time-mode)
          (setq display-time-format (or fmt display-time-format))
          'display-time-string)))
      'face
      ',face
      'local-map
      ',map)))


;;* Mode
(defconst exwm-blocks--buffer " *Minibuf-0*")

(defun exwm-blocks--message (fn fmt-string &rest args)
  (apply fn fmt-string args)
  (let (message-log-max)
    (apply fn
           (cons
            (concat fmt-string
                    (s-pad-left
                     (+ (frame-width) exwm-blocks-adjust
                        (-
                         (if fmt-string
                             (length (apply #'format (cons fmt-string args)))
                           0)))
                     " "
                     (s-trim-left
                      (format-mode-line exwm-blocks-format))))
            args))))

(defun exwm-blocks-update ()
  (interactive)
  (with-current-buffer exwm-blocks--buffer
    (erase-buffer)
    (insert (format-mode-line exwm-blocks-format))))

(defun exwm-blocks--add-advices ()
  (advice-add 'display-time-update :after #'exwm-blocks-update)
  (advice-add 'battery-update :after #'exwm-blocks-update)
  (advice-add 'user-error :around #'exwm-blocks--message)
  (advice-add 'message :around #'exwm-blocks--message))

(defun exwm-blocks--remove-advices ()
  (advice-remove 'display-time-update #'exwm-blocks-update)
  (advice-remove 'battery-update #'exwm-blocks-update)
  (advice-remove 'user-error #'exwm-blocks--message)
  (advice-remove 'message #'exwm-blocks--message))

;;;###autoload
(define-minor-mode exwm-blocks-mode
  "Display status info in the minibuffer window."
  :global t
  :require 'exwm-blocks
  (if exwm-blocks-mode
      (progn
        (exwm-blocks-i3blocks-mode -1)
        (exwm-blocks--add-advices)
        (exwm-blocks-update))
    (with-current-buffer exwm-blocks--buffer
      (erase-buffer))
    (exwm-blocks--remove-advices)))

;;* i3blocks
(defvar exwm-blocks--i3blocks-process nil)

(defun exwm-blocks--handle-i3blocks (proc out)
  (condition-case error
      (progn
        (let ((response (cl-loop
                         for thing across
                         (json-read-from-string (substring out 1))
                         concat (alist-get 'full_text thing))))
          (unless (s-blank-str? response)
            (setq exwm-blocks-format
                  (s-pad-left
                   (- (frame-width) 2)
                   " "
                   response))
            (exwm-blocks-update))))
    (error nil)))

;;;###autoload
(define-minor-mode exwm-blocks-i3blocks-mode
  "Parse i3blocks into "
  :global t
  :require 'exwm-blocks
  (if exwm-blocks-i3blocks-mode
      (progn
        (exwm-blocks-mode -1)
        (exwm-blocks--add-advices)
        (setq exwm-blocks--i3blocks-process (start-process-shell-command "exwm-blocks-i3blocks" nil "i3blocks"))
        (set-process-filter exwm-blocks--i3blocks-process
                            #'exwm-blocks--handle-i3blocks))
    (when (process-live-p exwm-blocks--i3blocks-process)
      (kill-process exwm-blocks--i3blocks-process))
    (exwm-blocks--remove-advices)))

(provide 'exwm-blocks)
