(require 'time)
(require 'battery)
(require 's)
(require 'json)

;;* Custom
(defgroup exwm-blocks nil
  "Echo area status bar for exwm."
  :group 'exwm)

;; (defcustom exwm-blocks-propertize 'all
;;   "Which part of the block to propertize."
;;   :group 'exwm-blocks
;;   :type '(choice (const :tag "Whole block" all)
;;                  (const :tag "Only icon" icon)
;;                  (const :tag "Only text" text)))

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

;;* Block def
(defvar exwm-blocks--saved-cmds (make-hash-table))

(defvar exwm-blocks--values (make-hash-table))

(defvar exwm-blocks--timers (make-hash-table))

(cl-defmacro exwm-blocks-exec (&rest args &key block name &allow-other-keys)
  (declare (indent defun))
  (cl-remf args :block)
  (cl-remf args :name)
  (or (gethash (car args) exwm-blocks--saved-cmds)
      (if (not block)
          `(lambda ()
             (interactive)
             (start-process-shell-command
              ,(car args)
              nil
              ,(car args)))
        (let ((func
               `(lambda ()
                  (interactive)
                  (let ((proc
                         (start-process-shell-command
                          ,(or (symbol-name name) (car args))
                          nil
                          ,(car args))))
                    ,(cond ((cdr args)
                            `(set-process-filter proc ,(cadr args)))
                           (block
                               `(set-process-filter
                                 proc
                                 (lambda (_ out)
                                   (puthash ',block
                                            (string-trim out)
                                            exwm-blocks--values)
                                   (exwm-blocks-update)))))))))
          (when name
            (puthash name func exwm-blocks--saved-cmds))
          func))))

(cl-defmacro exwm-blocks-set (&rest blocks)
  `(setq exwm-blocks-format
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
             exwm-blocks-separator)))))

(defun exwm-blocks-create-map (bindings)
  (let ((map (make-sparse-keymap)))
    (cl-macrolet ((blocks-exec
                   (lst)
                   `(exwm-blocks-exec ,@list)))
      (cl-loop
       for (key func) on bindings by #'cddr
       do  (let* ((key (if (stringp key) (kbd key) key))
                  (func (if (and (listp func) (eq (car func) 'exec))
                            (blocks-exec (cdr func))
                          func))
                  (func (cond ((symbolp func)
                               (symbol-value func))
                              ((functionp func)
                               func)
                              ((and (consp func) (or (eq (car func) 'function)
                                                     (eq (car func) 'quote)))
                               (cadr func))
                              (t
                               `(lambda ()
                                  (interactive)
                                  ,func)))))
             (define-key map key func))))
    map))

(defun exwm-block-value (value)
  (gethash value exwm-blocks--values))

(defun exwm-blocks--handle-shell-process (name script &optional filter)
  (let ((timer (gethash name exwm-blocks--timers)))
    (when timer
      (cancel-timer timer)))
  (let* ((filter (or filter `(lambda (_ out)
                               (puthash ',name
                                        (string-trim out)
                                        exwm-blocks--values)
                               (exwm-blocks-update))))
         (update-fn `(lambda ()
                       (let ((proc
                              (start-process-shell-command
                               ,block-name-str
                               nil
                               ,script)))
                         (set-process-filter
                          proc
                          ,filter)))))
    (puthash name
             (run-at-time 0 interval update-fn)
             exwm-blocks--timers)))

(cl-defun exwm-blocks-define-block (name
                                    &key
                                    icon
                                    script
                                    interval
                                    fmt
                                    face
                                    foreground
                                    color
                                    background
                                    bindings
                                    elisp)
  (let* ((foreground (or foreground color))
         (face (or face
                   (cond ((and foreground background)
                          (list :foreground foreground
                                :background background))
                         (foreground
                          (list :foreground foreground))
                         (background
                          (list :background background)))))
         (map (exwm-blocks-create-map bindings))
         (block-name-str (concat "exwm-blocks-" (symbol-name name))))
    `(propertize
      (format
       "%s%s%s"
       ,(or icon "")
       ,(if icon " " "")
       ,(cond
         (script
          (exwm-blocks--handle-shell-process name script)
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
