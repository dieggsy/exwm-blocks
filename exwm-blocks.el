(require 'time)
(require 'battery)
(require 's)
(require 'json)

(defconst exwm-blocks--buffer " *Minibuf-0*")

(defgroup exwm-blocks nil
  "Echo area status bar for exwm."
  :group 'exwm)

(defcustom exwm-blocks-propertize 'all
  "Which part of the block to propertize."
  :group 'exwm-blocks
  :type '(choice (const :tag "Whole block" all)
                 (const :tag "Only icon" icon)
                 (const :tag "Only text" text)))

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

(defun exwm-blocks-message-preserve-blocks (fn fmt-string &rest args)
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
          args)))

(defun exwm-blocks-update ()
  (with-current-buffer exwm-blocks--buffer
    (erase-buffer)
    (insert (format-mode-line exwm-blocks-format))))

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

(defmacro exwm-blocks--defvar (name)
  `(defvar ,name nil))

(defun exwm-blocks-format-with-face (txt &optional icn face)
  (let* ((txt (if (and face (eq exwm-blocks-propertize 'text))
                  `(propertize ,txt 'face ',face)
                txt))
         (icn (if (and face (eq exwm-blocks-propertize 'icon))
                  `(propertize ,icn 'face ',face)
                icn))
         (fmt (if icn
                  `(format "%s %s" ,icn ,txt)
                `(format "%s" ,txt))))
    (if (and face (eq exwm-blocks-propertize 'all))
        `(propertize ,fmt 'face ',face)
      fmt)))

(defvar exwm-blocks--values (make-hash-table))

(defvar exwm-blocks--timers (make-hash-table))

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
                                    body)
  (let* ((face (or face
                   `(:foreground
                     ,(or foreground
                          color)
                     :background
                     ,background)))
         (block-name-str (concat "exwm-blocks-block-" (symbol-name name)))
         (block-name (intern block-name-str)))
    (cond
     (script
      (let ((timer (gethash block-name exwm-blocks--timers)))
        (when timer
          (cancel-timer timer)))
      (puthash block-name
               (run-at-time 0
                            interval
                            `(lambda ()
                               (let ((proc
                                      (start-process-shell-command
                                       ,block-name-str
                                       nil
                                       ,script)))
                                 (set-process-filter
                                  proc
                                  (lambda (_ out)
                                    (puthash ',block-name
                                             (string-trim out)
                                             exwm-blocks--values)
                                    (exwm-blocks-update))))))
               exwm-blocks--timers)
      (exwm-blocks-format-with-face `(gethash ',block-name exwm-blocks--values)
                                    icon
                                    face))
     ((eq name 'battery-emacs)
      (display-battery-mode)
      (setq battery-mode-line-format (or fmt battery-mode-line-format))
      (exwm-blocks-format-with-face 'battery-mode-line-string icon face))
     ((eq name 'time-emacs)
      (display-time-mode)
      (setq display-time-format (or fmt display-time-format))
      (exwm-blocks-format-with-face 'display-time-string icon face)))))

(defun exwm-blocks--add-advices ()
  (advice-add 'display-time-update :after #'exwm-blocks-update)
  (advice-add 'battery-update :after #'exwm-blocks-update)
  (advice-add 'user-error :around #'exwm-blocks-message-preserve-blocks)
  (advice-add 'message :around #'exwm-blocks-message-preserve-blocks))

(defun exwm-blocks--remove-advices ()
  (advice-remove 'display-time-update #'exwm-blocks-update)
  (advice-remove 'battery-update #'exwm-blocks-update)
  (advice-remove 'user-error #'exwm-blocks-message-preserve-blocks)
  (advice-remove 'message #'exwm-blocks-message-preserve-blocks))

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
