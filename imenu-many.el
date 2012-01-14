;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defvar imenu--many-index-cache (make-hash-table))

(defun buffer-list-by-mode (&optional mode)
  (interactive)
  (unless mode
    (setq mode major-mode))
  (loop for buf in (buffer-list)
        if (eq (with-current-buffer buf major-mode) mode)
        append (list buf)))

;;(buffer-list-by-mode)

(defun imenu--make-many-index-alist (&optional buffers)
  (let ((current-mode major-mode))
    (append (list '("*Rescan*" . -99))
            (remove-if (lambda (item) (or (eq item nil)
                                          (string-equal (car item) "*Rescan*")))
                       (loop for buf in (or buffers
                                            (remove-if-not 'identity
                                                           (mapcar (lambda (buf)
                                                                     (when (eq (with-current-buffer buf major-mode)
                                                                               current-mode)
                                                                       buf)) (buffer-list))))
                             append (with-current-buffer buf
                                      (let* ((cache (gethash buf imenu--many-index-cache nil))
                                             (tick (buffer-modified-tick)))
                                        (cond ((or (not cache)
                                                   (< (car cache) tick))
                                               (let ((newcache (imenu--make-index-alist t)))
                                                 (puthash buf (list tick newcache) imenu--many-index-cache)
                                                 newcache))
                                              (t (cadr cache))))))))))

;;(imenu--make-many-index-alist (list (current-buffer)))

(defun imenu-choose-many-buffer-index (&optional prompt alist)
  (let (index-alist
        (mouse-triggered (listp last-nonmenu-event))
        (result t))
    ;; If selected by mouse, see to that the window where the mouse is
    ;; really is selected.
    (and mouse-triggered
         (not (equal last-nonmenu-event '(menu-bar)))
         (let ((window (posn-window (event-start last-nonmenu-event))))
           (or (framep window) (null window) (select-window window))))
    ;; Create a list for this buffer only when needed.
    (while (eq result t)
      (setq index-alist (if alist alist (imenu--make-many-index-alist)))
      (setq result
            (if (and imenu-use-popup-menu
                     (or (eq imenu-use-popup-menu t) mouse-triggered))
                (imenu--mouse-menu index-alist last-nonmenu-event)
              (imenu--completion-buffer index-alist prompt)))
      ;; (and (equal result imenu--rescan-item)
      ;;      (imenu--cleanup)
      ;;      (setq result t imenu--index-alist nil))
      )
    result))

(defun imenu-many (index-item)
  (interactive (list (imenu-choose-many-buffer-index)))
  (if (stringp index-item)
      (setq index-item (assoc index-item (imenu--make-many-index-alist))))
  (when index-item
    (push-mark)
    (print index-item)
    (let* ((is-special-item (listp (cdr index-item)))
           (function
            (if is-special-item
                (nth 2 index-item) imenu-default-goto-function))
           (position (if is-special-item
                         (cadr index-item) (cdr index-item)))
           (rest (if is-special-item (cddr index-item))))
      (switch-to-buffer (or (marker-buffer position) (current-buffer)))
      (print (current-buffer))
      (apply function (car index-item) position rest))
    (run-hooks 'imenu-after-jump-hook)))

;; (defun imenu-many (index-item)
;;   (interactive (list (imenu-choose-buffer-index)))
;;   (if (stringp index-item)
;;       (setq index-item (assoc index-item (imenu--make-many-index-alist))))
;;   (when index-item
;;     (push-mark)
;;     (print index-item)
;;     (let (position function rest buffer)
;;       (cond ((and (listp (nthcdr 1 index-item))
;;                   (listp (car (nth 1 index-item))))
;;              (setq position (nth 0 (caadr index-item))
;;                    function (nth 1 (caadr index-item))
;;                    rest (nth 2 (caadr index-item))
;;                    buffer (nth 1 (cadr index-item))))
;;             ((listp (nthcdr 1 index-item))
;;              (setq position (nth 0 (cadr index-item))
;;                    function imenu-default-goto-function
;;                    rest nil
;;                    buffer (nth 1 (cadr index-item))))
;;             ((< (nthcdr 1 index-item) 0)
;;              (setq position (cdr index-item)
;;                    function imenu-default-goto-function
;;                    rest nil
;;                    buffer nil)))
;;       (print function)
;;       (with-current-buffer (or buffer (current-buffer))
;;         (apply function (car index-item) position rest)))
;;     (run-hooks 'imenu-after-jump-hook)))

(provide 'imenu-many)