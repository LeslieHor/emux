;;; emux.el --- Syncronise inputs to terms

;; Copyright (C) 2020 Leslie Hor <Leslie.Hor@Gmail.com>

;; Author: Leslie Hor <Leslie.Hor@Gmail.com>
;; Maintainer Leslie Hor <Leslie.Hor@Gmail.com>
;; URL:
;; Keywords: term
;; Version: 1.0
;; Package-Requires: ()

;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Implementation of tmux's "syncronise-panes" feature

;;; Code:

(require 'term)

(defvar emux-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'emux-send-to-all-raw)
    (define-key map [backspace] 'emux-send-to-all-backspace)
    (define-key map [return] 'emux-send-to-all-return)
    (define-key map [up] 'emux-send-to-all-up)
    (define-key map [down] 'emux-send-to-all-down)
    (define-key map [right] 'emux-send-to-all-right)
    (define-key map [left] 'emux-send-to-all-left)

    (define-key map [C-up] 'emux-send-to-all-ctrl-up)
    (define-key map [C-down] 'emux-send-to-all-ctrl-down)
    (define-key map [C-right] 'emux-send-to-all-ctrl-right)
    (define-key map [C-left] 'emux-send-to-all-ctrl-left)
    (define-key map [home] 'emux-send-to-all-home)
    (define-key map [end] 'emux-send-to-all-end)
    (define-key map [insert] 'emux-send-to-all-insert)
    (define-key map [S-prior] 'scroll-down)
    (define-key map [S-next] 'scroll-up)
    (define-key map [delete] 'emux-send-to-all-del)
    (define-key map [deletechar] 'emux-send-to-all-del)
    (define-key map [S-insert] 'emux-send-to-all-paste)
    (define-key map [prior] 'emux-send-to-all-prior)
    (define-key map [next] 'emux-send-to-all-next)

    map)
  "Keymap for `emux-mode'.")

(defun evil-emux-set-keys ()
  (evil-define-key 'normal emux-mode-map
    "q" 'emux-quit
    "a" 'emux-add-buffer
    "r" 'emux-remove-buffer
    "A" 'emux-add-all-visible-terms
    "c" 'emux-reset-buffer-list
    "l" 'emux-load-group
    "s" 'emux-split-frame
    "S" 'emux-split-frame-new-frame
    "v" 'emux-load-terms-with-vars
    "V" 'emux-load-terms-with-vars-new-frame
    )
  (evil-define-key 'insert emux-mode-map
    (kbd "C-d") 'emux-send-to-all-exit
    (kbd "TAB") 'emux-send-to-all-raw
    (kbd "C-c C-c") 'emux-send-to-all-raw
    )
  )

(evil-emux-set-keys)

(define-derived-mode emux-mode text-mode "emux"
  "Major mode for emux mode."
  :keymap (let ((map (make-sparse-keymap)))
            map))

(defvar-local emux-buffer-list '())
(defvar-local emux-original-buffer-name "")

(defvar emux-groups
  '(("hardcoded" ("TEST" ("1" "2" "3")))
    ("shell-get" ("FROM_SHELL" "echo 1 2 3"))))

(defvar emux--frame-arrangements
  '(() ; 1
    (h) ; 2
    (h h) ; 3
    (h v r v) ; 4
    (v h h d h) ; 5
    (v h h d h h) ; 6
    (v h h h d h h) ; 7
    (v h h h d h h h) ; 8
    (v v h h d h h d h h) ; 9
    (v v h h h d h h d h h) ; 10
    (v v h h h d h h h d h h) ; 11
    (v v h h h d h h h d h h h) ; 12
    (v v h h h h d h h h d h h h) ; 13
    (v v h h h h d h h h h d h h h) ; 14
    (v v h h h h d h h h h d h h h h) ; 15
    (v v v h h h d h h h d h h h d h h h) ; 16
    (v v v h h h h d h h h d h h h d h h h) ; 17
    (v v v h h h h d h h h h d h h h d h h h) ; 18
    (v v v h h h h d h h h h d h h h h d h h h) ; 19
    (v v v h h h h d h h h h d h h h h d h h h h) ; 20
    )
  "Frame arrangements for emux. Spit all vertically first")

(defvar emux-frame-emux-window-size 5
  "Size of emux buffer when starting emux frame. Must be more than 3.")

(defun emux ()
  (interactive)
  (let ((buf (get-buffer-create "*emux-new*")))
    (with-current-buffer buf
      (rename-buffer "*emux*" t)
      (insert "Emux Mode\n\n"
              "[q] quit\n"
              "[c] clear buffers         [l] load value groups\n"
              "[a] add buffers           [A] add all visible terms\n"
              "[r] remove buffer\n"
              "[s] split frame           [S] split new frame\n"
              "[v] split with vars       [V] split with var new frame\n"
              "\n\n")
      (emux-mode)
      (switch-to-buffer buf))))

(defun emux-clean-buffer-list ()
  (setq emux-buffer-list
        (reverse
         (seq-filter (lambda (buf)
                       (buffer-name buf))
                     emux-buffer-list))))

(defun emux--filter-buffer-list (&optional buffer-list)
  (reverse
   (seq-filter (lambda (buf)
                 (with-current-buffer buf
                   (equal major-mode 'term-mode)))
               (cond ((buffer-list buffer-list))
                     (t (buffer-list))))))

(defun emux-get-term-buffers-alist (&optional current-buffers)
  (mapcar (lambda (buf)
            (cons (buffer-name buf)
                  buf))
          (set-difference (emux--filter-buffer-list)
                          current-buffers)))

(defun emux-print-buffers ()
  (insert "Buffer list: \n" (format "%s" (reverse emux-buffer-list)) "\n"))

(defun emux-remove-buffer ()
  (interactive)
  (let* ((buf-alist (mapcar (lambda (buf)
                              (cons (buffer-name buf) buf))
                            emux-buffer-list))
         (buf-name (completing-read "Buffer: " buf-alist))
         (buf (cdr (assoc buf-name buf-alist))))
    (message "%s" buf)
    (with-current-buffer buf
      (rename-buffer emux-original-buffer-name t))
    (setq emux-buffer-list
          (seq-remove (lambda (b) (eq b buf)) emux-buffer-list))
    (emux-print-buffers)))

(defun emux-add-buffer ()
  (interactive)
  (let* ((term-buffers-alist (cons (cons "FINISH" nil) (emux-get-term-buffers-alist emux-buffer-list)))
         (buf-name (completing-read "Buffer: " term-buffers-alist))
         (buf (cdr (assoc (buf-name term-buffers-alist nil nil 'string=)))))
    (when buf
      (emux--add-buffer-to-list buf)
      (emux-print-buffers)
      (emux-add-buffer))))

(defun emux--add-buffer-to-list (buf)
  (push buf emux-buffer-list)
  (let ((group-name (buffer-name)))
    (with-current-buffer buf
      (setq emux-original-buffer-name (buffer-name))
      (rename-buffer (concat (buffer-name) " (" group-name ")*") t))))

(defun emux--add-buffers-to-list (buffer-list)
  (dolist (buf buffer-list)
    (emux--add-buffer-to-list buf)))

(defun emux-clear-buffer-list ()
  (interactive)
  (setq emux-buffer-list '())
  (emux-print-buffers))

(defun emux-send-to-all-raw-string (string)
  (dolist (buf emux-buffer-list)
    (with-current-buffer buf
      (term-send-raw-string string))))

(defun emux-send-to-all-raw ()
  (interactive)
  (let ((keys (this-command-keys)))
    (emux-send-to-all-raw-string (string (aref keys (1- (length keys)))))))

(defun emux-send-to-all-backspace  () (interactive) (emux-send-to-all-raw-string "\C-?"))
(defun emux-send-to-all-return     () (interactive) (emux-send-to-all-raw-string "\n"))
(defun emux-send-to-all-up         () (interactive) (emux-send-to-all-raw-string "\eOA"))
(defun emux-send-to-all-down       () (interactive) (emux-send-to-all-raw-string "\eOB"))
(defun emux-send-to-all-right      () (interactive) (emux-send-to-all-raw-string "\eOC"))
(defun emux-send-to-all-left       () (interactive) (emux-send-to-all-raw-string "\eOD"))
(defun emux-send-to-all-ctrl-up    () (interactive) (emux-send-to-all-raw-string "\e[1;5A"))
(defun emux-send-to-all-ctrl-down  () (interactive) (emux-send-to-all-raw-string "\e[1;5B"))
(defun emux-send-to-all-ctrl-right () (interactive) (emux-send-to-all-raw-string "\e[1;5C"))
(defun emux-send-to-all-ctrl-left  () (interactive) (emux-send-to-all-raw-string "\e[1;5D"))
(defun emux-send-to-all-home       () (interactive) (emux-send-to-all-raw-string "\e[1~"))
(defun emux-send-to-all-insert     () (interactive) (emux-send-to-all-raw-string "\e[2~"))
(defun emux-send-to-all-end        () (interactive) (emux-send-to-all-raw-string "\e[4~"))
(defun emux-send-to-all-prior      () (interactive) (emux-send-to-all-raw-string "\e[5~"))
(defun emux-send-to-all-next       () (interactive) (emux-send-to-all-raw-string "\e[6~"))
(defun emux-send-to-all-del        () (interactive) (emux-send-to-all-raw-string "\e[3~"))

(defun emux-send-to-all-paste ()
  (interactive)
  (emux-send-to-all-raw-string (current-kill 0)))

(defun emux-send-to-all-exit ()
  (interactive)
  (emux-send-to-all-raw-string "\C-d")
  (emux-clear-buffer-list))

(defun emux-reset-buffer-list ()
  (interactive)
  (emux-clean-buffer-list)
  (dolist (buf emux-buffer-list)
    (with-current-buffer buf
      (rename-buffer emux-original-buffer-name t)))
  (emux-clear-buffer-list))

(defun emux-quit ()
  (interactive)
  (emux-reset-buffer-list)
  (kill-this-buffer))

(defun emux-load-group (&optional g)
  (interactive)
  (emux-clean-buffer-list)
  (let* ((group (cond (g g)
                      (t (completing-read "Group: " emux-groups))))
         (group-values (cdr (assoc group emux-groups)))
         (num-of-terms (length emux-buffer-list)))
    (dolist (key-values group-values)
      (let* ((key (car key-values))
             (val (cadr key-values))
             (values (cond ((listp val) val)
                           ((stringp val) (split-string
                                           (string-trim (shell-command-to-string val))
                                           " ")))))
        (insert (format "Key: %s\n" key))
        (insert (format "Values: %s\n" val))
        (insert (format "Evaluated Values: %s\n" values))
        (cond ((eq (length values) num-of-terms)
               (dolist (i (number-sequence 0 (- num-of-terms 1)))
                 (let ((term (nth i emux-buffer-list))
                       (val (nth i values)))
                   (insert (format "Setting key %s to value %s for term %s\n"
                                   key val
                                   (buffer-name term)))
                   (with-current-buffer term
                     (term-send-raw-string (format "\n%s=\"%s\"\n" key val))))))
              (t (user-error "Wrong number of terms. Expected %s" (length values))))))))

(defun emux-add-all-visible-terms ()
  (interactive)
  (emux-clean-buffer-list)
  (emux--add-buffers-to-list
   (set-difference
    (emux--filter-buffer-list (mapcar 'window-buffer
                                     (window-list)))
    emux-buffer-list))
  (emux-print-buffers))

(defun emux-set-up-frame (n &optional use-current-frame)
  (with-selected-frame (if use-current-frame (selected-frame) (make-frame))
    (let* ((arrangement (nth (- n 1) emux--frame-arrangements))
           (start-win (selected-window))
           (emux-buffer-height (max 3 emux-frame-emux-window-size))
           (height (- (frame-height) emux-buffer-height))
           (width (frame-width))
           (emux-win (split-window-below)))
      ;; Do the splits
      (dolist (c arrangement)
        (cond ((eq c 'h) (split-window-horizontally))
              ((eq c 'v) (split-window-vertically))
              ((eq c 'd) (windmove-down))
              ((eq c 'r) (windmove-right)))
        ;; Must be here to avoid "window too small to split" error
        (balance-windows))

      ;; Launch terminal in all windows (except the emux window)
      (with-selected-window start-win
        (dolist (win (set-difference (window-list) (list emux-win)))
          (with-selected-window win
            (ansi-term "/bin/bash"))))

      ;; Set the emux win size
      (minimize-window emux-win)
      (with-selected-window emux-win
        (enlarge-window (max (- emux-frame-emux-window-size 3) 0)))

      ;; Balance rows
      (let* ((rows (+ (count 'v arrangement) 1))
             (row-height (/ height rows)))
        (dolist (win (set-difference (window-list) (list emux-win)))
          (let ((delta (- row-height (window-height win))))
            (window-resize win delta))))

      (select-window emux-win)
      (with-current-buffer (if use-current-frame (window-buffer emux-win) (emux))
        (emux-add-all-visible-terms))
      emux-win)))

(defun emux-new-frame-group (&optional use-current-frame)
  (interactive)
  (emux-set-up-frame
   (string-to-number (read-string
                      (format "How many terms (%s max)? " (length emux--frame-arrangements))))
   use-current-frame))

(defun emux-launch-frame-group-with-variables (&optional use-current-frame)
  (interactive)
  (let* ((group (completing-read "Group: " emux-groups))
         (group-values (cadadr (assoc group emux-groups)))
         (num-of-terms (length group-values))
         (emux-win (emux-set-up-frame num-of-terms use-current-frame)))
    (with-current-buffer (window-buffer emux-win)
      (emux-load-group group))))

(defun emux-load-terms-with-vars ()
  (interactive)
  (emux-launch-frame-group-with-variables 't))

(defun emux-load-terms-with-vars-new-frame ()
  (interactive)
  (emux-launch-frame-group-with-variables))

(defun emux-split-frame ()
  (interactive)
  (emux-new-frame-group 't))

(defun emux-split-frame-new-frame ()
  (interactive)
  (emux-new-frame-group))

(provide 'emux)
