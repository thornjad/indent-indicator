;;; indent-indicator.el --- show vertical lines to guide indentation -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019 Jade Michael Thornton
;;
;; Author: Jade Michael Thornton
;; Copyright (c) 2019 Jade Michael Thornton
;; URL: https://gitlab.com/thornjad/rivet
;; Version: 2.4.0
;;
;; This file is not part of GNU Emacs
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; The software is provided "as is" and the author disclaims all warranties
;; with regard to this software including all implied warranties of
;; merchantability and fitness. In no event shall the author be liable for
;; any special, direct, indirect, or consequential damages or any damages
;; whatsoever resulting from loss of use, data or profits, whether in an
;; action of contract, negligence or other tortious action, arising out of
;; or in connection with the use or performance of this software.
;;
;;; Commentary:
;;
;; インデントを縦線で表示
;;
;; Require this script
;;
;;   (require 'indent-indicator)
;;
;; and call command "M-x indent-indicator-mode".
;;
;; If you want to enable indent-indicator-mode automatically,
;; call "global-indent-indicator-mode" function.
;;
;;   (global-indent-indicator-mode)
;;
;; Column lines are propertized with "indent-indicator-face". So you may
;; configure this face to make guides more pretty in your colorscheme.
;;
;;   (set-face-background 'indent-indicator-face "dimgray")
;;
;; You may also change the character for guides.
;;
;;   (setq indent-indicator-char ":")
;;
;; indent-indicator is based on indent-guide by zk-phi.
;;
;;; Changelog:

;; 1.0.0 first released
;; 1.0.1 cleaned and optimized code
;;       works better for the file without trailing-whitespaces
;; 1.0.2 modified behavior for lines with only whitespaces
;; 1.0.3 Allow custom indent guide char
;; 1.0.4 disabled in org-indent-mode
;; 1.0.5 faster update of indent-indicator (especially for huge files)
;; 1.1.0 work with tab-indented files
;; 1.1.1 turned into minor-mode
;; 1.1.2 an infinite-loop bug fix
;; 1.1.3 changed behavior for blank lines
;; 2.0.0 rewrite almost everything
;; 2.0.1 improve blank-line and tab handling
;; 2.0.2 fixed bug that sometimes newline gets invisible
;; 2.0.3 added global-indent-indicator-mode
;; 2.1.0 now lines are not drawn over the cursor
;; 2.1.1 work better with blank lines
;; 2.1.2 fixed bug in empty files
;; 2.1.3 better bob and eob handling
;; 2.1.4 use "display" property instead of "before-string"
;;       (now works better with hl-line and linum)
;; 2.1.5 add "indent-indicator-inhibit-modes"
;; 2.1.6 add option "indent-indicator-recursive"
;; 2.2.0 add option "indent-indicator-threshold"
;; 2.3.0 use regexp search to find the beginning of level
;; 2.3.1 add option "indent-indicator-lispy-modes"
;; 2.4.0 change delay default to 0.1 seconds

(require 'cl-lib)

;;; Code:

(defconst indent-indicator-version "2.4.0")

(defgroup indent-indicator nil
  "Show vertical lines to guide indentation."
  :group 'environment)

(defcustom indent-indicator-char "|"
  "Character used as vertical line."
  :type 'string
  :group 'indent-indicator)

(defcustom indent-indicator-inhibit-modes
  '(tabulated-list-mode
    special-mode
    dired-mode
    eww-mode
    eshell-mode
    Custom-mode)
  "List of major-modes in which indent-indicator should be turned off."
  :type '(repeat symbol)
  :group 'indent-indicator)

(defcustom indent-indicator-recursive nil
  "When non-nil, draw multiple guide lines recursively."
  :type 'boolean
  :group 'indent-indicator)

(defcustom indent-indicator-delay 0.1
  "When a positive number, rendering guide lines is delayed DELAY
seconds."
  :type 'number
  :group 'indent-indicator)

(defcustom indent-indicator-threshold -1
  "Guide lines are drawn only when the column number is over this
value."
  :type 'number
  :group 'indent-indicator)

(defcustom indent-indicator-lispy-modes
  '(lisp-mode emacs-lisp-mode scheme-mode
              lisp-interaction-mode gauche-mode scheme-mode
              clojure-mode racket-mode egison-mode)
  "List of lisp-like language modes, in which the last brace of
blocks are NOT placed at beginning of line."
  :type '(repeat symbol)
  :group 'indent-indicator)

(defface indent-indicator-face '((t (:foreground "#535353" :slant normal)))
  "Face used to indent guide lines."
  :group 'indent-indicator)

(defvar indent-indicator--timer-object nil)


;;; Utilities

(defun indent-indicator--active-overlays ()
  "Return the list of all overlays created by indent-indicator."
  (delq nil
        (mapcar
         (lambda (ov)
           (and (eq (overlay-get ov 'category) 'indent-indicator) ov))
         (overlays-in (point-min) (point-max)))))

(defun indent-indicator--indentation-candidates (level)
  "*Internal function for `indent-indicator--beginning-of-level'."
  (cond ((<= level 0)
         (list ""))
        ((>= level tab-width)
         (cons (concat "\t" (make-string (- level tab-width) ?\s))
               (cons (make-string level ?\s)
                     (indent-indicator--indentation-candidates (1- level)))))
        (t
         (cons (make-string level ?\s)
               (indent-indicator--indentation-candidates (1- level))))))

(defun indent-indicator--beginning-of-level ()
  "Move to the beginning of current indentation level and return
the point. When no such points are found, just return nil."
  (back-to-indentation)
  (let* ((base-level (if (not (eolp))
                         (current-column)
                       (max (save-excursion
                              (skip-chars-forward "\s\t\n")
                              (current-column))
                            (save-excursion
                              (skip-chars-backward "\s\t\n")
                              (back-to-indentation)
                              (current-column)))))
         (candidates (indent-indicator--indentation-candidates (1- base-level)))
         (regex (concat "^" (regexp-opt candidates t) "[^\s\t\n]")))
    (unless (zerop base-level)
      (and (search-backward-regexp regex nil t)
           (goto-char (match-end 1))))))


;;; Generation

(defun indent-indicator--make-overlay (line col)
  "draw line at (line, col)"
  (let (diff string ov prop)
    (save-excursion
      ;; try to goto (line, col)
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column col)
      ;; calculate difference from the actual col
      (setq diff (- col (current-column)))
      ;; make overlay or not
      (cond ((and (eolp) (<= 0 diff))   ; the line is too short
             ;; <-line-width->  <-diff->
             ;;               []        |
             (if (setq ov (cl-some
                           (lambda (ov)
                             (when (eq (overlay-get ov 'category) 'indent-indicator)
                               ov))
                           (overlays-in (point) (point))))
                 ;; we already have an overlay here => append to the existing overlay
                 ;; (important when "recursive" is enabled)
                 (setq string (let ((str (overlay-get ov 'before-string)))
                                (concat str
                                        (make-string (- diff (length str)) ?\s)
                                        (propertize indent-indicator-char 'face 'indent-indicator-face)))
                       prop   'before-string)
               (setq string (concat (make-string diff ?\s)
                                    (propertize indent-indicator-char 'face 'indent-indicator-face))
                     prop   'before-string
                     ov     (make-overlay (point) (point)))))
            ((< diff 0)                 ; the column is inside a tab
             ;;  <---tab-width-->
             ;;      <-(- diff)->
             ;;     |            []
             (if (setq ov (cl-some
                           (lambda (ov)
                             (when (eq (overlay-get ov 'category) 'indent-indicator)
                               ov))
                           (overlays-in (1- (point)) (point))))
                 ;; we already have an overlay here => modify the existing overlay
                 ;; (important when "recursive" is enabled)
                 (setq string (let ((str (overlay-get ov 'display)))
                                (aset str (+ 1 tab-width diff) ?|)
                                str)
                       prop   'display)
               (setq string (concat (make-string (+ tab-width diff) ?\s)
                                    (propertize indent-indicator-char 'face 'indent-indicator-face)
                                    (make-string (1- (- diff)) ?\s))
                     prop   'display
                     ov     (make-overlay (point) (1- (point))))))
            ((looking-at "\t")          ; okay but looking at tab
             ;;    <-tab-width->
             ;; [|]
             (setq string (concat (propertize indent-indicator-char 'face 'indent-indicator-face)
                                  (make-string (1- tab-width) ?\s))
                   prop   'display
                   ov     (make-overlay (point) (1+ (point)))))
            (t                          ; okay and looking at a space
             (setq string (propertize indent-indicator-char 'face 'indent-indicator-face)
                   prop   'display
                   ov     (make-overlay (point) (1+ (point))))))
      (when ov
        (overlay-put ov 'category 'indent-indicator)
        (overlay-put ov prop string)))))

(defun indent-indicator-show ()
  (interactive)
  (unless (or (indent-indicator--active-overlays)
              (active-minibuffer-window))
    (let ((win-start (window-start))
          (win-end (window-end nil t))
          line-col line-start line-end)
      ;; decide line-col, line-start
      (save-excursion
        (indent-indicator--beginning-of-level)
        (setq line-col (current-column)
              line-start (max (1+ (line-number-at-pos))
                              (line-number-at-pos win-start)))
        ;; if recursive draw is enabled and (line-col > 0), recurse
        ;; into lower level.
        (when (and indent-indicator-recursive (> line-col 0))
          (indent-indicator-show)))
      (when (> line-col indent-indicator-threshold)
        ;; decide line-end
        (save-excursion
          (while (and (progn (back-to-indentation)
                             (or (< line-col (current-column)) (eolp)))
                      (forward-line 1)
                      (not (eobp))
                      (<= (point) win-end)))
          (cond ((< line-col (current-column))
                 (setq line-end (line-number-at-pos)))
                ((not (memq major-mode indent-indicator-lispy-modes))
                 (setq line-end (1- (line-number-at-pos))))
                (t
                 (skip-chars-backward "\s\t\n")
                 (setq line-end (line-number-at-pos)))))
        ;; draw line
        (dotimes (tmp (- (1+ line-end) line-start))
          (indent-indicator--make-overlay (+ line-start tmp) line-col))
        (remove-overlays (point) (point) 'category 'indent-indicator)))))

(defun indent-indicator-remove ()
  (dolist (ov (indent-indicator--active-overlays))
    (delete-overlay ov)))


;;; Define minor modes

(defun indent-indicator-post-command-hook ()
  (if (null indent-indicator-delay)
      (indent-indicator-show)
    (when (null indent-indicator--timer-object)
      (setq indent-indicator--timer-object
            (run-with-idle-timer indent-indicator-delay nil
                                 (lambda ()
                                   (indent-indicator-show)
                                   (setq indent-indicator--timer-object nil)))))))

(defun indent-indicator-pre-command-hook ()
  ;; some commands' behavior may affected by indent-indicator overlays, so
  ;; remove all overlays in pre-command-hook.
  (indent-indicator-remove))

;;;###autoload
(define-minor-mode indent-indicator-mode
  "show vertical lines to guide indentation"
  :init-value nil
  :lighter " ing"
  :global nil
  (if indent-indicator-mode
      (progn
        (add-hook 'pre-command-hook 'indent-indicator-pre-command-hook nil t)
        (add-hook 'post-command-hook 'indent-indicator-post-command-hook nil t))
    (remove-hook 'pre-command-hook 'indent-indicator-pre-command-hook t)
    (remove-hook 'post-command-hook 'indent-indicator-post-command-hook t)))

;;;###autoload
(define-globalized-minor-mode global-indent-indicator-mode
  indent-indicator-mode
  (lambda ()
    (unless (cl-some 'derived-mode-p indent-indicator-inhibit-modes)
      (indent-indicator-mode 1))))


;;; indent-indicator.el ends here

(provide 'indent-indicator)
