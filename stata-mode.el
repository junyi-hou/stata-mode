;;; stata.el -- providing major mode for editing do files -*- lexical-binding: t; -*-

;;; Commentary:
;; the syntax table is borrowed directly from ado-mode (https://github.com/louabill/ado-mode)
;; REPL integration is taken from emacs-juyter and stata_kernel

;;; Code:

(require 'stata-font-lock)

(defgroup stata-mode nil
  "Stata mode: major mode for editing do files."
  :group 'local
  :prefix "stata-mode-")

(defcustom stata-mode-hook nil
  "Hook for Stata mode."
  :type '(hook)
  :options '(turn-on-auto-fill)
  :group 'stata-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.do\\'" . stata-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ado\\'" . stata-mode))

;; syntax table
(defvar stata-mode-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?\\ "." tbl) ;; nullify escape meaning

    ;; ` ' is a pair
    (modify-syntax-entry ?` "(\'" tbl)
    (modify-syntax-entry ?\' ")`" tbl)

    ;; operators
    (modify-syntax-entry ?\$ "." tbl)  ;; start of a macro
    (modify-syntax-entry ?+ "." tbl)
    (modify-syntax-entry ?- "." tbl)
    (modify-syntax-entry ?= "." tbl)
    (modify-syntax-entry ?% "." tbl)
    (modify-syntax-entry ?< "." tbl)
    (modify-syntax-entry ?> "." tbl)
    (modify-syntax-entry ?& "." tbl)
    (modify-syntax-entry ?| "." tbl)
    (modify-syntax-entry ?~ "." tbl)
    (modify-syntax-entry ?# "." tbl)  ;; interaction of fixed effects

    ;; comments
    (modify-syntax-entry ?/  ". 124b" tbl)
    (modify-syntax-entry ?*  ". 23b"   tbl)
    (modify-syntax-entry ?\n "> b"  tbl)
    (modify-syntax-entry ?\^m "> b" tbl)
    tbl)
  "Syntax table used while in `stata-mode'.")

;; stata-mode-map: define keymaps
(defvar stata-mode-map (make-sparse-keymap) "Keymap for `stata-mode'.")

;;;###autoload
(define-derived-mode stata-mode prog-mode "Stata"
  "Major mode for editing do and ado files.

\\{stata-mode-map}"

  (setq-local comment-use-syntax t)
  (setq-local comment-multi-line nil)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local paragraph-separate (concat  "[ \t\f]*$\\|" page-delimiter))
  (setq-local paragraph-start (concat "[ \t\f]*$\\|" page-delimiter))

  (setq font-lock-defaults
        '(stata-mode-font-lock-keywords nil nil ((?\. . "w"))))

  (run-mode-hooks 'stata-mode-hook))

;;;###autoload
(define-derived-mode jupyter-stata-mode stata-mode "Jupyter-Stata"
  "This allows `org-mode' to correctly highlights `jupyter-stata' src blocks without using `ess-mode'. To do so, please install `jupyter-emacs' package to enable `ob-jupyter', and the stata kernel (pip install stata-kernel --user) to enable jupyter to talk to stata kernel.")

(provide 'stata-mode)
;;; stata-mode.el ends here
