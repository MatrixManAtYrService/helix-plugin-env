;; I don't really know what I'm doing
;; I also got a lot of AI help wit this
;; Anyhow it's usable enough for me

; disable some ui elements
(tool-bar-mode -1)
(menu-bar-mode -1)

; use vi bindings
(require 'evil)
(evil-mode 1)

; no noises please
(setq visible-bell t)
(setq ring-bell-function 'ignore)

(defun eval-with-steel ()
  "Evaluate the s-expression under the cursor with the steel interpreter."
  (interactive)
  (let* ((output-buffer "*Steel Output*")
         (current-buffer (current-buffer))
         (sexp (thing-at-point 'sexp))
         (result (when sexp
                   (with-temp-buffer
                     (insert sexp)
                     (call-process-region (point-min) (point-max) "steel" t t nil)
                     (buffer-string))))
         (filtered-result (when result
                            (with-temp-buffer
                              (insert result)
                              (goto-char (point-min))
                              (delete-non-matching-lines "^λ > ")
                              (goto-char (point-min))
                              (while (re-search-forward "^λ > " nil t)
                                (replace-match ""))
                              (goto-char (point-max))
                              (when (re-search-backward "^CTRL-D$" nil t)
                                (delete-region (line-beginning-position) (point-max)))
                              (goto-char (point-min))
                              (while (re-search-forward "\n+" nil t)
                                (replace-match "\n"))
                              (string-trim (buffer-string))))))
    (if sexp
        (progn
          (with-current-buffer (get-buffer-create output-buffer)
            (let ((inhibit-read-only t))  ; Allow modification of read-only buffer
              (goto-char (point-max))
              (unless (bobp)
                (insert "\n"))
              (insert filtered-result)))
          (display-buffer output-buffer)
          (set-buffer current-buffer))
      (message "No s-expression found at point"))))


(defun symex-eval-scheme-custom ()
  "Eval Scheme symex using custom function."
  (eval-with-steel))

(defun symex-interface-register-scheme-custom ()
  "Register the custom Scheme runtime interface."
  (symex-interface-extend
   symex-scheme-modes
   (list
    :eval #'symex-eval-scheme-custom
    :eval-definition #'eval-with-steel
    :eval-pretty #'symex-eval-scheme-custom)))

(advice-add 'symex-interface-register-scheme :override #'symex-interface-register-scheme-custom)

(require 'use-package)
(use-package symex
  :custom
  (symex-modal-backend 'evil)
  :config
  (symex-initialize)

  ; press , in normal mode to enter symex mode
  (evil-define-key 'normal 'global (kbd ",") 'symex-mode-interface)

  ; press e in symex mode to evaluate the selected expression
  (evil-define-key '(normal) symex-mode-map (kbd "e") 'symex-eval-scheme-custom)

  ; press escape from insert mode to go to symex mode
  ; press it again to get back to normal mode
  ; this is awkward if you didn't enter insert from symex
  ; otherwise it feels pretty good
  (evil-define-key 'insert 'global [escape] 'symex-mode-interface)
)

; Highlight parentheses configuration
(require 'highlight-parentheses)
(use-package highlight-parentheses
  :ensure t
  :config
  (global-highlight-parentheses-mode)
  (add-hook 'scheme-mode-hook #'highlight-parentheses-mode)
  (add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup)

  ; Optional: Customize colors if you want
  (setq highlight-parentheses-colors '("magenta" "red" "orange1" "yellow1" "green1" 
    "cyan1" "slateblue1" "purple" "lavender"))
)
