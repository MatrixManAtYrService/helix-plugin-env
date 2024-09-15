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
  "Evaluate the current buffer with the steel interpreter."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (output-buffer "*Steel Output*"))
    (if file-name
        (progn
          (shell-command (concat "cat " file-name " | steel") output-buffer)
          (pop-to-buffer output-buffer))
      (message "Buffer not associated with a file!"))))

(require 'use-package)
(use-package symex
  :custom
  (symex-modal-backend 'evil)
  :config
  (symex-initialize)

  ; press , in normal mode to enter symex mode
  (evil-define-key 'normal 'global (kbd ",") 'symex-mode-interface)

  (evil-define-key 'normal symex-mode-map (kbd "e") 'eval-with-steel)
)
