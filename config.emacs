; disable some ui elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

; use vi bindings
(require 'evil)
(evil-mode 1)

; shut up
(setq visible-bell t)
(setq ring-bell-function 'ignore)

(require 'lispy)
(add-hook 'scheme-mode-hook (lambda ()
                              (lispy-mode 1)))

(require 'lispyville)
(add-hook 'scheme-mode-hook #'lispyville-mode)

(lispyville-set-key-theme '(operators c-w additional))
