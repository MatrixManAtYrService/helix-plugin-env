(require "helix/editor.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(provide shell git-add open-helix-scm open-init-scm)
;;@doc
;; Specialized shell implementation, where % is a wildcard for the current file
(define (shell cx . args)
 ;; Replace the % with the current file
 (define expanded
         (map (lambda (x)
               (if (equal? x "%")
                (current-path cx)
                x))
              args))
 (apply helix.run-shell-command expanded))

;;@doc
;; Adds the current file to git
(define (git-add cx)
 (shell cx "git" "add" "%"))

;; Functions to assist with the above
(define (current-path)
 (let* ((focus (editor-focus))
        (focus-doc-id (editor->doc-id focus))
        (document (editor-get-doc-if-exists focus-doc-id)))
  (if document
   (Document-path document)
   #f)))

(define (editor-get-doc-if-exists doc-id)
 (if (editor-doc-exists? doc-id)
  (editor->get-document doc-id)
  #f))

;;@doc
;; Open the helix.scm file
(define (open-helix-scm)
 (helix.open (helix.static.get-helix-scm-path)))

;;@doc
;; Opens the init.scm file
(define (open-init-scm)
 (helix.open (helix.static.get-init-scm-path)))
