;;; org2svelte.el --- Batch convert Org-mode files to individual Svelte components.

;; Author: RangHo Lee <hello@rangho.me>

;;; Commentary:

;; This is a simple batch converter for Org-mode files to Svelte components.
;; It is based on the `org-export-as-html' function, and disables cosmetic
;; features such as syntax highlighting and table of contents to allow easier
;; post-processing.

;;; Code:

(require 'json)
(require 'org)
(require 'org-element)
(require 'ox)
(require 'ox-html)

;; Disable general Org-mode export features
(setq org-export-with-section-numbers nil
      org-export-with-toc nil)

;; Disable unnecessary HTML-specific features
(setq org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-htmlize-output-type nil
      org-html-link-org-files-as-html nil)

;; Enable smart quotes
(setq org-export-with-smart-quotes t)
(add-to-list 'org-export-smart-quotes-alist
             '(("ko"
                (primary-opening :utf-8 "“" :html "&ldquo;")
                (primary-closing :utf-8 "”" :html "&rdquo;")
                (secondary-opening :utf-8 "‘" :html "&lsquo;")
                (secondary-closing :utf-8 "’" :html "&rsquo;")
                (apostrophe :utf-8 "’" :html "&rsquo;"))))

;; Replace parantheses/braces/brackets with HTML entities
(setq org-html-protect-char-alist
      `(,@org-html-protect-char-alist
        ("(" . "&lpar;")
        (")" . "&rpar;")
        ("{" . "&lbrace;")
        ("}" . "&rbrace;")))

(defconst org2svelte--component-module-script
  "
import Math from '$lib/components/Math.svelte';
import Code from '$lib/components/Code.svelte';
export const metadata = %s;"
  "Template for the component module script.")

(defvar org2svelte--current-file-metadata nil
  "Metadata of the current file being exported.")

;; Replace LaTeX environments with Svelte Math components
(define-advice org-html-latex-environment
    (:around (orig-fun &rest args))
  (let ((orig-res (apply orig-fun args)))
    (format "<Math expression={`%s`} display />" (string-replace "\\" "\\\\" orig-res))))

;; Replace LaTeX fragments with Svelte Math components
(define-advice org-html-latex-fragment
    (:around (orig-fun &rest args))
  (let ((orig-res (apply orig-fun args)))
    (format "<Math expression={`%s`} />"
            (replace-regexp-in-string (rx (or "\\\\(" "\\\\)"))
                                      ""
                                      (string-replace "\\" "\\\\" orig-res)))))

;; Generate semantic code blocks
(define-advice org-html-src-block
    (:override (src-block _contents info))
  (let ((lang (org-element-property :language src-block))
        (code (org-html-format-code src-block info)))
    (format "<Code lang=\"%s\" code={`%s`} />" lang code)))

(dolist (file command-line-args-left)
  (princ (format "Converting %s to a Svelte component..." file))
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (setq org2svelte--current-file-metadata
          (org-collect-keywords '("TITLE" "SUBTITLE" "AUTHOR" "DATE" "LANGUAGE")))
    (org-html-export-as-html nil ; synchronoously
                             nil ; export whole tree
                             nil ; export regardless of visibility
                             t)  ; export only the body
    ;; Inject metadata to component module context
    (let* ((metadata-sanitized (mapcar (lambda (element)
                                         (cons (downcase (car element))
                                               (car (cdr element))))
                                       org2svelte--current-file-metadata))
           (metadata-json (json-encode metadata-sanitized)))
      (if (re-search-forward "<script.*context=\"module\".*>" nil t)
          (progn
            (forward-line)
            (insert (format org2svelte--component-module-script metadata-json)))
        (insert (concat "<script context=\"module\">\n"
                        (format org2svelte--component-module-script metadata-json)
                        "\n</script>\n"))))
    (write-file (concat (file-name-sans-extension file) ".svelte")))
  (princ " done.\n"))

(provide 'org2svelte)

;;; org2svelte.el ends here
