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
      org-html-htmlize-output-type nil)

;; Replace parantheses/braces/brackets with HTML entities
(setq org-html-protect-char-alist
      `(,@org-html-protect-char-alist
        ("(" . "&lpar;")
        (")" . "&rpar;")
        ("{" . "&lbrace;")
        ("}" . "&rbrace;")))

(defvar org2svelte--current-file-metadata nil
  "Metadata of the current file being exported.")

(defun org2svelte--verbatim-html-advice (orig-fun &rest args)
  "Advice to wrap the output of ORIG-FUN with ARGS into Svelte verbatim HTML block."
  (let ((orig-res (apply orig-fun args)))
    (format "{@html `%s`}" (string-replace "\\" "\\\\" orig-res))))

;; Wrap LaTeX sources in Svelte verbatim HTML block to avoid parsing errors
(advice-add 'org-html-latex-environment
            :around #'org2svelte--verbatim-html-advice)
(advice-add 'org-html-latex-fragment
            :around #'org2svelte--verbatim-html-advice)

;; Generate semantic code blocks
(define-advice org-html-src-block
    (:override (src-block _contents info))
  (let ((lang (org-element-property :language src-block))
        (code (org-html-format-code src-block info)))
    (format "{@html `<pre><code class=\"language-%s\">%s</code></pre>`}" lang code)))

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
            (insert (format "export const metadata = %s;\n"
                            metadata-json)))
        (insert (format "<script context=\"module\">\nexport const metadata = %s;\n</script>\n"
                        metadata-json))))
    (write-file (concat (file-name-sans-extension file) ".svelte")))
  (princ " done.\n"))

(provide 'org2svelte)

;;; org2svelte.el ends here
