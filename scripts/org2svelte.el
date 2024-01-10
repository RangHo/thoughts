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
      org-html-with-latex nil)

(defvar org2svelte--current-file-metadata nil
  "Metadata of the current file being exported.")

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
    (let* ((metadata-sanitized (mapcar (lambda (element)
                                         (cons (downcase (car element))
                                               (car (cdr element))))
                                       org2svelte--current-file-metadata))
           (metadata-json (json-encode metadata-sanitized)))
      (if (re-search-forward "<script.*context=\"module\".*>" nil t)
          (progn
            (forward-line)
            (insert (format "export const metadata = %s;\n"
                            (json-encode org2svelte--current-file-metadata))))
        (insert (format "<script context=\"module\">\nexport const metadata = %s;\n</script>\n"
                        (json-encode org2svelte--current-file-metadata)))))
    (write-file (concat (file-name-sans-extension file) ".svelte")))
  (princ " done.\n"))

(provide 'org2svelte)

;;; org2svelte.el ends here
