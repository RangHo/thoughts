;;; convert.el --- Batch convert Org-mode files to individual Svelte components.

;; Author: RangHo Lee <hello@rangho.me>

;;; Commentary:

;; This is a simple batch converter for Org-mode files to Svelte components.
;; It uses `ox-svelte' module, bundled as a git submdoule.

;;; Code:

(require 'ox-svelte
         (expand-file-name "ox-svelte/ox-svelte.el" (file-name-directory load-file-name)))

;; Disable general Org-mode export features
(setq org-export-with-section-numbers nil)

;; Customize how code blocks and math expressions are exported
(setq org-svelte-component-import-alist '(("Math" . "$lib/components/Math.svelte")
                                          ("Code" . "$lib/components/Code.svelte"))
      org-svelte-latex-environment-format "<Math expression={%s} display />"
      org-svelte-latex-fragment-format "<Math expression={%s} />"
      org-svelte-src-block-format "<Code lang={'%s'} code={%s} />")

(dolist (file command-line-args-left)
  (princ (format "Converting %s... " file))
  (find-file file)
  (org-svelte-export-to-svelte)
  (kill-buffer)
  (princ "done.\n"))

(provide 'convert)

;;; convert.el ends here
