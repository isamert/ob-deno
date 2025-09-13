;;; ob-deno.el --- Babel Functions for Javascript/TypeScript with Deno      -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Isa Mert Gurbuz
;; Copyright (C) 2020-2024 HIGASHI Taiju

;; Author: HIGASHI Taiju (2020-2024), Isa Mert Gurbuz (2024-)
;; Keywords: literate programming, reproducible research, javascript, typescript, tools, deno
;; Homepage: https://github.com/isamert/ob-deno
;; Version: 2.0.1
;; Package-Requires: ((emacs "29.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ob-deno is babel functions for Javascript/TypeScript with Deno.
;; It's based on ob-js.

;; Parameters supported:
;;   - :cmd
;;   - :result-type
;;   - :var (You can specify a variable prefix with ob-deno-variable-prefix.)
;; Parameters not supported:
;;   - :session
;; Original Parameters:
;;   - :allow (Specifies a permission list for the deno command)

;;; Requirements:

;; - Deno https://deno.land/

;;; Code:

(require 'ob)
(require 'seq)

(defvar org-babel-default-header-args:deno '()
  "Default header arguments for JS/TS code blocks.")

(defcustom ob-deno-cmd "deno"
  "Name of command used to evaluate JS/TS blocks."
  :group 'ob-deno
  :type 'string)

(defcustom ob-deno-variable-prefix "let"
  "Type of variable prefix."
  :group 'ob-deno
  :type '(choice (const "const")
                 (const "let")
                 (const "var"))
  :safe #'stringp)

(defcustom ob-deno-function-wrapper
  "Deno.stdout.write(new TextEncoder().encode(JSON.stringify(await (async () => {%s})())));"
  "JS/TS code to print value of body.
%s is replaced with code body, without the imports.  Imports are
injected to the beginning of the file."
  :group 'ob-deno
  :type 'string)

(defconst ob-deno--treesit-imports-query
  (treesit-query-compile
   'typescript
   "[(import_statement) @import]")
  "Treesit query to find import statements for given typescript code.")

(defun ob-deno--split-imports-and-rest (body)
  "Split BODY into import statements and the rest of the lines return them."
  (with-temp-buffer
    (insert body)
    (let* ((root-node (treesit-buffer-root-node 'typescript))
           (imports (mapcar #'cdr (treesit-query-capture root-node ob-deno--treesit-imports-query)))
           (import-end-pos (if imports (treesit-node-end (car (last imports))) 0)))
      (list
       ;; :imports
       (string-join (mapcar (lambda (it) (treesit-node-text it t)) imports) "\n")
       ;; :rest
       (save-excursion
         (goto-char import-end-pos)
         (buffer-substring-no-properties
          (point)
          (point-max)))))))

(defun ob-deno--expand-body (imports params rest)
  "Create the full script to run.
IMPORTS are defined at the top and PARAMS are defined right after them.
REST is appended to the end."
  (concat
   imports
   "\n\n"
   (org-babel-expand-body:generic
    rest params (org-babel-variable-assignments:deno params))))

(defun ob-deno--to-lower-camel-case (str)
  "Convert STR to lower camel case."
  (let* ((case-fold-search nil)
         (words (split-string
                 (replace-regexp-in-string
                  "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" str)
                 "[^a-zA-Z0-9]+")))
    (if words
        (concat (downcase (car words))
                (mapconcat #'capitalize (cdr words) ""))
      "")))

(defun org-babel-expand-body:deno (body params &optional _var-lines)
  "Expand BODY with PARAMS.
This takes care of injecting parameters after the imports so that
produced code is a valid TypeScript code."
  (pcase-let* ((`(,imports ,rest) (ob-deno--split-imports-and-rest body)))
    (ob-deno--expand-body imports params rest)))

(defun org-babel-execute:deno (body params)
  "Execute a block of JS/TS code in `BODY' with org-babel.
You can also specify parameters in `PARAMS'.

This function is called by `org-babel-execute-src-block'."
  (pcase-let* ((no-color-env (getenv "NO_COLOR"))
               (ob-deno-cmd (or (cdr (assq :cmd params)) (format "%s run" ob-deno-cmd)))
               (allow (ob-deno-allow-params (cdr (assq :allow params))))
               (ob-deno-cmd-with-permission (concat ob-deno-cmd " " allow))
               (result-type (cdr (assq :result-type params)))
               (`(,imports ,rest) (ob-deno--split-imports-and-rest body))
               (result (let ((script-file (concat (org-babel-temp-file "deno-script-") ".ts")))
                         (with-temp-file script-file
                           (insert
                            (ob-deno--expand-body
                             imports
                             ;; return the value or the output
                             params
                             (if (string= result-type "value")
                                 (format ob-deno-function-wrapper rest)
                               rest))))
                         (setenv "NO_COLOR" "true")
                         (org-babel-eval
                          (format "%s %s" ob-deno-cmd-with-permission
                                  (org-babel-process-file-name script-file)) ""))))
    (setenv "NO_COLOR" no-color-env)
    (org-babel-result-cond (cdr (assq :result-params params))
      result (ob-deno-read result))))

(defun ob-deno-allow-params (allow-params)
  "Convert ALLOW-PARAMS to deno's allow-list parameter."
  (if (listp allow-params)
      (mapconcat #'ob-deno-allow-param-to-allow-list-str allow-params " ")
    (ob-deno-format-allow-param allow-params)))

(defun ob-deno-allow-param-to-allow-list-str (allow-param)
  "Convert ALLOW-PARAM to deno's allow-list parameter string."
  (if (listp allow-param)
      (ob-deno-format-allow-param (car allow-param) (cdr allow-param))
    (ob-deno-format-allow-param allow-param)))

(defun ob-deno-format-allow-param (allow-param &optional values)
  "Format ALLOW-PARAM to allow-list.
You can also specify values for the allow-list,
which can be specified by VALUES."
  (if values
      (format "--allow-%s=%s" allow-param (mapconcat (lambda (s) (format "%s" s)) values ","))
    (format "--allow-%s" allow-param)))

(defun ob-deno-read (results)
  "Convert RESULTS into an Org-mode table recognized by Org Babel.
Handles JSON arrays of objects, arrays, primitives, and single objects.
Preserves key order and empty rows."
  (let ((json-array (json-parse-string results)))
    (cond
     ;; Case 1: Single Object (hash table)
     ((hash-table-p json-array)
      (let ((keys '())
            (row '()))
        ;; Collect keys and build row in insertion order
        (maphash (lambda (key value)
                   (setq keys (append keys (list key)))
                   (setq row (append row (list (if value (format "%s" value) "")))))
                 json-array)
        (append (list keys 'hline) (list row))))

     ;; Case 2: Array of Objects (vector of hash tables)
     ((and (vectorp json-array) (hash-table-p (aref json-array 0)))
      (let ((keys '()))
        ;; Collect all keys in order of appearance across all objects
        (mapc (lambda (obj)
                (maphash (lambda (key _)
                           (unless (member key keys)
                             (setq keys (append keys (list key)))))
                         obj))
              json-array)
        (let ((rows (mapcar (lambda (obj)
                              (mapcar (lambda (key)
                                        (let ((value (gethash key obj)))
                                          (if value (format "%s" value) "")))
                                      keys))
                            (append json-array nil))))
          (append (list keys 'hline) rows))))

     ;; Case 3: Array of Arrays (vector of vectors)
     ((and (vectorp json-array) (vectorp (aref json-array 0)))
      (mapcar (lambda (row)
                (if (seq-empty-p row)
                    (make-list (length (aref json-array 0)) "")
                  (append row nil)))
              json-array))

     ;; Case 4: Array of Primitives (flat vector)
     ((vectorp json-array)
      (list (append json-array nil)))

     ;; Fallback: Return RESULTS unchanged
     (t results))))

(defun ob-deno-var-to-deno (val colnames &optional obj?)
  "Convert VAL into a JS/TS variable.
Convert an elisp value into a string of js/ts source code
specifying a variable of the same value.

COLNAMES are the column names from given table, if any.  OBJ? is an
argument indicating whether VAL should be an object or not."
  (cond
   ((and (listp val) (not obj?))
    (concat "[" (mapconcat (lambda (it) (ob-deno-var-to-deno it colnames colnames)) val ", ") "]"))
   ((and (listp val) obj?)
    (concat
     "{ "
     (string-join
      (seq-map-indexed
       (lambda (it idx)
         (format "%s: %s"
                 (ob-deno--to-lower-camel-case (nth idx colnames))
                 (ob-deno-var-to-deno it nil)))
       val)
      ", ")
     " }"))
   (:else
    (replace-regexp-in-string "\n" "\\\\n" (format "%S" val)))))

(defun org-babel-variable-assignments:deno (params)
  "Return list of JS/TS statements assigning the block's variables in PARAMS."
  (mapcar
   (lambda (pair)
     (format
      "%s %s = %s;"
      ob-deno-variable-prefix
      (car pair)
      (ob-deno-var-to-deno
       (cdr pair)
       (org-babel-pick-name
        (cdr (assq :colname-names params))
        (car pair)))))
   (org-babel--get-vars params)))

(provide 'ob-deno)

;;; ob-deno.el ends here
