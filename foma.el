;;; foma.el --- Download font files and quickly switch between different profiles  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Xiaoduanc

;; Author: Xiaoduan Chang <drcxd@sina.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: fonts
;; URL: https://github.com/drcxd/foma

;;; Commentary:

;; This package helps download fonts in your operating systems and
;; quickly switching between different font profiles in Emacs.  Unlike
;; other font packages that focus on persistent font configurations,
;; foma is designed for users who enjoy frequently switching between
;; different fonts.

;; Features:
;; - Help download font files.
;; - Allow quick switching between different font profiles.
;; - Adopt font profiles in several patterns (by date, randomly, etc.).

;; Usage:
;; See the README.org file.

;;; Code:

(require 'dired)
(require 'json)
(require 'url)
(require 'seq)

(defgroup foma nil
  "Quickly switch between font profiles."
  :group 'convenience
  :prefix "foma-")

(defcustom foma-default-weight
  'regular
  "Default weight to use in a font profile which does not specify it."
  :type '(symbol)
  :group 'foma)

(defcustom foma-default-height
  140
  "Default height to use in a font profile which does not specify it."
  :type '(number)
  :group 'foma)

(defcustom foma-default-mono
  nil
  "Default fixed pitch font to use in a font profile."
  :type '(string)
  :group 'foma)

(defcustom foma-default-variable
  nil
  "Default variable pitch font to use in a font profile."
  :type '(string)
  :group 'foma)

(defcustom foma-default-chinese
  nil
  "Default font for Chinese characters and punctuations."
  :type '(string)
  :group 'foma)

(defcustom foma-fonts-dir
  "fonts"
  "The directory to store downloaded font files.

It is relative to the user Emacs directory."
  :type '(string)
  :group 'foma)

(defvar foma-google-fonts-api-key ""
  "An API key that is required to download font files from Google Fonts.")

(defvar foma--current-profile nil
  "Currently selected profile.")

(defvar foma-fonts '()
  "A list of known fonts.  Each font is a list with the following elements:
- name: font name (identifier for zip, family name for Google Fonts)
- type: either \\='zip or \\='google
- url: URL to zip file (only required if type is \\='zip)")

(defvar foma-profiles '()
  "A list of font profiles.

A profile is a plist that contains the following elements: name,
fixed-pitch-font, variable-pitch-font, chinese-font, weight, height, and
face-specs.")

(defvar foma--process-output-buffer-name "*foma-process-output*"
  "Buffer name that contains the output of extracting program.")

(defun foma--font-name (font)
  "Return the name of FONT."
  (nth 0 font))

(defun foma--font-type (font)
  "Return the type of FONT."
  (nth 1 font))

(defun foma--font-url (font)
  "Return the URL of FONT."
  (nth 2 font))

(defun foma--download-dir ()
  "Compute the actual directory to store downloaded font files."
  (file-name-concat user-emacs-directory foma-fonts-dir))

(defun foma--query-google-fonts-api (family-name)
  "Query Google Fonts API for FAMILY-NAME and return parsed JSON response."
  (let* ((encoded-family (url-hexify-string family-name))
         (api-url (format "https://www.googleapis.com/webfonts/v1/webfonts?key=%s&family=%s"
                         foma-google-fonts-api-key
                         encoded-family))
         (buffer (url-retrieve-synchronously api-url t)))
    (unless buffer
      (error "Failed to retrieve data from Google Fonts API for family: %s" family-name))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "^$")
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'string))
        (ignore json-object-type json-array-type json-key-type)
        (json-read)))))

(defun foma--extract-font-urls-from-response (response)
  "Extract font file URLs and style names from Google Fonts API RESPONSE.
Returns an alist of (style-name . url) pairs."
  (let* ((items (alist-get "items" response nil nil #'string=))
         (first-item (car items))
         (files (alist-get "files" first-item nil nil #'string=)))
    files))

(defun foma--download-font-from-zip (font)
  "Download font from a zip file URL for FONT."
  (let* ((dir (foma--download-dir))
         (name (foma--font-name font))
         (url (foma--font-url font))
         (file-name (format "%s.%s" name (file-name-extension url))))
    (unless (file-exists-p dir)
      (dired-create-directory dir))
    (url-copy-file url (file-name-concat dir file-name) t)
    (message "Font %s has been downloaded from zip!" name)
    file-name))

(defun foma--download-font-from-google (font)
  "Download font files from Google Fonts for FONT.
Skips download if API key is not set."
  (let* ((dir (foma--download-dir))
         (family-name (foma--font-name font)))
    (if (string-empty-p foma-google-fonts-api-key)
        (warn "Skipping Google Font '%s': API key not set. Please set foma-google-fonts-api-key" family-name)
      (unless (file-exists-p dir)
        (dired-create-directory dir))
      (message "Querying Google Fonts API for %s..." family-name)
      (let* ((response (foma--query-google-fonts-api family-name))
             (style-url-pairs (foma--extract-font-urls-from-response response)))
        (if style-url-pairs
            (progn
              (message "Found %d font files for %s" (length style-url-pairs) family-name)
              (dolist (pair style-url-pairs)
                (let* ((style-name (car pair))
                       (url (cdr pair))
                       (file-name (format "%s-%s.ttf" family-name style-name))
                       (dest-path (file-name-concat dir file-name)))
                  (message "Downloading %s..." file-name)
                  (url-copy-file url dest-path t)))
              (message "All font files for %s downloaded!" family-name))
          (error "No font files found for family: %s" family-name))))))

;;;###autoload
(defun foma-download-all-fonts ()
  "Download all registered fonts."
  (interactive)
  (mapc #'foma-download-font foma-fonts))

(defun foma-download-font (font)
  "Download font files for a single FONT.
Dispatches to appropriate download function based on font type."
  (interactive
   (list
    (assoc (completing-read "Font name: " (mapcar #'car foma-fonts)) foma-fonts)))
  (let ((type (foma--font-type font)))
    (cond
     ((eq type 'zip)
      (let ((file-name (foma--download-font-from-zip font)))
        (foma--extract-font-files file-name)))
     ((eq type 'google)
      (foma--download-font-from-google font))
     (t
      (error "Unknown font type: %s" type)))))

(defun foma--extract-font-files (file-name)
  "Extract font files from FILE-NAME."
  (let ((dir (foma--download-dir)))
    (unless (file-directory-p dir)
      (error "Font directory does not exist: %s" dir))
    (let ((zip-file (file-name-concat dir file-name)))
      (message "Extracting: %s" (file-name-nondirectory zip-file))
      (let* ((temp-dir (make-temp-file (file-name-base zip-file) t))
             (extracted-files (foma--extract-zip-to-dir zip-file temp-dir)))
        (if extracted-files
            (foma--move-font-files temp-dir dir))
        (delete-directory temp-dir t)))))

(defun foma--extract-zip-to-dir (zip-file dir)
  "Extract ZIP-FILE to DIR and return list of extracted files."
  (unless (file-exists-p zip-file)
    (error "Zip file does not exist: %s" zip-file))

  (unless (file-directory-p dir)
    (make-directory dir t))

  (let* ((buf (or (get-buffer foma--process-output-buffer-name)
                  (generate-new-buffer foma--process-output-buffer-name)))
         (result (call-process "7z" nil buf nil "x" (expand-file-name zip-file)
                              (concat "-o" (expand-file-name dir)) "-y")))
    (if (eq result 0)
        (directory-files-recursively dir "")
      (message "Failed to extract %s (exit code: %s)"
               (file-name-nondirectory zip-file)
               result)
      nil)))

(defun foma--move-font-files (src-dir dst-dir)
  "Move all .ttf/.otf files from SRC-DIR to DST-DIR recursively."
  (let ((font-files (directory-files-recursively src-dir "\\.[ot]tf\\'")))
    (dolist (font-file font-files)
      (let ((dst-file (expand-file-name
                       (file-name-nondirectory font-file)
                       dst-dir)))
        (condition-case err
            (rename-file font-file dst-file t)
          (error
           (message "Failed to move %s: %s"
                    (file-name-nondirectory font-file)
                    (error-message-string err))))))))

;; TODO: Revert more properties than `:family'
(defun foma--apply-face-specs (face-specs &optional revert)
  "Apply FACE-SPECS.  Revert its effect if REVERT is non-nil."
  (dolist (specs-faces face-specs)
    (let ((specs (car specs-faces))
          (faces (cdr specs-faces)))
      (when revert
        (setq specs (cl-copy-list specs))
        (if (plist-member specs :family)
            (plist-put specs :family 'unspecified)))
      (dolist (face faces)
        (apply #'set-face-attribute face nil specs)))))

;;;###autoload
(defun foma-apply-profile (profile-name)
  "Apply the font profile specified by PROFILE-NAME."
  (interactive
   (let ((completion-ignore-case t))
     (list (completing-read "Font profile name: " (foma--get-profile-names)))))
  (let ((current-profile (foma--get-profile foma--current-profile)))
    (if current-profile
        (if-let* ((face-specs (foma--profile-face-specs current-profile)))
            (foma--apply-face-specs face-specs t))))
  (let ((profile (foma--get-profile profile-name)))
    (if profile
        (let* ((mono (or (foma--profile-mono profile)
                          foma-default-mono))
               (variable (or (foma--profile-variable profile)
                             foma-default-variable))
               (chinese (or (foma--profile-chinese profile)
                            foma-default-chinese))
               (weight (or (foma--profile-weight profile)
                           foma-default-weight))
               (height (or (foma--profile-height profile)
                           foma-default-height))
               (face-specs (foma--profile-face-specs profile)))
          (foma-setup-mono-font mono weight height)
          (if variable
              (foma-setup-variable-pitch-font variable))
          (if chinese
              (foma-setup-chinese-font chinese))
          (if face-specs
              (foma--apply-face-specs face-specs))
          (setq foma--current-profile profile-name))
      (warn "Missing font profile: %s" profile-name))))

(defun foma--profile-name (profile)
  (plist-get profile :name))

(defun foma--profile-mono (profile)
  (plist-get profile :mono))

(defun foma--profile-variable (profile)
  (plist-get profile :variable))

(defun foma--profile-chinese (profile)
  (plist-get profile :chinese))

(defun foma--profile-weight (profile)
  (plist-get profile :weight))

(defun foma--profile-height (profile)
  (plist-get profile :height))

(defun foma--profile-face-specs (profile)
  (plist-get profile :face-specs))

(defun foma--get-profile-names ()
  "Return the registered profile names."
  (mapcar (lambda (p)
            (foma--profile-name p))
          foma-profiles))

(defun foma--get-profile (name)
  "Return font profile with NAME."
  (seq-find (lambda (p)
              (string= name (foma--profile-name p)))
            foma-profiles))

(defun foma--collect-variable-fonts ()
  "Return a list of variable font names occur in profiles."
  (let (result)
    (dolist (profile foma-profiles)
      (when-let ((variable-font (foma--profile-variable profile)))
        (when (foma--font-available-p variable-font)
          (push variable-font result))))
    (sort result)))

(defun foma--nth-mod-list-length (list n)
  "Return the N modulo list length element of LIST."
  (nth (mod n (length list)) list))

(defun foma--date-to-day ()
  "Return the number of day computed from `current-time'."
  (/ (+ (car (current-time-zone))
        (time-convert (current-time) 'integer))
     (* 60 60 24)))

(defun foma--select-by-day (list)
  "Return an element of LIST depends on the current date."
  (foma--nth-mod-list-length list (foma--date-to-day)))

;; credit:
;; https://emacsredux.com/blog/2021/12/22/check-if-a-font-is-available-with-emacs-lisp/
;; and comments by Amit Patel
(defun foma--font-available-p (font-name)
  "Return non-nil if FONT-NAME is installed."
  (member font-name (font-family-list)))

(defun foma--check-font (font-name)
  "Warning if FONT-NAME is not installed."
  (unless (find-font (font-spec :family font-name))
    (warn "Font %s may not be available!" font-name)))

;;;###autoload
(defun foma-setup-mono-font (font weight height)
  "Apply FONT to face `default' and face `fixed-pitch'.

WEIGHT and HEIGHT are applied to default faces only."
  (interactive "sFont: \nSWeight: \nnHeight: ")
  (foma--check-font font)
  (set-face-attribute 'default nil
                      :family font
                      :weight weight
                      :height height)
  (set-face-attribute 'fixed-pitch nil
                      :family font
                      :weight weight
                      :height 1.0))

;;;###autoload
(defun foma-setup-variable-pitch-font (font)
  "Apply FONT to face `variable-pitch'.

FONT can be a string of the to-be-applied font family.

If FONT is symbol `by-date', then a font will be selected using
`foma--select-by-day' from the list of variable pitch fonts returned by
`foma--collect-variable-fonts'.

If FONT is symbol `random', then a font will be selected randomly from
the list of variable pitch fonts returned by
`foma--collect-variable-fonts'."
  (interactive
   (let ((completion-ignore-case t))
     (list (completing-read "Font: " (foma--collect-variable-fonts)))))
  (when (symbolp font)
    (let ((variable-fonts (foma--collect-variable-fonts)))
      (setq font (cond ((eq font 'by-date) (foma--select-by-day variable-fonts))
                       ((eq font 'random) (seq-random-elt variable-fonts))
                       (t (user-error "Unknown symbol %s" font))))))
  (foma--check-font font)
  (set-face-attribute 'variable-pitch nil
                      :family font))

;;;###autoload
(defun foma-setup-chinese-font (font)
  "Make Chinese characters use FONT.

FONT is a string of the font name."
  (interactive "sFont name: ")
  (foma--check-font font)
  (set-fontset-font t 'han (font-spec :family font))
  (set-fontset-font t 'cjk-misc (font-spec :family font)))

;;;###autoload
(defun foma-apply-profile-by-day ()
  "Apply a profile depends on the current date."
  (interactive)
  (foma-apply-profile (foma--select-by-day (foma--get-profile-names))))

;;;###autoload
(defun foma-apply-profile-rand ()
  "Apply a profile randomly."
  (interactive)
  (let* ((record (seq-random-elt foma-profiles))
         (profile-name (foma--profile-name record)))
    (foma-apply-profile profile-name)))

;;;###autoload
(defun foma-describe-current-profile ()
  "Describe the font profile currently used."
  (interactive)
  (message "The current foma profile is %s" foma--current-profile))

;;;###autoload
(defun foma-apply-variable-font-by-day ()
  "Apply a variable font depends on the current date."
  (interactive)
  (foma-setup-variable-pitch-font
   (foma--select-by-day (foma--collect-variable-fonts))))

(provide 'foma)
;;; foma.el ends here
