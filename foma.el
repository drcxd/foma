;;; foma.el --- Download font files and quickly switch between different profiles  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Xiaoduanc

;; Author: Xiaoduan Chang <drcxd@sina.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: fonts
;; URL: https://github.com/drcxd/foma

;;; Commentary:

;; This package helps download fonts in your operating systems and
;; quickly switching between different font profiles in Emacs. Unlike
;; other font packages that focus on persistent font configurations,
;; foma is designed for users who enjoy frequently switching between
;; different fonts.

;; Features:
;; - Help download font files
;; - Allow quick switching between different font profiles
;; - Adopt font profiles in several patterns (by date, randomly, etc.)

;; Usage:
;; TODO

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
  180
  "Default height to use in a font profile which does not specify it."
  :type '(number)
  :group 'foma)

(defcustom foma-default-variable-pitch-font
  nil
  "Default variable pitch font to use in a font profile which does not
specify it."
  :type '(string)
  :group 'foma)

(defcustom foma-default-chinese-font
  nil
  "Default font for Chinese characters and punctuations."
  :type '(string)
  :group 'foma)

(defcustom foma-fonts-dir
  "fonts"
  "The directory relative to the user emacs directory where downloaded font
files are stored."
  :type '(string)
  :group 'foma)

(defvar foma-google-fonts-api-key ""
  "An API key that is required to download font files from Google Fonts.")

(defvar foma--current-profile nil
  "Currently selected profile.")

(defvar foma-fonts '()
  "A list of known fonts. Each font is a list with the following elements:
- name: font name (identifier for zip, family name for Google Fonts)
- type: either \\='zip or \\='google
- url: URL to zip file (only required if type is \\='zip)")

(defvar foma-profiles '()
  "A list of font profiles. A profile is a plist that contains the
following elements: name, fixed-pitch-font, variable-pitch-font,
chinese-font, weight, and height.")

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
  "Extract font files from .zip files."
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

;;;###autoload
(defun foma-apply-profile (profile-name)
  "Apply the font profile specified by the given profile name."
  (interactive
   (let ((completion-ignore-case t))
     (list (completing-read "Font profile name: " (foma--get-profile-names)))))
  (let ((profile (foma--get-profile profile-name)))
    (if profile
        (let ((fixed (foma--profile-fixed-pitch-font profile))
              (variable (or (foma--profile-variable-pitch-font profile)
                            foma-default-variable-pitch-font))
              (chinese (or (foma--profile-chinese-font profile)
                           foma-default-chinese-font))
              (weight (or (foma--profile-weight profile)
                          foma-default-weight))
              (height (or (foma--profile-height profile)
                          foma-default-height)))
          (foma-setup-fixed-pitch-font fixed weight height)
          (if variable
              (foma-setup-variable-pitch-font variable))
          (if chinese
              (foma-setup-chinese-font chinese)))
      (warn "Missing font profile: %s" profile-name))))

(defun foma--profile-name (profile)
  (plist-get profile :name))

(defun foma--profile-fixed-pitch-font (profile)
  (plist-get profile :fixed-pitch-font))

(defun foma--profile-variable-pitch-font (profile)
  (plist-get profile :variable-pitch-font))

(defun foma--profile-chinese-font (profile)
  (plist-get profile :chinese-font))

(defun foma--profile-weight (profile)
  (plist-get profile :weight))

(defun foma--profile-height (profile)
  (plist-get profile :height))

(defun foma--get-profile-names ()
  "Return the registered profile names."
  (mapcar (lambda (p)
            (foma--profile-name p))
          foma-profiles))

(defun foma--get-profile (name)
  "Return font profile by name."
  (seq-find (lambda (p)
              (string= name (foma--profile-name p)))
            foma-profiles))

;;;###autoload
(defun foma-setup-fixed-pitch-font (font weight height)
  "Setup default and fixed-pitch faces."
  (interactive "sFont: \nSWeight: \nnHeight: ")
  (if (foma--font-available-p font)
      (progn
        (set-face-attribute 'default nil
                            :font font
                            :weight weight
                            :height height)
        (set-face-attribute 'fixed-pitch nil
                            :font font
                            :weight weight
                            :height 1.0))
    (warn (format "Font %s is not available!" font))))

;;;###autoload
(defun foma-setup-variable-pitch-font (font)
  "Setup variable pitch face."
  (interactive "sFont: ")
  (if (foma--font-available-p font)
      (set-face-attribute 'variable-pitch nil
                          :font font
                          :height 1.0)
    (warn (format "Font %s is not available!" font))))

;; credit: https://emacsredux.com/blog/2021/12/22/check-if-a-font-is-available-with-emacs-lisp/
(defun foma--font-available-p (font-name)
  "Return true if font specified by FONT-NAME is installed."
  (find-font (font-spec :name font-name)))

;;;###autoload
(defun foma-setup-chinese-font (font)
  "Chinese characters use the given font."
  (interactive "sFont name: ")
  (if (foma--font-available-p font)
      (progn
        (set-fontset-font "fontset-default" 'han font)
        (set-fontset-font t 'cjk-misc font))
    (warn (format "Font %s is not available!" font))))

(defun foma--apply-profile-by-num (n)
  "Apply the profile using an index into the profile list."
  (let ((len (length foma-profiles)))
    (if (= 0 len)
        (message "No font profiles!")
      (let* ((idx (% n len))
             (profile-name (foma--profile-name (nth idx foma-profiles))))
        (foma-apply-profile profile-name)))))

;;;###autoload
(defun foma-apply-profile-by-day ()
  "Apply a profile depends on date."
  (interactive)
  (let ((day (/ (time-convert (current-time) 'integer) (* 60 60 24))))
    (foma--apply-profile-by-num day)))

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

(provide 'foma)
;;; foma.el ends here
