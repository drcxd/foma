;;; font-manager.el --- Download, install and quickly switch between different font profiles  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Xiaoduanc

;; Author: Xiaoduan Chang <drcxd@sina.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: fonts
;; URL: https://github.com/drcxd/font-manager

;;; Commentary:

;; This package helps download and install fonts in your operating
;; systems and quickly switching between different font profiles in
;; Emacs. Unlike other font packages that focus on persistent font
;; configurations, font-manager is designed for users who enjoy
;; frequently changing their fonts.

;; Features:
;; - Help download and install font files specified by users
;; - Allow quick switching between different font profiles
;; - Adopt font profiles in several patterns (by date, session, etc.)

;; Usage:
;; (require 'font-manager)
;; ;; Configure your font profiles
;; ;; Switch between profiles with M-x font-manager-switch-profile

;;; Code:

(defgroup font-manager nil
  "Quickly switch between font profiles."
  :group 'convenience
  :prefix "font-manager-")

(defcustom font-manager-default-weight
  'regular
  "Default weight to use in a font profile which does not specify it."
  :type '(symbol)
  :group 'font-manager)

(defcustom font-manager-default-height
  180
  "Default height to use in a font profile which does not specify it."
  :type '(number)
  :group 'font-manager)

(defcustom font-manager-default-variable-pitch-font
  ""
  "Default variable pitch font to use in a font profile which does not
specify it."
  :type '(string)
  :group 'font-manager)

(defcustom font-manager-fonts-dir
  "fonts"
  "The directory relative to the user emacs directory where downloaded font
files are stored."
  :type '(string)
  :group 'font-manager)

(cl-defstruct font
  "A font object that specifies where to download it."
  name
  url)

(cl-defstruct font-profile
  "A font profile specifies fonts and face properties."
  name
  fixed-pitch-font
  variable-ptich-font
  weight
  height)

(defvar font-manager-fonts '()
  "A list of known fonts.")

(defvar font-manager-profiles '()
  "A list of font profiles.")

(defun font-manager-register-font (name url)
  "Register a font so that font manager knows where to download it."
  (cl-pushnew
   (make-font
    :name name
    :url url)
   font-manager-fonts))

(defun font-manager-register-fonts (font-list)
  "Helper function to register multiple fonts. FONT-LIST is a list of pairs
where the car is the font name and cdr is the URL."
  (dolist (pair font-list)
    (font-manager-regiser-font (car pari) (cdr pair))))

(defun font-manager--download-dir ()
  "Compute the actual directory to store downloaded font files."
  (file-name-concat user-emacs-directory font-manager-fonts-dir))

;;;###autoload
(defun font-manager-download-all-fonts ()
  "Download all registered fonts."
  (interactive)
  (mapc #'font-manager--download-font font-manager-fonts)
  (font-manager--extract-font-files))

(defun font-manager--download-font (font)
  "Download font files for a single font."
  (let ((dir (font-manager--download-dir)))
    (unless (file-exists-p dir)
      (dired-create-directory dir))
    (require 'url)
    (let* ((name (font-name font))
           (url (font-url font))
           (file-name (format "%s.%s" name (file-name-extension url))))
      (url-copy-file url (file-name-concat dir file-name) t)
      (message "Font %s has been downloaded!" name))))

(defun font-manager--extract-font-files ()
  "Extract font files from .zip files."
  (let ((dir (font-manager--download-dir)))
    (unless (file-directory-p dir)
      (error "Font directory does not exist: %s" dir))

    (let ((zip-files (directory-files dir t "\\.zip\\'")))
      (dolist (zip-file zip-files)
        (message "Extracting: %s" (file-name-nondirectory zip-file))
        (let* ((temp-dir (make-temp-file (file-name-base zip-file) t))
               (extracted-files (font-manager--extract-zip-to-dir zip-file temp-dir)))
          (if extracted-files
              (font-manager--move-ttf-files temp-dir dir)
            (message "Failed to extract: %s" (file-name-nondirectory zip-file)))
          (delete-directory temp-dir t))))))

(defun font-manager--extract-zip-to-dir (zip-file dir)
  "Extract ZIP-FILE to DIR and return list of extracted files."
  (unless (file-exists-p zip-file)
    (error "Zip file does not exist: %s" zip-file))

  (unless (file-directory-p dir)
    (make-directory dir t))

  (let ((result (call-process "7z" nil nil nil "x" zip-file
                              (concat "-o" dir) "-y")))
    (if (eq result 0)
        (directory-files-recursively dir "")
      (message "Failed to extract %s (exit code: %s)"
               (file-name-nondirectory zip-file)
               result)
      nil)))

(defun font-manager--move-ttf-files (src-dir dst-dir)
  "Move all .ttf files from SRC-DIR to DST-DIR recursively."
  (let ((ttf-files (directory-files-recursively src-dir "\\.ttf\\'")))
    (dolist (ttf-file ttf-files)
      (let ((dst-file (expand-file-name
                       (file-name-nondirectory ttf-file)
                       dst-dir)))
        (condition-case err
            (rename-file ttf-file dst-file t)
          (error
           (message "Failed to move %s: %s"
                    (file-name-nondirectory ttf-file)
                    (error-message-string err))))))))

(provide 'font-manager)
;;; font-manager.el ends here
