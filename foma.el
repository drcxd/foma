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

(require 'cl-lib)

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
  ""
  "Default variable pitch font to use in a font profile which does not
specify it."
  :type '(string)
  :group 'foma)

(defcustom foma-fonts-dir
  "fonts"
  "The directory relative to the user emacs directory where downloaded font
files are stored."
  :type '(string)
  :group 'foma)

(cl-defstruct foma-font
  "A font object that specifies where to download it."
  name
  url)

(cl-defstruct foma-profile
  "A font profile specifies fonts and face properties."
  name
  fixed-pitch-font
  variable-pitch-font
  weight
  height)

(defvar foma--current-profile nil
  "Currently selected profile.")

(defvar foma-fonts '()
  "A list of known fonts.")

(defvar foma-profiles '()
  "A list of font profiles.")

(defun foma-register-font (name url)
  "Register a font so that font manager knows where to download it."
  (unless
      (alist-get name foma-fonts nil nil #'equal)
    (push (cons name
                (make-foma-font
                 :name name
                 :url url))
          foma-fonts)))

(defun foma-register-fonts (font-list)
  "Helper function to register multiple fonts. FONT-LIST is a list of pairs
where the car is the font name and cdr is the URL."
  (dolist (pair font-list)
    (foma-register-font (car pair) (cdr pair))))

(defun foma--download-dir ()
  "Compute the actual directory to store downloaded font files."
  (file-name-concat user-emacs-directory foma-fonts-dir))

;;;###autoload
(defun foma-download-all-fonts ()
  "Download all registered fonts."
  (interactive)
  (mapc #'foma--download-font foma-fonts)
  (foma--extract-font-files))

(defun foma--download-font (record)
  "Download font files for a single font."
  (let ((dir (foma--download-dir))
        (font (cdr record)))
    (unless (file-exists-p dir)
      (dired-create-directory dir))
    (require 'url)
    (let* ((name (foma-font-name font))
           (url (foma-font-url font))
           (file-name (format "%s.%s" name (file-name-extension url))))
      (url-copy-file url (file-name-concat dir file-name) t)
      (message "Font %s has been downloaded!" name))))

(defun foma--extract-font-files ()
  "Extract font files from .zip files."
  (let ((dir (foma--download-dir)))
    (unless (file-directory-p dir)
      (error "Font directory does not exist: %s" dir))

    (let ((zip-files (directory-files dir t "\\.zip\\'")))
      (dolist (zip-file zip-files)
        (message "Extracting: %s" (file-name-nondirectory zip-file))
        (let* ((temp-dir (make-temp-file (file-name-base zip-file) t))
               (extracted-files (foma--extract-zip-to-dir zip-file temp-dir)))
          (if extracted-files
              (foma--move-ttf-files temp-dir dir)
            (message "Failed to extract: %s" (file-name-nondirectory zip-file)))
          (delete-directory temp-dir t))))))

(defun foma--extract-zip-to-dir (zip-file dir)
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

(defun foma--move-ttf-files (src-dir dst-dir)
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

(defun foma-register-profile (name fix &optional variable weight height)
  "Register a new font profile."
  (unless
      (alist-get name foma-profiles nil nil #'equal)
    (push (cons name
                (make-foma-profile
                 :name name
                 :fixed-pitch-font fix
                 :variable-pitch-font variable
                 :weight weight
                 :height height))
          foma-profiles)))

(defun foma-register-profiles (profile-list)
  "Helper function to register multiple profiles."
  (dolist (profile profile-list)
    (foma-register-profile
     (nth 0 profile)
     (nth 1 profile)
     (nth 2 profile)
     (nth 3 profile)
     (nth 4 profile))))

;;;###autoload
(defun foma-apply-profile (profile-name)
  "Apply the font profile specified by the given profile name."
  (interactive
   (let ((completion-ignore-case t))
     (list (completing-read "Font profile name: " (foma--get-profile-names)))))
  (let ((profile (foma--get-profile profile-name)))
    (if profile
        (progn (foma-setup-fonts
                (foma-profile-fixed-pitch-font profile)
                (or (foma-profile-variable-pitch-font profile)
                    foma-default-variable-pitch-font)
                (or (foma-profile-weight profile)
                    foma-default-weight)
                (or (foma-profile-height profile)
                    foma-default-height))
               (setq foma--current-profile profile-name))
      (warn "Missing font profile: %s" profile-name))))

(defun foma--get-profile-names ()
  "Return the registered profile names."
  (mapcar #'car foma-profiles))

(defun foma--get-profile (name)
  "Return font profile by name."
  (alist-get name foma-profiles nil nil #'equal))

;;;###autoload
(defun foma-setup-fonts (fixed variable weight height)
  "Apply given font properties in Emacs."
  (interactive "sFixed pitch font: \nsVariable pitch font: \nSWeight: \nnHeight: ")
  (foma-setup-fixed-pitch-font fixed weight height)
  (foma-setup-variable-pitch-font variable))

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
      (set-fontset-font "fontset-default" 'han font)
    (warn (format "Font %s is not available!" font))))

(defun foma--apply-profile-by-num (n)
  "Apply the profile using an index into the profile list."
  (let ((len (length foma-profiles)))
    (if (= 0 len)
        (message "No font profiles!")
      (let* ((idx (% n len))
             (profile-name (foma-profile-name (cdr (nth idx foma-profiles)))))
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
         (profile-name (car record)))
    (foma-apply-profile profile-name)))

;;;###autoload
(defun foma-describe-current-profile ()
  "Describe the font profile currently used."
  (interactive)
  (message "The current foma profile is %s" foma--current-profile))

(provide 'foma)
;;; foma.el ends here
