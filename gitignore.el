;;; gitignore.el --- Auto-add boilerplate .gitignores for many languages  -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author: GitHub user "jcaw" <40725916+jcaw@users.noreply.github.com>
;; Version: 1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, outlines, vc
;; URL: http://www.github.com/jcaw/gitignore.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Automatically add .gitignore files covering many languages. Choose a
;; language, e.g. "Emacs" or "Python" and the corresponding .gitignore file will
;; be added to your project.
;;
;; If you already have a .gitignore file, the new ignore patterns will be
;; appended to it.
;;
;; If you have Magit installed, it will prompt you for a gitignore when you
;; create a new project.
;;
;; The boileplate .gitignore files are pulled from
;; https://github.com/github/gitignore . Under the hood, this modules pulls that
;; repo into a local cache, then prompts you to select one. You must have
;; internet connection the first time you add a .gitignore, but after that you
;; can use it offline.

;;; Code:


(defvar gitignore--local-templates-directory
  (concat spacemacs-cache-directory "github-gitignores/")
  "Directory to clone the boilerplate .gitignores from github.")


(defvar gitignore--github-repo-address
  "https://github.com/github/gitignore"
  "Remote repository for boilerplate .gitignores, on github.")


(defvar gitignore--github-hostname
  "www.github.com"
  "Hostname to use to ping GitHub.")


(defvar gitignore--ping-timeout "2"
  "Time to wait for when pinging GitHub.

Might have to bump this up for extremely slow internet
connections.

NOTE: Must be a string.")


(defun gitignore-do-you-want-a-template (&rest -)
  "Prompt the user: would they like to insert a .gitignore template?"
  (when (y-or-n-p "Would you like to insert a .gitignore template? ")
    (gitignore-add-template)))


;;;###autoload
(defun gitignore-add-template ()
  "Add a boilerplate .gitignore to the current git directory.

Prompts the user to select a .gitignore template from a list of
templates such as Python, Emacs, etc. The list comes from the
GitHub boilerplate gitignores repo, stored in
`gitignore--github-repo-address'. By default this should be:
https://github.com/github/gitignore

The template will be added to the git project as \".gitignore\".
If a .gitignore file already exists, the contents of the new
template will be appended to it."
  (interactive)
  ;; Download most up-to-date templates
  (gitignore--update-github-templates)
  ;; Now choose a .gitignore and copy it in.
  (let* ((chosen-gitignore-path (gitignore--prompt-for-template))
         ;; Try a number of method to get the root dir
         (root-dir (or
                    ;; Use magit if it's installed
                    (when (fboundp 'magit-toplevel)
                      (magit-toplevel))
                    ;; `vc-root-dir' seems unreliable but try it in case it
                    ;; works.
                    (vc-root-dir)
                    (read-directory-name "Specify root dir: ")))
         (gitignore-path (expand-file-name ".gitignore" root-dir)))
    (if (file-exists-p gitignore-path)
        (when (y-or-n-p ".gitignore already exists. Append to it? ")
          (gitignore--append-to-gitignore chosen-gitignore-path gitignore-path))
      (copy-file chosen-gitignore-path gitignore-path))))


(defun gitignore--get-templates ()
  "Create an alist of .gitignore templates.

Mapping is '((name . full-path))."
  ;; Construct an alist of gitignore types to file paths. For example:
  ;; '(("Emacs" . "~/.emacs.d/.cache/github-gitignores/Emacs.gitignore")
  ;;   ("python" . "~/.emacs.d/.cache/github-gitignores/python.gitignore"))
  (mapcar (lambda (file)
            (let ((type (file-name-nondirectory
                         (file-name-sans-extension
                          file))))
              ;; Return a tuple of type to filepath. For example:
              ;; '("Emacs" . "~/.emacs.d/.cache/github-gitignores/Emacs.gitignore")
              (cons type file)))
          (directory-files-recursively gitignore--local-templates-directory "\.gitignore$")))


(defun gitignore--prompt-for-template ()
  "Get the user to choose a template. Return its path."
  (let* ((templates (gitignore--get-templates))
         (chosen-key (completing-read
                      "Choose a .gitignore template: "
                      templates nil t)))
    ;; `completing-read' will only return the key. Use that to extract the
    ;; value.
    ;;
    ;; NOTE: For some reason, alist-get didn't work here. Use this manual method
    ;; instead.
    (cdr (assoc chosen-key templates))))


(defun gitignore--read-lines (file-path)
  "Return a list of lines of a file at `FILE-PATH'."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" nil)))


(defun gitignore--read-and-strip-template (gitignore-path)
  "Read a .gitignore template, stripping lines we don't want.

Basically, strips file headers like hashbangs and mode
declarations."
  (let* ((lines (gitignore--read-lines gitignore-path))
         (trimmed-lines
          ;; Some lines will be nil. Clear them out.
          (remove
           nil
           ;; Get rid of unwanted lines
           (mapcar (lambda (line)
                     (unless (or
                              ;; Ignore hashbangs
                              ;;FIXME
                               (string-prefix-p "^#[ \t]*\\!" line)
                               ;; Get rid of mode declarations
                               (string-match-p "^#[ \t]*-\\*-" line))
                       (message "Line: %s" line)
                       line))
                   lines))))
    (string-join lines "\n")))


(defun gitignore--append-to-gitignore (chosen-gitignore-path gitignore-gitignore-path)
  "Append the contents of another gitignore to this gitignore."
  (let* ((new-ignores (gitignore--read-and-strip-template
                       chosen-gitignore-path))
         (string-to-insert
          ;; We want double blank lines before inserting.
          (concat "\n\n" (string-trim-left new-ignores))))
    ;; Append the new patterns but also make sure to visit the file to verify.
    (find-file-other-window gitignore-gitignore-path)
    (goto-char (point-max))
    (insert string-to-insert)
    (save-buffer)))


(defun gitignore--stream-process-output (command-list)
  "Call a shell command and stream the output as it comes in.

Returns the exit code of the process. Should be 0 if successful.

Good for commands that might take a while, like pulling from an
external repo."
  (let ((proc (make-process :name (string-join command-list "-")
                            :command command-list
                            :connection-type 'pipe
                            :filter (lambda (proc line) (message "%s" line))
                            :sentinel (lambda (proc line) t))))
    (while (eq (process-status proc) 'run)
      (accept-process-output proc)
      (redisplay t))
    (process-exit-status proc)))


(defun gitignore--can-ping-host (host)
  "Can we ping a host?

Uses a timeout of 2 seconds."
  ;; Might take a while and the output will be messaged, so notify the user
  ;; what's happening.
  (message "Testing connection to `%s'..." host)
  (eq 0 (if (eq system-type 'windows-nt)
            ;; Have to use a different method on Windows ("-n" for number of
            ;; tries instead of "-c".)
            ;;
            ;; "ping" options:
            ;; -n : number of tries (want just 1)
            ;; -w : timeout in seconds
            (gitignore--stream-process-output
             (list "ping" "-n" "1" "-w" gitignore--ping-timeout host))
          ;; This format tested and works on Linux. Untested on Mac, but should
          ;; work the same way.
          ;;
          ;; "ping" options:
          ;; -c : number of tries (want just 1)
          ;; -W : timeout in seconds
          (gitignore--stream-process-output
           ;; Force a timeout external to ping because a failure in the DNS
           ;; lookup might cause ping to hang, even with a timeout argument. This
           ;; doesn't occur on Windows.
           ;; Ping options:
           (list "timeout" gitignore--ping-timeout
                 "ping" "-c" "1" "-W" gitignore--ping-timeout host))
          )))


(defun gitignore--can-ping-github ()
  "Can we ping GitHub?

If not, this implies GitHub is down or the internet is disconnected."
  (gitignore--can-ping-host gitignore--github-hostname))


(defun gitignore--update-github-templates ()
  "Update the folder holding the .gitignores templates (from GitHub).

This will throw an error iff no copy of the folder can be
*cloned* from GitHub. If an old version exists, it will default
to using that."
  ;; We need the template directory to exist so we can work within it.
  (unless (file-directory-p gitignore--local-templates-directory)
    (mkdir gitignore--local-templates-directory))

  ;; Temporarily change the working directory.
  (with-temp-buffer
    (cd gitignore--local-templates-directory)

    (let ((can-ping-github (gitignore--can-ping-github))
          ;; We use a heuristic here - if the gitignores directory has a .git,
          ;; assume it's been cloned from GitHub. Otherwise, assume not.
          (need-to-clone-repo (not (file-directory-p ".git"))))
      (if need-to-clone-repo
          (progn
            ;; If we can't clone the repo, throw an error (because we won't be
            ;; able to access any templates.)
            ;;
            ;; Use the quick check first - ensure internet is connected and
            ;; GitHub is up.
            (unless can-ping-github
              (error (concat "Could not ping GitHub to download .gitignore "
                             "repo. Is the internet connected?")))
            ;; Now try to clone. If the process exits abnormally, the exit code
            ;; will not be 0 - then we throw an error.
            (unless (eq 0 (gitignore--stream-process-output
                           (list "git"
                                 "clone"
                                 gitignore--github-repo-address
                                 gitignore--local-templates-directory)))
              (error (concat "Could not clone the .gitignore templates repo. "
                             "See the \"*messages*\" buffer for details."))))
        ;; If the template repo already exists, try to update the contents of
        ;; the repo from GitHub.
        ;;
        ;; (Doing this every time we want to generate a template is expensive,
        ;;  but generating templates is rare so it doesn't matter.)
        (unless (and can-ping-github
                     (eq 0 (gitignore--stream-process-output (list "git" "pull"))))
          ;; Note that we only throw an error when we fail to _clone_ the repo
          ;; (not when we fail to update it). If updating fails, we still have a
          ;; local version of the repo. So we use that, since it's less
          ;; intrusive to the user.
          (display-warning
           "gitignore.el"
           (concat "Could not update .gitignore templates from the "
                   "GitHub repo. Using existing local copy.")))))))


;; Ask for a gitignore after initiating a new git project in magit.
(with-eval-after-load 'magit
  (advice-add 'magit-init :after 'gitignore-do-you-want-a-template))


(provide 'gitignore)
;;; gitignore.el ends here
