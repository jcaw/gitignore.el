


;; TODO: set require (Emacs 24.4 because of with-eval-after-load?)


(defvar my--github-gitignores-directory
  (concat spacemacs-cache-directory "github-gitignores/")
  "Directory to clone the boilerplate .gitignores from github.")


(defvar my--github-gitignores-remote-repo
  "https://github.com/github/gitignore"
  "Remote repository for boilerplate .gitignores, on github.")


(defvar my--github-hostname
  "www.github.com"
  "Hostname to use to ping GitHub.")


(defvar my--gitignore-timeout 2
  "Time to wait for when pinging GitHub.

Might have to bump this up for extremely slow internet
connections.")


;;;###autoload
(defun my-would-you-like-a-gitignore ()
  (interactive)
  (when (y-or-n-p "Would you like to insert a .gitignore template? ")
    (my-add-gitignore)))


;;;###autoload
(defun my-add-gitignore ()
  (interactive)
  ;; Download most up-to-date templates
  (my--update-github-gitignores)
  ;; Now choose a .gitignore and copy it in.
  (let* ((chosen-gitignore-path (my--prompt-for-gitignore-template))
         (root-dir (or (vc-root-dir)
                       (read-directory-name "Specify root dir: ")))
         (gitignore-path (expand-file-name ".gitignore" root-dir)))
    (if (file-exists-p gitignore-path)
        (when (y-or-n-p ".gitignore already exists. Append to it? ")
          (my--append-to-gitignore chosen-gitignore-path gitignore-path))
      (copy-file chosen-gitignore-path gitignore-path))))


(defun my--get-gitignore-templates ()
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
          (directory-files-recursively my--github-gitignores-directory "\.gitignore$")))


(defun my--prompt-for-gitignore-template ()
  "Get the user to choose a template. Return its path."
  (let* ((templates (my--get-gitignore-templates))
         (chosen-key (completing-read
                      "Choose a .gitignore template: "
                      templates nil t)))
    ;; `completing-read' will only return the key. Use that to extract the
    ;; value.
    ;;
    ;; NOTE: For some reason, alist-get didn't work here. Use this manual method
    ;; instead.
    (cdr (assoc chosen-key templates))))


(defun my--read-lines (file-path)
  "Return a list of lines of a file at `FILE-PATH'."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" nil)))


(defun my--read-and-strip-gitignore (gitignore-path)
  "Read a .gitignore template, stripping lines we don't want.

Basically, strips file headers like hashbangs and mode
declarations."
  (let* ((lines (my--read-lines gitignore-path))
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


(defun my--append-to-gitignore (chosen-gitignore-path my-gitignore-path)
  "Append the contents of another gitignore to this gitignore."
  (let* ((new-ignores (my--read-and-strip-gitignore
                       chosen-gitignore-path))
         (string-to-insert
          ;; We want double blank lines before inserting.
          (concat "\n\n" (string-trim-left new-ignores))))
    ;; Append the new patterns but also make sure to visit the file to verify.
    (find-file my-gitignore-path)
    (goto-char (point-max))
    (insert string-to-insert)
    (save-buffer)))


(defun my--stream-process-output (command-list)
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


(defun my--can-ping-host (host)
  "Can we ping a host?

Uses a timeout of 2 seconds."
  ;; Might take a while and the output will be messaged, so notify the user
  ;; what's happening.
  (message "Testing connection to `%s'..." host)
  (eq 0 (if (eq system-type 'windows-nt)
            ;; Have to use a different method on Windows ("-n" for number of
            ;; tries instead of "-c".)
            ;; -n : number of tries (want just 1)
            ;; -w : timeout in seconds
            (shell-command (format "ping -n 1 -w 2 %s"
                                   my--gitignore-timeout
                                   host))
          ;; This format tested and works on Linux. Untested on Mac, but should
          ;; work the same way.
          ;;
          ;; Force a timeout external to ping because a failure in the DNS
          ;; lookup might cause ping to hang, even with a timeout argument. This
          ;; doesn't occur on Windows.
          ;; Ping options:
          ;; -c : number of tries (want just 1)
          ;; -W : timeout in seconds
          (shell-command (format "timeout %s ping -c 1 -W %s %s"
                                 my--gitignore-timeout
                                 my--gitignore-timeout
                                 host)))))


(defun my--can-ping-github ()
  (my--can-ping-host my--github-hostname))


(defun my--update-github-gitignores ()
  "Update the folder holding the .gitignores templates (from GitHub).

This will throw an error iff no copy of the folder can be
*cloned* from GitHub. If an old version exists, it will default
to using that."
  ;; We need the template directory to exist so we can work within it.
  (unless (file-directory-p my--github-gitignores-directory)
    (mkdir my--github-gitignores-directory))

  ;; Temporarily change the working directory.
  (with-temp-buffer
    (cd my--github-gitignores-directory)

    (let ((can-ping-github (my--can-ping-github))
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
            (unless (eq 0 (my--stream-process-output
                           (list "git"
                                 "clone"
                                 my--github-gitignores-remote-repo
                                 my--github-gitignores-directory)))
              (error (concat "Could not clone the .gitignore templates repo. "
                             "See the \"*messages*\" buffer for details."))))
        ;; If the template repo already exists, try to update the contents of
        ;; the repo from GitHub.
        ;;
        ;; (Doing this every time we want to generate a template is expensive,
        ;;  but generating templates is rare so it doesn't matter.)
        (unless (and can-ping-github
                     (eq 0 (my--stream-process-output (list "git" "pull"))))
          ;; Note that we only throw an error when we fail to _clone_ the repo
          ;; (not when we fail to update it). If updating fails, we still have a
          ;; local version of the repo. So we use that, since it's less
          ;; intrusive to the user.
          (display-warning
           "auto-add-gitignore"
           (concat "Could not update .gitignore templates from the "
                   "GitHub repo. Using existing local copy.")))))))


(defun my--would-you-like-a-gitignore-proxy (&rest -)
  "Non-interactive proxy for `my-would-you-like-a-gitignore'.

This method is intended to be used as advice. Necessary so advice
doesn't interfere with `magit-init'."
  (my-would-you-like-a-gitignore))


;; Ask for a gitignore after initiating a new git project in magit.
(with-eval-after-load 'magit
  ;; Trying to use the interactive `my-would-you-like-a-gitignore' interferes
  ;; with `magit-init', causing it to throw a cryptic error when invoked. Use a
  ;; non-interactive proxy function to get around that.
  (advice-add 'magit-init :after 'my--would-you-like-a-gitignore-proxy))


(provide 'auto-add-gitignore)
;;; auto-add-gitignore.el ends here
