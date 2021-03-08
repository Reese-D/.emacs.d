(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer nil))

;;; Define a default fullscreen and non full-screen mode, then add a function to toggle between the two
(defun my-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth)       ;this makes the frame go fullscreen
  (tool-bar-mode -1)                                    ;these 3 lines turn off GUI junk
  (menu-bar-mode -1))

(defun my-non-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'width 82)
  (set-frame-parameter nil 'fullscreen 'fullheight)
  (menu-bar-mode t))                                    ;I don't turn tool-bar and scroll-bar back on b/c I never want them


(defun toggle-fullscreen ()
  (interactive)
  (if (eq (frame-parameter nil 'fullscreen) 'fullboth)  ;tests if already fullscreened
      (my-non-fullscreen)
    (my-fullscreen)))

;;copies the buffers current file path
(defun filename ()
  "Copy the full path of the current buffer."
  (interactive)
  (kill-new (buffer-file-name (window-buffer (minibuffer-selected-window)))))
