(setq package-enable-at-startup nil)

;; HACK Work around native compilation on macOS failing with 'ld: library not
;; found for -lemutls_w'.
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/554
;; (setenv "LIBRARY_PATH"
;; 	(string-join
;; 	 '("/opt/homebrew/opt/gcc/lib/gcc/current"
;; 	   "/opt/homebrew/opt/libgccjit/lib/gcc/current"
;; 	   "/opt/homebrew/opt/gcc/lib/gcc/current/gcc/"
;; 	   "/opt/homebrew/lib/gcc/current/gcc/aarch64-apple-darwin24/14/"
;; 	   "/opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin24/14/"
;; 	 ":")))
