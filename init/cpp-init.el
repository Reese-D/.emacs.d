(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package lsp-ui :commands lsp-ui-mode)

(setq ccls-executable "/usr/bin/ccls")

