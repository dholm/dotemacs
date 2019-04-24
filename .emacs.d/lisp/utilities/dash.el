;;; dash.el --- Dash documentation reading -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dash-docs
  :if (executable-find "sqlite3")
  :bind-wrap
  ((:key :doc :reference) . helm-dash-at-point))


(provide 'utilities/dash)
;;; dash.el ends here
