(setq-default mode-line-percent-position nil)
(setq-default mode-line-position-column-line-format '("(%l,%c)"))

(setq-default mode-line-format
              '("%e"
                (:eval (meow-indicator))
                " "
                mode-line-buffer-identification
                "  "
                mode-line-position

                mode-line-format-right-align
                (project-mode-line project-mode-line-format)
                " "
                mode-line-misc-info
                " "
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified
                  mode-line-remote mode-line-window-dedicated)
                 display (min-width (6.0)))
                mode-line-end-spaces))
