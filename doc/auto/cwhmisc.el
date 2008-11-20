(TeX-add-style-hook "cwhmisc"
 (lambda ()
    (LaTeX-add-environments
     '("Stable" 3))
    (LaTeX-add-labels
     "#3")
    (TeX-run-style-hooks
     "Sweave"
     "latex2e"
     "scrartcl11"
     "scrartcl"
     "a4paper"
     "11pt"
     "leqno")))

