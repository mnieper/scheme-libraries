((scheme-mode
  . ((eval
      . (progn
	  (put 'liquid-let 'scheme-indent-function 1)
	  (put 'let-liquid 'scheme-indent-function 1)
          (font-lock-add-keywords
           nil
           '(("(\\(define-liquid\\)\\>[ \t]*(*\\(\\sw+\\)?"
              (1 font-lock-keyword-face)
              (3 font-lock-function-name-face nil t))
	     ("(\\(let-liquid\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(liquid-let\\)\\>" 1 font-lock-keyword-face)
	     )))))))
