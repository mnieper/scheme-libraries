((scheme-mode
  . ((eval
      . (progn
          (put 'reset 'scheme-indent-function 0)
          (put 'reset-at 'scheme-indent-function 1)
          (put 'shift 'scheme-indent-function 1)
          (put 'shift-at 'scheme-indent-function 2)
	  (put 'with-amb 'scheme-indent-function 1)
	  (put 'sequence-case 'scheme-indent-function 1)
	  (put 'liquid-let 'scheme-indent-function 1)
	  (put 'let-liquid 'scheme-indent-function 1)
          (font-lock-add-keywords
           nil
           '(("(\\(define-liquid\\)\\>[ \t]*(*\\(\\sw+\\)?"
              (1 font-lock-keyword-face)
              (3 font-lock-function-name-face nil t))
	     ("(\\(reset\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(reset-at\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(shift\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(shift-at\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(with-amb\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(let-liquid\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(liquid-let\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(sequence-case\\)\\>" 1 font-lock-keyword-face)
	     )))))))
