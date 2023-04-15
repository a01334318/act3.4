#lang racket
(require 2htdp/batch-io)

;; Keywords
(define keyword
  '("abstract" "as" "base" "bool" "break" "byte" "case" "catch" "char" "checked" "class" "const" "continue" "decimal" "default" "delegate" "do" "double" "else"
    "enum" "event" "explicit" "extern" "false" "finally" "fixed" "float" "for" "foreach" "goto" "if" "implicit" "in" "int" "interface" "internal" "is" "lock"
    "long" "namespace" "new" "null" "object" "operator" "out" "override" "params" "private" "protected" "public" "readonly" "ref" "return" "sbyte" "sealed" "short"
    "sizeof" "stackalloc" "static" "string" "struct" "switch" "this" "throw" "true" "try" "typeof" "uint" "ulong" "unchecked" "unsafe" "ushort" "using" "virtual"
    "void" "volatile" "while"))

;; Operators
(define operator
  '("+" "-" "*" "/" "%" "&" "|" "^" "!" "~" "=" "<" ">" "+=" "-=" "*=" "/=" "%="
    "&=" "|=" "^=" "<<" ">>" ">>>" "<<=" ">>=" ">>>=" "==" "!=" "<=" ">=" "&&" "||"
    "++" "--" "?" "??"))

;; Separators
(define separator
  '(";" "," "." "(" ")" "[" "]" "{" "}" "<" ">" ":" "::"))

;; Regexp for numbers
(define number
  (regexp "(\\b[0-9]+(\\.[0-9]*)?\\b)"))

;; Regexp for strings
(define string
  (regexp "(\"(\\\\.|[^\\\\\"])*\")"))

;; Regexp for comments
(define comment
  (regexp "(//.*|/\\*.*\\*/|/\\*.*|.*\\*/)"))

;; Regexp for identifiers
(define identifier
  (regexp "[a-zA-Z_][a-zA-Z0-9_]*$"))

;; Replace comments
(define (replace-comment s)
  (string-append "<span class=\"comment\">" s "</span>"))

;; Categorize tokens
(define (categorize-token s)
    (cond
        [(member s keyword) (string-append "<span class=\"keyword\">" s "</span>")]
        [(member s operator) (string-append "<span class=\"operator\">" s "</span>")]
        [(member s separator) (string-append "<span class=\"separator\">" s "</span>")]
        [(regexp-match number s) (string-append "<span class=\"number\">" s "</span>")]
        [(regexp-match string s) (string-append "<span class=\"string\">" s "</span>")]
        [(regexp-match comment s) (string-append "<span class=\"comment\">" s "</span>")]
        [(regexp-match identifier s) (string-append "<span class=\"identifier\">" s "</span>")]
        [else s]
    )
)

;; Replace tokens
(define (replace-all-tokens s open-block-comment)
    (define word '())
    (define list-line '())
    (define open-quotes #f)
    (define possible-line-comment #f)
    (define open-line-comment #f)

    (define ch (regexp-split #px"" s))

     (for/last ([char ch])
      (when (and (eq? char (last ch)) (or open-line-comment open-block-comment))
        (set! list-line (append list-line (list word))))

      (cond 
        [open-block-comment (set! word (append word (list char)))]
        [(regexp-match #rx"#" char) (set! word (append word (list char)))]
        [(regexp-match? #rx"[a-zA-Z0-9_]" char)
         (set! word (append word (list char)))]
        
        [(regexp-match #px"/" char) 
          (cond 
            [possible-line-comment 
              ((lambda () 
                (set! possible-line-comment #f)
                (set! open-line-comment #t)
                (set! word (append word (list char)))))]
            [else 
              ((lambda () 
                (set! possible-line-comment #t)
                (set! word (append word (list char)))))])]

        [open-line-comment (set! word (append word (list char)))]

        [(regexp-match? #px"\"" char)
         ((lambda ()
            (set! open-quotes (not open-quotes))
            (set! word (append word (list char)))))]

        [open-quotes (set! word (append word (list char)))]

        [(member char operator)
         ((lambda ()
            (set! list-line (append list-line (list word)))
            (set! word '())
            (set! word (append word (list char)))
            (set! list-line (append list-line (list word)))
            (set! word '())))]

        [(member char separator)
         ((lambda ()
            (set! list-line (append list-line (list word)))
            (set! word '())
            (set! word (append word (list char)))
            (set! list-line (append list-line (list word)))
            (set! word '())))]

        [else
         ((lambda ()
            (set! list-line (append list-line (list word)))
            (set! word '())))])
    )

    (define tokens (map (lambda (x) (string-join x "")) list-line))
    (define (categorize-tokens tokens)
      (define (loop tokens open-block-comment)
        (cond
          [(null? tokens) '()]
          [(and (string=? (car tokens) "/*") (not open-block-comment))
            (append (list (categorize-token (car tokens))) (loop (cdr tokens) #t))]
          [(and (string=? (car tokens) "*/") open-block-comment)
            (append (list (categorize-token (car tokens))) (loop (cdr tokens) #f))]
          [open-block-comment
            (append (list (replace-comment (car tokens))) (loop (cdr tokens) #t))]
          [(string=? (car tokens) "//")
            (append (list (replace-comment (car tokens))) (loop (cdr tokens) #f))]
          [else
            (append (list (categorize-token (car tokens))) (loop (cdr tokens) #f))]))
      (loop tokens open-block-comment))

    (categorize-tokens tokens)
)

;; Input file
(define input-file "input.cs")

;; Output file
(define output-file "output.html")

;; HTML header
(define html-header
  "<!DOCTYPE html>
<html>
<head>
  <meta chet=\"utf-8\">
  <title>Actividad 3.4</title>
  <style>
  body {
    background-color: #25292E;
    color: #f8f8f2;
    font-family: Consolas, monospace;
    font-size: 18px;
    margin-left: 40px;
  }

  pre {
    margin: 0;
  }

  span {
    display: inline-block;
  }

  span.keyword {
    color: #A020F0;
  }

  span.operator {
    color: #D3747C;
    margin: 0 -7px;
  }

  span.separator {
    color: #f7f486;
    margin: 0 -5px;
  }

  span.identifier {
    color: #cccccc;
  }

  span.number {
    color: #fc6fa5;
  }

  span.string {
    color: #59c6c9;
    margin-right: -5px;
  }

  span.comment {
    color: #75715e;
  }
  </style>
</head>
<body>
  <pre>")

;; HTML footer
(define html-footer
  "</pre>
</body>
</html>")

;; Run
(define (Run input-file output-file)
  (define input-lines (file->lines input-file))
  (define output-port (open-output-file output-file))
  (write-string html-header output-port)

  (define open-block-comment #f)

  (for-each (lambda (s)
              (when (not open-block-comment) 
                  (set! open-block-comment (regexp-match? #px"/\\*" s)))
                  
              (define tokens (replace-all-tokens s open-block-comment))
              (define formatted-line (string-join tokens " "))

              (when open-block-comment
                  (set! open-block-comment (not (regexp-match? #px"\\*/" s))))

               (write-string (string-append "<pre>" formatted-line "</pre>") output-port))
            input-lines)
  
  (write-string html-footer output-port)
  (close-output-port output-port))

(time (Run input-file output-file))