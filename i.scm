(define convert
  (lambda (filename)
    (begin (set-current-input-port! (open-input-file filename))
      (statement (read)))))

(define statement
  (lambda (token)
    (cond ((eq? token 'declare) (declare_statement))
          ((eq? token 'begin) (begin_statement))
          ((eq? token 'for) (for_statement))
          ((eq? token 'if) (if_statement))
          ((eq? token 'set) (set_statement))
          ((eq? token 'end) ())
          ((eq? token '$) ())
          ((eq? token 'then) ())
          ((eq? token 'else) ())
          ((eq? token 'call) (call_expression))
          ((eof-object? 'token) ())
          (else(expression token)))))

(define declare_statement
  (lambda ()
    (let* ((d (declarations))
           (s (statements)))
          (list 'let* d s))))

(define declarations
  (lambda ()
    (let ((next-token (read)))
      (if (eq? next-token 'begin) ()
        (let* ((v next-token)
               (trash (read))
               (expression1 (expression(read))))
              (cons (list v expression1) (declarations)))))))

(define statements
  (lambda ()
    (let ((next-token (read)))
      (if (eq? next-token 'end) ()
              (cons (statement next-token) (statements))))))

(define expression
  (lambda (token)
      (if (eq? token 'call)
          (call_expression)
          token)))

(define begin_statement
  (lambda ()
      (cons 'begin (begin_body))))

(define begin_body
  (lambda ()
    (let ((next-token (read)))
      (cond ((eq? next-token 'end) ())
            ((eof-object? next-token) ())
            (else  (cons (statement next-token) (begin_body)))))))

(define call_expression
  (lambda ()
    (let ((next-token (read)))
      (cond ((eq? next-token '$) ())
      	    ((eof-object? next-token) ())
            (else  (list (statement next-token) (call_expression)))))))


(define for_statement
  (lambda ()
    (let* ((v (read))
          (trash1 (read))
          (expression1 (expression(read)))
          (trash2 (read))
          (expression2 (expression(read)))
          (the_statement (statement(read))))
       (cons 'do (cons (cons v (cons expression1 (cons (list '+ v '1) () ))) (list (list (list '> v expression2) v) (list the_statement)))))))


(define if_statement
  (lambda ()
    (let* ((expression1 (expression(read)))
           (trash1 (read))
           (statement1 (statement(read)))
           (trash2 (read))
           (statement2 (statement(read))))
          (cons 'if (cons expression1 (list statement1 statement2))))))

(define set_statement
  (lambda()
    (let* ((v (read))
           (trash (read))
           (expression1 (expression(read))))
          (list 'set! v expression1))))
