(module sm-getters racket
  (provide sm-getrules
           sm-getstates
           sm-getstart
           sm-getfinals
           sm-getalphabet
           sm-type
           sm-getstackalphabet
           sm-getaccept)

  (define (sm-getaccept M)
    (if (eq? (sm-type M) 'tm-language-recognizer)
        (M '() 0 'get-accept)
        (error "Only language recognizers have a specialized accept state.")))
        
  
  (define (sm-getrules M) 
    (let ((t1 (sm-type M)))
      (cond [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (M null 0 'get-rules)]
            [else (M null 'get-deltas)])))
  
  (define (sm-getstates M) 
    (let ((t1 (sm-type M)))
      (cond [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (M null 0 'get-states)]
            [else (M null 'get-states)])))
  
  (define (sm-getstart M) 
    (let ((t1 (sm-type M)))
      (cond [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (M null 0 'get-start)]
            [else (M null 'get-start)])))
  
  (define (sm-getfinals M) 
    (let ((t1 (sm-type M)))
      (cond [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (M null 0 'get-finals)]
            [else (M null 'get-finals)])))
  
  (define (sm-getalphabet M) 
    (let ((t1 (sm-type M)))
      (cond [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (M null 0 'get-alphabet)]
            [else (M null 'get-sigma)])))
  
  (define (sm-type M) (M 'whatami 0 'whatami))
  
  (define (sm-getstackalphabet M)
    (if (eq? (sm-type M) 'pda)
        (M null 'get-gamma)
        (error (format "A ~s does not have a stack alphabet." (sm-type M)))))
  )