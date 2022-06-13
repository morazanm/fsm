(module sm-getters racket
  (provide sm-getrules
           sm-getstates
           sm-getstart
           sm-getfinals
           sm-getalphabet
           sm-type
           sm-getstackalphabet
           sm-getaccept
           sm-getnumtapes)

  (define (sm-getaccept M)
    (let ((mtype (sm-type M)))
      (cond [(eq? mtype 'tm-language-recognizer) (M '() 0 'get-accept)]
            [(eq? (eq? mtype 'mttm-language-recognizer)) (M 'get-accept)]
            [else (error "Only language recognizers have a specialized accept state.")])))
        
  
  (define (sm-getrules M) 
    (let ((t1 (sm-type M)))
      (cond [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (M null 0 'get-rules)]
            [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer)) (M 'get-rules)]
            [else (M null 'get-deltas)])))
  
  (define (sm-getstates M) 
    (let ((t1 (sm-type M)))
      (cond [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (M null 0 'get-states)]
            [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer)) (M 'get-states)]
            [else (M null 'get-states)])))
  
  (define (sm-getstart M) 
    (let ((t1 (sm-type M)))
      (cond [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (M null 0 'get-start)]
            [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer)) (M 'get-start)]
            [else (M null 'get-start)])))
  
  (define (sm-getfinals M) 
    (let ((t1 (sm-type M)))
      (cond [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (M null 0 'get-finals)]
            [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer)) (M 'get-finals)]
            [else (M null 'get-finals)])))
  
  (define (sm-getalphabet M) 
    (let ((t1 (sm-type M)))
      (cond [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (M null 0 'get-alphabet)]
            [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer)) (M 'get-sigma)]
            [else (M null 'get-sigma)])))
  
  (define (sm-type M) (M 'whatami 0 'whatami))
  
  (define (sm-getstackalphabet M)
    (if (eq? (sm-type M) 'pda)
        (M null 'get-gamma)
        (error (format "A ~s does not have a stack alphabet." (sm-type M)))))

  (define (sm-getnumtapes M)
    (let ((t1 (sm-type M)))
    (if (or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer))
        (M 'get-numtapes)
        1)))
  )