(module sm-getters racket
  (provide sm-rules
           sm-states
           sm-start
           sm-finals
           sm-sigma
           sm-type
           sm-gamma
           sm-accept
           sm-numtapes)

  (define (sm-accept M)
    (let ((mtype (sm-type M)))
      (cond [(eq? mtype 'tm-language-recognizer) (M '() 0 'get-accept)]
            [(eq? mtype 'mttm-language-recognizer) (M 'get-accept)]
            [else (error "Only language recognizers have a specialized accept state.")])))
        
  
  (define (sm-rules M) 
    (let ((t1 (sm-type M)))
      (cond [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (M null 0 'get-rules)]
            [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer)) (M 'get-rules)]
            [else (M null 'get-deltas)])))
  
  (define (sm-states M) 
    (let ((t1 (sm-type M)))
      (cond [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (M null 0 'get-states)]
            [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer)) (M 'get-states)]
            [else (M null 'get-states)])))
  
  (define (sm-start M) 
    (let ((t1 (sm-type M)))
      (cond [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (M null 0 'get-start)]
            [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer)) (M 'get-start)]
            [else (M null 'get-start)])))
  
  (define (sm-finals M) 
    (let ((t1 (sm-type M)))
      (cond [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (M null 0 'get-finals)]
            [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer)) (M 'get-finals)]
            [else (M null 'get-finals)])))
  
  (define (sm-sigma M) 
    (let ((t1 (sm-type M)))
      (cond [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (M null 0 'get-alphabet)]
            [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer)) (M 'get-sigma)]
            [else (M null 'get-sigma)])))
  
  (define (sm-type M) (M 'whatami 0 'whatami))
  
  (define (sm-gamma M)
    (if (eq? (sm-type M) 'pda)
        (M null 'get-gamma)
        (error (format "A ~s does not have a stack alphabet." (sm-type M)))))

  (define (sm-numtapes M)
    (let ((t1 (sm-type M)))
    (if (or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer))
        (M 'get-numtapes)
        1)))
  )