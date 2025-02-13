#lang racket/gui

;; ---------------------------------------------------------------------------------------------------
;; provides the universe functionality (distributed worlds)

(provide 
 universe%
 ;; --- sample worlds and function on worlds ---
 iworld?  ;; Any -> Boolean 
 iworld=? ;; World World -> Boolean 
 iworld-name ;; World -> Symbol 
 iworld1  ;; sample worlds 
 iworld2
 iworld3
 ;; --- sending 'mail' to worlds ---
 ;; type Bundle = (make-bundle [Listof World] Universe [Listof Mail]) 
 ;; type Mail = (make-mail World S-expression)
 make-bundle ;; [Listof World] Universe [Listof Mail] -> Bundle 
 bundle?     ;; is this a bundle? 
 make-mail   ;; World S-expression -> Mail 
 mail?       ;; is this a real mail? 
 )

(require "checked-cell.rkt"
         "check-aux.rkt"
         "timer.rkt"    
         "last.rkt"
         "clauses-spec-aux.rkt"
         "stop.rkt"
         "logging-gui.rkt"
         htdp/error
         (only-in mzlib/etc evcase)
         string-constants)

;                                                          
;                                                          
;                                                          
;   ;   ;           ;                                      
;   ;   ;           ;                                      
;   ;   ;                                                  
;   ;   ;  ;;;;     ;    ;   ;   ;;;   ; ;;    ;;;    ;;;  
;   ;   ;  ;   ;    ;    ;   ;  ;   ;  ;;  ;  ;   ;  ;   ; 
;   ;   ;  ;   ;    ;     ; ;   ;;;;;  ;   ;   ;;;   ;;;;; 
;   ;   ;  ;   ;    ;     ; ;   ;      ;          ;  ;     
;   ;   ;  ;   ;    ;      ;    ;   ;  ;      ;   ;  ;   ; 
;    ;;;   ;   ;    ;      ;     ;;;   ;       ;;;    ;;;  
;                                                          
;                                                          
;                                                          

(define universe%
  (last-mixin
   (clock-mixin
    (class* object% (start-stop<%>) 
      (inspect #f) 
      (super-new)
      (init-field        ;; type Result
       ; = (make-bundle [Listof World] Universe [Listof Mail])
       universe0         ;; the initial state of the universe
       on-new            ;; Universe World -> Result
       on-msg            ;; Universe World Message -> Result
       port              ;; Number
       ;; tick              ;; Universe -> Result
       (state #f)        ;; Boolean 
       (on-disconnect    ;; Universe World -> Result
        (lambda (u w) (make-bundle u '() '())))
       (to-string #f)    ;; Universe -> String 
       (check-with True) ;; Any -> Boolean 
       )
      
      (field 
       [universe 
         (new checked-cell%
              [value0 universe0]
              [ok? check-with] 
              [display (if (string? state) 
                           (and (not (string=? "OliverFlatt" state)) state)
                           (and state "your server's state"))])])
 
      ;; -----------------------------------------------------------------------
      ;; dealing with events
      (define-syntax-rule 
        ;; A B ... -> Void
        (def/cback pub (pname a ...) 
          ;; Universe A B ... -> (cons Universe Mail)
          ;; effect: change server state, broadcast mails 
          name body ...)
        (begin
          (pub pname)
          (define (pname a ...) 
            (define (handler e) (stop! e))
            (with-handlers ([exn? handler])
              (define ___  (begin 'dummy body ...))
              (define n (if (object-name name) (object-name name) name))
              (define nxt (name (send universe get) a ...))
              (cond
                [(stop-the-world? nxt) (stop! (stop-the-world-world nxt))]
                [(bundle? nxt) 
                 (define-values (u mails bad) (bundle> n nxt))
                 (send universe set (format "value returned from ~a" 'name) u)
                 (unless (boolean? to-string) (send gui log (to-string u)))
                 (broadcast mails)
                 (for-each (lambda (iw) (kill iw "disconnected ~a")) bad)]
                [else ;; plain universe state 
                 (send universe set (format "value returned from ~a" 'name) nxt)
                 (unless (boolean? to-string) (send gui log (to-string nxt)))])))))
      
      ;; [Listof Mail] -> Void
      ;; send payload of messages to designated worlds 
      (define/private (broadcast lm)
        (for-each (lambda (p+m) 
                    (define w (mail-to p+m))
                    (define p (mail-content p+m))
                    (define n (iworld-name w))
                    (if (memq w iworlds)
                        (with-handlers ((exn:fail? (lambda (e) (kill w "broadcast failed to ~a"))))
                          (send gui log "universe --> ~a:\n~a\n" n p)
                          (iworld-send w p))
                        (send gui log "~s not on list" n)))
                  lm))
      
      (def/cback private (pnew iworld) on-new
        (set! iworlds (cons iworld iworlds))
        (send gui log "~a signed up" (iworld-name iworld)))
      
      (def/cback private (pmsg iworld r) on-msg
        (send gui log "~a --> universe:\n~a\n" (iworld-name iworld) r))
      
      (def/cback private (pdisconnect iworld) on-disconnect
        (kill iworld "~a !! closed port"))
      
      ;; tick, tock : deal with a tick event for this world 
      (def/cback pubment (ptock) (let ([on-tick (lambda (w) (pptock w))]) on-tick))
      (define/public (pptock w) (void))
      (define/public (name-of-tick-handler)
        "the on-tick-handler")
      
      ;; IWorld FormatString -> Void 
      ;; effect: remove from given iworld from iworlds 
      ;;         and shut down all connections 
      (define/private (kill w msg)
        (iworld-close w)
        (set! iworlds (remq w iworlds))
        (send gui log msg (iworld-name w))
        (when (null? iworlds) (restart)))
      
      ;; -----------------------------------------------------------------------
      ;; start and stop server, start and stop the universe
      
      (field [iworlds '()] ;; [Listof World]
             [gui
              (if (and (string? state) (string=? "OliverFlatt" state))
                  (new dummy-gui%)
                  (new gui%
                       [stop-server (lambda () (stop! (send universe get)))]
                       [restart     (lambda () (restart))]))]
             [dr:custodian  (current-custodian)]
             [the-custodian (make-custodian)])
      
      ;; start the universe, enable registrations and message exchanges
      (define/public (start!)
        (set! the-custodian (make-custodian))
        (parameterize ([current-custodian the-custodian])
          (define (loop)
            (apply sync 
                   (handle-evt (tcp-accept-evt tcp-listener) add-iworld)
                   (map (lambda (p) (handle-evt (iworld-in p) (process-message p))) iworlds))
            (loop))
          ;;; WHERE
          (define listener-msg
            "the universe could not be created (possibly because another universe is running)")
          (define tcp-listener 
            (with-handlers ((exn:fail:network? (lambda (x) (stop! x) (tp-error 'start listener-msg))))
              (tcp-listen port 4 #t)))
          ;; [list IPort OPort] -> Void 
          (define (add-iworld in-out)
            (define in (first in-out))
            (define out (second in-out))
            ;; is it possible to kill the server with lots of bad connections?
            (with-handlers ((tcp-eof? (lambda _ (void)))
                            (exn? (lambda (e) (printf "process registration failed!\n"))))
              (tcp-process-registration in out (lambda (info) (pnew (create-iworld in out info))))))
          ;; IWorld -> [IPort -> Void]
          (define (process-message p)
            (lambda (in)
              (with-handlers ((tcp-eof? (lambda (e) (pdisconnect p))))
                (pmsg p (tcp-receive in)))))
          ;; --- go universe go ---
          (set! iworlds '())
          (send gui show #t)
          (send universe set "initial expression" universe0)
          (send gui log "a new universe is up and running")
          (thread loop)))
      
      (define/private (restart)
        ;; I am running in a custodian that is about to be killed, 
        ;; so let's switch to one up in the hierarchy
        [define old-thread (current-thread)]
        [define all-done?  (make-semaphore)]
        (parameterize ([current-custodian dr:custodian])
          (thread
           (lambda ()
             (sync old-thread all-done?)
             (start!))))
        (send gui log "stopping the universe")
        (send gui log "----------------------------------")
        (for-each iworld-close iworlds)
        (custodian-shutdown-all the-custodian)
        (semaphore-post all-done?))
      
      (define/public (stop! msg) 
        (send gui show #f)
        (custodian-shutdown-all the-custodian))
      
      ;; -----------------------------------------------------------------------
      ;; initialize the universe and run 
      (start!)))))


;                                            
;                                            
;                                            
;   ;   ;                  ;        ;        
;   ;   ;                  ;        ;        
;   ;   ;                  ;        ;        
;   ;   ;   ;;;   ; ;;     ;     ;;;;   ;;;  
;   ;   ;  ;   ;  ;;  ;    ;    ;   ;  ;   ; 
;   ; ; ;  ;   ;  ;   ;    ;    ;   ;   ;;;  
;   ;; ;;  ;   ;  ;        ;    ;   ;      ; 
;   ;   ;  ;   ;  ;        ;    ;   ;  ;   ; 
;   ;   ;   ;;;   ;        ;;    ;;;;   ;;;  
;                                            
;                                            
;                                            

;; --- the server representation of a world --- 
(define-struct iworld (in out name info) #; #:transparent)
;; World = (make-iworld IPort OPort Symbol [Listof Sexp])

(define (iworld=? u v)
  (check-arg 'iworld=? (iworld? u) 'iworld "first" u)
  (check-arg 'iworld=? (iworld? v) 'iworld "second" v)
  (eq? u v))

(define (iw* n) (make-iworld (current-input-port) (current-output-port) n '()))
(define iworld1 (iw* "iworld1"))
(define iworld2 (iw* "iworld2"))
(define iworld3 (iw* "iworld3"))

;; IPort OPort Sexp -> IWorld 
(define (create-iworld i o info)
  (make-iworld i o info "info field not available"))

;; IWorld -> Void
(define (iworld-close p)
  (with-handlers ([exn:fail? void])
    (close-output-port (iworld-out p))
    (close-input-port (iworld-in p))))

;; Player S-exp -> Void
(define (iworld-send p sexp)
  (tcp-send (iworld-out p) sexp))


;                          
;                          
;                          
;     ;;;;  ;       ;;;;;  
;    ;    ; ;         ;    
;   ;       ;         ;    
;   ;       ;         ;    
;   ;    ;; ;         ;    
;   ;     ; ;         ;    
;   ;     ; ;         ;    
;    ;    ; ;;   ;    ;    
;     ;;;;   ;;;;;  ;;;;;  
;                          
;                          
;                          
;                          


;; EFFECT: create and show a gui with two buttons 
(define gui%
  (class logging-gui%
    (init stop-server restart)
    (inherit show)
    (super-new)
    (define cc '(center center))
    (field
     [end   (lambda _ (show #f) (stop-server))]
     [panel (new horizontal-panel% [parent this] [stretchable-height #f] [alignment cc])]
     [stop  (new button% [parent panel] [label "stop"] [callback end])]
     [s&re  (new button% [parent panel] [label "stop and restart"] [callback (λ (_b _e) (restart))])])
    (define/augment (on-close) (end))))

;                              
;                              
;  ;;; ;;;          ;     ;;   
;   ;; ;;                  ;   
;   ;; ;;   ;;;   ;;;      ;   
;   ; ; ;  ;   ;    ;      ;   
;   ; ; ;   ;;;;    ;      ;   
;   ;   ;  ;   ;    ;      ;   
;   ;   ;  ;   ;    ;      ;   
;  ;;; ;;;  ;;;;; ;;;;;  ;;;;; 
;                              
;                              
;                              
;                              

(define-struct bundle (state mails bad) #:transparent)

(set! make-bundle
      (let ([make-bundle make-bundle])
        (lambda (state mails bads)
          (check-arg-list 'make-bundle mails mail? "mail" "second")
          (check-arg-list 'make-bundle bads iworld? "iworld" "third")
          (make-bundle state mails bads))))

;; Symbol Any (Any -> Boolean) String String -> Void 
;; raise a TP exception if low is not a list of world? elements 
(define (check-arg-list tag low pred? msg ith)
  (check-arg tag (list? low) (format "list [of ~as]" msg) ith low)
  (for ((c (in-list low)))
    (unless (pred? c)
      (error 
       tag
       "expects a list of ~as as ~a argument, given a list that contains: ~e"
       msg
       ith
       c)))
  )

;; Symbol Any ->* Universe [Listof Mail] [Listof IWorld]
(define (bundle> tag r)
  (unless (bundle? r) 
    (tp-error tag "expected the ~a function to return a bundle, but it returned ~e" tag r))
  (values (bundle-state r) (bundle-mails r) (bundle-bad r)))

(define-struct mail (to content) #:transparent)

(set! make-mail
      (let ([make-mail make-mail])
        (lambda (to content)
          (check-arg 'make-mail (iworld? to) 'iworld "first" to)
          (check-arg 'make-mail (sexp? content) 'S-expression "second" content)
          (make-mail to content))))
