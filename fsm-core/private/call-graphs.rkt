(module call-graphs racket
  (require "../../fsm-gviz/private/callgraphs/callgraphs-ndfa.rkt"
           "../../fsm-gviz/private/callgraphs/callgraphs-pda.rkt"
           "../../fsm-gviz/private/callgraphs/callgraphs-tm.rkt"
           fsm)

  (provide sm-callgraph)

  (define DEFAULT-THRESH 8)

  ;; machine word [pos natnum] [threshold natnum] [cb symbol] --> image
  ;; Purpose: Return the call graph for the given machine and word
  ;;          For pda and tm, if provided, use given threshold to cut off computation length
  ;;          For tm, if provided, use given pos as initial head position
  ;;          Use, if provided, cb for the color blind palette
  (define (sm-callgraph M w #:pos[pos 0] #:threshold[threshold DEFAULT-THRESH] #:cb[cb 'default])
    (cond [(or (eq? (sm-type M) 'dfa)
               (eq? (sm-type M) 'ndfa))
           (computation-diagram-fsa M w)]
          [(eq? (sm-type M) 'pda) (computation-diagram-pda M w threshold)]
          [(or (eq? (sm-type M) 'tm)
               (eq? (sm-type M) 'tm-language-recognizer))
           (computation-diagram-tm M w pos threshold cb)]
          [(eq? (sm-type M) 'mttm) (error "Call graphs for mttms are coming soon!")]
          [else (error "Unknown machine type given to sm-callgraph")]))

  

  )