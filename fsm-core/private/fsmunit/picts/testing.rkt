#lang racket/base
(require hyperlink-snip
         racket/class)
(new hyperlink-snip%
     [text-str "Please click here to contact the FSM developers on Github"]
     [url-str "https://github.com/morazanm/fsm"])