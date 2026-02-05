#lang racket/base

(provide (struct-out val-stx-pair))

(struct val-stx-pair (val stx))