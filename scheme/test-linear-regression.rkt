#lang racket

(require "linear-regression.rkt")


(define tr-data (make-training-data "http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv"))
(training-data-shape tr-data)
;(sleep 1)

(define lr-data-tv (train-lr tr-data "TV" "Sales"))
(define lr-data-newspaper (train-lr tr-data "Newspaper" "Sales"))
(define lr-data-radio (train-lr tr-data "Radio" "Sales"))


(list (plot-lr-model lr-data-tv)
      (plot-lr-model lr-data-newspaper)
      (plot-lr-model lr-data-radio))

(predict lr-data-tv 50)
