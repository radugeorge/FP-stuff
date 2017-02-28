#lang racket

(provide lr-model
         train-lr
         make-training-data
         training-data-shape
         print-lr-model
         plot-lr-model
         predict)
  
(require csv-reading
         plot
         net/url)

(struct training-data (regressors regressand))
(struct lr-model (beta alpha regressor x regressand y))

(define (make-training-data data-source)
  (define (make-regressors data)
    (let ([working-data (take data (- (length data) 1))])
      (foldl (lambda (e acc) (hash-set acc (first e) (rest e))) (hash) working-data)))
  (define (make-regressand data)
    (let ([working-data (last data)])
      (hash (first working-data) (rest working-data))))
  (define (transpose data)
    (apply map list data))
  (let ([working-data (transpose (get-data data-source))])
    (training-data (make-regressors working-data) (make-regressand working-data))))

(define (training-data-shape tr-data)
  (when (training-data? tr-data)
    (begin
      (display "Training data shape:\n")
      (display "\tRegressor ->")
      (for ([i (hash-keys (training-data-regressors tr-data))])
        (display (~a " " i)))
      (display "\n\tRegressand -> ")
      (for ([i (hash-keys (training-data-regressand tr-data))])
        (display (~a " " i)))
      (display "\n"))))

(define (get-data url-string)
  (define (get-csv-data url-string)
    ((compose csv->list get-pure-port string->url) url-string))
  (define (safe-drop coll n)
    (if (or (> n (length coll)) (< n 0))
        coll
        (drop coll n)))
  (define (trim-data data rows columns)
    (map (lambda (coll) (safe-drop coll columns)) (safe-drop data rows)))
  (trim-data (get-csv-data url-string) 0 1))

(define (repeat-action f n)
  (foldl compose (lambda (x) x) (build-list n (const f))))

;linear regression algorithm
(define (train-lr tr-data regressor regressand)
  (define (sum x)
    (foldl + 0 x))
  (define (square x)
    (* x x))
  (let* ([x (map string->number (hash-ref (training-data-regressors tr-data) regressor))]
         [y (map string->number (hash-ref (training-data-regressand tr-data) regressand))]
         [mean-x (/ (sum x) (length x))]
         [mean-y (/ (sum y) (length y))]
         [num (sum (map (lambda (xi yi) (* (- xi mean-x) (- yi mean-y))) x y))]
         [den (sum (map (lambda (xi) (square (- xi mean-x))) x))]
         [beta (/ num den)]
         [alpha (- mean-y (* beta mean-x))])
    (lr-model beta alpha regressor x regressand y)))

(define (print-lr-model lr-data)
  (when (lr-model? lr-data)
    (printf "y = ~ax + ~a" (lr-model-beta lr-data) (lr-model-alpha lr-data))))

(define (plot-lr-model lr-data)
  (define lr-model-eq (~a "y = " (lr-model-beta lr-data) "x +" (lr-model-alpha lr-data)))
  (when (lr-model? lr-data)
    (plot (list (points (map list (lr-model-x lr-data) (lr-model-y lr-data)))
          (function (lambda (x) (+ (* x (lr-model-beta lr-data)) (lr-model-alpha lr-data))) (- (apply min (lr-model-x lr-data)) 10) (+ (apply max (lr-model-x lr-data)) 10)))
          #:title lr-model-eq
          #:x-label (lr-model-regressor lr-data)
          #:y-label (lr-model-regressand lr-data))))

(define (predict lr-data n)
  (when (lr-model? lr-data)
    ((lambda (x) (+ (* x (lr-model-beta lr-data)) (lr-model-alpha lr-data))) n)))


