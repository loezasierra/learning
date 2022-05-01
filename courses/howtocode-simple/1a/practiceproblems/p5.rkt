#lang racket

(> (image-height IMAGE1) (image-height IMAGE2))

(> (image-width IMAGE1) (image-width IMAGE2))

(and (= (image-height IMAGE1) (image-height IMAGE2))
     (= (image-width IMAGE1) (image-width IMAGE2)))