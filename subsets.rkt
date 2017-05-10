;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname subsets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************
;;   Zihao Wang (20649126)
;;   CS 135 Fall 2016
;;   Assignment 10, Problem 2
;; ***************************************
;;

;; (a)
(define (subsets1 lon)
  (foldr (lambda (x y) (append y (map (lambda (z) (cons x z)) y)))
         (list empty) lon))


;; (b)
(define (subsets2 lon)
  (foldr (lambda (x y) (append y (map (lambda (z) (cons x z)) y)))
         (list empty) lon))