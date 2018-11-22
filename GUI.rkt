#lang racket/gui
(require racket/draw net/url)

;MAIN
(struct c (pic value))
(define myFrame (new frame%
                     [label "Guess My Number!"]
                     [width 500]
                     [height 500]
                     [stretchable-width #f]
                     [stretchable-height #f]))

;EQN
(define sumoflist (λ (x)
                    (cond
                      ((empty? x) 0)
                      (#t (+ (first x) (sumoflist (rest x)))))))

;FONTS
(define font1 (make-font #:size 32
                         #:family 'script
                         #:face "Mickey"
                         #:weight 'light))

(define font2 (make-font #:size 16
                         #:family 'roman
                         #:face "Waltograph"
                         #:weight 'bold))

(define font3 (make-font #:size 12
                         #:family 'script
                         #:face "Courier Std"
                         #:weight 'bold))

(define font4 (make-font #:size 16
                         #:family 'script
                         #:face "Harrington"
                         #:weight 'light))

(define font5 (make-font #:size 28
                         #:family 'script
                         #:face "Mickey"
                         #:weight 'light))


;CARDS
(define card0 (c (make-object bitmap% "images/card0.png") 0))
(define card1 (c (make-object bitmap% "images/card1.png") 1))
(define card2 (c (make-object bitmap% "images/card2.png") 2))
(define card4 (c (make-object bitmap% "images/card4.png") 4))
(define card8 (c (make-object bitmap% "images/card8.png") 8))
(define card16 (c (make-object bitmap% "images/card16.png") 16))
(define card32 (c (make-object bitmap% "images/card32.png") 32))
(define scard0 (make-object bitmap% "images/sm_card0.png"))
(define scard1 (make-object bitmap% "images/sm_card1.png"))
(define scard2 (make-object bitmap% "images/sm_card2.png"))
(define scard4 (make-object bitmap% "images/sm_card4.png"))
(define scard8 (make-object bitmap% "images/sm_card8.png"))
(define scard16 (make-object bitmap% "images/sm_card16.png"))
(define scard32 (make-object bitmap% "images/sm_card32.png"))

;LISTS FOR CALC.
(define myList (list ))
(define notList (list ))
(define viewList (list ))

;PARTS OF GUI
(define msg1 (new message%
                  [label "∴*☆ Guess The Number ☆*∴"]
                  [parent myFrame]
                  [font font1]
                  [auto-resize #t]))

(define msg2 (new message%
                  [label "Think of a whole number between 1 and 63 and I will guess it."]
                  [parent myFrame]
                  [font font3]
                  [auto-resize #t]))

(define msg3 (new message%
                  [label "Click the button when you've thought of your number."]
                  [parent myFrame]
                  [font font3]
                  [auto-resize #t]))

(define start (new button%
                   [label "Let's Begin"]
                   [parent myFrame]
                   [callback (λ (o e)
                               (send msg3 show #f)
                               (send start show #f)
                               (send vpan show #t))]
                   [font font2]))

(define vpan (new vertical-panel%
                  [parent myFrame]))

(define hpan (new horizontal-panel%
                  [parent vpan]))

(define msg4 (new message%
                  [label "Now tell me in which of these cards does your number appear."]
                  [parent vpan]
                  [font font3]
                  [auto-resize #t]))

;CARD LIST TO DISPLAY
(define listOfCards (list card1 card2 card4 card8 card16 card32 card0))

;CALC. FUNCTION
(define f1 (λ (x y) (cond
                      ((equal? (length x) 1) (send yes enable #f)
                                             (send no enable #f)
                                             (set! viewList myList)
                                             (cond
                                               ((empty? myList)
                                                (send msganswer set-label
                                                      (string-append
                                                       "Not between 1 and 63...\n"
                                                       "       Try again.")))
                                               (#t (send msganswer set-label
                                                         (number->string
                                                          (sumoflist myList)))))
                                             (send vpan2 show #t))
                      ((equal? (c-pic (first x)) (send msgdisplay get-label))
                       (send msgdisplay set-label (c-pic (second x)))
                       (cons (c-value (first x)) y))
                      (#t (f1 (rest x) y)))))

;MSG TO DISPLAY CARDS
(define msgdisplay (new message%
                        [label (c-pic card1)]
                        [parent vpan]
                        [auto-resize #t]))
;BLANK TO DISPLAY
(define msg5 (new message%
                  [label " "]
                  [parent vpan]
                  [font font3]
                  [auto-resize #t]))

(define f2 (λ () (cond
                   ((equal? (send msgdisplay get-label) (c-pic card0))
                    (send no show #f)
                    (send msg5 set-label "Hmmm... let me think... Ready?")))))

(define hpan2 (new horizontal-panel%
                   [parent vpan]
                   [alignment (list 'center 'center)]))

;YES & NO
(define yes (new button%
                 [label "Yes"]
                 [parent hpan2]
                 [callback (λ (o e) (set! myList (f1 listOfCards myList))
                             (f2))]
                 [font font2]))

(define no (new button%
                [label "No"]
                [parent hpan2]
                [callback (λ (o e) (set! notList (f1 listOfCards notList))
                            (f2))]
                [font font2]))

;ANSWER
(define vpan2 (new vertical-panel%
                   [parent myFrame]
                   [min-height 175]
                   [alignment (list 'center 'center)]))

(define msg6 (new message%
                  [label "The number you were thinking of is:"]
                  [parent vpan2]
                  [font font4]
                  ))

(define msganswer (new message%
                       [label " "]
                       [parent vpan2]
                       [font font5]
                       [auto-resize #t]))

;RESTART
(define restrt (new button%
                    [parent vpan2]
                    [label "Restart"]
                    [callback (λ (o e)
                                (send msg2 show #t)
                                (set! myList '())
                                (send msgdisplay set-label (c-pic card1))
                                (send yes enable #t)
                                (send no enable #t)
                                (send msganswer set-label " ")
                                (send vpan2 show #f)
                                (send msg5 set-label " ")
                                (send no show #t))]
                    [font font2]))

;EXECUTION
(send myFrame show #t)
(send vpan show #f)
(send vpan2 show #f)
(f2)