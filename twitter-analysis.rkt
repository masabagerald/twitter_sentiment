#lang racket

(require data-science-master)
(require plot)
(require math)
(require json)
(require srfi/19)
(require racket/stream)


(require net/url 
  net/uri-codec
  web-server/stuffers/hmac-sha1
  net/base64
  rackunit)



;Procedure replaces the word "false" from the raw twitter file
(define (remove-false str)
  (regexp-replace* #px"false" str " "))

;Procedure replaces the word "true" from the raw twitter file
(define (remove-true str)
  (regexp-replace* #px"true" str " "))

;Procedure replaces the word "sensitive" from the raw twitter file
(define (remove-sensitive str)
  (regexp-replace* #px"sensitive" str " "))

;Procedure replaces the word "favorited" from the raw twitter file
(define (remove-favorited str)
  (regexp-replace* #px"favorited" str " "))

;Procedure replaces the word "favorite" from the raw twitter file
(define (remove-favorite str)
  (regexp-replace* #px"favorite" str " "))

;Reads all characters from in and returns them as a string.Reading from a file in this case we are reading from data.json
(define input_data (port->string (open-input-file "datasets/uganda_tweet.json")))


;changing all words to lower case, then removing urls,then remove punctuation makes
;the normalizing the space between words the cleaned data is save as cleanedinutdata and the input is rawiputdata
(define normalized_data (string-normalize-spaces
                         (remove-punctuation
                          (remove-false
                           (remove-true
                            (remove-sensitive
                             (remove-favorite
                              (remove-favorited
                               (remove-urls
                                (string-downcase input_data))))))))))

;;This proceedure outputs quoted strings in the list. The order of the strings is not maintained from input to output
;;Further cleaning is done by removing stop words using the defaults lexicon 

;;Returns a list of pairs. Each pair consists of a unique word/token from str with its frequency.
;;the number of times a word occurs in the tweets selected
(define clean-data (document->tokens normalized_data #:sort? #t))

;;; Using the nrc lexicon, we can label each (non stop-word) with an
;;; emotional label.
(define sentiment (list->sentiment clean-data #:lexicon 'nrc))


;;; sentiment, created above, consists of a list of triplets of the pattern
;;; (token sentiment freq) for each token in the document. Many words will have 
;;; the same sentiment label, so we aggregrate (by summing) across such tokens.
(define sentimize_twitter_data (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq)))
(newline)
sentimize_twitter_data

(newline)
(newline)
(display "SENTIMENTAL ANALYSIS USING DISCRETE-HISTOGRAM")
(newline)
(newline)
;;;We can visualize this result as a barplot (discrete-histogram)
(let ([counts sentimize_twitter_data])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "purple"
	    #:line-color "purple"))
	  #:x-label "Moods"
	  #:y-label "No of tweets")))

;;;Using the bing lexicon to determine the ratio ofpositive-to-negative words
(newline)
(newline)
(display "SENTIMENTAL POLARITY USING BING LEXICON")
(newline)
(newline)
(define sentiment1 (list->sentiment clean-data #:lexicon 'bing))
(define better_histogram (aggregate sum ($ sentiment1 'sentiment) ($ sentiment1 'freq)))
(parameterize ([plot-height 200])
  (plot (discrete-histogram
	 better_histogram
	 #:y-min 0
	 ;#:y-max 2000
	 #:invert? #t
	 #:color "Blue"
	 #:line-color "Blue")
	#:x-label "Frequency"
	#:y-label "Sentiment Polarity"))


;;; We can also look at which words are contributing the most to our
;;; positive and negative sentiment scores. We'll look at the top 20
;;; influential (i.e., most frequent) positive and negative words
(define negative-sentiments
  (take (cdr (subset sentiment1 'sentiment "negative")) 20))
(define positive-sentiments
  (take (cdr (subset sentiment1 'sentiment "positive")) 20))

;;; Some clever reshaping for plotting purposes
(define n (map (λ (x) (list (first x) (- 0 (third x))))
	       negative-sentiments))
(define p (sort (map (λ (x) (list (first x) (third x)))
		     positive-sentiments)
		(λ (x y) (< (second x) (second y)))))

;;; Plot the results
(display "Negative Sentiment vs Positive Sentiment")
(newline)
(newline)
(parameterize ((plot-width 800)
	       (plot-x-tick-label-anchor 'right)
	       (plot-x-tick-label-angle 90))
  (plot (list
	 (tick-grid)
	 (discrete-histogram n #:y-min -500
			     #:y-max 0
			     #:color "Red"
			     #:line-color "Red"
			     #:label "Negative Sentiment") 
	 (discrete-histogram p #:y-min 0
			     #:y-max 500
			     #:x-min 20
			     #:color "blue"
			     #:line-color "blue"
			     #:label "Positive Sentiment"))
	#:x-label "Twiiter words"
	#:y-label "Contribution to sentiment"))

