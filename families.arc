										; "Families" card game
										;
										; Martijn Wijns, 2009
										;

										; The data containing the game state is structured as follows:
										;
										; *game* - (stack lines)
										; stack  - (card . card ... )
										; lines  - (line . line ... )
										; line   - (card . card ... )


(def init ()
										; Initially shuffle two decks onto the stack,
										; the lines are empty.
										; Then put a card closed on each line, followed
										; by an open card, a closed card and yet another
										; open card
	 (draw-open
	  (draw-closed
	   (draw-open
		(draw-closed
		 (deal 2 12))))))

(def deal (decks nroflines)
	 (list 
	  (shuffle:dup-list-content (generate-deck) decks) 
	  (dup-list nil nroflines)))

(def get-lines (game)
	 (cadr game))

(def get-deck (game)
	 (car game))

;; DRAWING CARDS

(def draw-player (game)
	 (draw [set-state _ 'open] draw-test game))

										; Draw new cards and open them (both for game initialization and throughout the game)
(def draw-open (game)
	 (draw [set-state _ 'open] distr-test game))

										; Draw new cards and keep them closed (for game initialization)
(def draw-closed (game)
	 (draw [set-state _ 'closed] distr-test game))

										; Draw at most the number of lines cards from the deck and distribute them 
										; over the lines.
(def draw (action test game)
	 (let deck (get-deck game)
	   (let lines (get-lines game)
		 (let n (min (len deck) (len lines))
		   (let d (distr (map1 action (firstn n deck)) lines test) ; when drawing, use a different test
			 (list (join (nthcdr n deck) (map1 [set-state _ 'closed] (car d))) (cadr d))))))) ; cards need to be close before being return to the deck

										; Distribute elements in seqs over lists in seqs
										; Returns a tuple (elts, seqs) where elts are the remaining 
										; elements that could not be distributed and seqs are the
										; new sequences
										; @test is a test function that is run on the element before it is inserted
										; it should be a function accepting 2 parameters: the element to insert and the sequence
										; to which the element will be added
(def distr (elts seqs test)
	 (if (or (no elts) (no seqs))
		 (list elts seqs)
		 (let testresult (test (car elts) (car seqs))
		   (let telt (if testresult (cdr elts) elts) ; tail elements
			 (let tail (distr telt (cdr seqs) test)
			   (list 
				(car tail) 
				(if testresult 
					(cons (cons (car elts) (car seqs)) (cadr tail))
					(cons (car seqs) (cadr tail)))))))))

(def draw-test (elt line)
	 (or (no line)
		 (~and (is-open:car line) (ace-in-sequence line))))

										; Detect ace in the open part of a line, although it's called ace-in-sequence
										; it doesn't have to be a proper sequence
(def ace-in-sequence (seq)
	 (and (~no seq) ; sequence empty -> no ace in sequence, return false at once
		  (let c (car seq)
			(and (is-open c) ; closed card -> no ace in sequence, return false at once
				 (or (is 14 (get-value c)) (ace-in-sequence:cdr seq))))))

(def distr-test (elt line)
	 (if (no line) (or (is 'closed (get-state elt)) (is 14 (get-value elt))) ; no cards on line, only closed element or open ace
		 (or (is 'closed (get-state:car line))                               ; upper card is closed
			 (is 'closed (get-state elt))                                    ; card from deck is closed
			 (cards-fit elt (car line)))))                                   ; upper card is open and matches

(def cards-fit (upper lower)
	 (and (value-fits upper lower)
		  (color-fits upper lower)))

(def value-fits (upper lower)
	 (is (get-value upper) (- (get-value lower) 1)))

(def color-fits (upper lower)
	 (~is (get-color upper) (get-color lower)))

(def remove-sequence (from game)
	 (let seq (firstn 13 (get-line-n from game))
	   (if (and (is 13 (len seq)) (is-proper-sequence seq))
		   (remove-cards seq from game))))

;; MOVING (STACKS OF) CARDS AROUND

										; Move @nrofcards from the @from line to the @to line of the game, 
										; but only if this is allowed
(def move (nrofcards from to game)
	 (if (~allowed-to-take nrofcards from game)
		 (and (prn "you are not allowed to take those cards") game) ; return the old game state
		 (let newstate (take-cards nrofcards from game) 
		   (let hand (car newstate) ; take the cards in the hand
			 (let newgame (cadr newstate)
			   (if (~allowed-to-place hand to newgame)
				   (and (prn "you are not allowed to place those cards") game) ; return the old game state
				   (place-cards hand to newgame))))))) ; place the cards on the designated line

										; Check if it is allowed to take @nrofcards cards from the @from line of the game
										; Allowed iff all nrofcards cards on the from line are a proper sequence
(def allowed-to-take (nrofcards from game)
	 (is-proper-sequence:peek-cards nrofcards from game))

										; Check if it is allowed to place @cards on the @to line of the game
										; Allowed iff the bottom of the sequence matches the top card of the @to line of the game
(def allowed-to-place (cards to game)
	 (distr-test (last cards) (get-line-n to game))) ; Re-use distr-test

										; Take @nrofcards from the @from line of the game, returns the hand and the new game state
(def take-cards (nrofcards from game)
	 (let hand (peek-cards nrofcards from game)
	   (list hand (remove-cards hand from game))))

(def remove-cards (cards from game)
	 (list (get-deck game) (remove-cards-from-lines cards from (get-lines game))))

(def remove-cards-from-lines (cards from lines)
	 (join
	  (firstn from lines)     ; leave the first n-1 lines alone
	  (list (rem-seq cards (nth from lines)))  ; remove the card from the nth line
	  (nthcdr (+ from 1) lines)))         ; leave the last lines alone

										; Place @cards on the @to line of the game, returns the new game state
(def place-cards (cards to game)
	 (list (get-deck game) (place-cards-on-lines cards to (get-lines game))))

(def place-cards-on-lines (cards to lines)
	 (join
	  (firstn to lines) ; leave the first n-1 lines alone
	  (list (join cards (nth to lines))) ; add cards to the nth line
	  (nthcdr (+ to 1) lines))) ; leave the last lines alone

										; Check whether the sequence of cards is proper, it is iff:
										; - Each card is open
										; - Each card lies on top of a card with a different color and a 1 higher value
(def is-proper-sequence (cards)
	 (and (all [is-open _] cards)
		  (all [neighbours-fit _] (neighbours cards))))

(def neighbours-fit (p)
	 (cards-fit (car p) (cadr p)))

										; Generate a list of neighbours (pair of neighbouring elements)
(def neighbours (xs)
	 (if (< (len xs) 2)
		 nil
		 (cons (list (car xs) (cadr xs)) (neighbours (cdr xs)))))

										; Retrieve the first @nrofcards nr of cards from the @from line
(def peek-cards (nrofcards from game)
	 (firstn nrofcards (get-line-n from game)))

										; Retrieve the @from line of the game
(def get-line-n (from game)
	 (nth from (get-lines game)))

										; Retrieve the nth element of a sequence
(def nth (n xs)
	 (if (is n 0) (car xs) (nth (- n 1) (cdr xs))))

;; GENERAL CARD ACTIONS

(def open-card (line game)
	 (list (get-deck game) (open-card-in-line line (get-lines game))))

(def open-card-in-line (line lines)
	 (join
	  (firstn line lines) ; leave the first n-1 lines alone
	  (list (open-first-card (nth line lines))) ; add cards to the nth line
	  (nthcdr (+ line 1) lines))) ; leave the last lines alone

(def open-first-card (cards)
	 (if (no cards) 
		 nil
		 (cons (set-state (car cards) 'open) (cdr cards))))

										; Randomize a sequence
(def shuffle (seq)
	 (do-shuffle seq nil))

										; Internal implementation of shuffle
(def do-shuffle (from to)
	 (if (no from)
		 to
		 (let r (rand-elt from)
		   (do-shuffle (rem1 r from) (cons r to)))))

										; rem removes all occurrences from the sequence, rem1 only removes 1
(def rem1 (test seq)
	 (let f (testify test)
	   (if (alist seq)
		   ((afn (s)
				 (if (no s)       nil
					 (f (car s))  (cdr s)
					 (cons (car s) (self (cdr s)))))
			seq)
		   (coerce (rem1 test (coerce seq 'cons)) 'string))))

(def rem-seq (seq xs)
	 (if (no seq)
		 xs
		 (rem-seq (cdr seq) (rem1 [iso _ (car seq)] xs)))) 

										; Duplicate the contents of a list @copies times
(def dup-list (l copies)
	 (if (> copies 0)
		 (cons l (dup-list l (- copies 1)))
		 nil))

(def dup-list-content (l copies)
	 (apply + (dup-list l copies)))

(def generate-deck ()
	 (apply + (map1 generate-suit '(hearts clubs diamonds spades))))

(def generate-suit (suit)
	 (map1 (fn (x) (list suit x 'closed)) (range 2 14)))

;; SINGLE CARD OPERATIONS

(def get-suit (card)
	 (car card))

(def get-color (card)
	 (case (get-suit card)
	   clubs 'black
	   spades 'black
	   diamonds 'red
	   hearts 'red))

(def get-value (card)
	 (cadr card))

(def get-state (card)
	 (car:cddr card))

(def is-open (card)
	 (is 'open (get-state card)))

(def set-state (card state)
	 (list (get-suit card) (get-value card) state))

(def open-cards (cards)
	 (map1 (fn (card) (set-state card 'open)) cards))


;; PRETTY PRINTING

(def pr-value (value)
	 (if (some value (range 2 10))
		 value
		 (case value
		   11 "J"
		   12 "Q"
		   13 "K"
		   14 "A")))

(def pr-state (state)
	 state)

(def pr-suit (suit)
	 (case suit
	   hearts "";"(h)"
	   diamonds "";"(d)"
	   spades "";"(s)"
	   clubs ""));"(c)"))

(def pr-card (card)
	 (pr
	  (if (is 'closed (get-state card))
		  ""
		  (string 
		   (pr-suit:get-suit card) 
		   " "
		   (pr-value:get-value card)
										;" "
										;(pr-state:get-state card)
		   ))))

(def pr-deck (deck)
	 (apply +
			"deck:"
			(map1 (fn (x) (+ (pr-card x) ", ")) deck)))

(def pr-line (line)
	 (apply +
			"line: "
			(map1 (fn (x) (+ (pr-card x) ", ")) line)))

(def pr-lines (lines)
	 (apply +
			"lines:\n"
			(map1 (fn (x) (+ (pr-line x) "\n")) lines)))			

(def dump-game-state (game)
	 (prn 
	  (pr-deck:get-deck game)
	  (pr-lines:get-lines game)))

;; TEST-CODE

(def run-tests ()
	 (and (test-distr)
		  (test-distr-test)
		  (test-distr2)
		  (test-sequence)
		  (test-rem-seq)
		  (test-remove-cards-from-lines)
		  (test-place-cards-on-lines)
		  (test-ace-in-sequence)
		  (test-init)))

(def test-distr ()
	 (and 
	  (prn "starting test-distr")
	  (iso '(nil ((a 1 2 3) (b 4 5 6) (c 7 8 9))) (distr '(a b c) '((1 2 3) (4 5 6) (7 8 9)) always-fits))
	  (iso '((d) ((a 1 2 3) (b 4 5 6) (c 7 8 9))) (distr '(a b c d) '((1 2 3) (4 5 6) (7 8 9)) always-fits))
	  (iso '(nil ((a 1 2 3) (b 4 5 6) (7 8 9))) (distr '(a b) '((1 2 3) (4 5 6) (7 8 9)) always-fits))
	  (prn "test-distr succeeded")))

(def always-fits (elt seq)
	 t)

(def test-distr-test ()
	 (and
	  (prn "starting test-distr-test")
	  (distr-test '(spades 2 open) '((hearts 3 open)))
	  (prn "test-distr-test succeeded")))

(def test-distr2 ()
	 (and
	  (prn "starting test-distr2")
	  (iso 
	   '(nil (((spades 3 open) (hearts 4 open)) ((spades 4 open)))) 
	   (distr '((spades 3 open)) '(((hearts 4 open)) ((spades 4 open))) distr-test))
	  (prn "matching card test succeeded")
	  (iso
	   '(nil (((hearts 4 open)) ((diamonds 3 open) (spades 4 open))))
	   (distr '((diamonds 3 open)) '(((hearts 4 open)) ((spades 4 open))) distr-test))
	  (prn "test-distr2 succeeded")))

(def test-sequence ()
	 (and
	  (prn "starting test-sequence")
	  (is-proper-sequence '((clubs 2 open) (diamonds 3 open) (clubs 4 open)))
	  (~is-proper-sequence '((clubs 2 open) (diamonds 3 closed) (clubs 4 open)))
	  (~is-proper-sequence '((clubs 2 open) (diamonds 3 open) (clubs 5 open)))
	  (prn "test-sequence succeeded")))

(def test-rem-seq ()
	 (and
	  (prn "starting test-rem-seq")
	  (iso '(4 5 6) (rem-seq '(1 2 3) '(1 2 3 4 5 6)))
	  (iso '(1 3 5) (rem-seq '(2 4 6) '(1 2 3 4 5 6)))
	  (prn "test-rem-seq succeeded")))

(def test-remove-cards-from-lines ()
	 (and
	  (prn "starting test-remove-cards-from-lines")
	  (iso 
	   '(((clubs 2 open)(hearts 3 open)(clubs 4 open))((hearts 3 open)(clubs 4 open))((clubs 2 open)(hearts 3 open)(clubs 4 open)))
	   (remove-cards-from-lines '((clubs 2 open)) 1 '(((clubs 2 open)(hearts 3 open)(clubs 4 open))((clubs 2 open)(hearts 3 open)(clubs 4 open))((clubs 2 open)(hearts 3 open)(clubs 4 open)))))
	  (prn "test-remove-cards-from-lines succeeded")))

(def test-place-cards-on-lines ()
	 (and
	  (prn "starting test-place-cards-on-lines")
	  (iso
	   '(nil ((clubs 2 open)) nil)
	   (place-cards-on-lines '((clubs 2 open)) 1 '(nil nil nil)))
	  (prn "test-place-cards-on-lines succeeded")))

(def test-ace-in-sequence ()
	 (and
	  (prn "starting test-ace-in-sequence")
	  (~ace-in-sequence '())
	  (~ace-in-sequence '((hearts 13 open)))
	  (~ace-in-sequence '((hearts 14 closed)))
	  (ace-in-sequence '((hearts 14 open)))
	  (ace-in-sequence '((hearts 2 open) (hearts 14 open)))
	  (~ace-in-sequence '((hearts 2 closed) (hearts 14 open)))
	  (prn "test-ace-in-sequence succeeded")))

(def test-open-first-card ()
	 (and
	  (prn "starting test-open-first-card")
	  (iso '() (open-first-card '()))
	  (prn "test-open-first-card succeeded")))

;; test the initial state of the game
(def test-init ()
	 (and
	  (prn "starting test-init")
	  (let game (init)
		(and 
		 (is 56 (len:get-deck game))
		 (prn "game deck length correct")
		 (is 12 (len:get-lines game))
		 (prn "12 lines found")
		 (all (fn (x) (is 4 (len x))) (get-lines game))
		 (prn "each line contains exactly 4 cards")))
	  (prn "test-init succeeded")))



;; APPLICATION SERVER STUFF

;; operations

(defop show req
  (showgame))

(defop move req
  (handle-move req))

(defop new req
  (handle-new req))

(defop draw req
  (aif
   (= *game* (draw-player *game*))
   (showgame)))

(defop remove req
  (handle-remove req))

(defop select req
  (handle-select req))

(defop open req
  (handle-open req))

;; /operations

;; handlers, may be called from form submission or http get request

(def handle-new (req)
	 (aif
	  (= *selection* nil *game* (init)) ; reset state variables (selection must be first, otherwise nil will cause showgame not to be called)
	  (showgame)))

(def handle-move (req)
	 (aif 
	  (= *game* (move (arg-int req "nrofcards") (arg-int req "from") (arg-int req "to") *game*))
	  (showgame)))

(def handle-open (req)
	 (aif
	  (= *game* (open-card (arg-int req "line") *game*))
	  (showgame)))

(def handle-remove (req)
	 (aif
	  (= *game* (remove-sequence (arg-int req "line") *game*))
	  (showgame)))

(def handle-select (req)
	 (let line (arg-int req "line") 
	   (let nrofcards (arg-int req "nrofcards")
		 (list
		  (if (no *selection*)
			  (if (is 13 nrofcards)
				  (= *game* (remove-sequence line *game*)) ; try to remove family
				  (= *selection* (list line nrofcards))) ; set selection
			  (aif
			   (= *game* (move (cadr *selection*) (car *selection*) line *game*))
			   (= *selection* nil)))
		  (showgame)))))

;; /handlers

(def get-html-color (card)
	 (if (is-open card)
		 (case (get-color card)
		   red (color 255 0 0)
		   black (gray 0))
		 (gray 118)))

(mac gamepage body
	 `(whitepage:center (tag (table width 1000 height 600) ,@body)))

										; Operation to show the game
(def showgame ()
	 (gamepage
	  (tag (table width "100%" height "100%"  bgcolor (color 22 179 56))
		   (row
			(display-deck:get-deck *game*)
			(display-lines:get-lines *game*)))))

(def arg-int (req index)
	 (coerce (arg req index) 'int))

(def layout-card (card top)
	 (let c (get-html-color card)
	   (tr 
		(tag (td valign 'top align 'left)  (fontcolor c (pr-card card)))
		(tag (td valign 'top align 'right) (fontcolor c (pr-card card))))
	   (if top
		   (tr
			(tag (td valign 'bottom align 'left) (fontcolor c (pr-card card)))
			(tag (td valign 'bottom align 'right) (fontcolor c (pr-card card)))))))

(def display-card (card top linenr cardnr)
	 (tr:td 
	  (tag (a 
			href
			(if (is -1 linenr)
				"draw" 
				(if (is-open card)
					(+ "select?" "line=" (string linenr) "&" "nrofcards=" (string cardnr))
					(+ "open?line=" (string linenr)))))
		   (tag (table
				 cellpadding 3
				 cellspacing 0 
				 width 70 
				 background (if (is-open card) "front-blank.png" "back-sioux.png")
				 height (if top 107 28))
				(layout-card card top)))))

(def display-sequence (cards linenr)
	 (let cardnr (+ (len cards) 1)
	   (tag (table cellpadding 0 cellspacing 0)
			(map1 (fn (card) (display-card card (is (last cards) card) linenr (= cardnr (- cardnr 1)))) cards))))

(def display-deck (deck)
	 (display-sequence (firstn 8 deck) -1))

(def display-line (line n)
	 (tag (td valign 'top) 
		  (tag (table cellpadding 0 cellspacing 0 width 70) 
			   (row
				(if (no line)
					(tag (a href (+ "select?" "line=" (string n) "&" "nrofcards=0"))
						 (tag (table border 0 cellpadding 0 cellspacing 0 width "100%" height 104 bgcolor (color 2 159 36))
							  (row ""))) ; There needs to be at least some content for it to render properly
					(display-sequence (rev line) n))))))

(def display-lines (lines)
	 (let n -1
	   (tag (table height "100%") (tr (map1 [display-line _ (= n (+ n 1))] lines)))))

(def fsv ()
	 (asv))
