(let ((GRID (make-array '(9 9) :initial-element 0)))


  (defun print-line()
    (format t "~%  -------------------------"))


  ;;affiche une grille de sudoku
  (defun print-grid(grid)
    (format t "    a b c   d e f   g h i")
    (do ((i 0 (1+ i)))
	((= i 9))
      (if (= (mod i 3) 0)
	  (print-line))
      (format t "~%~d | ~d ~d ~d | ~d ~d ~d | ~d ~d ~d |"
	      (1+ i)
	      (aref grid i 0)
	      (aref grid i 1)
	      (aref grid i 2)
	      (aref grid i 3)
	      (aref grid i 4)
	      (aref grid i 5)
	      (aref grid i 6)
	      (aref grid i 7)
	      (aref grid i 8)))
    (print-line))


  ;;initialise une grille (vide pour l'instant) et commence le jeu
  (defun sudoku()
    (empty-grid GRID)
    (print-grid GRID)
    (play))


  ;;commence le jeu
  (defun play()
    (format t "~%choisissez un nombre, une colonne et une ligne (ex 6 a 1): ")
    (let ((nb (read)))      
      (if(eq nb 'n)
	 (sudoku))
      (let ((col (read))
	    (line (read)))
	(cond ((eq col 'n)
	       (sudoku))
	      ((or (not (member col '(a b c d e f g h i)))
		   (not (member (or nb line) '(1 2 3 4 5 6 7 8 9))))
	       (format t "erreur : nombre ~d ou case ~d ~d non valide" nb col line)
	       (play))
	      ((eq col 'a) (setf (aref GRID (1- line) 0) nb))
	      ((eq col 'b) (setf (aref GRID (1- line) 1) nb))
	      ((eq col 'c) (setf (aref GRID (1- line) 2) nb))
	      ((eq col 'd) (setf (aref GRID (1- line) 3) nb))
	      ((eq col 'e) (setf (aref GRID (1- line) 4) nb))
	      ((eq col 'f) (setf (aref GRID (1- line) 5) nb))
	      ((eq col 'g) (setf (aref GRID (1- line) 6) nb))
	      ((eq col 'h) (setf (aref GRID (1- line) 7) nb))
	      ((eq col 'i) (setf (aref GRID (1- line) 8) nb)))))
  (print-grid GRID)
  (play))
  

  ;;vide une grille
  (defun empty-grid(tab)
    (do ((i 0 (1+ i)))
	((= i (car (array-dimensions tab))))
      (empty-line tab i)))


  ;;vide la ligne d'une grille
  (defun empty-line(tab l)
    (do ((i 0 (1+ i)))
	((= i (car (array-dimensions tab))))
      (setf (aref tab l i) 0)))
  

  ;;retourne si n est dans la ligne l de la grille
  (defun in-line(tab l n)
    (let ((res NIL))
      (do ((i 0 (1+ i)))
	  ((= i (car (array-dimensions tab))))
	(if (= (aref tab l i) n)
	    (setf res T)))
      res))

  
  ;;retourne si n est dans la colonne c de la grille
  (defun in-col(tab c n)
    (let ((res NIL))
      (do ((i 0 (1+ i)))
	  ((= i (car (array-dimensions tab))))
	(if (= (aref tab i c) n)
	    (setf res T)))
      res))
  

  ;;retourne si n est dans la zone contenant la position l c d'une grille
  (defun in-zone(tab l c n)
    (let ((res NIL)
  	  (z (zone l c)))
      (do ((i 0 (1+ i)))
	  ((= i 3))
	(do ((j 0 (1+ j)))
	    ((= j 3))
	  (if (= (aref tab
		       (+ i (* (f z) 3))
		       (+ j (* (mod z 3) 3))) n)
	      (setf res T))))
      res))
  

  ;;retourne la zone contenant la case l c
  ;; 
  ;;  -------------------------
  ;;  |       |       |       |
  ;;  |   0   |   1   |   2   |
  ;;  |       |       |       |
  ;;  -------------------------
  ;;  |       |       |       |
  ;;  |   3   |   4   |   5   |
  ;;  |       |       |       |
  ;;  -------------------------
  ;;  |       |       |       |
  ;;  |   6   |   7   |   8   |
  ;;  |       |       |       |
  ;;  -------------------------
  ;;
  (defun zone(l c)
    (let ((z 0))
      (cond ((< l 3)
	     (cond ((< c 3) (setf z 0))
		   ((and (>= c 3) (< c 6)) (setf z 1))
		   ((>= 6) (setf z 2))))
	    ((and (>= l 3) (< l 6))
	     (cond ((< c 3) (setf z 3))
		   ((and (>= c 3) (< c 6)) (setf z 4))
		   ((>= 6) (setf z 5))))
	    ((>= l 6)
	     (cond ((< c 3) (setf z 6))
		   ((and (>= c 3) (< c 6)) (setf z 7))
		   ((>= 6) (setf z 8)))))
      z))


  ;;retourne la liste des nombres que la case l c ne peut pas contenir
  (defun forbid-numb(tab l c)
    (let ((res '()))
      (do ((i 1 (1+ i)))
	  ((= i 10))
	(if (or (in-col tab c i) (in-line tab l i) (in-zone tab l c i))
	    (setf res (cons i res))))
      res))


  ;;retourne la liste des nombres que la case l c peut contenir
  (defun possible-numb(tab l c)
    (let ((list '(1 2 3 4 5 6 7 8 9)))
      (set-difference list (forbid-numb tab l c))))


  ;;retourne une grille de sudoku aléatoire et complète
  (defun random-grid (tab)
    (empty-grid tab)
    (random-grid-aux tab 0))

  
  (defun random-grid-aux(tab a)
    (let ((r)
	  (list '()))
      (do ((i a (1+ i)))
	  ((= i 9))
	(empty-line tab i)
	(do ((j 0 (1+ j)))
	    ((= j 9))
	  (setf list (possible-numb tab i j))
	  (if (eq list NIL)
	      (random-grid-aux tab i))
	  (setf r (random (length list)))
	  (setf (aref tab i j) (nth r list)))))
    tab)
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;;;;;;;;;;;;;;;;;;;;; FONCTIONS ANNEXES ;;;;;;;;;;;;;;;;;;;;;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  


  (defun f(x)
    (cond ((member x '(0 1 2))
	   0)
	  ((member x '(3 4 5))
	   1)
	  ((member x '(6 7 8))
	   2)))


  (defun copy-grid(src dest l)
    (let ((m (cadr (array-dimensions dest))))
      (cond ((not (= (length src) m))
	     (format t "error: src array size should be ~d~%" m))
	    ((= (length src) (cadr (array-dimensions dest)))
	     (let ((n (length src)))
	       (do ((i 0 (1+ i)))
		   ((= i n))
		 (setf (aref dest l i) (aref src i)))))))
    dest)

  
  );end let


