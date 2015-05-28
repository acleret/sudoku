(defvar *sqrt-size* 3)
(defvar *size* (* *sqrt-size* *sqrt-size*))
(defvar *grid* (make-grid))
(defvar *grid-protected* (make-grid))
(defvar *squares* (loop for i from 1 to *size* collect i))

(defvar *list-alpha* '(a b c d e f g h i j k l m n o p q r s t u v w x y))

;; defclass game (grid)
;; make-game, init-game, run-game, game-over
;; defclass grid ou tableau de squares
;; defclass square (value,protected, possible values

(defun make-grid ()
  (format t "taille : ~d~%" *size*)
  (make-array (list *size* *size*) :initial-element 0))

;; demande un choix, initialise une grille, l'affiche et commence le jeu
(defun sudoku()
  (chose-size)
  (init-grid *grid*)
  (print-grid *grid*)
  (play))

;; 
(defun chose-size()
  (format t "    1 : quitter
    2 : 4x4
    3 : 9x9
    4 : 16x16
    5 : 25x25
Choisissez la taille de la grille : ")
  (let ((choice (read)))
    (cond ((= choice 1)
	   (error "vous avez quitté la partie"))
	  ((not (and (>= choice 2) (<= choice 5)))
	   (format t "ERREUR: choix non valide~%")
	   (chose-size))
	  ((and (>= choice 2) (<= choice 5))
	   (update-size choice)))))

;; actualise les variables globales
(defun update-size(size)
  (setf *sqrt-size* size)
  (setf *size* (* *sqrt-size* *sqrt-size*))
  (setf *grid* (make-grid))
  (setf *grid-protected* (make-grid))
  (setf *squares* (loop for i from 1 to *size* collect i)))

;; initialise les grille 4x4 et 9x9
(defun init-grid (grid)
  (if (= *size* 4)
      (progn (setf *grid* (make-array '(4 4) :initial-contents '((4 0 0 0)
								 (0 3 0 0)
								 (0 0 0 3)
								 (0 0 4 1))))
	     (protected-grid)))
  (if (= *size* 9)
      (progn (setf *grid* (make-array '(9 9) :initial-contents '((0 0 0 0 6 0 0 0 2)
								 (0 3 0 8 0 0 6 9 0)
								 (6 0 0 3 9 2 0 0 8)
								 (8 0 3 0 0 6 4 7 0)
								 (0 0 9 7 0 0 0 0 0)
								 (7 0 0 5 0 1 9 0 3)
								 (0 9 0 0 0 0 0 6 0)
								 (1 6 8 0 0 5 0 2 0)
								 (0 0 2 0 7 0 8 0 5))))
	     (protected-grid))
      (empty-grid grid)))

;; remplit la grille contenant les cases protégées
;; (0 pour non protégé, 1 pour non protégé)
(defun protected-grid()
  (empty-grid *grid-protected*)
  (do ((i 0 (1+ i)))
      ((= i *size*))
    (do ((j 0 (1+ j)))
	((= j *size*))
      (if (not (= (aref *grid* i j) 0))
	  (setf (aref *grid-protected* i j) 1)))))

;; affiche une grille de sudoku
(defun print-grid(grid)
  (if (not (check-is-valid-square grid))
      (format t "ERREUR: taille non valide")
      (progn (print-letters)
	     (print-grid-aux grid)
	     (print-line)
	     (format t "~%"))))

;; affiche la ligne des lettres en fonction de la taille de la grille
(defun print-letters()
  (format t "   ")
  (do ((i 0 (1+ i)))
      ((= i *size*))
    (if (= 0 (mod i *sqrt-size*))
	(format t "  "))
    (format t "~d " (code-char (+ i 97)))))

;; affiche une barre en fonction de la taille de la grille
(defun print-line ()
  (format t "~%   ")
  (do ((i 0 (1+ i)))
      ((= i (+ (* 2 *size*) (* 2 *sqrt-size*) 1)))
    (format t "-")))

;; affiche une grille
(defun print-grid-aux(grid)
  (do ((i 0 (1+ i)))
      ((= i *size*))
    (if (= (mod i *sqrt-size*) 0)
	(print-line))
    (format t "~%~2d " (1+ i))
    (do ((j 0 (1+ j)))
	((= j *size*))
      (if (= (mod j *sqrt-size*) 0)
	  (format t "| "))
      (format t "~d " (aref grid i j)))
    (format t "|")))

;;commence le jeu
(defun play()
  (format t "choisissez un nombre, une colonne et une ligne (ex 6 a 1): ")
  (let ((nb (read)))    
    (cond ((eq nb 'n)
	   (sudoku))
	  ((eq nb 'q)
	   (error "vous avez quitté la partie"))
	  ((eq nb 'p)
	   (print-grid *grid*)
	   (play)))
    (let ((col (read))
	  (line (read)))
      (manage-errors nb col line)
      (setf (aref *grid* (1- line) (letter-position col)) nb)))
  (if (full-grid *grid*)
      (end-sudoku))
  (print-grid *grid*)
  (play))

;; affiche les erreurs en fonction du choix du joueurs
(defun manage-errors(nb col line)
  (cond ((or (not (member col (firsts-elem *list-alpha* *size*)))
	     (not (member (or nb line) *squares*)))
	 (format t "ERREUR: nombre ~d ou case ~d ~d non valide~%" nb col line)
	 (play)))
  (if (in-col *grid* (position col *list-alpha*) nb)
      (format t "ERREUR: nombre ~d déjà dans la colonne ~d~%" nb col))
  (if (in-line *grid* (1- line) nb)
      (format t "ERREUR: nombre ~d déjà dans la ligne ~d~%" nb line))
  (if (in-zone *grid* (1- line) (position col *list-alpha*) nb)
      (format t "ERREUR: nombre ~d déjà dans la zone ~d~%" nb
	      (zone (1- line) (position col *list-alpha*))))
  (if (= (aref *grid-protected* (1- line) (letter-position col)) 1)
      (format t "ERREUR: case ~d ~d protégée~%" col line))      
  (if (or (in-col *grid* (position col *list-alpha*) nb)
	  (in-line *grid* (1- line) nb)
	  (in-zone *grid* (1- line) (position col *list-alpha*) nb)
	  (= (aref *grid-protected* (1- line) (letter-position col)) 1))
      (play)))

;; retourne la position de l dans *list-alpha*
(defun letter-position(l)
  (let ((res -1))
    (do ((i 0 (1+ i)))
	((= i *size*))
      (if (eq l (nth i *list-alpha*))
	  (setf res i)))
    res))

;; termine la partie
(defun end-sudoku()
  (format t "
 -------------------------------------------------------
|  Félicitation, vous avez terminé la grille de sudoku  |
 -------------------------------------------------------
Choisissez q pour quitter 
Choisissez n pour une nouvelle partie 
mon choix : ")
  (let ((choice (read)))
    (cond ((eq choice 'n)
	   (sudoku))
	  ((eq choice 'q)
	   (error "vous avez quitté la partie"))))
  (end-sudoku))

;; vide une grille
(defun empty-grid(tab)
  (do ((i 0 (1+ i)))
      ((= i (car (array-dimensions tab))))
    (empty-line tab i)))

;; vide la ligne d'une grille
(defun empty-line(tab l)
  (do ((i 0 (1+ i)))
      ((= i (car (array-dimensions tab))))
    (setf (aref tab l i) 0)))

;; retourne si n est dans la ligne l de la grille
(defun in-line(tab l n)
  (let ((res NIL))
    (do ((i 0 (1+ i)))
	((= i (car (array-dimensions tab))))
      (if (= (aref tab l i) n)
	  (setf res T)))
    res))

;; retourne si n est dans la colonne c de la grille
(defun in-col(tab c n)
  (let ((res NIL))
    (do ((i 0 (1+ i)))
	((= i (car (array-dimensions tab))))
      (if (= (aref tab i c) n)
	  (setf res T)))
    res))

;; retourne si n est dans la zone contenant la position l c d'une grille
(defun in-zone(tab l c n)
  (let ((res NIL)
	(z (zone l c)))
    (do ((i 0 (1+ i)))
	((= i *sqrt-size*))
      (do ((j 0 (1+ j)))
	  ((= j *sqrt-size*))
	(if (= (aref tab
		     (+ i (* (floor (/ z *sqrt-size*)) *sqrt-size*))
		     (+ j (* (mod z *sqrt-size*) *sqrt-size*))) n)
	    (setf res T))))
    res))

;;retourne la zone contenant la case l c
;;
;; -----  -------  -------------  ----------------
;; |0|1|  |0|1|2|  | 0| 1| 2| 3|  | 0| 1| 2| 3| 4|
;; -----  -------  -------------  ----------------
;; |2|3|  |3|4|5|  | 4| 5| 6| 7|  | 5| 6| 7| 8| 9|
;; -----  -------  -------------  ----------------
;;        |6|7|8|  | 8| 9|10|11|  |10|11|12|13|14|
;;        -------  -------------  ----------------
;;                 |12|13|14|15|  |15|16|17|18|19|
;;                 -------------  ----------------
;;                                |20|21|22|23|24|
;;                                ----------------
(defun zone(l c)
  (let ((a 0)
	(b 0))
    (do ((i 0 (1+ i)))
	((= i *sqrt-size*))
      (do ((j 0 (1+ j)))
	  ((= j *sqrt-size*))
	(if (and (< l (* (1+ i) *sqrt-size*))
		 (>= l (* i *sqrt-size*))
		 (< c (* (1+ j) *sqrt-size*))
		 (>= c (* j *sqrt-size*)))
	    (progn (setf a i)
		   (setf b j)))))
    (+ (* a *sqrt-size*) b)))

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
  (let ((list *squares*))
    (set-difference list (forbid-numb tab l c))))

;;retourne une grille de sudoku aléatoire et complète
(defun random-grid (tab)
  (empty-grid tab)
  (random-grid-aux tab 0))

(defun random-grid-aux(tab a) ;; decomposer par lignes
  (let ((r)
	(list '()))
    (do ((i a (1+ i)))
	((= i *size*))
      (empty-line tab i)
      (do ((j 0 (1+ j)))
	  ((= j *size*))
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

;; carré de x
(defun square (x)
  (* x x))

;; est une racine carré entière parmi 2² 3² 4² 5² 6²
(defun is-valid-sqr(x)
  (member x (map 'list 'square '(2 3 4 5))))

;; retourne si la grille grid est carrée de taille parmis 2² 3² 4² 5² 6²
(defun check-is-valid-square(grid)
  (if (or (not (is-valid-sqr (car (array-dimensions grid))))
	  (not (= (car (array-dimensions grid)) (car (cdr (array-dimensions grid))))))
      NIL
      T))

;; retourne le n premiers éléments de la liste l
(defun firsts-elem(l n)
  (if (= (length l) n)
      l
      (firsts-elem (butlast l) n)))

;; retourn si la grille est pleine (sans 0)
(defun full-grid(grid)
  (let ((res T))
    (do ((i 0 (1+ i)))
	((= i (car (array-dimensions grid))))
      (do ((j 0 (1+ j)))
	  ((= j (car (cdr (array-dimensions grid)))))
	(if (= (aref grid i j) 0)
	    (setf res NIL))))
    res))

;; copie la grille src dans dest
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

(defun list-char-letters(begin size)
  (list-char-letters-aux '() begin size 0))

(defun list-char-letters-aux(l begin size i)
  (if (= i size)
      l
      (list-char-letters-aux (append l (list (+ i begin))) begin size (1+ i))))

;; à modifier
(defun f(x)
  (cond ((member x '(0 1 2))
	 0)
	((member x '(3 4 5))
	 1)
	((member x '(6 7 8))
	 2)))

(defun make-grid-from-list (l)
  l);;(make-grid l))

(defun load-grid-from-stream (stream)
  (let ((l (read stream)))
    (make-grid-from-list l)))

(defun load-grid-from-file (filename)
  (with-open-file (stream filename)
    (load-grid-from-stream stream)))
