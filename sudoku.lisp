(defvar *sqrt-size*)
(defvar *size*)
(defvar *grid*)
(defvar *grid-protected*)
(defvar *squares*)
(defvar *list-alpha*)


(setf *sqrt-size* 3)
(setf *size* (* *sqrt-size* *sqrt-size*))


(defun make-grid (&optional l)
  (if l
      (make-array (list *size* *size*) :initial-contents l)
      (make-array (list *size* *size*) :initial-element 0)))


(setf *grid* (make-grid))
(setf *grid-protected* (make-grid))
(setf *squares* (loop for i from 1 to *size* collect i))
(setf *list-alpha* '(a b c d e f g h i j k l m n o p q r s t u v w x y))


(defun copy-grid (grid)
  (let* ((dimensions (array-dimensions grid))
	 (g (make-array dimensions)))
    (loop
      for i from 0 below (first dimensions)
      do (loop
	   for j from 0 below (second dimensions)
	   do (setf (aref g i j) (aref grid i j))))
    g))


(defgeneric grid (game))
(defgeneric protected-grid (game))


(defclass game ()
  ((grid :accessor grid :initarg :grid)
   (protected-grid :reader protected-grid :initarg :protected-grid)))


(defun make-game (grid)
  (make-instance 'game :protected-grid grid))


(defgeneric init-game (game))
(defmethod init-game ((game game))
  (setf (grid game) (copy-grid (protected-grid game))))


(defgeneric run-game (game))
(defmethod run-game ((game game))
  (init-game game)
  (play game))


(defun sudoku ()
  (message-begin)
  (let ((grid (create-easy-grid)))
    (run-game (make-game grid))))

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
  (dotimes (i *size*)
    (if (= 0 (mod i *sqrt-size*))
	(format t "  "))
    (format t "~d " (code-char (+ i 97)))))

;; affiche une barre en fonction de la taille de la grille
(defun print-line ()
  (format t "~%   ")
  (dotimes (i (+ (* 2 *size*) (* 2 *sqrt-size*) 1))
    (format t "-")))

;; affiche une grille
(defun print-grid-aux(grid)
  (dotimes (i *size*)
    (if (= (mod i *sqrt-size*) 0)
	(print-line))
    (format t "~%~2d " (1+ i))
    (dotimes (j *size*)
      (if (= (mod j *sqrt-size*) 0)
	  (format t "| "))
      (if (zerop (aref grid i j))
	  (format t "_ ")
	  (if (>= *size* 10)
	      (if (>= (aref grid i j) 10)
		  (format t "~d " (code-char (+ (aref grid i j) 55)))
		  (format t "~d " (aref grid i j)))
	      (format t "~d " (aref grid i j)))))
    (format t "|")))


(defun message-begin ()
  (format t "
 -----------------------------------------------------------------
                           SUDOKU ~dx~d 
 -----------------------------------------------------------------
choisissez à tout moment : - n pour nouvelle partie
                           - r pour recommencer la partie
                           - q pour quitter~%" *size* *size*))
             


;;commence le jeu
(defun play-in-square (grid l c n)
  (setf (aref grid l c) n))


;; affiche les erreurs en fonction du choix du joueurs
(defun manage-errors (game nb col line)
  (let ((err nil))
    (if (or (not (member col (firsts-elem *list-alpha* *size*)))
	    (not (member nb *squares*))
	    (not (member line *squares*)))
	(progn (format t "ERREUR: nombre ~d ou case ~d ~d non valide~%" nb col line)
	       (setq err t))
	(progn (when (in-col (grid game) (position col *list-alpha*) nb)
		 (format t "ERREUR: nombre ~d déjà dans la colonne ~d~%" nb col)
		 (setq err t))
	       (when (in-line (grid game) (1- line) nb)
		 (format t "ERREUR: nombre ~d déjà dans la ligne ~d~%" nb line)
		 (setq err t))
	       (when (in-zone (grid game) (1- line) (position col *list-alpha*) nb)
		 (format t "ERREUR: nombre ~d déjà dans la zone ~d~%" nb
			 (zone (1- line) (position col *list-alpha*)))
		 (setq err t))
	       (when (= (aref (protected-grid game) (1- line) (letter-int col)) 1)
		 (format t "ERREUR: case ~d ~d protégée~%" col line)
		 (setq err t))
	       (let ((cp (copy-grid (grid game))))
	       	 (setf (aref cp (1- line) (letter-int col)) nb)
	       	 (when (not (is-solvable cp))
	       	   (format t "ERREUR: l'affectation de ~d cette case ~d ~d rend la grille non résoluble~%"
			   nb col line)
		   (setf err T)))))
    err))


(defun play (game)
  (let ((grid (grid game)))
    (print-grid grid)
    (format t "choisissez un nombre, une colonne et une ligne (ex 6 a 1): ")
    (let ((nb (read)))    
      (cond ((eq nb 'n)
	     (run-game (make-game (create-easy-grid))))
	    ((eq nb 'r)
	     (run-game game))
	    ((eq nb 'q)
	     (format t "vous avez quitté la partie")
	     (return-from play))
	    ((eq nb 'p)
	     (print-grid grid)
	     (play game)))
      (let* ((col (read))
	     (line (read))
	     (err (manage-errors game nb col line)))
	(if err
	    (play game)
	    (progn
	      (play-in-square grid (1- line) (letter-int col) nb)
	      (if (full-grid grid)
		  (end-game game)
		  (play game))))))))


;; retourne l'entier de la lettre la (ex a:0, b:1, c:2...)
(defun letter-int(l)
  (position l *list-alpha*))

;; termine la partie
(defgeneric end-game (game))
(defmethod end-game ((game game))
  (format t "
 -------------------------------------------------------
|  Félicitation, vous avez terminé la grille de sudoku  |
 -------------------------------------------------------
Choisissez q pour quitter 
Choisissez n pour une nouvelle partie 
mon choix : ")
  (let ((choice (read)))
    (cond ((eq choice 'n)
	   (run-game (make-game (create-easy-grid))))
	  ((eq choice 'r)
	   (run-game game))
	  ((eq choice 'q)
	   (error "vous avez quitté la partie"))))
  (end-game game))


;; vide une grille
(defun empty-grid(tab)
  (dotimes (i (first (array-dimensions tab)))
    (empty-line tab i)))


;; vide la ligne d'une grille
(defun empty-line(tab l)
  (dotimes (i (first (array-dimensions tab)))
    (setf (aref tab l i) 0)))

;; retourne si n est dans la ligne l de la grille
(defun in-line(tab l n)
  (let ((res NIL))
    (dotimes (i (first (array-dimensions tab)) res)
      (if (= (aref tab l i) n)
	  (setf res T)))))


;; retourne si n est dans la colonne c de la grille
(defun in-col(tab c n)
  (let ((res NIL))
    (dotimes (i (first (array-dimensions tab)) res)
      (if (= (aref tab i c) n)
	  (setf res T)))))


;; retourne si n est dans la zone contenant la position l c d'une grille
(defun in-zone(tab l c n)
  (let ((res NIL)
	(z (zone l c)))
    (dotimes (i *sqrt-size* res)
      (dotimes (j *sqrt-size*)
	(if (= (aref tab
		     (+ i (* (floor (/ z *sqrt-size*)) *sqrt-size*))
		     (+ j (* (mod z *sqrt-size*) *sqrt-size*))) n)
	    (setf res T))))))


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
    (dotimes (i *sqrt-size*)
      (dotimes (j *sqrt-size*)
	(if (and (< l (* (1+ i) *sqrt-size*))
		 (>= l (* i *sqrt-size*))
		 (< c (* (1+ j) *sqrt-size*))
		 (>= c (* j *sqrt-size*)))
	    (progn (setf a i)
		   (setf b j)))))
    (+ (* a *sqrt-size*) b)))

;;retourne la liste des nombres que la case l c ne peut pas contenir
(defun forbid-numb(tab l c)
  (let ((res '())
	(dimension (first (array-dimensions tab))))
    (do ((i 1 (1+ i)))
	((= i (1+ dimension)))
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
  (dotimes (i *size* tab)
    (random-line tab i 0)))

;; retourne une ligne aléatoire
(defun random-line (tab l cpt) ;; decomposer par lignes
  (when (> cpt (* *size* *size*))
    (random-grid tab))
  (let ((r)
	(possibilities '()))
    (empty-line tab l)
    (dotimes (i *size* tab)
      (setf possibilities (possible-numb tab l i))
      (cond ((eq possibilities NIL)
	     (random-line tab l (1+ cpt))
	     (return-from random-line)))
      (setf r (random (length possibilities)))
      (setf (aref tab l i) (nth r possibilities)))))

;; retourn si une case de la grille n'a qu'une seule possibilité (si elle est résoluble)
(defun is-solvable-case (grid i j)
  (if (and (zerop (aref grid i j))
	   (= (length (possible-numb grid i j)) 1))
      T
      NIL))

;; retourne le nombre de case résoluble
(defun numb-solvable-case (grid)
  (let ((cpt 0))
    (dotimes (i (first (array-dimensions grid)) cpt)
      (dotimes (j (second (array-dimensions grid)))
	(if (is-solvable-case grid i j)
	    (setf cpt (1+ cpt)))))))

;; retourne si grid est résolvable
(defun is-solvable (grid)
  (let ((cp (copy-grid grid)))
    (solve-grid cp)
    (if (full-grid cp)
	T
	NIL)))

;; retourne la grille résolue
(defun solve-grid (grid)
  (if (full-grid grid)
      grid
      (progn
	(dotimes (i (first (array-dimensions grid)))
	  (dotimes (j (second (array-dimensions grid)))
	    (if (is-solvable-case grid i j)
		(progn
		  (setf (aref grid i j) (car (possible-numb grid i j)))
		  (solve-grid grid))
		grid))))))

;; retourne une grille aléatoire à compléter
(defun create-easy-grid ()
  (let ((grid (create-easy-grid-aux (random-grid (make-grid)))))
    (format t "cases initiales : ~d~%" (square-numb grid))
    grid))

(defun create-easy-grid-aux (grid)
  (let* ((rcase (random-full-case grid))
	 (val (aref grid (first rcase) (second rcase))))
    (setf (aref grid (first rcase) (second rcase)) 0)
    (if (is-solvable grid)
	(create-easy-grid-aux grid)
	(progn (setf (aref grid (first rcase) (second rcase)) val)
	       grid))))

;; retourne les coordonnées sous forme de liste d'une case non vide de grid
(defun random-full-case (grid)
  (let* ((dimensions (array-dimensions grid))
	 (rl (random (first dimensions)))
	 (rc (random (second dimensions))))
    (random-full-case-aux grid rl rc dimensions)))

(defun random-full-case-aux (grid rl rc dim)
  (if (zerop (aref grid rl rc))
      (progn (setf rl (random (first dim)))
	     (setf rc (random (second dim)))
	     (random-full-case-aux grid rl rc dim))
      (list rl rc)))


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
  (and
   (is-valid-sqr (car (array-dimensions grid)))
   (= (car (array-dimensions grid)) (car (cdr (array-dimensions grid))))))

;; retourne le n premiers éléments de la liste l
(defun firsts-elem(l n)
  (if (= (length l) n)
      l
      (firsts-elem (butlast l) n)))

;; retourne si la grille est pleine (sans 0)
(defun full-grid (grid)
  (let ((dimensions (array-dimensions grid)))
    (dotimes (i (first dimensions) t)
      (dotimes (j (second dimensions))
	(when (zerop (aref grid i j))
	  (return-from full-grid nil))))))

;; retourne le nombre de cases différentes de 0
(defun square-numb (grid)
  (let ((cpt 0)
	(dimensions (array-dimensions grid)))
    (dotimes (i (first dimensions) cpt)
      (dotimes (j (second dimensions))
	(when (not (zerop (aref grid i j)))
	  (setf cpt (1+ cpt)))))))	  

(defun list-char-letters(begin size)
  (list-char-letters-aux '() begin size 0))

(defun list-char-letters-aux(l begin size i)
  (if (= i size)
      l
      (list-char-letters-aux (append l (list (+ i begin))) begin size (1+ i))))

(defun make-grid-from-list (l)
  (make-grid l))

(defun load-grid-from-stream (stream)
  (let ((l (read stream)))
    (make-grid-from-list l)))

(defun load-grid-from-file (filename)
  (with-open-file (stream filename)
    (load-grid-from-stream stream)))

(defun load-grid-randomly()
  (let* ((r (random 8))
	 (c (code-char (+ r 49)))
	 (f ".lisp"))
    (setf f (concatenate 'string "grid/9x9-" (string c) f))
    (format t "file: ~d~%" f)
    (load-grid-from-file f)))
