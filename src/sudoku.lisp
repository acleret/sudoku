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
  (let ((grid (create-grid)))
    (run-game (make-game grid))))


(defun print-grid (grid)
  (when (not (check-is-valid-square grid))  ;; vérifie que grid est un tableau carré
    (format t "ERREUR: taille non valide")  ;; dont la taille est 2², 3², 4² ou 5²
    (return-from print-grid)) 
  (print-letters)                           ;; affiche le nom des colonnes
  (dotimes (i *size* (print-line))
    (when (zerop (mod i *sqrt-size*))
      (print-line))
    (print-number-line (1+ i))              ;; affiche le nom de la ligne
    (dotimes (j *size* (format t "|~%"))
      (if (zerop (mod j *sqrt-size*))       
	  (format t "| "))            ;; affiche un "|" toutes les *sqrt-size* cases
      (if (zerop (aref grid i j))
	  (format t "_ ")             ;; affiche un "_" pour les cases vides
	  (if (>= *size* 10)
	      (if (>= (aref grid i j) 10)   ;; affiche une lettre si le nombre >= 10
		  (print-number-to-letter (aref grid i j))
		  (format t "~d " (aref grid i j)))
	      (format t "~d " (aref grid i j)))))))

;; affiche le nom des colonnes en fonction de la taille de la grille
(defun print-letters ()
  (format t "   ")
  (dotimes (i *size* (format t "~%"))
    (when (zerop (mod i *sqrt-size*))
      (format t "  "))
    (format t "~d " (code-char (+ i 97)))))

;; affiche une barre en fonction de la taille de la grille
(defun print-line ()
  (format t "   ")
  (dotimes (i (+ (* 2 *size*) (* 2 *sqrt-size*) 1) (format t "~%"))
    (format t "-")))

;; affiche le numéro de la ligne n
(defun print-number-line (n)
  (format t "~2d " n))

;; affiche la lettre majuscule correspondante à l'entier n à partir de 10
;; n=10 affiche A, n=11 affiche B, n=12 affiche C, etc. 
(defun print-number-to-letter (n)
  (format t "~d " (code-char (+ n 55))))

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
	       (when (not (zerop (aref (protected-grid game) (1- line) (letter-int col))))
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
	     (run-game (make-game (create-grid))))
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
	   (run-game (make-game (create-grid))))
	  ((eq choice 'r)
	   (run-game game))
	  ((eq choice 'q)
	   (error "vous avez quitté la partie"))))
  (end-game game))

;; vide une grille
(defun empty-grid(grid)
  (dotimes (i (first (array-dimensions grid)))
    (empty-line grid i)))

;; vide la ligne d'une grille
(defun empty-line(grid l)
  (dotimes (i (first (array-dimensions grid)))
    (setf (aref grid l i) 0)))

;; retourne si n est dans la ligne l de la grille
(defun in-line(grid l n)
  (let ((res NIL))
    (dotimes (i (first (array-dimensions grid)) res)
      (if (= (aref grid l i) n)
	  (setf res T)))))

;; retourne si n est dans la colonne c de la grille
(defun in-col(grid c n)
  (let ((res NIL))
    (dotimes (i (first (array-dimensions grid)) res)
      (if (= (aref grid i c) n)
	  (setf res T)))))

;; retourne si n est dans la zone contenant la position l c d'une grille
(defun in-zone(grid l c n)
  (let ((res NIL)
	(z (zone l c)))
    (dotimes (i *sqrt-size* res)
      (dotimes (j *sqrt-size*)
	(if (= (aref grid
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
(defun forbid-numb(grid l c)
  (let ((res '())
	(dimension (first (array-dimensions grid))))
    (do ((i 1 (1+ i)))
	((= i (1+ dimension)))
      (if (or (in-col grid c i) (in-line grid l i) (in-zone grid l c i))
	  (setf res (cons i res))))
    res))

;;retourne la liste des nombres que la case l c peut contenir
(defun possible-numb-empty (grid l c)
  (assert (zerop (aref grid l c)))
  (set-difference *squares* (forbid-numb grid l c)))

(defun possible-numb(grid l c)
  (and (zerop (aref grid l c))
       (possible-numb-empty grid l c)))

;;retourne une grille de sudoku aléatoire et complète
(defun random-grid (grid)
  (empty-grid grid)          ;; on vide la grille grid
  (dotimes (i *size* grid)   ;; pour chaque ligne de grid
    (random-line grid i 0))) ;; on appelle random-line

;; affecte à la ligne l de grid une ligne aléatoire si cpt <= *size* x *size*
(defun random-line (grid l cpt) 
  (when (> cpt (* *size* *size*))
    (random-grid grid))
  (let ((r)
	(possibilities '()))        
    (empty-line grid l)                    ;; on vide la ligne
    (dotimes (i *size* grid)
      (setf possibilities (possible-numb grid l i)) ;; on récupère les possibilités de la case
      (cond ((eq possibilities NIL)        ;; s'il n'y a pas de possibilités
	     (random-line grid l (1+ cpt)) ;; on recommence et on incrémente cpt
	     (return-from random-line)))   ;; on sort de la boucle si cpt non valide
      (setf r (random (length possibilities))) ;; sinon on récupère un possibilité aléatoirement 
      (setf (aref grid l i) (nth r possibilities))))) ;; et on l'affecte


;; retourne si grid est résolvable
(defun is-solvable (grid)
  (let ((cp (copy-grid grid)))
    (solve-grid cp)
    (if (full-grid cp)
	T
	NIL)))

;; retourne la grille résolue
(defun solve-grid (grid)
  (solve-grid-aux grid (make-array (array-dimensions grid)))
  grid)

(defun solve-grid-aux (grid-init grid-prev)
  (if (cmp-grid grid-init grid-prev)
      grid-init
      (progn
	(setf grid-prev (copy-grid grid-init))
	(tactics-naked-single grid-init)
	(tactics-single-in-group grid-init)
	(solve-grid-aux grid-init grid-prev))))
   
;; retourne une grille aléatoire à compléter
(defun create-grid ()
  (let ((grid (create-grid-aux (random-grid (make-grid)))))
    (format t "cases initiales : ~d~%" (square-numb grid))
    grid))

(defun create-grid-aux (grid)
  (let* ((rcase (random-full-case grid))
	 (val (aref grid (first rcase) (second rcase))))
    (setf (aref grid (first rcase) (second rcase)) 0)
    (if (is-solvable grid)
	(create-grid-aux grid)
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

;; retourne si une case de la grille n'a qu'une seule possibilité
(defun naked-single (grid i j)
  (and (zerop (aref grid i j))                   ;; la case est vide et
       (= (length (possible-numb grid i j)) 1))) ;; il y a exactement 1 possibilité

;; affecte les cases n'ayant qu'une seule possibilité
(defun tactics-naked-single (grid)
  (if (full-grid grid) ;; si la grille est résolue alors on la retourne
      grid
      (progn ;; sinon
	(dotimes (i (first (array-dimensions grid)) grid) ;; on parcours chaque case
	  (dotimes (j (second (array-dimensions grid))) ;; de la grille
	    (when (naked-single grid i j) ;; s'il n'y a qu'une seule possibilité
	      (setf (aref grid i j) (car (possible-numb grid i j))) ;; on affecte la case 
	      (tactics-naked-single grid)
	      (return-from tactics-naked-single))))))) ;; et on recommence

;; retourne si nb n'est possible que dans la case l c de la zone où se trouve la case
(defun single-in-zone (grid nb l c)
  (let ((zone (zone l c)))                   ;; on récupère la zone de la case l c
    (dotimes (i (first (array-dimensions grid)) T) ;; on parcours la grille et on renvoie T
      (dotimes (j (second (array-dimensions grid)))
	(when (and (= zone (zone i j))       ;; si on est dans la zone
		   (not (coor-egal l c i j)) ;; la case n'est pas celle de l c
		   (is-member nb (possible-numb grid i j))) ;; et on trouve nb parmi les possibilités 
	  (return-from single-in-zone NIL))))))             ;; alors on sort et on retourne NIL

;; retourne si nb n'est possible que dans la case l c de la colonne où se trouve la case
(defun single-in-col (grid nb l c)
  (dotimes (i (first (array-dimensions grid)) T)   ;; on parcours la colonne et on renvoie T
    (when (and (not (equal (list l c) (list i c))) ;; si la case n'est pas l c
	       (is-member nb (possible-numb grid i c))) ;; et on trouve nb parmi les possibilités
      (return-from single-in-col NIL))))))              ;; alors on sort et on retourne NIL


;; retourne si nb n'est possible que dans la case l c de la ligne où se trouve la case
(defun single-in-line (grid nb l c)
  (dotimes (i (first (array-dimensions grid)) T)   ;; on parcours la ligne et on renvoie T
    (when (and (not (equal (list l c) (list l i))) ;; si la case n'est pas l c
	       (is-member nb (possible-numb grid l i))) ;; et on trouve nb parmi les possibiités
      (return-from single-in-line NIL))))))             ;; alors on sort et on retourne NIL

;; affecte les cases dont une possibilité n'y est possible que dans la zone, ligne ou colonne
(defun tactics-single-in-group (grid)
  (if (full-grid grid) ;; si la grille est résolue
      grid             ;; alors on la retourne
      (progn           ;; sinon
	(dotimes (i (first (array-dimensions grid)) grid) ;; on parcours chaque
	  (dotimes (j (second (array-dimensions grid)))   ;; case de la grille
	    (let ((l (possible-numb grid i j))) ;; on récupère les possibilités
	      (dotimes (nb (length l))          ;; et pour chaque possibilité
		(when (or
		       (single-in-zone grid (nth nb l) i j)  ;; on vérifie si c'est la
		       (single-in-col grid (nth nb l) i j)   ;; seule possibilité de la 
		       (single-in-line grid (nth nb l) i j)) ;; zone, ligne ou colonne
		  (setf (aref grid i j) (nth nb l))          ;; on affecte la case
		  (tactics-single-in-group grid)             ;; et on recommence
		  (return-from tactics-single-in-group))))))))) 


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; ;;;;;;;;;;;;;;;;;;;;;; FONCTIONS ANNEXES ;;;;;;;;;;;;;;;;;;;;;;;  
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

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

;; retourne si n appartient à la liste l
(defun is-member (n l)
  (not (eq (find n l) NIL)))

;; retourne si les grilles sont les memes
(defun cmp-grid (g1 g2)
  (let ((res T))
    (if (not (equal (array-dimensions g1) (array-dimensions g2)))
	(setf res NIL)
	(progn
	  (let* ((dimensions (array-dimensions g1)))
	  (dotimes (i (first dimensions))
	    (dotimes (j (second dimensions))
	      (if (not (= (aref g1 i j) (aref g2 i j)))
		  (setf res nil)))))))
    res))

(defun coor-egal (a b c d)
  (equal (list a b) (list c d)))

;; (defun is-possible (nb grid l c)
;; 		 (is-member nb (possible-numb grid i j)))

(defun make-grid-from-list (l)
  (make-grid l))

(defun load-grid-from-stream (stream)
  (let ((l (read stream)))
    (make-grid-from-list l)))

(defun load-grid-from-file (filename)
  (with-open-file (stream filename)
    (load-grid-from-stream stream)))

;; charge un fichier de la forme "../grid/sxs-n.lisp"
(defun load-grid-randomly()
  (let* ((r (random 8))	       ;; on initialise un nombre aléatoire parmi le nombre
	 (num (string (code-char (+ r 49))))  ;; de fichier que l’on peut charger
	 (size (string (code-char (+ *size* 48))))  ;; 48 vaut "0" en ascii
	 (file ".lisp"))		      ;; on initialise le nom du fichier
    (setf file (concatenate 'string "../grid/" size "x" size "-" num file))
    (format t "file: ~d~%" file)
    (load-grid-from-file file)))
