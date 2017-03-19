;///////////// TP 3 ///////////////////
;--------------------------------------
;Sy-Toan Ngo
;Marion Chan-Renous-Legoubin
;--------------------------------------

;------------------------------------------------------
; **** Initialisation de la base de faits  (BF) **** ;
;-------------------------------------------------------

(defparameter *BF* nil)

;------------------------------------------------------
; **** Initialisation de la base de regles  (BR) **** ;
;-------------------------------------------------------

(defparameter *BR* 
  '((R1 ((> taille 190) (sexe masculin)) (taille grande))
    (R2 ((> taille 174) (sexe feminin)) (taille grande))
    (R3 ((< taille 163) (sexe masculin)) (taille petite))
    (R4 ((< taille 152) (sexe feminin)) (taille petite))
    (R5 ((>= taille 152) (<= taille 174) (sexe feminin)) (taille moyenne))
    (R6 ((>= taille 163) (<= taille 187) (sexe masculin)) (taille moyenne))
    (R7 ((> puissance 5) (> resistance 5)) (qualite force))
    (R8 ((> fatigue 5) (> duree 5)) (qualite endurance))
    (R9 ((> reactivite 5) (> explosivite 5)) (qualite vitesse))
    (R10 ((> habilete 5) (> precision 5)) (qualite adresse))
    (R11 ((> amplitude 5) (> flexibilite 5)) (qualite souplesse))
    (R12 ((> extraversion 5) (> ouverture 5)) (pratique individuel))
    (R13 ((> extraversion 5) (> consciencieux 5)) (pratique individuel))
    (R14 ((> ouverture 5) (> consciencieux 5)) (pratique individuel))
    (R15 ((< extraversion 6) (< ouverture 6)) (pratique collectif))
    (R16 ((< extraversion 6) (< consciencieux 6)) (pratique collectif))
    (R17 ((< consciencieux 6) (< ouverture 6)) (pratique collectif))
    (R18 ((> IMC 25)) (silhouette large))
    (R19 ((>= IMC 18.5) (<= IMC 25)) (silhouette moyenne))
    (R20 ((< IMC 18.5)) (silhouette fine))
    (R21 ((pratique individuel) (qualite endurance) (qualite vitesse)) (sport athletisme))
    (R22 ((pratique collectif) (taille moyenne) (qualite adresse)) (sport basketball))
    (R23 ((pratique collectif) (taille grande) (qualite adresse)) (sport basketball))
    (R24 ((pratique collectif) (silhouette fine) (qualite endurance)) (sport football))
    (R25 ((pratique collectif) (silhouette moyenne) (qualite endurance)) (sport football))
    (R26 ((pratique individuel) (qualite force) (qualite endurance)) (sport natation))
    (R27 ((pratique collectif) (silhouette moyenne) (qualite force) (qualite endurance)) (sport rugby))
    (R28 ((pratique collectif) (silhouette large) (qualite force) (qualite endurance)) (sport rugby))
    (R29 ((pratique individuel) (qualite souplesse)) (sport danse))
    (R30 ((pratique individuel) (silhouette fine) (qualite adresse)) (sport equitation))
    (R31 ((pratique individuel) (silhouette moyenne) (qualite adresse)) (sport equitation))
    (R32 ((pratique individuel) (qualite force) (qualite adresse)) (sport tennis))
    (R33 ((pratique individuel) (qualite adresse) (sport ski)))
    (R34 ((pratique individuel) (silhouette fine) (qualite endurance)) (sport cyclisme))
    (R35 ((pratique individuel) (silhouette moyenne) (qualite endurance)) (sport cyclisme))
    (R36 ((pratique individuel) (silhouette fine) (qualite adresse)) (sport escrime))
    (R37 ((pratique individuel) (silhouette moyenne) (qualite adresse)) (sport escrime))
    (R38 ((pratique individuel) (silhouette fine) (qualite souplesse)) (sport gymnastique))
    (R39 ((pratique individuel) (silhouette moyenne) (qualite souplesse)) (sport gymnastique))
    (R40 ((pratique individuel) (silhouette fine) (qualite force) (qualite endurance)) (sport aviron))
    (R41 ((pratique individuel) (silhouette moyenne) (qualite force) (qualite endurance)) (sport aviron))
    (R42 ((pratique individuel) (silhouette moyenne) (qualite force)) (sport lutte))
    (R43 ((pratique individuel) (silhouette large) (qualite force)) (sport lutte))
    (R44 ((pratique individuel) (silhouette moyenne) (qualite adresse)) (sport BMX))
    (R45 ((pratique individuel) (silhouette fine) (qualite adresse)) (sport BMX))
    (R46 ((pratique individuel) (qualite adresse)) (sport golf))
    (R47 ((pratique collectif) (silhouette moyenne) (qualite adresse) (qualite endurance)) (sport handball))
    (R48 ((pratique collectif) (silhouette large) (qualite adresse) (qualite endurance)) (sport handball))
    )
  )


;-----------------------------------------------------------------------------------------------------------------------
; ****  Fonction permettant de calculer l'IMC de la personne et d'ajouter l'IMC directement dans la base de faits **** ;
;-----------------------------------------------------------------------------------------------------------------------

(defun calculIMC ()
  (let ((IMC 0) (taille (cadr (assoc 'taille *BF*))) (poids (cadr (assoc 'poids *BF*))))
    (setq IMC (/ poids (* (/ taille 100) (/ taille 100))))
    (push (list 'IMC IMC) *BF*)
    IMC
    )
  )

(calculIMC)


;-----------------------------------------------------------
; **** Fonction qui renvoie les premisses d'une regle **** ;
;-----------------------------------------------------------
(defun premisses (regle)
  (let (result)
    (setq result (cadr (assoc regle *BR*)))
    )
  )

(premisses 'R1)

;-----------------------------------------------------------
; **** Fonction qui renvoie la conclusion d'une regle **** ;
;-----------------------------------------------------------
(defun conclusion (regle)
  (let (result)
    (setq result (caddr (assoc regle *BR*)))
    )
  )

(conclusion 'R1)

;---------------------------------------------------------------------
; **** Fonction qui verifie si le but est dans la base de faits **** ;
;---------------------------------------------------------------------
(defun verifier_dans_BF (but)
  (member but *BF* :test #'equal)
  )


;-------------------------------------------------------------------------
; **** Fonction qui verifie les premisses de longueur 2 d'une regle **** ;
;-------------------------------------------------------------------------
(defun verifier2 (premisse)
           (let ((ok nil))
           (loop for f in *BF*
               while (null ok) do
                 (if (and (eq (car f) (car premisse))(eq (cadr f) (cadr premisse))) (setq ok t))
                 )
           ok
             )
           )

(verifier2 '(taille 165))

;-------------------------------------------------------------------------
; **** Fonction qui verifie les premisses de longueur 3 d'une regle **** ;
;-------------------------------------------------------------------------
(defun verifier3 (premisse)
           (let ((ok nil) (tmp nil))
           (loop for f in *BF*
               while (null ok) do
                 (cond
                  ((and (eq (car f) (cadr premisse)) (numberp (cadr f))) (setq tmp (list (car premisse) (cadr f)(caddr premisse))) (setq ok (eval tmp)))
                  (t nil)
                  )
                 )
           ok
             )
  )

(verifier3 '(> taille 1000))

;----------------------------------------------------------------------------------
; **** Fonction qui verifie les premisses d'une regle en fonction des faits **** ;
;----------------------------------------------------------------------------------
(defun verifier_premisses (regle)
  (let ((ok t) (listepremisses (premisses regle)))
    (loop for p in listepremisses
        while ok do
          (cond
           ((eq (length p) 2) (setq ok (verifier2 p)))
           ((eq (length p) 3) (setq ok (verifier3 p)))
           (t nil)
           )
          )
    ok
    )
  )
(verifier_premisses 'R1)

;----------------------------------------------------------------------------------------
; **** Fonction qui renvoie les regles applicables en fonction de la base de faits **** ;
;----------------------------------------------------------------------------------------
(defun regles_candidates ()
  (let ((result nil))
    (dolist (var *BR* result)
      (if (eq t (verifier_premisses (car var))) (push (car var) result))
      )
    )
  )
(regles_candidates)


;-------------------------------------------------------------------------------------
; **** Fonction en chainage avant en largeur d'abord pour vérifier un but donné **** ;
;-------------------------------------------------------------------------------------
(defun chainage_avant (but)
  (let ((conflits (regles_candidates)))
    (loop while (and (eq nil (verifier_dans_BF but)) (not (null conflits))) do
          (setq conflits (regles_candidates))
          (print conflits)
          (dolist (var conflits)
            (push (conclusion var) *BF*)
            (defparameter *BR* (delete var *BR* :test 'equal :key 'car))            )
          (print *BR*)
          (print *BF*)
          )
    (if (verifier_dans_BF but) (print 'ok))
    )
  )

(chainage_avant '(sport danse))

;-----------------------------------------------------------------------------------------------------------------------
; **** Fonction en chainage avant en largeur d'abord pour donner la liste des sports pratiquables par la personne **** ;
;-----------------------------------------------------------------------------------------------------------------------
(defun chainage_avant2 ()
  (let ((conflits (regles_candidates)))
    (loop while (not (null conflits)) do
          (dolist (var conflits)
            (push (conclusion var) *BF*)
            (setq *BR* (delete var *BR* :test 'equal :key 'car))            
            )
          (setq conflits (regles_candidates))
          )
    (dolist (var *BF*)
      (if (eq 'sport (car var)) (print var))
      )
    )
  )

(chainage_avant2)

;-------------------------------------------------------------------------------------
; **** Fonction en chainage arriere en largeur d'abord pour vérifier un but donné **** ;
;-------------------------------------------------------------------------------------
(defun VERIFIER (but)
  (let ((OK 0) (EC))
    (if (or (verifier2 but) (verifier3 but)) (setq OK 1)
      (progn 
       (setq EC '())
        (dolist (element *BR*) (if (and (eq (car (conclusion (car element))) (car but)) 
                                        (eq (cadr (conclusion (car element))) (cadr but))) 
                                   (push element EC)))
       (dolist (Ri EC)
         (if (equal OK 0)
           (setq OK (VERIFIER_ET Ri))
           )
         )
        ))
    OK
    ))

(defun VERIFIER_ET (regle)
  (let ((OK 1) (premisses))
    (setq premisses (cadr regle))
    (dolist (Pre premisses) 
      (if (equal OK 1)
        (setq OK (VERIFIER Pre))
        )
      )
    OK
    ))

;----------------------------------------------------------------------------------------------------------
; ****  Fonction permettant de poser des questions a l'utilisateur afin de remplir la base de faits **** ;
;----------------------------------------------------------------------------------------------------------

(defun poser_question()
  (let ((result) (value) (symbol))
    (terpri)
    (princ "Sexe (masculin/feminin): ")
    (setq value (read))
    (setq symbol 'sexe)
    (push (list symbol value) result)
    (terpri)
    (princ "Taille (cm): ")
    (setq value (read))
    (setq symbol 'taille)
    (push (list symbol value) result)
    (terpri)
    (princ "Poids (kg): ")
    (setq value (read))
    (setq symbol 'poids)
    (push (list symbol value) result)
    (terpri)
    (princ "Donnez une note à votre puissance physique (de 1 a 10) : ")
    (setq value (read))
    (setq symbol 'puissance)
    (push (list symbol value) result)
    (terpri)
    (princ "Donnez une note à votre resistance durant l'effort (de 1 a 10) : ")
    (setq value (read))
    (setq symbol 'resistance)
    (push (list symbol value) result)
    (terpri)
    (princ "Donnez une note à votre capacité à resister à la fatigue (de 1 a 10) : ")
    (setq value (read))
    (setq symbol 'fatigue)
    (push (list symbol value) result)
    (terpri)
    (princ "Donnez une note à votre reactivite (de 1 a 10) : ")
    (setq value (read))
    (setq symbol 'reactivite)
    (push (list symbol value) result)
    (terpri)
    (princ "Donnez une note à votre explosivite (de 1 a 10) : ")
    (setq value (read))
    (setq symbol 'explosivite)
    (push (list symbol value) result)
    (terpri)
    (princ "Donnez une note à votre habilete (de 1 a 10) : ")
    (setq value (read))
    (setq symbol 'habilete)
    (push (list symbol value) result)
    (terpri)
    (princ "Donnez une note à votre precision (de 1 a 10) : ")
    (setq value (read))
    (setq symbol 'precision)
    (push (list symbol value) result)
    (terpri)
    (princ "Donnez une note à votre amplitude de mouvements possible (de 1 a 10): ")
    (setq value (read))
    (setq symbol 'amplitude)
    (push (list symbol value) result)
    (terpri)
    (princ "Donnez une note à votre flexibilite (de 1 a 10): ")
    (setq value (read))
    (setq symbol 'flexibilite)
    (push (list symbol value) result)
    (terpri)
    (princ "Donnez une note sur votre capacité à produire un effort sur de longues durees (de 1 a 10): ")
    (setq value (read))
    (setq symbol 'duree)
    (push (list symbol value) result)
    (terpri)
    (princ "Donnez une note sur votre extraversion (de 1 a 10): ")
    (setq value (read))
    (setq symbol 'extraversion)
    (push (list symbol value) result)
    (terpri)
    (princ "Donnez une note à votre ouverture d'esprit (de 1 a 10) : ")
    (setq value (read))
    (setq symbol 'ouverture)
    (push (list symbol value) result)
    (terpri)
    (princ "Donnez une note à votre capacité à etre consciencieux (de 1 a 10) : ")
    (setq value (read))
    (setq symbol 'consciencieux)
    (push (list symbol value) result)
    (setf *BF* result))
    (calculIMC)
    )

(poser_question)

;------------------------------------------
; **** Menu de notre Systeme Expert **** ;
;------------------------------------------

(defun menuSE()
  (let ((option) (value) (enquete) (result))
  (terpri)
  (poser_question)
  (terpri)
  (princ "Option 1 : Determiner si une personne est faite pour un sport donné")
  (terpri)
  (princ "Option 2 : Donner la liste des sports possibles pour une personne")
  (terpri)
  (princ "Option 1 ou 2: ")
  (setq option (read))
  (if (equal option 1)
      (progn 
        (terpri)
        (princ "Votre sport: ")
        (setq value (read))
        (setq enquete (list 'sport value))
        (setq result (verifier enquete))
        (if (equal result 1) (princ "Oui!") (princ "Non!"))
        )
    (chainage_avant2)
        )
    )
  )

(menuSE)



