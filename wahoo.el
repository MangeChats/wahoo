(defvar plan nil "liste d'intervalles faits de sous-intervalles")
(defvar base (make-hash-table :test 'equal) "Hash indexé par id-sub d'intervalles contenant des sous-intervalles types")
(defvar dest "~/elisp/plans" "destination pour les fichiers de l'application")

;; un plan est une liste d'intervalles
;; un intervalle est (:nom :rep :subs)
;; seq est une liste de subs
;; subs est une liste de sub
;; un sub est (:nom :duree :puissance :cadence-min :cadence-max)

;;;------------------------------------- utilitaires

(defun chk-file (nom)
  "Vérifie que le nom passé a un chemin complet, sinon ajoute le
   répertoire par défaut"
  (cond ((string-match-p "^/" nom) nom)
	(t (format "%s/%s" dest nom))))

(defun id-sub (nom duree)
  "Calcule un id-sub du type Nom - "
  (format "%s - %s" (duree-h duree) nom))

(defun duree-h (secondes)
  "Formate une durée en seconde pour être du type HH:MM'SS\" ou MM'SS\"
  secondes peut être un entier ou une string"
  (let* ((hr (floor secondes 3600))
	 (r-sec (%  secondes 3600))
	 (min   (floor r-sec 60))
	 (sec   (% r-sec 60)))
    (if (zerop hr)
	(format "%d'%02d\"" min sec)
      (format "%d:%02d'%02d\"" hr min sec))))

(defun titres (a-base)
  "Construction de la liste des titres des sub de a-base"
  (hash-table-keys a-base))

(defun liste-champs (propli)
  "Liste des noms de champs d'une property list"
  (if (null propli)
      nil
    (cons (car propli) (liste-champs (cddr propli)))))

(defun modif-champ-sub (nom sub)
  "Modifie le champ nom du sub dans base.
   Si le nom ou la durée sont modifiés, crée une nouvelle entrée
   retourne l'ID du sub une fois modifié"
  (let* ((autre-sub (copy-tree sub))
	 (val (cl-getf sub (intern-soft nom)))
	 (le-prompt (format "%s (%s) = " nom val))
	 (autre-val
	  (if (numberp val)
	      (or (read-number le-prompt) val)
	    (or (read-string le-prompt) val)))
	 (autre-id (progn
		     (setf (cl-getf autre-sub (intern-soft nom)) autre-val)
		     (id-sub (cl-getf autre-sub :nom)
			     (cl-getf autre-sub :duree)))))
    (setf (gethash autre-id
		   base)
	  autre-sub)
    autre-id))

;;;------------------------------------- Commandes

(defun vide-plan ()
  (interactive)
  (setf plan nil)
  'plan-vide)

(defun create-sub-intervalle (nom duree)
  (interactive "sNom: \ndDuree en secondes: ")
  (setf (gethash
	 (id-sub nom duree)
	 base)
	(list
	 :nom nom
	 :duree duree
	 :puissance (read-number "Puissance: ")
	 :cadence-min (read-number "Cadence minimum: ")
	 :cadence-max (read-number "Cadence maximum: "))))

(defun sauve-base (&optional fic)
  (interactive)
  (let ((desti (chk-file (or fic "base.dat"))))
    (with-temp-file desti
      (let ((curb (current-buffer)))
	(maphash (lambda (id tout)
		   (print id curb)
		   (print tout curb))
		 base)))
    (message "Enregistré dans %s" desti)))

(defun lire-base (&optional fic)
  (interactive)
  (clrhash base)
  (find-file (chk-file (or fic "base.dat")))
  (let ((curb (current-buffer)))
    (beginning-of-buffer)
    (while (null (eobp))
      (let ((id (read curb)))
	(forward-line 1)
	(setf (gethash id base) (read curb))
	(forward-line 1)))
    (kill-buffer curb)
    (titres base)))

(defun modifier-sub (id)
  "Utiliser helm pour choisir un champ à modifier, puis reboucler jusqu'à modifier/dupliquer par Validez"
  (let* ((default (gethash id base))
	 (champs (liste-champs default))
	 (champs-src '((name . "Sélectionnez un champ ou actions avec F2, F3 (TAB Aide)")
		       (candidates . champs)
		       (action . (("Modifier le champ" .
				   (lambda (x)
				     (modifier-sub (modif-champ-sub x default))))
				  ("Valider sous-intervalle" .
				   (lambda (x)
				     (ignore x)
				     (gethash id base))))))))
    (helm :sources champs-src :prompt "Filtre de sélection champ: ")))

(defun choisir-sub ()
  "Utiliser helm pour sélectionner un sous-intervalle de la base"
  (interactive)
  (let* ((liste-titres (titres base))
	 (sub-src '((name . "Sélectionnez un sous-intervalle (sub)")
		    (candidates . liste-titres)
		    (action . (
			       ("Retourner le sub" . (lambda (x) (message "Choix %s => %s" x (gethash x base))
						       (gethash x base)))
			       ("Voir le détail" . (lambda (x)
						     (read-string (format "%s => %s " x (gethash x base)))
						     (choisir-sub)))
			       ("Retourner rien (arrêter)" . (lambda (x) nil))
			       ("Modifier le sub" . (lambda (x) (message "Modifier %s" x)
						      (modifier-sub x)
						      (choisir-sub)))
			       ("Supprimer le sub" . (lambda (x)
						       (message "%s Supprimé" x)
						       (remhash x base)
						       (choisir-sub))))))))
    (helm :sources sub-src :prompt "Filtre de sélection sous-intervalle (sub): ")))

(defun liste-base ()
  (interactive)
  (maphash
   (lambda (id val)
     (print (format "%s => %s" id val)))
   base))

(defun display-plan ()
  "Affiche le plan dans le buffer dédié *wahoo-plan* en version compacte"
  (interactive)
  (switch-to-buffer (get-buffer-create "*wahoo-plan*"))
  (erase-buffer)
  (insert "Le plan est actuellement... pas encore affichable !"))

(defun aff-sub (s)
  (insert (format "\t%s\n" s)))

(defun display-intervalle (un-intervalle)
  "Affiche un intervalle dans le buffer dédié *wahoo-intervalle* en version détaillée"
  (interactive)
  ;;(save-current-buffer XXX) ; si on veut retourner à l'affichage précédent
  (switch-to-buffer (get-buffer-create "*wahoo-intervalle*"))
  (erase-buffer)
  (insert (format "%s répété %d fois:\n\t"
		  (cl-getf un-intervalle :nom) ; TODO: vérif récup champ nom dans un-intervalle
		  (cl-getf un-intervalle :rep))
	  (mapc 'aff-sub (cl-getf un-intervalle :subs))))

(defun ajoute-intervalle ()
  "Ajoute au plan un intervalle fait de plusieurs sub et une répétition pour le tout.
   Méthode itérative avec helm, manque de vue d'ensemble
   Remplacer par un mode aff intervalle"
  (interactive)
  (let ((un-intervalle)) ;; Invervalle en cours de construction
    (edebug)
    (while (setq choix (choisir-sub))
      (if choix
	  (push choix un-intervalle)
	(progn
	  (edebug)
	  (push ((read-string "Nombre de répétitions: " 1) . un-intervalle) plan)
	  (display-plan)
	  (return)))
      (display-intervalle un-intervalle))))

;;-------------------------------------- Minor modes
(define-minor-mode base-mode
  "Mode dédié à l'affichage et modification de la base de subs"
  :lighter " subs"
  :after-hook (hl-line-mode 1)
  :keymmap (let ((map (make-sparse-keymap)))
	     (define-key map (kbd "<home>") 'beginning-of-buffer)
	     (define-key map (kbd "<end>") 'end-of-buffer)
	     (define-key map (kbd "i" 'insert-sub)) ; en restant sur les subs
	     (define-key map (kbd "I" 'insert-sub)) ; puis retour au plan
	     (define-key map (kbd "m" 'modif-sub))
	     (define-key map (kbd "d" 'delete-sub))
	     ))

