(defvar plan nil "liste d'intervals faits de sous-intervals")
(defvar base (make-hash-table :test 'equal) "Hash indexé par id-sub d'intervals contenant des sous-intervals types")
(defvar sequence nil "Séquence en cours de saisie (liste de sub-intervalles)")
(defvar dest "~/elisp/plans" "destination pour les fichiers de l'application")
(defvar base-file "~/elisp/plans/base.dat" "Fichier de base de subintervals")
(defvar train-file "~/elisp/plans/train.dat" "Fichier d'entrainement")

;; un plan est une liste d'intervalles
;; un intervalle est (nom rep subs)
;; seq est une liste de subs
;; subs est une liste de sub
;; un sub est (nom duree puissance cadence-min cadence-max

;;------------------------------------------------------- Utilitaires
;; highlight the current line; set a custom face, so we can
;; recognize from the normal marking (selection) - cf helm-color
(defface hl-line '((t (:background "DarkSlateGray")))
  "Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hl-line)
;;(global-hl-line-mode t)
(set-face-attribute 'hl-line nil :foreground "black" :background "yellow")

(defun chk-file (nom)
  "Vérifie que le nom passé a un chemin complet, sinon ajoute le
   répertoire par défaut"
  (cond ((string-match-p "^[/~]" nom) nom)
	((string-match-p "/$" dest) (format "%s%s" dest nom))
	(t (format "%s/%s" dest nom))))

(defun id-sub (nom duree)
  "Calcule un id-sub du type Nom - Durée avec Durée en version human readable"
  (format "%s - %s" (duree-h duree) nom))

(defun id-sub-auto (sub)
  (id-sub (cl-getf sub 'nom)
	  (cl-getf sub 'duree)))

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

(defun base-titres ()
  "Construction de la liste des titres des sub dans base"
  (nreverse (hash-table-keys base)))

(defun base-titre-n (n)
  "Retourne le titre d'un sub à partir de son no"
  (nth (- n 1) (base-titres)))

(defun lire-repertoire (&optional prompt)
  (interactive)
  (setq default-directory
	(transient-read-existing-directory
	 (or prompt "Répertoire: ") default-directory nil)))

(defun lire-fichier (&optional prompt def)
  (interactive)
  (transient-read-existing-file
   (or prompt "Fichier: ") def nil))

(defun permute (liste n1 n2)
  "permute le contenu de la liste aux positions n1 et n2 (en commençant à 0)"
  (when (or (< n1 0) (>= n2 (length liste)))
    (error "décalage impossible: fin de la liste"))
  (cl-rotatef (nth n1 liste)
	      (nth n2 liste))
  liste)

(defun glisse (liste n &optional reverse)
  "Déplace l'élément n vers la fin de la liste (le début si reverse)"
  (if reverse
       (permute liste (1- n) n)
    (permute liste n (1+ n))))

;;------------------------------------------------------- Bases
(defun train-base-edit (sub)
  "Edition du sub"
  (switch-to-buffer (get-buffer-create "*wahoo-sub*"))
  (erase-buffer)
  (display-assoc-form sub)
  (beginning-of-buffer)
  (move-end-of-line nil)
  (base-form-mode 1))

(defun train-base-modif-line ()
  "Modifie le sub de la ligne courante (no ligne de la base))"
  (interactive)
  (let (;;(nom (buffer-name (current-buffer)))
	(titre (base-titre-n (line-number-at-pos))))
    (if titre
	(train-base-edit (gethash titre base ""))
      (train-base-insert-line))))

(defun train-base-insert-line ()
  (interactive)
  (train-base-edit ((nom "" duree 60 puissance 70 cadence-min 50 cadence-max 80))))

(defun train-base-delete-line ()
  "Supprpime le sub de la ligne courante (no ligne de la base))"
  (interactive)
  (let (;;(nom (buffer-name (current-buffer)))
	(titre (base-titre-n (line-number-at-pos))))
    (remhash titre base))
  (display-base))

(defun seq-up ()
  "Remonte la ligne courante dans la liste de la sequence et reste sur la bonne ligne du buffer"
  (interactive)
  (let ((ligne (line-number-at-pos)))
    (glisse sequence (1- ligne) t)
    (display-sequence sequence)
    (switch-to-buffer "*wahoo-seq*")
    (goto-line (1- ligne))))

(defun seq-down ()
  "Descend la ligne courante dans la liste de la sequence"
  (interactive)
  (let ((ligne (line-number-at-pos)))
    (glisse sequence (1- ligne) nil)
    (display-sequence sequence)
    (switch-to-buffer "*wahoo-seq*")
    (goto-line (1- ligne))))

(defun seq-retire ()
  "Supprime la ligne courante de la sequence"
  (interactive)
  (seq-remove-at-position sequence (line-number-at-pos))
  (display-sequence sequence))

;;------------------------------------------------------- Réserve
(defun liste-champs (propli)
  "Liste des noms de champs d'une property list"
  (if (null propli)
      nil
    (cons (car propli) (liste-champs (cddr propli)))))

(defun modif-champ-sub (nom sub)
  "OBSOLETE Modifie le champ nom du sub dans base.
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
		     (id-sub-auto autre-sub))))
    (setf (gethash autre-id
		   base)
	  autre-sub)
    autre-id))

;;------------------------------------------------------- Manipulation des séquences

;;------------------------------------------------------- Lecture/Sauvegardes
(defun vide-plan ()
  (interactive)
  (setf plan nil)
  'plan-vide)

(defun sauve-base (&optional fic)
  (interactive)
  (let ((desti (chk-file (or fic base-file))))
    (with-temp-file desti
      (let ((curb (current-buffer))) ; Nécesaire ?
	(maphash (lambda (id tout)
		   (prin1 id curb) (terpri curb)
		   (prin1 tout curb) (terpri curb))
		 base)))
    (message "Enregistré dans %s" desti)))

(defun lire-base (&optional fic)
  "Lecture des sous-intervals depuis un fichier"
  (interactive)
  (clrhash base)
  (find-file (chk-file (or fic base-file)))
  (let ((curb (current-buffer)))
    (beginning-of-buffer)
    (while (null (eobp))
      (let ((id (read curb)))
	;;(forward-line 1)
	(setf (gethash id base) (read curb))
	(forward-line 1)
	))
    (kill-buffer curb)
    (message "%s chargé" (chk-file (or fic base-file)))
    (display-base)))

(defun lire-train (&optional fic)
  (interactive f"Nom du fichier d'entrainement")
  (clrhash base)
  (find-file (chk-file (or fic train-file)))
  (let ((curb (current-buffer)))
    (beginning-of-buffer)
    (while (null (eobp))
      (let ((id (read curb)))
	(forward-line 1)
	(setf (gethash id base) (read curb))
	(forward-line 1)))
    (kill-buffer curb)
    (train-titres)))

(defun sauve-train (&optional fic)
  (interactive f"Nom du fichier d'entrainement")
  (let ((desti (chk-file (or fic train-file))))
    (with-temp-file desti
      (let ((curb (current-buffer)))
	(maphash (lambda (id tout)
		   (print id curb)
		   (print tout curb))
		 base)))
    (message "Enregistré dans %s" desti)))

;;------------------------------------------------------- Commandes autres
(defun insert-train ()
  "INUTILE - était fait pour une interface précédente !"
  (interactive)
  (let ((la-seq)))
  (while (setq choix (choisir-sub))
    (if choix
	(push choix la-seq)
      (progn
	(push ((read-string "Nombre de répétitions: " 1) . la-seq) plan)
	(display-plan)
	))
    (display-sequence la-seq)))

(defun train-modify ()
  "INUTILE ?"
  (interactive)
  (let ((nom (buffer-name (current-buffer)))
	(ligne (line-number-at-pos)))
    (cond 
     ((string= "*wahoo-base*" nom) (train-base-modif ligne (base-titre-n ligne)))
     (t (message "ChaipassekeC")))))

(defun mv-down-train ()
  "INUTILE"
  (interactive)
  (message "mv-down-train"))

;;------------------------------------------------------- Transcient
(transient-define-prefix train-transient ()
  "Choix initial pour la gestion des entrainements."
  [["Fichiers:"
    ("c" "Charger le fichier d'entrainement" lire-train)
    ("s" "Sauver le fichier d'entrainement" sauve-train)
    ("C" "Charger le fichier de sous-intervals" lire-base)
    ("S" "Sauver le fichier de sous-intervals" sauve-base)]
   ["Paramètres:"
    ("r" "répertoire courant" select-dest)
    (:info (lambda () (format "répertoire: %s" dest)))
    ("b" "fichier de Base de sous-intervals" select-base)
    (:info (lambda () (format "base: %s" base-file)))
    ("e" "fichier d'entrainement" select-train)
    (:info (lambda () (format "entrainement: %s" train-file)))
    ("k" "garder ces paramètres par valeur par défaut" transient-save)]
   ["Autre:"
    ("a" "show Arguments" dbg-args)
    ("q" "Quitter" transient-quit-all)]])
(global-set-key (kbd "<f8>") 'train-transient)

(transient-define-prefix train-base-transient ()
  "Choix initial pour la gestion de la base de sous-intervals."
  ;; TODO: A adapter à partir des touches définies par le minor mode train-base
  [["Fichiers:"
    ("c" "Charger le fichier d'entrainement" lire-train)
    ("s" "Sauver le fichier d'entrainement" sauve-train)
    ("C" "Charger le fichier de sous-intervals" lire-base)
    ("S" "Sauver le fichier de sous-intervals" sauve-base)]
   ["Paramètres:"
    ("r" "répertoire courant" select-dest)
    (:info (lambda () (format "répertoire: %s" dest)))
    ("b" "fichier de Base de sous-intervals" select-base)
    (:info (lambda () (format "base: %s" base-file)))
    ("e" "fichier d'entrainement" select-train)
    (:info (lambda () (format "entrainement: %s" train-file)))
    ("k" "garder ces paramètres par valeur par défaut" transient-save)]
   ["Autre:"
    ("q" "Quitter" transient-quit-all)]])


(transient-define-suffix select-dest ()
  "Selection d'un répertoire de travail"
  :transient t
  (interactive)
  (setf dest (lire-repertoire "Répertoire de travail: "))
  (setf default-directory dest))

(transient-define-suffix select-base ()
  "Selection d'un fichier de Base"
  :transient t
  (interactive)
  (setf base-file (lire-fichier "Base: " base-file)))

(transient-define-suffix select-train ()
  "Selection d'un fichier d'Entrainement"
  :transient t
  (interactive)
  (setf base-file (lire-fichier "Entrainement: " train-file)))

(transient-define-infix transient-ent-file-option ()
  :description "Fichier"
  :class 'transient-option
  :shortarg "e"
  :argument "--ent="
  :choices (directory-files dest))

(defun dbg-args (&optional fic)
  "Juste pour vérifier ce qu'on fait passer depuis train-transient"
  (interactive (list (transient-args 'train-transient)))
  (message "Fichier: %s"
	   (if (listp fic)
	       (replace-regexp-in-string "--fic=" "" (car fic))
	     fic)))

(transient-define-prefix train-transient ()
  "Aide sur les touches et menus pour le mode wahoo-base"
  [["Fichiers:"
    ("c" "Charger le fichier de sous-intervals" lire-base)
    ("s" "Sauver le fichier de sous-intervals" sauve-base)
    ("e" "Afficher la séquence en cours" (lambda () (interactive) (switch-to-buffer "*wahoo-seq*")))
    ("E" "Commencer une nouvelle séquence" display-sequence)]
   ["Paramètres:"
    ("r" "répertoire courant" select-dest)
    (:info (lambda () (format "répertoire: %s" dest)))
    ("b" "fichier de Base de sous-intervals" select-base)
    (:info (lambda () (format "base: %s" base-file)))
    ("k" "garder ces paramètres par valeur par défaut" transient-save)]
   ["Aide touches sous-intervalles"
    ("?" "Quitter cette aide" transient-quit-one)
    ("m" "Modifier sous-intervalle" train-base-modif-line)
    ("d" "Supprimer sous-intervalle" train-base-delete-line)
    ("i" "Insérer un sous-intervalle" train-base-insert-line)
   ["Aide touches séquence"
    ("n" "Commencer une séquence" train-seq-new)
    ("a" "Ajouter à la séquence" train-seq-add)
    ("S-<up>" "Remonter un sous-intervalle" seq-up)
    ("S-<down>" "Redescendre un sous-intervalle" seq-down)]
    ("<delete>" "Supprimer ligne de la séquence" seq-retire)]])

(transient-define-prefix train-form-aide ()
  "Aide sur les touches du formulaire d'un sub (sous-intervalle)"
  [["Aide touches édition d'un sous-intervalle"
    ("?" "Sortir de cette aide" transient-quit-one)
    ("<up>" "Paramètre précédent" base-form-up)
    ("<down>" "Paramètre suivant" base-form-down)
    ("<del>" "Effacer la valeur actuelle" train-del-base-param)
    ("<return>" "Valider le formulaire" (lambda () (interactive) (base-form-exit nil)))
    ("<escape>" "Abandon des modifications" (lambda () (interactive) (base-form-exit t)))]])

;;------------------------------------------------------- Affichage
(defun display-sequence (une-seq)
  "Affiche le détail d'une séquence dans le buffer dédié *wahoo-sequence*"
  (interactive "sSequence")
  ;;(save-current-buffer XXX) ; si on veut retourner à l'affichage précédent
  (switch-to-buffer (get-buffer-create "*wahoo-seq*"))
  (erase-buffer)
  (train-seq-mode 0)
  (read-only-mode 0)
  (erase-buffer)
  (mapc (lambda (e)
	  (insert (format "%s\n" (id-sub-auto e)))) ; TODO: plutôt afficher l'ID du sub que son détail !
	une-seq)
  (train-seq-mode 1)
  ;; retourner à la fenêtre qui nous a appellé, soit la base des sub
  (other-window 1))

(defun display-plan ()
  "Affiche le plan dans le buffer dédié *wahoo-plan*"
  (interactive)
  (switch-to-buffer (get-buffer-create "*wahoo-plan*"))
  (erase-buffer)
  ;; TODO: affiche le plan
  )

(defun display-base ()
  "Affiche les sous-intervals dans le buffer dédié *wahoo-base*"
  (interactive)
  (switch-to-buffer (get-buffer-create "*wahoo-base*"))
  (train-base-mode 0)
  (read-only-mode 0)
  (erase-buffer)
  (maphash (lambda (k v)
	     (insert (format "%-25s => %s\n" k v)))
   base)
  (train-base-mode 1)
  (beginning-of-buffer))

(defun display-assoc-form (asso)
  "Affiche un formulaire pour saisir les champs de asso (récursivement)"
  (cond ((null asso) nil)
    (t (insert (format "%15s: %s\n" (symbol-name (car asso))
		       (cadr asso)))
       (display-assoc-form (cddr asso)))))

(defun get-assoc-form-field (ligne)
  "transforme la ligne passée en property en (list nom-champ valeur).
La valeur est numérique sauf pour nom."
  (cl-assert (string-match " *\\(.*\\): \\(.*\\)$" ligne))
  (let ((k (intern (match-string 1 ligne)))
	(v (match-string 2 ligne)))
    (list k ; k est par exemple 'nom et on retourne '(nom "Unijamb") 
	  (if (equal k 'nom)
	      v
	    (string-to-number v)))))

(defun get-assoc-form ()
  "Contruit le sub correspondant à la saisie utilisateur (aurait besoin d'un reader spécifique ?)"
  (switch-to-buffer "*wahoo-sub*")
  (beginning-of-buffer)
  (setf res nil)
  (while (not (eobp))
    (push  (get-assoc-form-field
	    (thing-at-point 'line t))
	   res)
    (next-line))
  (message "le sub saisi est %s" (flatten-tree (reverse res)))
  (flatten-tree (reverse res)))

;;------------------------------------------------------- Formulaire base
(defun base-store-sub (a-sub)
  (cl-assert (length a-sub))
  (setf (gethash (id-sub-auto a-sub) base) a-sub))

(defun base-form-exit (abort)
  "Abandonner les modifications et réafficher le buffer"
  (unless abort
    ;; TODO: Si le nom a ét modifié, supprimer l'ancien sub !
    (base-store-sub (get-assoc-form))    
    (message "Sauvegarde du formulaire dans la base issue de %s" base-file))
  (kill-buffer)
  (display-base))

(defun base-form-up ()
  (interactive)
  (previous-line)
  (move-end-of-line nil))

(defun base-form-down ()
  (interactive)
  (next-line)
  (move-end-of-line nil))

(defun base-form-right ()
  "Se déplacer à droite, pas plus loin que la fin de la ligne"
  (interactive)
  (let ((curli (line-number-at-pos)))
    (right-char)
    (unless (= curli (line-number-at-pos))
      (left-char))))

(defun base-form-left ()
  "Se déplacer à gauche, pas plus loin que le : "
  (interactive)
  (let ((but
	 (save-excursion
	   (move-end-of-line nil)
	   (search-backward ": " nil t)
	   (point))))
    (unless (< (point) (+ 3 but))
	(left-char))))

(defun train-del-base-param ()
  "Vider complètement le paramètre courant"
  (interactive)
  (move-end-of-line nil)
  (search-backward ": ") (right-char 2) (kill-line))

(defun train-base-insert-line ()
  "Saisie pour l'ajout d'un sous-interval dans la base"
  (interactive)
  (train-base-edit '(nom "" duree 0 puissance 60 cadence-min 60 cadence-max 80)))

(defun train-seq-new ()
  "Commencer une nouvelle séquence"
  (interactive)
  (delete-other-windows)
  (split-window-below)
  (setf sequence nil)
  (display-sequence sequence))

(defun train-seq-add ()
  "Ajouter le sub de la ligne courante de la base à la fin de la séquence courante"
  ;; TODO: éviter d'échanger les positions des 2 fenêtres base et sequence
  (interactive)
  (let ((titre (base-titre-n (line-number-at-pos))))
    (unless titre (error "Pas de titre de sous-intervalle sur cette ligne"))
    (setf sequence (reverse (cons (gethash titre base "") (reverse sequence))))
    (display-sequence sequence)) ; Retourner dans le buffer *wahoo-base*
  (switch-to-buffer "*wahoo-base*"))

;;------------------------------------------------------- Minor-modes
;;;###autoload
(define-minor-mode train-base-mode
  "Edition de la base wahoo"
  :lighter " train-base"
  :after-hook (hl-line-mode 1)
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "?") 'train-base-transient)
	    (define-key map (kbd "<home>") 'beginning-of-buffer)
	    (define-key map (kbd "<end>") 'end-of-buffer)
	    (define-key map (kbd "m") 'train-base-modif-line)
	    (define-key map (kbd "i") 'train-base-insert-line)
	    (define-key map (kbd "d") 'train-base-delete-line)
	    (define-key map (kbd "s") 'sauve-base)
	    (define-key map (kbd "a") 'train-seq-add)
	    ;; TODO: f pour filter et ne montrer que les subs qui matchent
	    (define-key map (kbd "n") 'train-seq-new)
	    map)
  :after-hook (progn
		(read-only-mode 1)
		(message "Entré dans le mode train-base-mode")))

(define-minor-mode base-form-mode
  "Edition d'un sous-intervalle de la base"
  :lighter " base-form"
  :after-hook (hl-line-mode 1)
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<f8>") 'train-transient)
	    (define-key map (kbd "?") 'train-form-aide)
            (define-key map (kbd "<up>") 'base-form-up)
            (define-key map (kbd "<down>") 'base-form-down)
            (define-key map (kbd "<left>") 'base-form-left)
            (define-key map (kbd "<right>") 'base-form-right)
	    (define-key map (kbd "<delete>") 'train-del-base-param)
            (define-key map (kbd "<return>") (lambda () (interactive) (base-form-exit nil)))
            (define-key map (kbd "<escape>") (lambda () (interactive) (base-form-exit t)))
            map))

(define-minor-mode train-seq-mode
  "Edition d'une séquence de plusieurs subs (sous-intervalles)"
  :lighter " train-seq"
  :after-hook (hl-line-mode 1)
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<f8>") 'train-transient)
	    (define-key map (kbd "?") 'train-seq-transient)
            (define-key map (kbd "i") 'seq-insert)
            (define-key map (kbd "S-<up>") 'seq-up)
            (define-key map (kbd "S-<down>") 'seq-down)
	    (define-key map (kbd "<delete>") 'seq-del)
            (define-key map (kbd "<escape>") 'seq-abort)
            (define-key map (kbd "<return>") 'seq-val)
            map))
;;;###autoload
;;(add-hook 'text-mode-hook 'foo-mode)

(provide 'train-base-mode)
