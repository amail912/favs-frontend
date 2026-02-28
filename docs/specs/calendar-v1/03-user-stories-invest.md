# Calendar V1 - User stories INVEST priorisees

Note: dans le code et l'UI, le module "Calendar" remplace l'ancien "Agenda".

Priorisation par valeur metier: P0 (essentiel), P1 (forte valeur), P2 (valeur complementaire).

## P0

### US-001 - Creer une intention
En tant qu'utilisateur, je veux creer une intention avec une fenetre temporelle afin de capturer une action a realiser.

Critere d'acceptation:
- Creation reussie si `titre`, `fenetre_debut`, `fenetre_fin` valides.
- L'item apparait dans la vue Jour.

### US-002 - Planifier une intention
En tant qu'utilisateur, je veux creer un bloc planifie a partir d'une intention afin de reserver un creneau concret.

Critere d'acceptation:
- Le bloc planifie cree un lien `source_item_id`.
- L'intention d'origine n'est pas supprimee automatiquement.

### US-003 - Visualiser les conflits
En tant qu'utilisateur, je veux voir immediatement les chevauchements de blocs planifies afin d'eviter les collisions.

Critere d'acceptation:
- Les items en chevauchement sont flagges.
- Un filtre "en conflit" existe dans la vue Jour.

### US-004 - Resoudre un conflit manuellement
En tant qu'utilisateur, je veux une interface de resolution de conflit avec strategies proposees afin de garder le controle final.

Critere d'acceptation:
- L'ecran liste les items du conflit.
- Aucune action irreversible sans confirmation explicite.

### US-005 - Utiliser l'agenda hors ligne
En tant qu'utilisateur, je veux continuer a creer/modifier mes items sans reseau afin que l'app reste fiable partout.

Critere d'acceptation:
- CRUD local possible hors ligne.
- Synchronisation automatique a la reconnexion.
- Si conflit de sync, choix utilisateur requis.

### US-006 - Valider une tache
En tant qu'utilisateur, je veux marquer une tache comme faite afin de suivre mon execution quotidienne.

Critere d'acceptation:
- Passage statut vers `FAIT`.
- Saisie optionnelle de `duree_reelle_minutes`.
- Si non saisie, proposition `start -> now`.

## P1

### US-007 - Vue Jour mobile-first
En tant qu'utilisateur mobile, je veux une timeline verticale fluide afin d'organiser ma journee rapidement.

Critere d'acceptation:
- Affichage timeline minute.
- Drag-and-drop actif sur mobile.
- Tri disponible (statut, categorie, conflit).

### US-008 - Recurrence et exceptions
En tant qu'utilisateur, je veux creer des items recurrents et exclure certaines dates afin d'eviter les ressaisies.

Critere d'acceptation:
- Regles: quotidien, hebdo, mensuel, annuel, tous les X jours.
- Exceptions appliquees a la generation.

### US-009 - Routines en etapes dependantes
En tant qu'utilisateur, je veux instancier une routine avec dependances temporelles entre etapes afin de piloter des processus complexes.

Critere d'acceptation:
- Support des deux dependances V1.
- Modification d'instance sans impact template.

### US-010 - Notifications des intentions non planifiees
En tant qu'utilisateur, je veux etre notifie des intentions non planifiees afin de ne pas rater mes echeances.

Critere d'acceptation:
- Defaut: rappel 06:00 jour de debut + 24h avant fin.
- Surcharge possible par utilisateur et par item.

### US-011 - Templates de taches frequentes
En tant qu'utilisateur, je veux creer des templates reutilisables afin d'accelerer la creation d'items frequents.

Critere d'acceptation:
- Creation/edition/suppression de template.
- Creation d'item pre-rempli depuis template.

## P2

### US-012 - Import CSV
En tant qu'utilisateur, je veux importer mes donnees depuis CSV afin de migrer un historique existant.

Critere d'acceptation:
- Colonnes minimales supportees.
- Rapport d'erreurs de lignes invalide.

### US-013 - Import ICS
En tant qu'utilisateur, je veux importer un calendrier ICS afin de recuperer mes elements existants.

Critere d'acceptation:
- Mapping des champs dates/titre/description/categorie.
- Rapport des elements ignores.

### US-014 - Export ICS/CSV filtre
En tant qu'utilisateur, je veux exporter mes items avec filtres afin de partager ou archiver un sous-ensemble.

Critere d'acceptation:
- Formats ICS et CSV.
- Filtres par type, categorie, statut, periode.

### US-015 - Vues Semaine et Mois
En tant qu'utilisateur, je veux completer la vue Jour par Semaine et Mois afin de planifier a plus long terme.

Critere d'acceptation:
- Navigation fluide Jour/Semaine/Mois.
- Representation coherente des conflits et statuts.
