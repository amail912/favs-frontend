# Agenda V1 - Cadrage produit

## Objectif produit
Permettre a un utilisateur connecte de planifier et suivre son quotidien, avec un focus mobile, une vue jour ergonomique, des conflits explicites et une utilisabilite hors ligne.

## Valeur metier cible
- Transformer les intentions en actions planifiees et executees.
- Rendre visibles les conflits de planning et aider a les resoudre.
- Permettre un usage fiable sans reseau puis synchroniser ensuite.

## Persona primaire
- Utilisateur individuel (pas de multi-user en V1).
- Organise des actions ponctuelles et des routines complexes (ex: processus en etapes avec contraintes temporelles).

## Perimetre V1
- `ItemCalendrier` unique cote UI/domaine.
- Types metier distincts:
  - `INTENTION` (a planifier, flexible).
  - `BLOC_PLANIFIE` (creneau concret).
- Routines: templates + instances avec dependances temporelles.
- Recurrence + exceptions.
- Notifications configurables.
- Vues: Jour (prioritaire), Semaine, Mois.
- Import/export: ICS et CSV.
- Full offline avec resolution de conflits a la reconnexion.

## Hors perimetre V1
- Multi-utilisateur et permissions fines.
- Audit complet (on garde `created_at`/`updated_at` seulement).
- Auto-resolution obligatoire des conflits (seulement propositions).
- SMS/email de notification (prepare plus tard cote back).

## Principes UX directeurs
- Mobile-first.
- Drag-and-drop indispensable.
- Granularite minute.
- Raccourcis metier sans entrer en conflit avec VimVixen.
- Un conflit n'est jamais cache: il est visible, filtrable, resolvable.
