# Checklists

## Description
La feature Checklists permet de gérer des listes de tâches. Chaque checklist a un nom et une liste d'items, édités inline.

## Objectif
Faciliter la création rapide de listes, avec édition directe et persistance automatique.

## Invariants
- Une checklist est soit `NewChecklist` sans `storageId`, soit `ServerChecklist` avec `storageId`.
- Une checklist nouvelle doit être persistée via `POST`, une checklist existante via `PUT`.
- Les changements de nom ou de libellé d'item déclenchent une sauvegarde immédiatement.
- La suppression d'un item recalcule la checklist puis la sauvegarde.

## Choix techniques
- Modèle en somme pour le choix `POST` vs `PUT`.
- Les items sont stockés dans `ChecklistItem` pour garder le label et l'état checked.
- Édition inline avec `editingState` et focus automatique sur l'input en cours d'édition.

## Flux/API
- Chargement initial: `GET /api/checklist`.
- Création: ajout d'un `NewChecklist` local, sauvegarde lors de la première modification.
- Mise à jour: `PUT /api/checklist` sur chaque changement.
- Suppression checklist: `DELETE /api/checklist/:id`.
- Suppression item: suppression locale, puis sauvegarde de la checklist.

## États UI
- Aucun contenu: message "No checklist yet".
- Édition nom: input focus.
- Édition item: input inline par item, focus automatique.
- Après sauvegarde: retour à `editingState = None`.
