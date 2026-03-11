# Notes

## Description
La feature Notes permet de consulter, créer, éditer et supprimer des notes. Une note contient un titre et un contenu libre. L'édition est inline dans la liste.

## Objectif
Fournir un flux rapide de prise de note avec sauvegarde transparente côté serveur et retour d'état minimal dans l'UI.

## Invariants
- Une note est soit `NewNote` sans `storageId`, soit `ServerNote` avec `storageId`.
- Une note nouvelle doit être persistée via `POST`, une note existante via `PUT`.
- Les modifications de titre ou de contenu déclenchent une sauvegarde immédiatement.
- La suppression n'est possible que pour une note avec `storageId`.

## Choix techniques
- Modèle `Note` en somme (`NewNote` ou `ServerNote`) pour simplifier le choix `POST` vs `PUT`.
- Encodage JSON tolérant à deux formats en lecture.
- Éditeur inline basé sur `editingState` avec focus et scroll automatiques vers l'élément édité.

## Flux/API
- Chargement initial: `GET /api/note` puis décodage JSON.
- Création: ajout d'un `NewNote` local puis sauvegarde via `POST /api/note` sur modification.
- Mise à jour: `PUT /api/note` à chaque changement de champ.
- Suppression: `DELETE /api/note/:id`.
- Rechargement: après chaque écriture, une nouvelle lecture est faite pour resynchroniser.

## États UI
- Aucun contenu: message "No notes yet".
- Édition titre: champ input focus.
- Édition contenu: zone textarea focus.
- Après sauvegarde: retour à `editingState = None`.
