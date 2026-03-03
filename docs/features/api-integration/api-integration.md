# Intégration API

## Description
La feature Intégration API décrit comment le front communique avec le backend via Affjax et le proxy `/api`.

## Objectif
Assurer des appels HTTP cohérents et un encodage JSON stable pour Notes, Checklists et Auth.

## Invariants
- Toute réponse attendue doit être en statut 2xx pour être considérée valide.
- Les entités Notes et Checklists doivent pouvoir être décodées depuis deux formats JSON.
- Les erreurs réseau sont journalisées pour Notes et Checklists.

## Choix techniques
- Affjax pour les appels HTTP avec `ResponseFormat.json` ou `ResponseFormat.string`.
- Encodage manuel via `EncodeJson` pour contrôler le format des payloads.
- Gestion d'erreurs via `ExceptT` et conversion d'erreurs en `FatalError` (Notes/Checklists).

## Flux/API
- Base: appels sur `/api/...` via proxy local.
- Notes: `GET /api/note`, `POST /api/note`, `PUT /api/note`, `DELETE /api/note/:id`.
- Checklists: `GET /api/checklist`, `POST /api/checklist`, `PUT /api/checklist`, `DELETE /api/checklist/:id`.
- Auth: `POST /api/signup`, `POST /api/signin`, `POST /api/signout`.

## États UI
- En cas d'échec: message de feedback dans les formulaires auth.
- Les pages Notes et Checklists restent interactives, les erreurs sont loggées.
