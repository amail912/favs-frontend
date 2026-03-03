# Authentication

## Description
La feature Authentication couvre l'inscription, la connexion et la déconnexion, ainsi que la validation locale des champs.

## Objectif
Permettre à un utilisateur de créer un compte, se connecter, puis quitter la session, avec un feedback clair sur les erreurs.

## Invariants
- Un `username` est non vide, entre 3 et 32 caractères, et ne contient que lettres, chiffres, '.', '_' ou '-'.
- Un `password` est non vide et fait au moins 12 caractères.
- Les actions réussies naviguent automatiquement vers l'écran cible.

## Choix techniques
- Validation côté client avant envoi pour éviter des requêtes inutiles.
- Feedback texte dans le formulaire en cas d'erreur réseau ou statut non 2xx.
- Probe auth basée sur `GET /api/note` pour déterminer `isAuthenticated`.

## Flux/API
- Signup: `POST /api/signup` avec `{ username, password }`.
- Signin: `POST /api/signin` avec `{ username, password }`.
- Signout: `POST /api/signout`.
- Vérification auth: `GET /api/note` au démarrage pour définir l'état.

## Navigation après succès
- Signup réussi: redirection vers Signin.
- Signin réussi: redirection vers Notes.
- Signout: redirection vers Signup.

## États UI
- Formulaires signup et signin avec validation inline.
- État de soumission: bouton désactivé et libellé "Submitting..." ou "Signing in...".
- En cas de succès: navigation automatique vers la vue suivante.
