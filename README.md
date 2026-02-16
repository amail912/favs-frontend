# favs-frontend

Application front-end en **PureScript + Halogen** pour gérer son quotidien

## Stack technique

- PureScript
- Spago
- Halogen
- Affjax (appels HTTP)
- Express + `http-proxy-middleware` (proxy local de développement)

## Prérequis

- Debian (ou Linux compatible) avec `git` installé
- droits `sudo` (ou session root) pour l'installation automatique

## Installation

### Installation recommandée (from scratch)

```bash
scripts/setup.sh
```

Ce script installe Node.js/npm via les dépôts Debian, les dépendances système Playwright, les dépendances npm du projet (dont `spago` et `purescript`), et Chromium pour les tests E2E.

Documentation complète :

- `docs/setup-debian-from-scratch.md`
- `docs/setup-script-reference.md`

### Installation manuelle (alternative)

```bash
npm install
```

## Scripts disponibles

- `npm run build` : compile et bundle l'application dans `static/index.js`
- `npm run start-dev` : lance un serveur local sur `http://localhost:1234` avec proxy API vers `http://localhost:8081`
- `npm run start-dev-proxy-prod` : lance le même serveur local, avec proxy API vers l’URL de prod configurée dans `dev/proxy-server-prod.js`
- `npm test` : lance `spago test`

## Lancer le projet en local

1. Lancer le backend attendu sur `http://localhost:8081`.
2. Compiler le front :

```bash
npm run build
```

3. Démarrer le proxy/serveur statique :

```bash
npm run start-dev
```

4. Ouvrir : `http://localhost:1234`.

## Architecture du code

- `src/Main.purs` : point d’entrée Halogen
- `src/App.purs` : composant racine + routing browser (pushState)
- `src/Notes.purs` : module Notes (affichage/édition/suppression)
- `src/Checklists.purs` : module Checklists (affichage/édition/suppression)
- `src/Utils.purs` : helpers UI (ex: classes CSS)
- `dev/` : serveurs Express de proxy dev/prod
- `static/` : assets statiques (HTML, CSS, Bootstrap, bundle)

## Routing

Le front utilise des routes navigateur (history API) :

- `/` (redirection vers `/notes`)
- `/notes`
- `/checklists`
- `/signup`
- `/signin`

## API attendue

Le front appelle les endpoints suivants (préfixés localement par `/api` via le proxy) :

### Notes

- `GET /note`
- `POST /note`
- `PUT /note`
- `DELETE /note/:id`

### Checklists

- `GET /checklist`
- `POST /checklist`
- `PUT /checklist`
- `DELETE /checklist/:id`

## Tests

```bash
npm test
```

> `spago` est installé localement via `npm ci` (pas besoin d'installation globale).

## Tests E2E (Playwright)

Pré-requis:
- backend disponible sur `http://localhost:8081`

Commandes:

```bash
npm run e2e:install
npm run e2e
```

Options utiles:

```bash
npm run e2e:headed
npm run e2e:ui
```

## Remarques

- Le dossier `docs/` contient une analyse initiale du dépôt (`docs/repo-analysis.md`) avec des pistes d'amélioration priorisées.
