# favs-frontend

Application front-end en **PureScript + Halogen** pour gérer des favoris sous forme de **notes** et de **checklists**.

## Stack technique

- PureScript
- Spago
- Halogen
- Affjax (appels HTTP)
- Express + `http-proxy-middleware` (proxy local de développement)

## Prérequis

- Node.js (version LTS recommandée)
- npm
- Outils PureScript :
  - `purescript`
  - `spago`

Exemple d'installation globale des outils PureScript :

```bash
npm install -g purescript spago
```

## Installation

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
- `src/App.purs` : composant racine + routing hash
- `src/Notes.purs` : module Notes (affichage/édition/suppression)
- `src/Checklists.purs` : module Checklists (affichage/édition/suppression)
- `src/Utils.purs` : helpers UI (ex: classes CSS)
- `dev/` : serveurs Express de proxy dev/prod
- `static/` : assets statiques (HTML, CSS, Bootstrap, bundle)

## Routing

Le front utilise un routing hash avec ces routes :

- `#/notes`
- `#/checklists`
- `#/signup`

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

> Cette commande nécessite `spago` installé sur la machine.

## Remarques

- Le dossier `docs/` contient une analyse initiale du dépôt (`docs/repo-analysis.md`) avec des pistes d'amélioration priorisées.
