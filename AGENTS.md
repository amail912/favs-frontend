# AGENTS.md

## Objectif
Frontend pour gérer le quotidien avec un focus sur la gestion d’un calendrier. Le backend correspondant est situé dans `/home/dev/workspace/foucl2`.

## Stack technique
- PureScript + Halogen (UI).
- Spago (build/test). Package set: `psc-0.15.4-20220805` (voir `spago.yaml`).
- Node.js + Express pour le proxy dev (via `http-proxy-middleware`).
- Playwright pour les tests E2E.
- UI actuelle basée sur Bootstrap (voir `static/`).

## Principes de code
- TDD par défaut.
- Pour chaque fonctionalité, des tests  unitaires et des tests d'intégration doivent être faits.
- Tu proposeras de réaliser des tests dès que cela te semble nécessaire
- Utilise la compilation comme une grande aide au développement. PureScript est un language fortement typé qui permet d'avoir beaucoup de garanties avec un système de types adapté.
- On veillera à ne laisser aucun warning de compilation.
- Avant chaque commit, run tous les tests.
- La logique et les vues de chaque domaine restent dans leur module de domaine, sans fichier centralisateur.
- Les helpers partagés sont uniquement techniques, pas métier.
- Les fonctions `render*` des domaines émettent leurs propres actions de domaine ; la conversion vers l’action page se fait au niveau de l’appelant.
- Les fonctions `render*` appelées dans la page passent par un helper local portant le même nom, qui encapsule le `map`.
- Les imports des rendus de domaine sont faits en qualifié avec un alias court mais signifiant.
- Quand un module doit émettre des actions multi-domaines, exposer un type `*Action` et faire la conversion vers l’action page au niveau de l’appelant.
- Préférer la destructuration de premier niveau dans la signature des fonctions, et éviter les alias inutiles.
- Éviter les suffixes inutiles dans les noms de lenses.

## Principes de design
- Ergonomie maximale.
- Mobile first.

## Commandes clés (npm scripts)
- `npm run build` : bundle PureScript vers `static/index.js`.
- `npm run start-dev` : serveur Express + proxy API local sur `http://localhost:1234`, proxy vers `http://localhost:8081`.
- `npm run start-dev-proxy-prod` : même serveur, proxy vers l’URL prod déclarée dans `dev/proxy-server-prod.js`.
- `npm test` : `spago test` (tests unitaires/intégration).
- `npm run e2e` / `e2e:headed` / `e2e:ui` : build + Playwright.
- `npm run e2e:install` : installe Chromium pour Playwright.

## Architecture du code
- Entrée Halogen: `src/Main.purs` → `Pages.App.component`.
- Routing browser (pushState) + auth: `src/Pages/App.purs`.
- Pages principales:
  - `src/Pages/Notes.purs`
  - `src/Pages/Checklists.purs`
  - `src/Pages/Calendar.purs` (agenda riche, logique métier + UI).
- Domaine/Modèles:
  - `src/Domain/*` (Notes, Checklists, Calendar)
  - `src/Calendar/Model.purs` (types métier calendar, templates, récurrence, notifications, exports).
- API:
  - `src/Api/*` + `src/Api/*Contract.purs`.
  - Notes: `/api/note`
  - Checklists: `/api/checklist`
  - Agenda v1: `/api/v1/calendar-items` + `/api/v1/calendar-items/:id/validate` (update via `POST` sur le même path).
- UI/Utilitaires:
  - `src/Ui/*` (modales, gestion d’erreurs, helpers CSS).
- Assets statiques: `static/` (HTML/CSS/Bootstrap). Le bundle généré est `static/index.js`.

## Flux principaux (diagrammes textuels)
Flux UI / routing:
1. `src/Main.purs` monte `Pages.App.component` dans le `body`.
2. `Pages.App` lit la route (pushState) et choisit un composant (`Notes`, `Checklists`, `Agenda`, `Signup`, `Signin`).
3. Les actions UI déclenchent `NavigateTo` → `pushState` → re-render.

Flux auth:
1. Au démarrage: `InitializeRouting` → `RefreshAuthStatus`.
2. `RefreshAuthStatus` appelle `GET /api/note` pour sonder l’auth.
3. Si OK → `isAuthenticated = true`.
4. `SignOut` → `POST /api/signout` → `isAuthenticated = false` et redirection.

Flux data Notes/Checklists:
1. UI déclenche un fetch via `Api.*` (`GET /api/note` ou `GET /api/checklist`).
2. Création/mise à jour via `POST` ou `PUT` sur `/api/note` ou `/api/checklist` selon type.
3. Suppression via `DELETE /api/note/:id` ou `/api/checklist/:id`.

Flux data Calendar:
1. Chargement via `GET /api/v1/calendar-items`.
2. Création via `POST /api/v1/calendar-items`.
3. Mise à jour via `POST /api/v1/calendar-items` (contrat actuel: update POST même endpoint).
4. Validation via `POST /api/v1/calendar-items/:id/validate` avec `duree_reelle_minutes`.

Flux sync offline (calendar):
1. App passe en mode offline (local) → mutations stockées localement (queue `pendingSync`).
2. UI reflète immédiatement les mutations (optimistic UI).
3. A la reconnexion, tentative d’envoi séquentielle vers l’API agenda.
4. Si conflit serveur détecté → `syncConflict` est renseigné et l’UI demande un choix utilisateur.
5. Après résolution manuelle, la version finale est envoyée puis `pendingSync` est purgée.

## Calendar (contexte fonctionnel)
Le module Calendar couvre:
- Intentions vs blocs planifiés, conflits visibles.
- Drag-and-drop + timeline minute.
- Récurrence + exceptions.
- Templates de routines + instanciation.
- Import/export CSV/ICS.
- Mode offline + sync + résolution manuelle de conflits.
Références de specs produit et user stories: `docs/specs/calendar-v1/`.

## Tests
- Suite principale: `test/SpecSuite.purs` (Notes, Checklists, Calendar, UI helpers).
- E2E: `e2e/tests/*.spec.js` (navigation, lifecycle notes/checklists, agenda integration, signup feedback).
- Support E2E: `e2e/support/auth-session.js` (signup/signin via `/api/signup` + `/api/signin`).

## Dev proxy
- `dev/proxy-server.js` expose `http://localhost:1234` et proxy `/api` → `http://localhost:8081`.
- `playwright.config.js` lance `npm run start-dev` comme webServer et réutilise le serveur si déjà lancé.

## Consignes
- Au démarrage d'une session Codex, tu me demanderas le PATH pour accéder aux outils de développement.
- Tu utiliseras ce PATH pour exécuter les commandes lors du développement sans me demander.
- Ne jamais toucher au répo foucl2, si jamais son démarrage est nécessaire, me demander.
- Ne jamais faire d'actions git en écriture sur le répo distant (push en particulier)
- Ce fichier doit être mis à jour à chaque commit, qu'il soit fait par toi ou bien lorsque je t'avertit que j'en ai fait un.

## Journal des commits
- 2026-03-03 : Retrait des specs API du `test/SpecSuite.purs` suite à la suppression des fichiers `test/Api/*`.
