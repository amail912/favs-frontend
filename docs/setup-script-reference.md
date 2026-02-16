# Documentation détaillée de `scripts/setup.sh`

Ce document explique la logique interne du script et les choix d'implémentation.

## Objectif du script

Automatiser l'installation complète du tooling projet sur Debian, depuis un clone vierge, en une seule commande :

```bash
scripts/setup.sh
```

## Structure globale

Le script est organisé en fonctions, puis un `main` orchestre les étapes :

1. `install_nodejs_from_debian_repo`
2. `install_playwright_system_libs`
3. `install_project_dependencies`
4. `print_next_steps`

## Explication des parties importantes

### `set -euo pipefail`

- `-e` : stoppe le script à la première commande en erreur.
- `-u` : échoue si une variable non définie est utilisée.
- `pipefail` : une pipeline échoue si l'une des commandes échoue.

Ce trio améliore la fiabilité et évite les faux positifs de setup.

### Détermination des chemins du script et du repo

```bash
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
```

Ces lignes rendent le script robuste quel que soit le dossier courant de l'utilisateur au moment de l'exécution.

### Gestion `root`/`sudo` avec `run_as_root`

La fonction :

- exécute directement la commande si l'utilisateur est `root`,
- sinon préfixe la commande avec `sudo` si disponible,
- sinon échoue explicitement avec un message clair.

Cela évite de dupliquer `sudo` partout et centralise la politique de privilèges.

### Pourquoi abandonner NodeSource ici

Dans cette version, le script **privilégie le dépôt APT Debian officiel** (`nodejs` + `npm`) pour :

- réduire la dépendance à un dépôt tiers,
- rester aligné avec la logique de maintenance/sécurité de la distribution,
- simplifier le bootstrap sur machine Debian fraîche.

Le script contrôle ensuite la version Node et échoue proprement si elle est trop ancienne (`< 18`).

### `apt-get` vs `apt` dans un script

**État de l'art (pratique actuelle)** :

- `apt` est orienté **usage interactif** (humains au terminal) avec une UX plus agréable.
- `apt-get` (et `apt-cache`) restent la référence pour **l'automatisation** (scripts, CI, provisionning), car leur comportement/format est plus stable dans le temps.

Position recommandée:

- dans la doc utilisateur manuelle : exemples `apt` possibles,
- dans les scripts versionnés (`setup.sh`) : préférer `apt-get`.

Cette recommandation suit la documentation Debian/Ubuntu et les pages de manuel APT.

### `deb` vs `apt`: ce n'est pas la même chose

- `apt` / `apt-get`: outils pour installer des paquets.
- `deb ...` : format de ligne de dépôt dans `sources.list`.

Donc `deb` n'est pas une alternative à `apt`; c'est la déclaration d'une source APT.

### Installation des outils PureScript: local vs global

Le projet adopte une approche **locale et reproductible** :

- `spago` est dans `devDependencies`,
- `purescript` (`purs`) est aussi dans `devDependencies`.

Conséquences positives :

- même version d'outils pour tous les contributeurs/CI,
- pas de dépendance à des installations globales machine,
- `npm ci` suffit à rendre `npm run build` et `npm run test` opérationnels.

C'est la raison pour laquelle le script n'installe plus `purescript` globalement.

### Installation dépendances Linux Playwright

Le script installe quelques libs système fréquemment requises (`libnss3`, `libxss1`, `libasound2`, `libgbm1`) avant l'installation Playwright.

Puis il lance :

```bash
npx playwright install --with-deps chromium
```

`--with-deps` est crucial sur Debian fraîche : il récupère Chromium et complète les dépendances natives nécessaires à son exécution.

### Installation dépendances du projet

```bash
npm ci
```

`npm ci` est choisi (au lieu de `npm install`) pour garantir une installation fidèle au `package-lock.json`, plus reproductible en CI et en local.

## Idempotence et comportement attendu

- Relancer le script est supporté.
- Le script vérifie la version de Node et échoue explicitement si insuffisante.
- Les installations npm/playwright sont rejouables en cas d'échec réseau intermittent.

## Références (bonnes pratiques)

- Node.js — installation via gestionnaire de paquets, gestionnaires de version, et bonnes pratiques générales: https://nodejs.org/en/download/package-manager
- Documentation Debian APT (usage de `apt-get`/`apt` et référentiels): https://wiki.debian.org/Apt
- APT `man apt` (section SCRIPT USAGE: `apt` n'a pas de garantie d'interface stable pour scripts): https://manpages.debian.org/bookworm/apt/apt.8.en.html
- Ubuntu/Debian community guidance: `apt` for humans, `apt-get` for scripts: https://askubuntu.com/a/445386
- Playwright — installation Linux et dépendances navigateurs: https://playwright.dev/docs/intro

## Limites connues

- Le script cible Debian et suppose APT disponible.
- Les commandes `start-dev`/`e2e` nécessitent un backend local sur `http://localhost:8081` pour les appels API proxifiés.
