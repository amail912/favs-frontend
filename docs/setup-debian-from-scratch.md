# Installation du projet depuis une Debian fraîche

Ce guide décrit le scénario cible suivant :

1. machine Debian fraîche,
2. `git` déjà installé,
3. clone du dépôt,
4. exécution de `scripts/setup.sh`,
5. utilisation directe des commandes du projet (`npm run ...`) sans installation manuelle supplémentaire.

---

## 1) Précondition minimale

- Debian fraîche (bookworm ou équivalent récent)
- `git` installé
- utilisateur avec droits `sudo` (ou exécution en `root`)

---

## 2) Clone du dépôt

```bash
git clone <URL_DU_REPO>
cd favs-frontend
```

---

## 3) Lancer le setup automatisé

```bash
scripts/setup.sh
```

Le script installe automatiquement :

- Node.js + npm via les dépôts Debian officiels (`apt-get install nodejs npm`),
- les dépendances npm du projet (`npm ci`) incluant `spago` et `purescript` en local,
- les dépendances système nécessaires à Playwright/Chromium,
- le navigateur Chromium pour les tests E2E (`npx playwright install --with-deps chromium`).

> Le script vérifie que Node.js est au moins en version majeure 18.

---

## 4) Commandes de cycle de vie prêtes à l'emploi

Après succès du setup :

```bash
npm run build
npm run test
npm run start-dev
npm run e2e
```

Commandes complémentaires utiles :

```bash
npm run e2e:headed
npm run e2e:ui
npm run start-dev-proxy-prod
```

---

## 5) Dépannage rapide

### `sudo: command not found`
Utiliser `root` ou installer `sudo` :

```bash
su -
apt-get update && apt-get install -y sudo
```


### Pourquoi le script utilise `apt-get` (et pas `apt`)
En non-interactif, `apt-get` est préféré pour sa stabilité de comportement en automatisation. `apt` est surtout destiné à l'usage interactif humain.

### Erreur APT temporaire (miroir indisponible)
Relancer simplement :

```bash
scripts/setup.sh
```

### Version Node trop ancienne
Le script échoue explicitement si la version majeure est `< 18`.

- Recommandé: Debian récente (Bookworm+)
- Alternative: gestionnaire de version Node (`nvm`, `fnm`, `Volta`)

### Backend absent pour le mode dev/e2e
Le front attend une API disponible sur `http://localhost:8081` pour les appels proxy `/api`.

---

## 6) Résumé ultra-court

```bash
git clone <URL_DU_REPO>
cd favs-frontend
scripts/setup.sh
npm run build
npm run start-dev
```
