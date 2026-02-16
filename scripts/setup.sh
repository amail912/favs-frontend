#!/usr/bin/env bash
set -euo pipefail

# Script d'installation "from scratch" pour Debian.
# Objectif : cloner le repo, exécuter ce script, puis pouvoir lancer directement
# les scripts npm du cycle de vie du projet.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
MIN_NODE_MAJOR=18

# Dépendances système Debian: on privilégie le dépôt officiel de la distribution
# pour limiter les dépôts tiers et rester aligné avec les pratiques Debian.
APT_PACKAGES=(
  nodejs
  npm
)

# Prérequis d'exécution Playwright sur Linux via Chromium.
# On force --with-deps ensuite, mais on garde aussi les libs usuelles en filet de sécurité
# pour les environnements Debian les plus minimalistes.
PLAYWRIGHT_LINUX_PACKAGES=(
  libnss3
  libxss1
  libasound2
  libgbm1
)

log() {
  printf '\n[setup] %s\n' "$*"
}

run_as_root() {
  if [[ "${EUID}" -eq 0 ]]; then
    "$@"
  elif command -v sudo >/dev/null 2>&1; then
    sudo "$@"
  else
    log "Erreur: sudo est requis pour exécuter: $*"
    exit 1
  fi
}

install_nodejs_from_debian_repo() {
  log "Installation de Node.js et npm via les dépôts Debian officiels..."
  # Bon usage reconnu: `apt` pour l'interactif humain, `apt-get` pour les scripts
  # (interface plus stable pour l'automatisation, cf. `man apt`).
  run_as_root apt-get update
  run_as_root apt-get install -y "${APT_PACKAGES[@]}"

  if ! command -v node >/dev/null 2>&1; then
    log "Erreur: node n'a pas été trouvé après installation apt."
    exit 1
  fi

  local current_major
  current_major="$(node -p 'process.versions.node.split(".")[0]')"

  if [[ "${current_major}" -lt "${MIN_NODE_MAJOR}" ]]; then
    log "Erreur: Node.js $(node -v) est trop ancien (<${MIN_NODE_MAJOR})."
    log "Utilise une Debian plus récente ou un gestionnaire de version (nvm/fnm/Volta)."
    exit 1
  fi

  log "Node.js installé: $(node -v)"
  log "npm installé: $(npm -v)"
}

install_playwright_system_libs() {
  log "Installation des librairies Linux recommandées pour Playwright/Chromium..."
  run_as_root apt-get update
  run_as_root apt-get install -y "${PLAYWRIGHT_LINUX_PACKAGES[@]}"
}

install_project_dependencies() {
  log "Installation des dépendances npm du projet (npm ci)..."
  cd "${REPO_ROOT}"
  npm ci

  # --with-deps installe Chromium ET ses dépendances système nécessaires via apt.
  # Cette étape évite les erreurs de lancement navigateur en e2e sur machine fraîche.
  log "Installation de Chromium Playwright et dépendances système associées..."
  npx playwright install --with-deps chromium
}

print_next_steps() {
  cat <<'EON'

[setup] ✅ Installation terminée.

Commandes disponibles ensuite :
  npm run build
  npm run test
  npm run start-dev
  npm run e2e

EON
}

main() {
  log "Répertoire du projet: ${REPO_ROOT}"
  install_nodejs_from_debian_repo
  install_playwright_system_libs
  install_project_dependencies
  print_next_steps
}

main "$@"
