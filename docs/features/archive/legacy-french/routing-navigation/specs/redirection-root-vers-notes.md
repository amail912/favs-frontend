# Redirection de `/` vers `/notes`

## Probleme
Un utilisateur qui arrive sur la racine (`/`) doit etre guide immediatement vers une page utile. Une redirection mal geree peut aussi perturber le bouton de retour du navigateur.

## Solution
- L acces a `/` amene directement l utilisateur a la page Notes (`/notes`).
- Cette redirection ne cree pas d etape inutile dans l historique du navigateur.

## Tests
### Tests unitaires
- `parseRouteString "/"` retourne `Root`.

### Tests d integration
- Simuler `RouteChanged Root` et verifier que l etat courant est `Route Note`.
- Verifier que la navigation utilise un remplacement d etat pour la redirection racine.

### Tests E2E (Playwright)
- Acceder a `/` et verifier que l URL finale est `/notes`.
- Simuler un retour arriere et verifier qu aucune etape `/` n apparait.
