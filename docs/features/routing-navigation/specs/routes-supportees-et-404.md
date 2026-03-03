# Routes supportees et 404

## Probleme
Un utilisateur peut arriver sur une URL inattendue (lien externe, favoris ancien, faute de frappe). Sans regle claire, il risque de voir un ecran vide ou une page incoherente.

## Solution
- Les pages accessibles sont limitees a `/notes`, `/checklists`, `/signup` et `/signin`.
- Toute autre URL affiche une page 404 explicite.
- La page 404 propose un chemin de retour vers une page valide.

## Tests
### Tests unitaires
- `parseRouteString "/"` retourne `Root`.
- `parseRouteString "/notes"` retourne `Route Note`.
- `parseRouteString "/checklists"` retourne `Route Checklist`.
- `parseRouteString "/signup"` retourne `Route Signup`.
- `parseRouteString "/signin"` retourne `Route Signin`.
- `parseRouteString "/foo"` retourne `NotFound`.

### Tests d integration
- Simuler `RouteChanged` avec `NotFound` et verifier que l etat courant reste une route 404.
- Simuler `RouteChanged` avec une route valide et verifier que l etat courant change en consequence.

### Tests E2E (Playwright)
- Acceder a chaque route valide et verifier que la page correspondante s affiche.
- Acceder a `/foo` et verifier l affichage d une page 404.
