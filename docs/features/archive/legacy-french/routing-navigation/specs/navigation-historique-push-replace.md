# Navigation et historique

## Probleme
Un utilisateur s attend a pouvoir utiliser le bouton "retour" pour revenir a la page precedente. Si la navigation cree des etapes inutiles, le retour devient confus et imprevu.

## Solution
- Quand l utilisateur change de page via les onglets, une nouvelle etape est ajoutee a l historique.
- Quand l application redirige automatiquement depuis la racine (`/`), cette redirection ne cree pas d etape supplementaire.
- Le bouton "retour" ramene donc l utilisateur a la page qu il a reellement visitee auparavant.

## Tests
### Tests unitaires
- Pas de logique pure critique a isoler pour cette specification.

### Tests d integration
- Simuler `NavigateTo` et verifier que l intention de navigation ajoute une etape d historique.
- Simuler `RouteChanged Root` et verifier que l intention de navigation remplace l etape courante.

### Tests E2E (Playwright)
- Naviguer de Notes a Checklists, puis action "back" et verifier le retour sur Notes.
- Demarrer sur `/`, verifier la redirection vers `/notes`, puis "back" sans etape `/`.
