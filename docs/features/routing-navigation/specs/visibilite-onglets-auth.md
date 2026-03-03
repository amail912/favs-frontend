# Visibilite des onglets

## Probleme
Sur les pages d authentification, afficher des onglets de navigation peut distraire l utilisateur et donner l impression qu il peut acceder au contenu sans se connecter.

## Solution
- Les onglets Notes et Checklists s affichent uniquement sur les pages applicatives (Notes/Checklists).
- Les onglets sont masques sur les pages Signup et Signin.

## Tests
### Tests unitaires
- Pas de logique pure critique a isoler pour cette specification.

### Tests d integration
- Simuler une route `Signup` et verifier que l onglet Notes/Checklists n est pas rendu.
- Simuler une route `Signin` et verifier que l onglet Notes/Checklists n est pas rendu.
- Simuler une route `Note` ou `Checklist` et verifier que les onglets sont rendus.

### Tests E2E (Playwright)
- Naviguer vers `/signup` et verifier l absence des onglets.
- Naviguer vers `/notes` et verifier la presence des onglets.
