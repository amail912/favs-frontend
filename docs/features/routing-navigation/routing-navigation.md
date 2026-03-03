# Routage et navigation

## Description
La feature Routage et navigation gère la navigation par routes navigateur, l'affichage des onglets et la page 404.

## Objectif
Offrir une navigation simple entre Notes, Checklists et Auth, tout en conservant un historique navigateur cohérent.

## Invariants
- `/` redirige vers `/notes`.
- Les routes supportées sont `/notes`, `/checklists`, `/signup`, `/signin`.
- Les onglets ne sont affichés que hors des pages signup et signin.
- Une route inconnue affiche une page 404.

## Choix techniques
- `Routing.Duplex` pour l'encodage et le parsing des routes.
- `Routing.PushState` pour l'historique navigateur.
- Séparation entre `Route` et `DefinedRoute` pour gérer `Root` et `NotFound`.

## Flux/API
- Au chargement: `InitializeRouting` installe le listener et rafraîchit l'auth.
- Navigation par clic: action `NavigateTo` puis `pushState`.
- Redirection `/`: action `RouteChanged Root` puis `replaceState` vers Notes.

## Spécifications
- [Routes supportées et 404](specs/routes-supportees-et-404.md) — Garantit que seules les pages prévues sont accessibles, les autres affichent une 404.
- [Redirection de `/` vers `/notes`](specs/redirection-root-vers-notes.md) — Garantit que la racine amène directement aux notes sans gêner le retour arrière.
- [Navigation et historique](specs/navigation-historique-push-replace.md) — Garantit une navigation cohérente pour l'utilisateur, sans ajouter d'étape inutile lors de la redirection automatique.
- [Visibilité des onglets](specs/visibilite-onglets-auth.md) — Garantit que les onglets ne s'affichent pas sur Signup/Signin.
