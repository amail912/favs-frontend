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

## États UI
- Onglets Notes et Checklists quand l'utilisateur n'est pas sur un écran d'auth.
- Bouton auth dans le header: Signup si non connecté, Sign out si connecté.
- Page 404 avec actions de retour.
