# Analyse initiale du dépôt `favs-frontend`

## Vue d'ensemble

- Front-end PureScript/Halogen (point d’entrée `src/Main.purs` + composant racine `src/App.purs`).
- Deux domaines métier principaux : **Notes** et **Checklists**.
- Le front appelle un backend via `/api/*`, avec un proxy Express en local.

## Commandes exécutées

```bash
rg --files
npm test
```

Résultat notable : `npm test` échoue car `spago` n’est pas installé dans cet environnement (`sh: 1: spago: not found`).

## Observations principales

1. **Navigation/routing partiellement incohérent**
   - La route `Signup` existe dans le codec et dans le rendu de composant.
   - En revanche, la navigation n’expose que deux onglets (`Notes`, `Checklists`).
   - La logique de sélection de route dépend d’un test sur le libellé texte (`if title == "Notes" then ... else ...`), ce qui est fragile (i18n, renommage UI).

2. **Couverture de tests quasi inexistante**
   - Le test actuel est un placeholder (« You should add some tests. »).

3. **Duplication importante Notes / Checklists**
   - Les deux modules répliquent la même structure : état `editingState`, CRUD distant via Affjax, gestion d’erreurs `ExceptT`, manipulation DOM (scroll/focus), etc.
   - Ça augmente le coût de maintenance et le risque de divergence.

4. **Comportement fonctionnel suspect dans Notes**
   - Lors de `CreateNewNote`, la valeur initiale de `noteContent` utilisée dans `handleAction` est différente de celle définie par `newNote` et semble être une coquille (`"What's your new title?"` au lieu de contenu).

5. **Suppression d’item checklist non implémentée**
   - L’action existe côté UI mais la fonction `deleteChecklistItem` est un no-op (`pure unit`), donc l’intention utilisateur n’est pas réellement persistée.

6. **Configuration d’environnement à durcir**
   - Un script proxy pointe vers une URL de prod codée en dur.
   - Le package set Spago est fixé sur une version ancienne (août 2022), possiblement source de dette technique.

7. **Polish produit/documentation**
   - Le `<title>` HTML est encore un intitulé d’exemple (`Halogen Example - Basic button`).
   - Pas de README expliquant setup, architecture, conventions, et workflow dev.

## Suggestions d’amélioration priorisées

### Priorité haute (quick wins + risques fonctionnels)

1. **Corriger la navigation en mode typé**
   - Faire porter `tab` sur `DefinedRoute` plutôt que sur `String`.
   - Ajouter l’onglet Signup (ou supprimer la route si non utilisée).

2. **Corriger les incohérences de création de note**
   - Centraliser la valeur initiale via `newNote` pour éviter les divergences.

3. **Implémenter réellement `DeleteChecklistItem`**
   - Modifier le checklist côté front, persister via `writeToServer`, puis rafraîchir.

4. **Mettre en place des tests minimaux utiles**
   - Unit tests encode/decode JSON (`Note`, `Checklist`).
   - Tests purs sur transitions d’état/action handlers extraits en fonctions testables.

### Priorité moyenne (maintenabilité)

5. **Factoriser les patterns communs Notes/Checklists**
   - Extraire un module utilitaire pour :
     - wrappers Affjax (GET/POST/PUT/DELETE + contrôle de statut)
     - gestion d’erreurs typées
     - helpers focus/scroll DOM

6. **Externaliser la config d’environnement**
   - Utiliser variables d’env (`API_TARGET`) pour proxies dev/prod au lieu d’URL codées en dur.

7. **Mettre à niveau l’outillage**
   - Vérifier montée de version package set Spago + dépendances.
   - Ajouter vérifications CI (`spago test`, format/lint si disponible).

### Priorité basse (qualité perçue)

8. **Nettoyage UI/UX rapide**
   - Mettre un vrai `<title>`.
   - Ajouter états loading/error visibles plutôt que simples logs console.

9. **Documenter le projet**
   - README : installation (Node + Spago), démarrage local, endpoints backend attendus, scripts npm.
