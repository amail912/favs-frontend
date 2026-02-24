# Agenda V1 - Specification executable

## 1. Modele de donnees (contract-first)

### 1.1 Entite `ItemCalendrier`
Champs minimaux:
- `id: UUID`
- `type: INTENTION | BLOC_PLANIFIE`
- `titre: string` (non vide)
- `notes: string?`
- `categorie: string?` (personnalisable)
- `couleur: string?`
- `fenetre_debut: datetime` (obligatoire)
- `fenetre_fin: datetime` (obligatoire, strictement > `fenetre_debut`)
- `duree_estimee_minutes: int?` (>= 1)
- `duree_reelle_minutes: int?` (>= 1)
- `statut: TODO | EN_COURS | FAIT | ANNULE`
- `source_item_id: UUID?` (si `BLOC_PLANIFIE` issu d'une intention)
- `is_conflict: boolean`
- `conflict_group_id: UUID?`
- `recurrence_rule: RecurrenceRule?`
- `recurrence_exception_dates: date[]`
- `created_at: datetime`
- `updated_at: datetime`

### 1.2 Entites routines
- `RoutineTemplate`
- `RoutineTemplateStep`
- `RoutineInstance`
- `RoutineInstanceStep`

Dependances V1 supportees entre etapes:
- `debut(B) = fin(A) + offset_minutes`
- `debut(B) = debut(A) - offset_minutes`

## 2. Invariants metier (obligatoires)
- INV-01: `fenetre_fin > fenetre_debut` pour tout item.
- INV-02: `BLOC_PLANIFIE` doit representer un creneau concret (debut/fin determines).
- INV-03: un chevauchement entre deux `BLOC_PLANIFIE` est autorise techniquement mais doit etre flagge (`is_conflict = true`) sur tous les items concernes.
- INV-04: un item en conflit doit appartenir a un `conflict_group_id`.
- INV-05: validation d'un item (`statut -> FAIT`) autorise la saisie de `duree_reelle_minutes`; si non fournie, proposition par defaut `maintenant - fenetre_debut` (minimum 1 minute).
- INV-06: modifier une `RoutineInstance` ne modifie jamais le `RoutineTemplate` d'origine.
- INV-07: recurrence et exceptions doivent produire des occurrences deterministes en timezone locale (pas de gestion fuseau V1).
- INV-08: mode offline: toute mutation locale doit etre conservable sans reseau et synchronisable ensuite.
- INV-09: en conflit de sync (offline vs serveur), le choix final revient a l'utilisateur.
- INV-10: export doit inclure tous les items par defaut, avec options de filtre.

## 3. Regles de comportement
- REG-01: une `INTENTION` peut chevaucher d'autres items sans creer de conflit bloquant.
- REG-02: un `BLOC_PLANIFIE` cree a partir d'une intention est lie via `source_item_id`.
- REG-03: le passage en mode "resolution de conflits" propose des strategies mais n'applique rien sans confirmation utilisateur.
- REG-04: notifications configurables globalement et par item.
- REG-05: valeurs de rappel par defaut pour `INTENTION` non planifiee:
  - 06:00 le jour de `fenetre_debut`
  - 24h avant `fenetre_fin`
- REG-06: raccourcis clavier metier autorises uniquement s'ils ne perturbent pas la navigation browser/plugins type VimVixen.

## 4. Scenarios executables (Gherkin)

```gherkin
Feature: Planification quotidienne

  Scenario: Creer une intention
    Given un utilisateur authentifie
    When il cree un item de type INTENTION avec un titre, un debut et une fin valides
    Then l'item est en statut TODO
    And l'item est visible dans la vue Jour

  Scenario: Convertir une intention en bloc planifie
    Given une INTENTION existante
    When l'utilisateur cree un BLOC_PLANIFIE lie a cette intention
    Then le BLOC_PLANIFIE reference la source via source_item_id
    And les deux items restent modifiables independamment

  Scenario: Detecter un conflit de blocs planifies
    Given deux BLOC_PLANIFIE qui se chevauchent
    When le chevauchement est persiste
    Then les deux items sont marques is_conflict=true
    And ils partagent le meme conflict_group_id

  Scenario: Resoudre un conflit sans auto-application
    Given un groupe de conflit existant
    When l'utilisateur ouvre l'ecran de resolution
    Then l'application affiche des strategies de resolution
    And aucune strategie n'est appliquee sans action explicite

  Scenario: Valider une tache avec duree reelle calculee
    Given un item en cours avec fenetre_debut definie
    When l'utilisateur clique Valider sans saisir de duree
    Then l'application propose une duree_reelle = now - fenetre_debut
    And le statut devient FAIT apres confirmation

  Scenario: Recurrence avec exception
    Given une regle de recurrence hebdomadaire
    And une date d'exception
    When les occurrences sont generees pour le mois
    Then l'occurrence de la date d'exception est absente

  Scenario: Instance de routine modifiee sans impact template
    Given un RoutineTemplate avec 3 etapes
    When l'utilisateur instancie puis modifie l'etape 2 de l'instance
    Then le template d'origine reste inchange

  Scenario: Utilisation hors ligne
    Given l'application est hors ligne
    When l'utilisateur cree et modifie des items
    Then les mutations sont visibles localement immediatement
    And elles sont envoyees a la reconnexion

  Scenario: Conflit de synchronisation
    Given un item modifie localement hors ligne
    And le meme item modifie sur le serveur
    When la synchronisation demarre
    Then l'utilisateur doit choisir la resolution finale

  Scenario Outline: Export filtre
    Given des items de types varies existent
    When l'utilisateur exporte en <format> avec filtre <filtre>
    Then seuls les items correspondant au filtre sont exportes

    Examples:
      | format | filtre               |
      | ICS    | tous                 |
      | ICS    | categorie=Sport      |
      | CSV    | statut=TODO          |
      | CSV    | type=BLOC_PLANIFIE   |
```

## 5. Contrat API cible (draft)
Endpoints (versionnes):
- `GET /api/v1/calendar-items`
- `POST /api/v1/calendar-items`
- `POST /api/v1/calendar-items` (creation ou mise a jour via payload ServerCalendarItem)
- `DELETE /api/v1/calendar-items/:id`
- `POST /api/v1/calendar-items/:id/validate`
- `POST /api/v1/conflicts/:conflictGroupId/resolve`
- `GET /api/v1/routine-templates`
- `POST /api/v1/routine-templates`
- `POST /api/v1/routine-templates/:id/instantiate`
- `POST /api/v1/import/ics`
- `POST /api/v1/import/csv`
- `GET /api/v1/export/ics`
- `GET /api/v1/export/csv`

Note d'alignement backend: ce contrat est une cible V1 a aligner avec `../foucl2` avant implementation.
