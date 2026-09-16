# RStock — Cadre technique complet  
## Walk-forward batché + End-to-end + calibrations globales + reprise durable

## 1. Objectif général

Faire évoluer RStock pour supporter de très gros univers sans changer la signification scientifique des expériences.

Deux capacités sont ajoutées :

**A. Walk-forward batché générique**

Le batching appartient au Walk-forward, pas au End-to-end. Un Walk-forward régulier peut donc lui aussi être automatiquement découpé lorsqu’il dépasse la capacité configurée de l’environnement.

**B. Nouveau type d’expérience `End-to-end`**

Pipeline V1 :

```text
Walk-forward
→ Calibration automatique XGBoost
→ Calibration automatique des paramètres de seuils
→ Calibration des seuils
→ Promotion automatique optionnelle
→ FIN
```

Ne pas ajouter dans cette version :

```text
Entraînement automatique
Activation automatique
```

Ces deux opérations demeurent manuelles.

---

# 2. Principes architecturaux obligatoires

Ne pas créer un nouveau framework de jobs parallèle.

Réutiliser au maximum :

- `RunRepository`
- `RunService`
- worker existant
- `RunLease`
- `SlotLease`
- heartbeat
- `ProgressReporter`
- `CheckpointManager`
- `WorkflowRegistry`
- snapshots `ExperimentSpec`
- fingerprints
- provenance `source_*_run`
- règles de compatibilité historique
- `PromotionService`

L’architecture actuelle fournit déjà ces briques; le nouveau système doit les étendre plutôt que les contourner. 

Trois nouvelles fondations sont toutefois nécessaires :

```text
1. CombinationPlan paresseux et indexable
2. Relations persistantes parent / enfants
3. Orchestration durable par manifests
```

L’état d’un enfant doit toujours être lu depuis son propre `status.json` / `progress.json`. Un manifest parent ne doit jamais devenir une seconde source d’autorité pour le statut.

---

# 3. Deux niveaux de batching distincts

Ne pas confondre les deux concepts.

### Batch d’exécution WF

Nouveau concept.

Exemple :

```text
2 200 000 combinaisons brutes maximum par enfant WF
```

Il crée de vrais runs techniques enfants.

### Sous-batch mémoire/checkpoint

Concept actuel.

Exemple :

```text
25 combinaisons
```

Il reste utilisé à l’intérieur d’un run enfant pour :

- mémoire;
- parallélisme;
- checkpoints;
- progression.

Les deux mécanismes doivent coexister.

---

# 4. Paramètres globaux à ajouter

| Section UI | Paramètre | Nom technique proposé | Défaut moderne |
|---|---|---|---:|
| **Walk-forward / Exécution** | Combinaisons maximales par batch | `walk_forward_max_combinations_per_batch` | **2 200 000** |
| **Calibration XGBoost** | Combinaisons qualifiées maximales pour calibration XGBoost globale | `xgboost_global_max_qualified_combinations` | **500** |
| **Calibration des seuils** | Modèles maximum pour calibration automatique des paramètres de seuils | `threshold_parameter_calibration_max_models` | **500** |

Ces trois valeurs doivent être configurables globalement pour adapter RStock à la capacité de l’environnement.

Elles doivent être snapshotées au lancement du run. Modifier les paramètres globaux par la suite ne doit jamais modifier un run existant.

### Compatibilité historique

Un ancien snapshot dans lequel ces champs n’existent pas ne doit jamais recevoir silencieusement les nouveaux defaults.

Sémantique historique :

```text
walk_forward_max_combinations_per_batch = None
→ comportement historique mono-run

xgboost_global_max_qualified_combinations = None
→ politique historique per_target_v1

threshold_parameter_calibration_max_models = None
→ politique historique per_target_v1
```

Ajouter ces absences historiques explicitement à `HISTORICAL_MISSING_CONFIG_DEFAULTS`.

---

# 5. Prévisualisation avant lancement

La prévisualisation doit être disponible pour :

```text
Walk-forward
End-to-end
```

Elle doit utiliser exactement le même `CombinationPlan` que l’exécution. Il est interdit de coder une seconde formule simplifiée dans Streamlit.

Afficher avant le bouton Lancer :

```text
Cibles
Symboles de contexte
Prédicteurs
Profondeur
Combinaisons brutes exactes
Maximum de combinaisons par batch
Nombre maximal de batchs requis
```

Exemple avec la sémantique actuelle :

```text
Cibles : 150
Contexte : 15
Profondeur : 3
Combinaisons brutes : 110 294 100
Maximum par batch : 2 200 000
Batchs requis : 51
```

L’ancien exemple de 24,7 M était illustratif. **Ne pas modifier la sémantique actuelle du générateur pour reproduire cet exemple.**

La formule actuelle demeure la source de vérité. L’audit a établi qu’avec les règles actuelles, 150 cibles + 15 contextes + profondeur 3 donnent bien 110 294 100 combinaisons. 

---

# 6. Brut vs préfiltré

La prévisualisation avant lancement affiche le **nombre brut exact**.

Le nombre de combinaisons qui survivront au préfiltre ne peut pas être connu avant d’exécuter le préfiltre.

Ne pas demander une seconde confirmation à l’utilisateur après le préfiltre.

Après démarrage, le WF parent doit conserver séparément :

```text
raw_combination_count
prefiltered_combination_count
planned_batch_count
executed_batch_count
```

La grille/les détails peuvent alors montrer :

```text
110 294 100 combinaisons brutes
1 487 230 retenues après préfiltre
51 partitions brutes planifiées
...
```

selon la stratégie effective retenue.

### Contrainte importante

Le découpage ne doit jamais modifier le résultat du préfiltrage.

Si le préfiltre actuel dépend d’informations globales, il doit être calculé/frozen au niveau parent puis appliqué uniformément aux slices.

Ne pas exécuter un préfiltre scientifiquement différent dans chaque enfant simplement parce qu’on a découpé le workload.

Un test d’équivalence mono-run / batché est obligatoire.

---

# 7. Planificateur de combinaisons

Créer préférablement :

```text
rstock/combination_planning.py
```

Le `CombinationPlan` doit être :

```text
paresseux
déterministe
indexable
reproductible
versionné
```

Il doit savoir :

```text
count()
slice(start, stop)
iter_range(start, stop)
plan_sha256
```

sans créer un `DataFrame` contenant 110 millions de combinaisons.

L’ordre canonique doit être le même que celui de la génération actuelle.

Test fondamental :

```text
concat(plan.slice(0,a), plan.slice(a,b), ...)
==
générateur historique
```

sur des populations assez petites pour comparer exhaustivement.

Le nombre, l’ordre, les sets et les identifiants doivent être identiques.

---

# 8. `max_generated_sets`

Le `max_generated_sets` actuel appartient à l’ancienne architecture qui matérialise la population.

Il ne doit pas empêcher un nouveau WF utilisant le `CombinationPlan`.

Ne pas simplement supprimer sa sémantique historique.

Pour les anciens runs :

```text
comportement historique inchangé
```

Pour les nouveaux runs utilisant le planner :

```text
la limite globale de matérialisation n'est plus appliquée de la même manière
```

Documenter explicitement cette évolution.

L’audit a identifié que le défaut moderne actuel de 100 000 empêcherait sinon pratiquement tous les gros WF visés. 

---

# 9. Structure parent / enfants du Walk-forward

Lorsqu’un WF nécessite plusieurs batchs :

```text
WALK_FORWARD parent
│
├── WALK_FORWARD_BATCH 1
├── WALK_FORWARD_BATCH 2
├── WALK_FORWARD_BATCH 3
└── ...
```

Le WF parent est **l’autorité scientifique**.

Les enfants sont uniquement des unités techniques d’exécution.

### Si un seul batch suffit

Préserver autant que possible le chemin WF actuel.

Il n’est pas nécessaire de créer artificiellement un enfant technique si :

```text
batch_count == 1
```

Le WF peut suivre le chemin mono-run actuel.

Ainsi :

```text
petit WF
→ comportement aussi proche que possible de l'existant

gros WF
→ parent + enfants techniques
```

Cette règle minimise les régressions.

---

# 10. Métadonnées relationnelles

Ajouter un `metadata.json` séparé du snapshot scientifique.

Champs proposés :

```text
schema_version
run_role
visible_in_history

parent_run_id
root_run_id
created_by_run_id

relation_type
stage_key
stage_index

batch_id
batch_index
batch_count
```

Valeurs possibles de `run_role` :

```text
standalone
pipeline_parent
pipeline_stage
technical_batch
```

### Compatibilité historique

Run historique sans `metadata.json` :

```text
run_role = standalone
visible_in_history = true
```

Ne pas migrer physiquement tous les vieux runs.

Interpréter l’absence selon cette règle.

---

# 11. `ExperimentSpec`

Ajouter seulement les champs qui modifient réellement l’identité scientifique ou l’exécution reproductible du run.

Champs proposés :

```text
source_end_to_end_run
source_threshold_calibration_run

auto_promote_candidates

pipeline_version
calibration_sampling_policy_version
combination_plan_version
combination_plan_sha256

combination_range_start
combination_range_stop
```

Pour `WALK_FORWARD_BATCH`, la range fait partie du fingerprint puisque deux ranges différentes ne produisent pas les mêmes résultats.

Ne pas mettre les propriétés purement UI dans `ExperimentSpec`.

---

# 12. Manifest du WF batché

Le parent écrit atomiquement :

```text
orchestration/walk_forward_batches.json
```

avant de lancer ses enfants.

Contenu figé :

```text
schema_version
parent_run_id
parent_fingerprint

combination_plan_version
combination_plan_sha256

targets_sha256
predictors_sha256
context_sha256

target_count
predictor_count
context_count
depth

raw_combination_count
max_combinations_per_batch
batch_count
created_at
```

Pour chaque batch :

```text
batch_id
batch_index
batch_count

range_start
range_stop
combination_count

child_run_id
expected_child_fingerprint
```

Le manifest ne doit pas être la source d’autorité pour :

```text
status
progress
error
timestamps runtime
```

Ces valeurs viennent du run enfant.

---

# 13. Création idempotente des enfants

Une reprise ne doit jamais générer un second run pour un batch qui existe déjà.

Ajouter au `RunRepository` / service une primitive idempotente du genre conceptuel :

```text
get_or_create_child(parent, relation_key)
```

La clé relationnelle doit permettre de retrouver sans ambiguïté :

```text
WF parent + batch 17
End-to-end parent + xgboost
End-to-end parent + threshold calibration
```

Si l’enfant existe :

```text
réutiliser le même run_id
```

S’il n’existe pas :

```text
le créer puis inscrire le run_id dans le manifest
```

Toujours relire le manifest persistant avant écriture finale pour éviter l’écrasement depuis un objet mémoire périmé.

---

# 14. Cycle de vie d’un WF batché

Cycle cible :

```text
1. Création du parent
2. Préparation/gel des données
3. Construction du CombinationPlan
4. Préfiltrage selon sémantique existante
5. Gel du plan effectif
6. Écriture du batch manifest
7. Création idempotente des enfants
8. Exécution des enfants
9. Réconciliation de leurs statuts
10. Agrégation logique globale
11. Qualification globale
12. Holdout final
13. Risque / score / classement globaux
14. Publication des artefacts canoniques du parent
15. Parent -> completed
```

Les enfants ne doivent pas produire un classement final qui fait autorité.

Tout ce qui dépend de la population complète reste parent/global.

## 14.1

Règle de batching WF après préfiltrage :

Le nombre affiché avant lancement est basé sur le nombre brut exact de combinaisons :
preview_batch_count = ceil(raw_combination_count / walk_forward_max_combinations_per_batch)
Après exécution du préfiltre, le nombre réel de batchs WF est recalculé sur la population retenue :
planned_batch_count = ceil(prefiltered_combination_count / walk_forward_max_combinations_per_batch)
Seuls les batchs du plan effectif post-préfiltre créent des runs enfants WALK_FORWARD_BATCH.
Le manifest du WF parent doit conserver au minimum :
- raw_combination_count
- preview_batch_count
- prefiltered_combination_count
- planned_batch_count
- prefilter_digest
Le préfiltre doit rester scientifiquement global et déterministe : le batching ne doit jamais modifier la population retenue par rapport à un WF équivalent non batché.

---

# 15. Artefacts scientifiques du WF parent

Les consommateurs existants doivent continuer à trouver les artefacts canoniques au même endroit sur le parent :

```text
windows.csv
predictions.csv
aggregate_by_window.csv
aggregate_by_set.csv
aggregate_global.csv
qualification.csv
final_holdout.csv
final_holdout_predictions.csv
selection_results.csv
artefacts de risque
run_configuration.json
```

L’audit confirme que plusieurs composants descendants supposent aujourd’hui ces noms. 

Les enfants peuvent avoir leurs propres fichiers intermédiaires, mais les étapes suivantes ne doivent pas avoir à connaître chaque enfant.

---

# 16. Nouvel onglet `Batchs WF`

Dans le détail d’un WF parent batché, ajouter :

```text
Batchs WF
```

Afficher de façon compacte :

```text
Batch
Statut
Combinaisons
Progression
Durée
Run ID
Erreur
```

Permettre d’ouvrir le run technique pour diagnostic.

Les enfants `WALK_FORWARD_BATCH` ne doivent jamais encombrer la grille principale Historique.

---

# 17. Historique

Décision finale de visibilité :

### Toujours visibles

```text
End-to-end parent
WF scientifique parent
Calibration XGBoost
Calibration automatique des paramètres de seuils
Calibration des seuils
```

Ces étapes scientifiques restent visibles comme aujourd’hui même lorsqu’elles appartiennent à un End-to-end.

### Cachés par défaut

```text
WALK_FORWARD_BATCH
```

et toute autre unité strictement technique future.

Un filtre avancé `Inclure les runs techniques` pourra éventuellement être ajouté plus tard, mais n’est pas requis en V1.

Cette décision diffère volontairement d’une proposition de l’audit qui envisageait de masquer tous les enfants du End-to-end : **nous voulons préserver la visibilité des véritables expériences scientifiques; seuls les batchs techniques disparaissent de la grille principale.**

---

# 18. Reprise d’un WF batché

Exemple :

```text
Batch 1 ✅
Batch 2 ✅
Batch 3 ❌
Batch 4 —
Batch 5 —
```

`Reprendre` doit :

```text
réutiliser batch 1
réutiliser batch 2
reprendre le SAME run_id du batch 3
puis poursuivre 4 et 5
```

Ne jamais recalculer un enfant `completed`.

Les checkpoints internes actuels de l’enfant restent utilisables. Ainsi, la reprise du batch 3 peut elle-même reprendre au dernier sous-batch checkpointé.

Après succès de tous les enfants, si le parent échoue pendant l’agrégation :

```text
Reprendre
→ ne relance aucun enfant
→ reprend l’agrégation/checkpoint parent
```

---

# 19. Resume vs Restart

Conserver une distinction forte.

### Resume / Reprendre

```text
même parent_run_id
mêmes child_run_id
même CombinationPlan
mêmes snapshots
mêmes paramètres
```

### Restart / Relancer comme nouveau run

```text
nouveau parent_run_id
nouvelle chaîne
nouveaux enfants
```

mais selon les règles historiques/current parameters déjà existantes dans RStock.

Ne pas faire d’un restart une reprise déguisée.

---

# 20. SlotLease et deadlock

C’est un point bloquant à traiter explicitement.

Aujourd’hui, un worker prend un slot lourd pour toute sa durée.

Un parent orchestrateur qui conserve :

```text
heavy slot = 1
```

puis attend un enfant qui doit lui aussi prendre un slot crée un deadlock.

L’audit l’a identifié explicitement. 

Architecture cible :

```text
Parent garde son RunLease

Parent acquiert SlotLease
→ seulement pendant son propre calcul lourd

Parent libère SlotLease
→ avant d’attendre/lancer un enfant lourd

Enfant prend son SlotLease

Parent continue à orchestrer sans monopoliser un heavy slot
```

Tester impérativement avec :

```text
max_concurrent_heavy_jobs = 1
```

---

# 21. Calibration XGBoost globale

Après un WF batché :

```text
NE PAS calibrer XGBoost par batch.
```

Une seule calibration XGBoost pour toute la population globale.

Source :

```text
source_walk_forward_run = WF_PARENT
```

Flux :

```text
WF parent qualification.csv
→ combinaisons qualifiées globales
→ déduplication
→ sampling global éventuel
→ Calibration XGBoost Up / Down
```

Le résultat ne doit pas dépendre du nombre ou de l’ordre des batchs WF.

---

# 22. Nouvelle politique de sampling XGBoost

Créer une politique versionnée :

```text
per_target_v1
global_stratified_v2
```

### `per_target_v1`

Politique historique.

Utilisée pour :

```text
anciens runs
anciens snapshots
replay historique
```

### `global_stratified_v2`

Politique moderne pour les nouveaux workflows concernés.

Règle :

```text
population qualifiée <= plafond
→ utiliser 100 %

population qualifiée > plafond
→ échantillon déterministe global jusqu'au plafond
```

Le plafond est :

```text
xgboost_global_max_qualified_combinations
```

Défaut initial :

```text
500
```

---

# 23. Pas de double sampling XGBoost

Le mécanisme actuel `combinations_per_target` ne doit pas être appliqué avant ou après `global_stratified_v2`.

Sinon :

```text
sample par cible
→ sample global
```

produirait un échantillon d’échantillons.

Ce n’est pas voulu.

Pour `global_stratified_v2` :

```text
population qualifiée globale
→ UN sampling
```

Le quota historique `combinations_per_target` reste seulement pour `per_target_v1`.

L’audit recommande explicitement de versionner ces deux comportements. 

---

# 24. Sampling global déterministe XGBoost

Le sample V2 doit être :

```text
déterministe
reproductible
indépendant de l'ordre d'arrivée
indépendant du nombre de batchs WF
raisonnablement représentatif des cibles
```

Approche recommandée :

```text
SHA-256(seed + policy_version + set_id)
```

avec stratification par cible.

L’allocation entre cibles doit être déterministe.

Publier :

```text
sampling_manifest.json
```

avec au minimum :

```text
policy_version
population_size
sample_size
cap
seed
target_distribution
sample_sha256
selected_set_ids ou digest reproductible
```

---

# 25. Calibration automatique des paramètres de seuils

Même principe :

```text
population <= 500 modèles directionnels
→ tous les modèles

population > 500
→ sample déterministe de 500 modèles directionnels
```

Ici, **un modèle signifie `(set, direction)`**.

Donc :

```text
204 sets Up/Down
= 408 modèles
```

et la calibration utilise les 408.

Le paramètre :

```text
threshold_parameter_calibration_max_models = 500
```

représente donc bien **500 couples `(set, direction)`**, pas 500 sets.

---

# 26. Calibration finale des seuils

Ne pas échantillonner la calibration finale à cause de ce nouveau plafond.

Différence fondamentale :

```text
Calibration des paramètres de seuils
→ choisit la politique
→ sampling permis au-dessus du plafond

Calibration finale des seuils
→ applique cette politique
→ TOUS les modèles concernés
```

L’audit a identifié qu’actuellement les deux étapes passent par `combinations_per_target`. La calibration finale doit être modifiée pour devenir exhaustive dans la nouvelle politique. 

Si elle devient trop coûteuse à l’avenir, on pourra la batcher avec reprise, mais **sans réduire la population scientifique**.

Ce n’est pas requis dans la V1 sauf si nécessaire techniquement pour le End-to-end massif.

---

# 27. Nouveau type `End-to-end`

Ajouter :

```text
JobType.END_TO_END
```

Un End-to-end est un orchestrateur durable.

Structure :

```text
END_TO_END
│
├── WALK_FORWARD
│   ├── WALK_FORWARD_BATCH 1
│   ├── WALK_FORWARD_BATCH 2
│   └── ...
│
├── XGBOOST_CALIBRATION
├── THRESHOLD_PARAMETER_CALIBRATION
├── THRESHOLD_CALIBRATION
└── AUTO_PROMOTION éventuelle
```

Le WF, XGBoost et les deux calibrations restent de vrais runs scientifiques existants.

Réutiliser leurs handlers actuels plutôt que recopier leur logique dans le handler End-to-end.

---

# 28. Manifest du End-to-end

Créer :

```text
orchestration/pipeline.json
```

avec :

```text
schema_version
pipeline_version
root_run_id

auto_promote_candidates

stages:
  walk_forward
  xgboost_calibration
  threshold_parameter_calibration
  threshold_calibration
  promotion
```

Chaque étape conserve :

```text
stage_key
expected_job_type
child_run_id
expected_fingerprint
dependency_run_ids
artifact_digests nécessaires
```

L’état courant de l’enfant doit être relu depuis le repository.

---

# 29. Paramètre End-to-end au lancement

Ajouter uniquement :

```text
Promotion automatique des candidats
Oui / Non
```

Pas d’option :

```text
Entraîner
Activer
```

dans cette version.

---

# 30. Holdout et promotion automatique

Décision finale :

```text
Promotion automatique = Oui
→ Holdout obligatoire
```

Si l’utilisateur désactive le holdout puis active promotion automatique :

```text
refuser le lancement
```

avec message clair.

Ne pas activer silencieusement le holdout à sa place.

Si :

```text
Promotion automatique = Non
```

le comportement normal du paramètre holdout reste disponible.

---

# 31. Politique simple `Candidat / Non candidat`

L’auto-promotion doit utiliser la nouvelle règle simple de RStock.

Un modèle directionnel Up est `Candidat` seulement si tous les critères passent :

```text
seuil calibré présent
seuil marqué selected

holdout signals >= 20
holdout AUC >= 0.60
holdout precision >= 0.40
directional mean return > 0
opposite movement frequency <= 0.30

aucune métrique requise absente
```

Sinon :

```text
Non candidat
```

Ne pas réintroduire :

```text
score composite
Candidat fort
À examiner
pondérations
normalisations
```

---

# 32. Promotion par combinaison

Le statut est diagnostiqué par direction, mais le `PromotionService` crée un modèle correspondant à une combinaison.

Décision :

```text
Si la ligne Up d'une combinaison est Candidat
→ la combinaison peut être promue
```

Promouvoir chaque combinaison unique une seule fois.

Le modèle promu conserve évidemment les paramètres nécessaires Up/Down déjà gelés.

La promotion automatique V1 est donc orientée vers le cas de Production actuellement utilisé : signaux haussiers.

---

# 33. Auto-promotion idempotente

Une interruption pendant la promotion ne doit jamais produire de doublons.

Exemple :

```text
10 candidats
1-6 promus
plantage sur 7
```

Reprise :

```text
1-6 détectés comme déjà promus
7 repris
8-10 poursuivis
```

Utiliser le fingerprint/idempotence déjà fourni par `PromotionService`.

La promotion peut être modélisée comme une étape technique `AUTO_PROMOTION` si cela simplifie la reprise.

Elle n’a pas besoin de polluer Historique si elle est strictement une action technique; son état doit toutefois être visible dans le détail End-to-end.

---

# 34. Reprise End-to-end

Exemple :

```text
WF ✅
XGB ✅
Calibration paramètres seuils ✅
Calibration seuils ❌
Promotion —
```

`Reprendre` doit :

```text
valider les digests des trois premières étapes
réutiliser leurs run_id
reprendre le SAME run de calibration des seuils
continuer ensuite la promotion
```

Ne jamais recréer automatiquement une étape `completed`.

Le manifest parent est réconcilié avec les vrais statuts des enfants avant toute décision.

---

# 35. Reprise après changement des paramètres globaux

Exemple :

```text
Run créé avec batch max = 2 200 000
Utilisateur change ensuite global = 1 000 000
```

Resume du vieux run :

```text
reste à 2 200 000
```

Nouveau run :

```text
utilise 1 000 000
```

Même règle pour :

```text
XGBoost global cap
Threshold parameter calibration cap
```

---

# 36. Restart d’un End-to-end

Un restart n’est pas une reprise.

Il crée :

```text
nouveau END_TO_END parent
nouveau manifest
nouveaux stage runs
nouveaux batch child runs si nécessaires
```

selon les règles existantes :

```text
Paramètres du run
Paramètres actuels
```

Respecter impérativement le stage ownership déjà implémenté dans RStock.

---

# 37. Fingerprints historiques

L’audit a identifié un risque : charger un ancien config et y injecter de nouveaux champs peut produire un fingerprint différent de celui qui existait lors de l’écriture du checkpoint.

Pour une reprise historique :

```text
utiliser le fingerprint persisté du run/checkpoint
```

lorsque celui-ci existe et est l’autorité historique.

Ne pas recalculer aveuglément un fingerprint moderne à partir d’un ancien snapshot enrichi en mémoire. 

Ajouter des tests spécifiques.

---

# 38. Dataset du WF parent

Le dataset préparé doit être figé au parent.

Les enfants WF ne doivent pas :

```text
retélécharger les données
reconstruire une date de fin différente
appliquer chacun un cutoff différent
```

Ils travaillent tous sur :

```text
même dataset
même historique
mêmes dates
mêmes paramètres
même plan
```

Le batching ne doit changer que l’unité d’exécution.

---

# 39. Invariant scientifique principal

Invariant obligatoire :

> Un WF batché doit produire les mêmes résultats scientifiques qu’un WF mono-run exécuté avec exactement les mêmes paramètres et données.

Sur un univers de test assez petit pour permettre les deux modes, comparer :

```text
sets
windows
predictions
qualification
holdout
scores
rangs
résultats globaux
```

en tolérant uniquement les différences de tri/serialization explicitement normalisées.

Si cet invariant échoue, ne pas poursuivre au End-to-end.

---

# 40. Interface End-to-end

Dans Expériences, ajouter :

```text
Type : End-to-end
```

Réutiliser les paramètres standards pertinents du WF.

Ajouter :

```text
Promotion automatique des candidats [Oui/Non]
```

Afficher la prévisualisation de charge avant lancement.

Le batching n’a pas de toggle.

```text
total <= max
→ normal

total > max
→ batching automatique
```

---

# 41. Détail End-to-end

Ajouter un onglet :

```text
Étapes
```

Affichage proposé :

```text
Walk-forward                     ✅  2h14
Calibration XGBoost              ✅  28m
Calibration paramètres seuils    ✅  31m
Calibration seuils               ❌
Promotion                        —
```

Afficher aussi :

```text
run_id
durée
source
erreur
bouton ouvrir le run
```

Pour le WF :

```text
14 batchs / 14
```

avec accès au détail `Batchs WF` du run WF.

---

# 42. Progression End-to-end

Le parent End-to-end doit pouvoir afficher une progression logique par étape.

Ne pas prétendre qu’une simple moyenne des pourcentages correspond nécessairement au temps réel.

V1 acceptable :

```text
Étape 3 / 5
Calibration automatique des paramètres de seuils
63 %
```

avec le vrai progress de l’enfant courant.

---

# 43. Annulation

Annuler un parent End-to-end doit :

```text
demander l'annulation de l'enfant actif
arrêter la création des étapes suivantes
conserver les étapes déjà completed
```

Annuler un WF parent batché doit :

```text
annuler l'enfant batch actif
ne pas lancer les suivants
conserver les batchs completed
```

Une reprise ultérieure reprend à la bonne étape.

---

# 44. Erreurs

Le parent ne doit pas transformer une erreur enfant en simple message texte et perdre la provenance.

Conserver :

```text
child_run_id
job_type
stage
batch_id si applicable
error
trace/log accessible
```

Le parent peut afficher une synthèse, mais l’enfant reste la source détaillée.

---

# 45. Concurrence des batchs

Ne pas introduire obligatoirement du parallélisme multi-batch dans V1.

Respecter :

```text
max_concurrent_heavy_jobs
```

Si l’environnement n’autorise qu’un job lourd :

```text
batchs séquentiels
```

Si ultérieurement plusieurs slots sont autorisés, l’orchestrateur pourra exploiter plusieurs enfants en parallèle sans changer le manifest ni la science.

Ne pas coder une hypothèse `un seul batch à la fois` profondément dans le modèle de données.

---

# 46. Tests obligatoires — Combinaisons

Tester au minimum :

```text
comptage exact
ordre canonique
slicing
concaténation slices
profondeur 1/2/3
context jamais target
absence de doublons
plan SHA stable
modification du global après création sans effet sur le plan
```

---

# 47. Tests obligatoires — WF batché

Tester :

```text
1 batch vs N batchs scientifiquement équivalents
enfant masqué Historique
parent visible
manifest correctement figé
batch completed jamais recalculé
batch failed repris avec même run_id
échec pendant agrégation sans relancer les enfants
manifest périmé réconcilié
child fingerprint incohérent refusé
```

---

# 48. Tests obligatoires — slots et reprise

Tester explicitement :

```text
max_concurrent_heavy_jobs = 1
```

et prouver qu’il n’y a aucun deadlock parent/enfant.

Tester également :

```text
crash parent pendant enfant
crash enfant
PID faux négatif
heartbeat
lease actif
reprise après interruption
```

Ne pas régresser le correctif récent `interrupted`.

---

# 49. Tests obligatoires — XGBoost

Tester :

```text
population < plafond → 100 %
population = plafond → 100 %
population > plafond → cap exact

sampling déterministe
indépendant de l'ordre
indépendant du partitionnement WF
stratification stable
aucun double sampling

anciens runs → per_target_v1
nouveaux runs → global_stratified_v2
```

---

# 50. Tests obligatoires — paramètres de seuils

Tester :

```text
< 500 modèles → tous
= 500 → tous
> 500 → sample exact de 500

set/direction correctement comptés
sample stable
aucune dépendance aux batchs WF
```

---

# 51. Tests obligatoires — calibration finale

Prouver que sous la nouvelle politique :

```text
threshold_parameter_calibration
→ peut être samplée

threshold_calibration
→ reçoit tous les modèles attendus
```

Aucune deuxième réduction cachée.

---

# 52. Tests obligatoires — End-to-end

Injecter un échec à chacune des étapes :

```text
WF
XGBoost
Threshold Parameter Calibration
Threshold Calibration
Promotion
```

Pour chaque test :

```text
reprendre
même parent
mêmes enfants déjà créés
étapes completed non recalculées
étape échouée reprise
pipeline continue
```

---

# 53. Tests obligatoires — auto-promotion

Tester :

```text
promotion auto off
promotion auto on

holdout off + promotion on → lancement refusé

Up candidat → promotion
Up non candidat → pas de promotion

promotion partielle + crash
→ reprise idempotente
→ aucun doublon
```

---

# 54. Tests obligatoires — historique

Tester :

```text
ancien run sans metadata → visible

WF parent → visible
WF batch child → caché

End-to-end → visible
XGB child scientifique → visible
Threshold parameter child → visible
Threshold child → visible
```

---

# 55. Tests obligatoires — compatibilité historique

Tester :

```text
désérialisation vieux RStockConfig
vieux ExperimentSpec
vieux sampling per_target_v1
vieux WF sans parent
restart vieux WF
duplicate paramètres du run
duplicate paramètres actuels
ancien checkpoint + fingerprint persisté
```

Aucune migration silencieuse vers la nouvelle sémantique.

---

# 56. Documentation

Mettre à jour au minimum :

```text
README.md
MIGRATION.md
rstock/docs/user_guide.md
ROADMAP.MD si pertinent
AGENTS.md seulement si une règle durable nouvelle doit être imposée
```

Le guide utilisateur doit expliquer clairement :

```text
batch technique
sous-batch mémoire
WF parent
End-to-end
reprise
sampling global
promotion automatique
```

---

# 57. Fichiers probablement touchés

L’audit a identifié notamment :

```text
rstock/config.py
rstock/application/domain.py
rstock/application/repository.py
rstock/application/runner.py
rstock/application/worker.py
rstock/application/workflows.py
rstock/checkpoints.py

rstock/combinations.py
rstock/parallel.py
rstock/streaming_walk_forward.py

rstock/calibration.py
rstock/threshold_parameter_calibration.py
rstock/threshold_calibration.py

rstock/application/production_services.py
rstock/application/history_analysis.py

rstock/application/streamlit_app.py
rstock/application/history_ui.py
rstock/application/experiment_duplication.py
```

Modules nouveaux possibles :

```text
rstock/combination_planning.py
rstock/application/orchestration.py
rstock/application/promotion_policy.py
```

La liste exacte doit rester guidée par la conception réelle et éviter les refactors inutiles. 

---

# 58. Ordre de développement obligatoire

Ne pas implémenter tout ceci en une seule passe.

### Phase 1 — Contrats, config et compatibilité

Implémenter :

```text
nouveaux champs RStockConfig
historical missing defaults
JobTypes nécessaires
metadata relationnelles
ExperimentSpec
fingerprints historiques
repository parent/enfants
tests
```

Aucun nouveau comportement scientifique.

**Stop après tests complets.**

### Phase 2 — CombinationPlan

Implémenter :

```text
count
slicing
lazy iteration
plan version/hash
preview UI partagée
équivalence avec générateur historique
```

Pas encore de WF multi-enfants si ce n’est pas nécessaire au test.

**Stop après tests complets.**

### Phase 3 — WF parent/enfants

Implémenter :

```text
batch manifest
child creation
slot lifecycle
resume
aggregation parent
Batchs WF UI
Historique hidden technical children
scientific equivalence
```

**Stop après validation réelle.**

### Phase 4 — Calibrations globales

Implémenter :

```text
qualification depuis WF parent
XGB global_stratified_v2
sampling manifest
threshold parameter global sampling
threshold final exhaustive
compatibilité per_target_v1
```

**Stop après validation scientifique.**

### Phase 5 — End-to-end

Implémenter :

```text
END_TO_END parent
pipeline manifest
stage children
resume
cancel propagation
holdout validation
```

**Stop après tests d’échec injecté.**

### Phase 6 — Promotion automatique

Implémenter :

```text
promotion policy hors UI
Candidat / Non candidat
auto promotion
idempotence
reprise partielle
```

### Phase 7 — UI et documentation finale

Polish seulement après validation du moteur.

L’ordre général rejoint celui recommandé par l’audit, qui place lui aussi contrats, planificateur, WF parent/enfants, calibrations globales, End-to-end puis promotion/UI dans cet ordre. 

---

# 59. Procédure Codex pour chaque phase

Avant de coder chaque phase :

```text
1. Relire AGENTS.md
2. Relire ce cadre
3. Inspecter les fichiers réellement concernés
4. Présenter brièvement le plan de la phase
5. Identifier toute contradiction découverte dans l'existant
6. Ne pas modifier la portée fonctionnelle sans accord
```

Puis implémenter uniquement cette phase.

À la fin :

```text
fichiers modifiés
architecture retenue
compatibilité historique
tests ciblés
suite complète
risques/restes
aucune modification hors portée
```

Ne pas commencer automatiquement la phase suivante.

---

# 60. Critères d’acceptation globaux

La feature complète n’est terminée que si :

```text
un petit WF donne le même résultat qu'avant

un WF batché donne le même résultat scientifique qu'un mono-run équivalent

une panne de batch ne force pas à refaire les batchs réussis

une panne d'étape End-to-end ne force pas à refaire les étapes réussies

les paramètres sont figés au lancement

un changement global n'altère pas les runs existants

XGBoost est calibré globalement, jamais par batch

aucun double sampling n'existe

la calibration des paramètres de seuils peut être plafonnée

la calibration finale traite toute la population

les anciens runs restent lisibles/rejouables

les batchs techniques ne polluent pas Historique

la promotion auto est idempotente

promotion auto exige un holdout

aucun entraînement ni activation automatiques n'ont été ajoutés

aucun deadlock avec max_concurrent_heavy_jobs=1
