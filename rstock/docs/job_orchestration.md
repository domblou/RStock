# Jobs, lots et reprise durable

Contrats de maintenance des workflows actuels. Pour lancer et suivre une expérience,
voir le [guide utilisateur](user_guide.md). Les règles scientifiques restent dans
la [roadmap scientifique](scientific_roadmap.md) ; les politiques de calibration
sont décrites dans [Sélection et calibrations XGBoost](CHRONOLOGICAL_XGBOOST.md).

## Architecture et autorités persistantes

`ExperimentService`, `RunService`, `RunRepository`, `WorkflowRegistry` et le worker
partagent l'infrastructure des jobs de recherche et opérationnels. `ExperimentSpec`
fige configuration, sources et versions ; `metadata.json` porte les relations et
la visibilité, sans modifier l'identité scientifique.

- `status.json` et `progress.json` de chaque enfant font autorité pour son état.
  Un manifest parent décrit le plan et les dépendances, pas un second statut autonome.
- Les manifests réservent les identifiants avant la création des enfants. Une
  reprise conserve ces réservations et vérifie relations, fingerprints et artefacts.
  Les anciens identifiants déterministes restent compatibles ; les nouveaux IDs
  réservés ne sont pas recalculés à partir du fingerprint.
- Les rôles `standalone`, `pipeline_parent`, `pipeline_stage` et `technical_batch`
  distinguent expérience et exécution technique. Sans métadonnées historiques,
  un run est interprété comme autonome et visible, sans migration physique.
- Avant une écriture finale, recharger ou réconcilier les données persistantes
  susceptibles d'avoir été modifiées par un autre composant.

Sources : `application/domain.py`, `repository.py`, `runner.py`, `worker.py`,
`workflows.py`, `walk_forward_batches.py` et `end_to_end.py`, sous `rstock/`.

## Analyses scientifiques sur artefacts existants

`SCIENTIFIC_ANALYSIS` utilise `RunService`, `RunRepository`, `WorkflowRegistry`,
le worker, les leases et les slots existants. Le catalogue et le contrat léger
de module sont dans `application/scientific_analysis.py` ; un module définit
préconditions, paramètres, traitement, sorties et vues spécialisées.

La source est un WF terminé, directement ou résolu depuis le manifeste E2E,
y compris une étape héritée. L'analyse fige propriétaire, source physique, étape,
version de définition, paramètres et empreintes dans son propre manifeste.
Un jeton de soumission empêche les doubles lancements accidentels ; un nouveau
jeton permet une exécution indépendante. La source n'est pas réécrite.

Le précontrôle ne score pas les probabilités et ne hache pas les grandes
prédictions lorsque le lancement est bloqué. La résolution du calendrier peut
nécessiter une première lecture des colonnes utiles du CSV par blocs, puis utilise
un cache mémoire invalidé par les révisions des artefacts. À la soumission et à l'exécution, les contrats et
empreintes requis sont vérifiés. Les checkpoints et sorties d'analyse sont
propres au job ; la publication complète et la restauration des sorties doivent
rester distinctes des artefacts partiels. Une reprise refuse des paramètres ou
une source modifiés. Aucun entraînement n'est une étape de ce workflow.

Le module `rare_relations_analysis.py` conserve les contrôles des masques,
constantes train et pertes appariées, mais n'exécute pas de sélection scientifique
indéfinie. Son calendrier est l'union ordonnée des séances évaluables réellement
archivées, vérifiées contre les fenêtres et le snapshot. Une matrice booléenne
compacte conserve la couverture de chaque relation, sans inventer de lignes.
Les fenêtres partielles restent visibles. Une proportion de sélection (60 % par
défaut, arrondi inférieur) ou un nombre de séances résout une seule coupure commune ;
l'évaluation couvre toutes les séances restantes. Le précontrôle retourne les
paramètres résolus, dates, empreinte du calendrier et références source à figer
dans le manifeste normal du job.

La gestion de protocoles est reportée, sans registre, structure, empreinte ou
approbation dédiée dans le code des analyses. Le lancement de persistance reste
bloqué par l'absence de critères de sélection, support, traitement des couvertures,
contrôle des recherches multiples/dépendances et critères d'évaluation/décision.
Le découpage temporel seul ne définit pas ces règles. Les anciennes métadonnées
restent lisibles sans dépendance active ni migration ; une ancienne exécution ne
reprend pas silencieusement avec la nouvelle sémantique (définition version 2).

## Walk-forward : deux niveaux de lots

| Niveau | Responsabilité |
|---|---|
| Enfant `WALK_FORWARD_BATCH` | Partition du plan effectif ; véritable run technique avec ID et checkpoints. |
| Sous-batch interne | Mémoire, parallélisme, progression et checkpoint à l'intérieur du WF. |

`combination_planning.py::CombinationPlan` compte et parcourt une population
paresseuse, déterministe et versionnée (`count`, `slice`, `iter_range`, `plan_sha256`).
Il préserve l'ordre canonique, les sets et les identités sans matérialiser toute
la population. La prévisualisation UI utilise ce même plan.

La prévisualisation compte les combinaisons **brutes** ; le préfiltre global ou
hérité détermine ensuite le plan **effectif**, figé avant partitionnement.
Pour une capacité C, les nombres de lots sont respectivement `ceil(brut / C)`
et `ceil(effectif / C)`. Un plan effectif vide est refusé. Un seul lot reste
sur le chemin mono-run ; plusieurs lots créent les enfants techniques.
Le découpage ne modifie pas le préfiltre et n'apporte aucune preuve que ses biais
scientifiques sont résolus.

`orchestration/walk_forward_batches.json` conserve les empreintes du parent,
des plans et des univers, la politique/digest du préfiltre, les comptes
`raw_combination_count`, `preview_batch_count`, `prefiltered_combination_count`,
`planned_batch_count` et les réservations. Chaque réservation porte la plage
`[range_start, range_stop)`, le nombre de combinaisons, l'ID et le fingerprint
attendu. Le snapshot préparé est chargé depuis le parent par tous les enfants :
aucun téléchargement ou cutoff indépendant dans un batch.

Les enfants sont actuellement exécutés séquentiellement. Le parent ingère leurs
checkpoints dans l'ordre du plan puis réalise l'agrégation, la qualification,
le Holdout lorsqu'il est demandé, le risque et le classement globaux.
Les consommateurs lisent les artefacts canoniques dans `results/` du parent :
`windows.csv`, `predictions.csv`, `aggregate_by_window.csv`, `aggregate_by_set.csv`,
`aggregate_global.csv`, `qualification.csv`, `final_holdout.csv`,
`final_holdout_predictions.csv`, `selection_results.csv`, les artefacts de risque
et `run_configuration.json`, selon les étapes exécutées.
Les batchs techniques sont masqués de l'Historique et consultables via **Batchs WF**.

**Invariant de maintenance :** à données et paramètres identiques, le batching
préserve les résultats scientifiques d'un mono-run équivalent. Les tests de
planification, batching et reprise couvrent ce contrat ; leur présence ne prouve
pas une réussite actuelle de la suite ni un avantage prédictif.

## Pipeline expérimental et promotion

`orchestration/pipeline.json` réserve les étapes, leurs contrats de paramètres,
sources, dépendances et digests. La structure dépend des versions persistées :

- À partir de la version 3 : WF → calibration XGBoost → paramètres de seuils →
  seuils → évaluation Holdout → qualification de promotion.
- À partir de la version 4, avec préfiltre actif et parcours compatible : un
  préfiltre autonome précède le WF.
- Les anciennes chaînes conservent leurs étapes composites en lecture/reprise.
  Les dérivations réutilisent les étapes selon leurs contrats de propriété et
  invalident les étapes dépendantes ; elles ne changent pas silencieusement de protocole.

Les calibrations consomment la population scientifique du WF parent, jamais des
calibrations séparées par batch. La promotion automatique optionnelle est une
étape interne persistée, pas un job enfant autonome. Elle exige le Holdout et
les critères figés du run ; seuls les candidats Up admissibles sont considérés,
une fois par combinaison. `PromotionService` écrit le registre ; le coordinateur
et l'exécution utilisent `orchestration/promotion_trigger.json` et
`orchestration/promotion.json` pour figer la décision, la population, les digests
et les checkpoints par identité. Une publication antérieure à un crash est
réconciliée sans doublon. Promotion, entraînement et activation restent distincts.

La validation temporelle moderne comporte une redécouverte décalée et une
revalidation forcée des candidats de référence avec paramètres et seuils figés.
La comparaison et ses digests sont persistés dans
`orchestration/temporal_validation.json` et `results/temporal_validation_comparison.json`.
Les états `passed`, `failed`, `inconclusive`, `invalid` restent distincts ; la
largeur maximale d'intervalle concerne l'avantage de précision. Avec validation
active, la promotion exige `passed` et, pour le parcours à trois passes, utilise
les candidats qualifiés de la revalidation forcée. Les anciennes versions gardent
leur contrat. Le [guide](user_guide.md) décrit les passes et critères de lecture.

## Ressources, interruption et reprise

`RunLease` protège l'exécution exclusive d'un run. `SlotLease` limite les jobs
lourds avec mutex de fichiers et file d'attente persistante ; le worker conserve
son RunLease mais libère le slot avant d'exécuter un enfant, puis le reprend.
Ce mécanisme évite que le parent bloque son enfant avec un seul slot autorisé.
Les heartbeats et contrôles de processus participent à la récupération des leases.
Le parallélisme interne des combinaisons et les threads XGBoost sont distincts
du nombre de slots : augmenter tous ces paramètres peut saturer CPU et mémoire.

`CheckpointManager` conserve snapshot, phases et sous-batches terminés.
Une reprise garde parent, enfants, plan, paramètres et sources ; elle réutilise
les étapes terminées **après validation des contrats et empreintes**. Un enfant
en échec, annulé ou interrompu reprend sous le même ID. Une panne d'agrégation
ne relance pas les enfants terminés. Un résultat publié avant interruption est
validé avant de terminer le run. Des données ou artefacts incompatibles bloquent
la réutilisation plutôt que déclencher une substitution silencieuse.

L'annulation du parent est prise en compte par l'enfant actif, arrête les étapes
suivantes et conserve le travail checkpointé. Les erreurs gardent l'ID enfant,
l'étape et les logs accessibles. La progression décrit le travail logique ;
elle n'est pas une estimation garantie de durée restante.

Une relance/duplication crée un nouveau run ; une dérivation crée une nouvelle
chaîne avec héritage explicite. Aucune de ces actions n'est une reprise.
Les paramètres globaux modifiés après lancement n'altèrent pas le snapshot.
Pour un ancien checkpoint, le fingerprint persisté faisant autorité est conservé,
sans recalcul aveugle depuis une configuration enrichie de defaults modernes.
La disponibilité des artefacts après purge conditionne les opérations possibles.

Limite actuelle : la consultation UI peut appeler la récupération automatique
de Forward pending ; elle ne doit pas être assimilée à une autorisation de lancement.

Contrats de tests : `test_combination_planning.py`, `test_walk_forward_batching.py`,
`test_walk_forward_resume.py`, `test_end_to_end.py`, `test_checkpoints.py`,
`test_heavy_slot_leases.py`, `test_heavy_slot_fairness.py`, `test_publication_recovery.py`
et `test_calibration_sampling.py`, sous `tests/`. Aucun calcul métier ou test de
performance n'est nécessaire pour consulter ce document.
