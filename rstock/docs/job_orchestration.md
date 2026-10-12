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

Les WF logistiques utilisent ces mêmes lots, checkpoints et réservations d'enfants.

Les deux moteurs de préfiltre utilisent aussi ces lots. Les audits des fenêtres
et de la constante sont committés avec chaque lot, puis exportés en
`prefilter_windows.csv` et `prefilter_evaluation_contract.json`. Le contrat public
schema 3 en protège les empreintes. Le checkpoint contrôle l'entrée figée, la
frontière Holdout et le contrat L2 actif; une reprise réutilise les lots committés
et refuse une entrée modifiée. Les archives schemas 1/2 gardent leur interprétation.
L'algorithme et ses paramètres sont dans le snapshot/fingerprint du job; les identités
de modèles incluent le contrat logistique versionné. Les lots terminés sont réutilisés
après vérification, y compris après interruption de l'agrégation parent. Standardisation
et coefficients sont propres à chaque train; ils ne sont jamais repris d'une autre
origine. Les diagnostics de convergence/itérations/durée sont persistés par fenêtre
et direction. Les réglages de tours XGBoost ne s'appliquent pas au moteur logistique.

E2E logistique réutilise les enfants et contrats existants. Le contrat versionné L2
est figé par étape; la calibration XGBoost conserve sa place et ses artefacts de
provenance avec statut sans objet, sans paramètres XGBoost fictifs. Les audits
logistiques des probabilités sont gelés avec les résultats. Les changements
d'algorithme/L2 appartiennent au fork WF et invalident ses consommateurs; les
paramètres des forks aval restent verrouillés. Les contrats historiques XGBoost
ne reçoivent pas de paramètres logistiques actifs lors d'une reprise.

Forward/Production logistiques utilisent les mêmes services, jobs et checkpoints,
avec variables externes seulement. Les fichiers Up/Down `*.logistic.json` et leurs
empreintes accompagnent les diagnostics d'entraînement et le contrat L2 source.
Le gel Forward publie les métadonnées de chaque modèle atomiquement; une reprise
identique vérifie son entrée, ses seuils et fichiers avant réutilisation. Le Forward
ne réentraîne pas les modèles et conserve la précision des probabilités à la
relecture des checkpoints CSV. Le reporter distingue gel entraîné et réutilisé.
L'entraînement Production publie les deux modèles par la transaction existante,
puis met à jour le registre en réconciliant son état persistant. Une interruption
entre publication et registre permet de réutiliser le bundle L2 validé, sans
recalcul. Chaque direction publie sa progression; les paramètres globaux ne
remplacent jamais les réglages L2 figés. Un modèle historique reste XGBoost.

La validation temporelle E2E logistique réutilise les trois passes et les jobs
existants : référence, E2E offset 63 et revalidation forcée. Le job forcé conserve
le contrat L2 dans sa configuration, les seuils de référence et le verrou de
période du deuxième E2E; ses sources et paramètres XGBoost sont absents. Les
comparaisons et critères de promotion restent inchangés. Les enfants terminés
sont réutilisés après interruption, avec vérification des empreintes persistées.

## Analyses scientifiques sur artefacts existants

`SCIENTIFIC_ANALYSIS` utilise `RunService`, `RunRepository`, `WorkflowRegistry`,
le worker, les leases et les slots existants. Le catalogue et le contrat léger
de module sont dans `application/scientific_analysis.py` ; un module définit
préconditions, paramètres, traitement, sorties et vues spécialisées.

Chaque `AnalysisDefinition` doit aussi fournir `interpret_results(result, manifest,
status)`, une fonction pure et spécifique à son type. Le contrat retourné contient
une `version`, trois listes de paragraphes non vides (`understanding`, `findings`,
`limits`) et un `verdict` non vide. Aucun appel IA, calcul scientifique ou accès
aux données dans cette fonction. Les règles scientifiques d'interprétation sont
précisées par le directeur avant implémentation; les seuils proviennent des
artefacts persistés et du contrat de version explicitement pris en charge.
Un type/version incompatible, un résultat incomplet, une règle manquante ou des
compteurs incohérents ne deviennent jamais un résultat nul ou positif.

Le worker valide ce contrat et écrit `scientific_interpretation.json`, avec
l'empreinte des entrées scientifiques (résultats hors liste de fichiers, type,
version, paramètres effectifs, source et état terminé). Cet artefact fait partie
des sorties gelées et restaurées par le checkpoint existant. Le Résumé affiche
le texte persisté lorsqu'il correspond aux entrées; une divergence rend le verdict
indisponible. Pour les anciens runs sans cet artefact, une interprétation en lecture
seule est possible uniquement si le contrat est pris en charge. L'interface ne lit
que des JSON bornés à 2 Mio, jamais les CSV complets ou snapshots pour interpréter.
Les trois volets sont obligatoires dans le Résumé de toute nouvelle analyse.

La persistance version 3 interprète les compteurs et motifs déjà calculés. Les
nouvelles sorties ajoutent `admissible_count` et `reason_counts` au bilan; les anciens
bilans sans ces champs ne reçoivent pas de motifs inventés. Aucune sélection rend
la proportion de persistance « sans objet » et la grille vide attendue. Les
indéterminées restent au dénominateur de la proportion soutenue et ne sont pas
qualifiées d'échecs scientifiques. L'interprétation ne change aucun seuil ni
critère de sélection/évaluation et ne démontre pas une efficacité prédictive.

Le suivi utilise les événements `phase_event` du `ProgressReporter` et le
`ResourceRecorder` du worker, sans recorder distinct. La persistance expose
préparation/validation source, calendrier/références train, pertes appariées,
sélection, évaluation, export, validation/gel des sorties, puis publication.
Les durées et compteurs CPU/mémoire sont attribués aux phases réellement actives.
Les unités sont les octets hachés/copiés, les lignes validées/scorées, les fenêtres
reconstruites, les relations analysées et les fichiers exportés. Les mises à jour
intermédiaires sont limitées à une par seconde, avec début et fin transmis; les
ressources restent échantillonnées toutes les 5 secondes. Les écritures sont
coopérativement annulables aux limites de blocs. Pour le calendrier partiel,
le total initial est explicitement un maximum issu des fenêtres; la lecture
complète établit le total réellement archivé.
Le plan de progression global utilise des poids de présentation, pas des durées
scientifiquement calibrées. Les sous-étapes peuvent changer d'unité et réinitialiser
leur compteur; l'ETA provient du débit observé. Seule la publication réussie permet
100 % global. Une reprise d'une analyse terminée expose préparation, restauration
et finalisation, sans nouvelles phases de calcul. Une sélection déjà checkpointée
est signalée comme rechargée. Une phase annulée/échouée conserve sa durée et son
état final en télémétrie, sans événement de réussite. Les anciens artefacts ne
sont pas reconstruits rétroactivement.

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

Le module `rare_relations_analysis.py` exécute la sélection et l'évaluation Up sur
les pertes appariées à la constante du train exact. Son calendrier est l'union ordonnée des séances évaluables réellement
archivées, vérifiées contre les fenêtres et le snapshot. Une matrice booléenne
compacte conserve la couverture de chaque relation, sans inventer de lignes.
Les fenêtres partielles restent visibles. Une proportion de sélection (60 % par
défaut, arrondi inférieur) ou un nombre de séances résout une seule coupure commune ;
l'évaluation couvre toutes les séances restantes. Le précontrôle retourne les
paramètres résolus, dates, empreinte du calendrier et références source à figer
dans le manifeste normal du job.

La gestion de protocoles reste reportée, sans registre ni approbation dédiée.
La définition version 3 fige `rules` dans les paramètres à la soumission et refuse
leur modification. Le contrat préparé conserve calendrier, références et empreintes.
Les contrôles scientifiques sont décrits dans le [guide](user_guide.md).
La sélection est commitée dans l'artefact `rare_selection`, puis marquée terminée;
une interruption avant cette marque recalcule la sélection. Une reprise après
cette marque recharge la liste et les scores figés, sans nouvelle sélection.
La lecture des prédictions en blocs valide les doublons interblocs et les labels;
quatre tableaux numériques conservent les pertes par relation/séance, sans imputation.
Une lecture interrompue est recommencée; aucune sortie partielle n'est publiée comme
résultat terminé. Le workflow commun revalide la source avant gel/publication des
sorties et restaure les sorties terminées depuis leurs empreintes checkpointées.

Pour chaque relation complète, avec `x_t=g_t-mean(g)` et `gamma_k=sum(x_t*x_(t-k))/n`,
la variance HAC est `gamma_0+2*sum((1-k/(q+1))*gamma_k)` et l'erreur-type sa racine
divisée par `sqrt(n)`. La p unilatérale est `1-Phi(mean(g)/SE)`; retenir le maximum
aux deux retards L et 2L. Variance non positive/non finie : inférence indisponible.
Holm conserve tous les membres de chaque famille, les non admissibles avec p=1;
ses p ajustées sont le maximum cumulatif de `(m-j+1)*p_(j)`, borné à 1.
`scientific_rules.json` conserve version, règles, coupure, sélection et empreintes.
Les anciennes métadonnées restent lisibles sans migration; les définitions 1/2
ne reprennent pas silencieusement avec la sémantique de la version 3.

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


## Analyses scientifiques : demande légère et préparation worker

Les nouveaux manifests `scientific_analysis.schema_version=2` figent la définition,
sa version, les paramètres demandés, les sources propriétaire/physique et l'identité
(taille, mtime, ctime, inode) des artefacts. L'UI et les méthodes `submit`, `resume`
et `restart` ne parcourent pas les prédictions ni ne calculent leurs empreintes.
La validation légère vérifie disponibilité scientifique, paramètres et métadonnées.
Un protocole connu comme bloqué empêche immédiatement la soumission.

Le run est persisté avant le lancement via `LocalProcessBackend`. Le worker utilise
les leases, slots, ressources et progression existants. Sa phase `preparation`
calcule les digests, vérifie la stabilité des sources et les contrats hérités E2E,
puis résout calendrier et paramètres. Son artefact checkpointé `prepared_contract`
est distinct de la demande : ni `config.json` ni le fingerprint ne sont enrichis.
Le contrat est aussi publié dans `results/analysis_preparation.json`.

La reprise revalide les empreintes dans le worker et réutilise la préparation et
les résultats terminés ; une interruption avant validation complète ne valide pas
une phase. Les erreurs et annulations restent dans le run. Les manifests historiques
v1 conservent leurs paramètres et digests ; leurs validations approfondies sont
également réservées au worker. La publication vérifie les résultats checkpointés.

Les identités de fichiers détectent les mutations ordinaires entre demande et
préparation ; elles ne prouvent pas une immutabilité cryptographique au moment de
la confirmation. Les digests établis par le worker deviennent le contrat de reprise.
Les digests de snapshot et de stages E2E déjà persistés sont vérifiés lorsqu'ils existent.
Aucun protocole scientifique nouveau n'est défini par cette séparation technique.

Le worker produit des fichiers `*.csv.preview.json`, limités à 500 lignes et environ
2 Mio. Les vues d'analyse ne lisent plus les CSV complets. Les anciennes publications
sans aperçu restent archivées ; la vue signale l'absence au lieu de scanner les données.
Tests de contrat : `test_scientific_analysis.py`, `test_rare_relations_analysis.py`,
`test_history_index.py` et `test_publication_recovery.py`, sur fixtures uniquement.
