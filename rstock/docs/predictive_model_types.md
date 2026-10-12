# Types de modèles prédictifs

Chaque job Walk-forward ou End-to-End utilise un seul `predictive_model_type`, figé dans son snapshot.
Les quatre types se comparent en exécutant des jobs distincts, puis en sélectionnant leurs résultats dans Comparer.

| Type | Entrées | Préfiltre |
| --- | --- | --- |
| `external_only` (défaut, y compris snapshots historiques sans champ) | Prédicteurs actuels | Optionnel |
| `constant_probability` | Aucune | Non applicable |
| `target_only` | Retards intrajournaliers du titre cible | Non applicable |
| `target_and_external` | Retards du titre cible et des externes sélectionnés | Optionnel, sur les externes |

Les variables calendaires explicitement autorisées restent disponibles dans le mode historique `external_only`.
Les nouveaux types n'en ajoutent pas. La profondeur des combinaisons compte les externes ; les retards de la cible
sont ajoutés systématiquement dans le mode mixte. Constant et cible seule produisent un candidat par cible.
Le `Set` reste une clé d'exécution locale au run ; les identités directionnelles versionnées incluent le type.

## Labels et entraînement

<a id="logistique-phases-1-2"></a>

### Algorithme logistique L2 : phases 1 et 2

`predictive_algorithm` distingue `xgboost`, `logistic_l2` et `constant_probability`.
Les types de variables existants restent dans `predictive_model_type`. La valeur
historique `constant_probability` de ce dernier champ reste reconnue; le nouvel
algorithme constant normalise ce même contrat interne, sans utilisation de variables.
La phase 3 expose deux sélecteurs communs : **Algorithme prédictif** (XGBoost,
Régression logistique L2, Probabilité constante) et **Type de variables** (externes,
cible seule, cible + externes). Le second est désactivé pour la constante.
Lancements, paramètres et dérivations WF/E2E utilisent ce composant; Comparer
et les affichages de configuration distinguent aussi ces deux dimensions.

La logistique est utilisable dans un WF autonome, ses lots, son Holdout final et
les étapes scientifiques E2E, depuis les formulaires. Ses paramètres
sont `logistic_l2_penalty=0.05`, `logistic_max_iterations=1000`,
`logistic_gradient_tolerance=1e-8` et `logistic_objective_tolerance=1e-12`.
L'objectif est moyenne log loss + pénalité × somme des coefficients au carré,
sans pénaliser l'intercept. Standardisation moyenne/écart-type `ddof=0` sur le
train seulement, écart-type nul remplacé par 1; solveur L-BFGS-B sans poids de
classes. Up/Down sont entraînés séparément, avec les mêmes labels et règles WF.
La sélection des tours XGBoost est sans objet. Le préfiltre conserve son moteur
XGBoost dédié, indépendamment de l'algorithme final.

L'apprentissage monoclasse, les entrées non finies et la non-convergence provoquent
une erreur explicite, sans substitution de modèle. L'annulation est vérifiée pendant
l'optimisation. Les fenêtres enregistrent convergence, itérations, durée et identité
d'apprentissage; le Holdout enregistre les mêmes diagnostics essentiels.
Les identités logistiques incluent le contrat versionné et la pénalité, sans modifier
les identités historiques XGBoost. Les empreintes persistées des checkpoints restent
l'autorité lors de la reprise.

Les artefacts `*.logistic.json` conservent coefficients, standardisation, ordre des
variables et diagnostics. Le chargeur valide leur version et leurs dimensions;
les bundles et métadonnées XGBoost historiques restent lisibles. L'absence historique
des nouveaux champs garde XGBoost (ou l'ancien type constant); les réglages logistiques
par défaut sont inactifs. Une valeur explicitement enregistrée reste prioritaire.

E2E conserve qualification, calibration des seuils et Holdout dédié. La calibration
XGBoost est marquée `not_applicable` avec raison `logistic_l2`; aucune pénalité L2
n'est optimisée automatiquement. Les audits d'entraînement logistiques sont persistés
dans `logistic_training.csv`. Comparer reconstruit la constante sur l'apprentissage
commun compatible, également pour les variantes logistiques; les labels incompatibles
restent refusés. Les log loss brutes de cibles à seuils différents ne sont pas
présentées comme une comparaison alignée.

Changer l'algorithme ou la pénalité dans une dérivation impose la reprise au WF
et l'invalidation aval; les forks aval verrouillent ces paramètres. Les snapshots,
candidats compatibles et sources physiques restent figés. Les anciens contrats
XGBoost et constantes conservent leur interprétation sans migration des runs.
Forward, Production et promotion automatique prennent en charge la logistique
avec variables externes seulement, selon les règles de qualification existantes.
La validation temporelle automatique E2E prend également en charge la logistique.
Elle conserve le décalage de 63 séances, les critères temporels existants et la
revalidation forcée des candidats avec les paramètres L2 et seuils de référence.
La calibration XGBoost reste sans objet; aucun paramètre XGBoost n'est lu ou figé
pour la revalidation logistique. Les snapshots, périodes verrouillées, empreintes
et checkpoints existants gouvernent sa reprise. La promotion automatique exige
toujours la réussite des critères temporels et l'admissibilité des candidats.
Comme pour XGBoost, cette option exige un E2E de référence à offset 0 sans cutoff
historique explicite. Ces capacités techniques ne prouvent aucune efficacité prédictive.

Le gel Forward persiste les modèles Up/Down `*.logistic.json`, coefficients,
standardisation train, ordre des variables, contrat L2 et empreintes des fichiers.
L'inférence charge ces modèles sans entraînement. Une reprise vérifie données,
seuils et contrat avant de réutiliser un modèle de gel terminé; les checkpoints
d'inférence conservent leur progression et la précision des probabilités CSV.
Un artefact modifié ou une configuration incompatible est refusé.

Le registre Production conserve l'algorithme et les quatre réglages L2 dans sa
configuration source. Les paramètres XGBoost sont vides et la sélection des tours
est sans objet. Entraînement, observation, activation et prédiction réutilisent
les services existants. Les paramètres globaux ne remplacent pas le contrat source;
un ancien registre sans algorithme explicite reste XGBoost. L'entraînement publie
Up/Down et métadonnées atomiquement, puis réconcilie le registre; si cette dernière
écriture est interrompue, une reprise identique réutilise les artefacts vérifiés.

### Volatilité passée optionnelle du titre cible

`target_volatility_enabled` (défaut `False`) ajoute une seule variable à chaque modèle
XGBoost final, sans changer ses combinaisons. `target_volatility_sessions` (défaut `21`,
minimum `2`) est le nombre de rendements logarithmiques clôture ajustée/clôture ajustée.
L'écart-type `ddof=1` est annualisé par `sqrt(252)` et décalé d'une séance : pour J,
les N rendements se terminent à J−1. N+1 clôtures valides et consécutives sur le calendrier
du marché sont exigées ; aucune valeur manquante n'est remplie. La constante ignore
cette option, et le préfiltre conserve exclusivement ses variables historiques.

Le réglage se trouve dans Paramètres → Modèles et combinaisons, dans les lancements
WF/E2E et leurs dérivations. `external_only` enrichi utilise aussi la volatilité cible,
ce que la liste des colonnes et Comparer rendent explicite. Les snapshots historiques
sans ces champs restent désactivés avec N=21 inactif.

Le préfixe nécessaire au calcul est acquis séparément puis retiré de la période scientifique.
Un dérivé fige les clôtures compatibles du cache dans un nouvel artefact référencé par SHA-256,
en conservant les labels, retards et dates du parent. Il ne consulte plus le cache mutable lors
de la reprise. Les colonnes, définition, couverture et provenance accompagnent les résultats.
La liste ordonnée des variables et les réglages propres au modèle restent figés en Forward/
Production pour les types actuellement autorisés ; les autres types restent bloqués.

Les données Yahoo sont **rétrospectives, sans garantie point-in-time**. Le décalage J−1
empêche l'usage de la clôture J mais ne recrée pas les millésimes historiques d'ajustement.
Les sources sont marquées `retrospective_not_pit` : le test historique est exploratoire.
Une conclusion prospective exige un protocole arrêté avant les nouveaux labels et des
entrées/prédictions archivées à leur date d'utilisation. Aucun changement de N ne doit être
choisi a posteriori selon les performances.

Up et Down conservent leurs définitions binaires et leurs seuils actuels. Ils ne sont pas complémentaires.
Les décisions conservent la convention strictement supérieure au seuil de probabilité.

Le constant estime séparément chaque prévalence sur les labels connus de la fenêtre d'apprentissage.
Il la fige pendant le bloc de test, puis la réestime à l'origine suivante selon la géométrie expanding/rolling.
Il n'utilise ni observations futures ni lissage. Les prévalences 0 et 1 sont valides ; un apprentissage sans label
connu est refusé. Aucun XGBoost n'est chargé pour ce modèle. L'étape de calibration XGBoost E2E porte la raison
`constant_probability` et le statut `not_applicable`, sans paramètres sélectionnés fictifs.
Les calibrations de seuils utilisent les probabilités de développement ; le holdout reste séparé.

Les trois types XGBoost réutilisent les moteurs et politiques de sélection des tours existants.
Le préfiltre conserve ses paramètres dédiés et sa méthode scientifique, indépendamment du type du modèle final.

### Algorithme et comparaison du préfiltre

`prefilter_algorithm` choisit XGBoost (défaut et interprétation historique) ou
`logistic_l2`, indépendamment du moteur WF final. Les quatre réglages
`prefilter_logistic_*` réutilisent le moteur, objectif et standardisation L2
existants, avec configuration figée propre au préfiltre. Les paramètres XGBoost
restent conservés mais inactifs lorsque L2 est utilisé; les tours sont sans objet.
La sélection reste univariée Up, avec les mêmes critères, Top-N et corrélation.

Chaque fenêtre compare les probabilités du modèle à la prévalence Up du train,
sur exactement ses observations test. La constante apporte log loss, Brier et
AUC descriptive lorsqu'elle est définie; elle ne participe jamais au classement.
Une erreur d'apprentissage L2, notamment monoclasse ou non-convergence, reste
explicite, sans remplacement silencieux par XGBoost ou par la constante.

Le snapshot complet demeure figé jusqu'au cutoff. Le développement se termine
avant `ordered.index[-final_holdout_size]`, comme le WF existant; aucune seconde
exclusion du Holdout n'est appliquée. La taille 63 demeure le défaut configurable.
Les origines de consensus sont également vérifiées contre le Holdout principal.
Les labels intrajournaliers du train sont connus après clôture, avant la prochaine
séance test. Cette causalité ne constitue pas une garantie fournisseur Point-in-Time.

`prefilter_windows.csv` conserve bornes, nombres d'observations, empreintes des
valeurs/dates train et test, labels, ordre des variables et métriques. Le contrat
d'évaluation résume la couverture et ses empreintes; le contrat public préfiltre
schema 3 protège ces fichiers. Les schemas historiques 1/2 restent lisibles.
Comparer signale une comparaison directe non vérifiable si l'audit est absent ou
incomplet, et incompatible si observations, cibles ou frontières diffèrent.
Les moyennes de pertes pondèrent les fenêtres par leurs observations, avec les
chevauchements existants; ce ne sont pas des observations indépendantes.

Une dérivation d'algorithme réutilise snapshots et paires brutes vérifiés, puis
réévalue les mêmes fenêtres compatibles dans ses propres checkpoints. Elle ne copie
pas les résultats de sélection du parent. Un historique sans audit peut servir
de source de données pour deux nouveaux runs XGBoost/L2; il ne devient pas une
référence comparable par simple ajout de métadonnées. Les résultats WF de
développement partagent le biais de sélection du préfiltre; le Holdout et le
Forward conservent leurs frontières existantes.

## Comparaison commune

Comparer recalcule les métriques à partir des prédictions individuelles disponibles, sur le développement
avant qualification et, lorsque les fichiers existent pour tous les runs, sur le holdout.
Pour chaque cible et direction, les dates doivent être présentes dans tous les runs et tous leurs candidats concernés.
Chaque candidat est évalué séparément sur ces mêmes dates : aucune moyenne de scores ne crée un modèle combiné.
Les scores, labels et décisions doivent être valides ; les labels doivent correspondre entre les runs.

En cas de fenêtres WF chevauchantes, une seule prédiction est conservée par candidat et date : celle de l'origine
d'apprentissage la plus récente, strictement antérieure à la prédiction. Des prédictions contradictoires à une même
origine rendent la comparaison indisponible. Les dates et origines d'apprentissage peuvent différer entre modèles ;
la comparaison décrit une population de test commune et ne démontre pas à elle seule une supériorité statistique.
Les décisions holdout sont celles obtenues avec les seuils figés, jamais des décisions reconstruites avec le seuil global.

Lorsqu’une constante est comparée à des variantes XGBoost en développement, Comparer reconstruit
un calendrier et un masque de disponibilité communs à partir des snapshots locaux et des colonnes
requises par chaque variante. Les fenêtres sont produites par le mécanisme WF existant. Leurs dates,
effectifs et labels sont vérifiés contre les fenêtres et prédictions XGBoost enregistrées, sans entraînement.
La constante est alors recalculée sur les seuls labels de cet apprentissage, strictement antérieur au test,
et figée pendant chaque bloc test. Les diagnostics exposent les dates, effectifs et empreintes des
observations/labels d’apprentissage et des dates test. Les runs historiques restent inchangés.

Si les entraînements ou frontières diffèrent, ou si les snapshots nécessaires sont absents, cette
reconstruction est refusée : les métriques historiques sur dates test communes restent disponibles,
avec un avertissement explicite indiquant que l’apprentissage n’est pas aligné. Le fonctionnement
autonome de la constante et la comparaison holdout conservent leurs probabilités enregistrées.

Une intersection vide, des labels incohérents ou des prédictions absentes sont signalés explicitement.
Aucune métrique agrégée n'est présentée comme une comparaison sur observations communes.

## Résultats, dérivation et opérations

La qualification et l'exécution holdout sont deux états distincts, indépendants de la disponibilité des données.
`no_qualified_candidates`, `not_executed` et `unavailable` ne sont jamais remplacés par une métrique zéro.
Un E2E sans candidat WF qualifié conserve les résultats WF et termine normalement avec les étapes aval non exécutées.
Les seuils de qualification ne sont pas assouplis pour favoriser le constant.

Un changement de type exige de recalculer le WF et ses consommateurs. Les points de dérivation aval verrouillent le type.
Le snapshot et les univers restent figés ; les candidats sans externes sont reconstruits, tandis que les populations
externes existantes sont réutilisables entre les deux types qui les consomment. Un snapshot privé des colonnes requises
est refusé avant lancement. Les snapshots historiques absents du nouveau champ conservent explicitement `external_only`.

Les nouveaux types sont destinés aux jobs de recherche. Leur promotion et leur activation Forward/Production
sont bloquées aux frontières de contrat. Les opérations existantes restent en `external_only`, même si le défaut
choisi dans les paramètres de recherche change. Les checkpoints restent liés au fingerprint du snapshot ; les batches
committés sont réconciliés lors de la reprise et les manifests E2E sont rechargés avant les mises à jour finales.
