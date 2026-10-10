# Analyse ponctuelle XGBoost fixe / chronologique

End-to-End fixe : `20261009T115931_8bf594b3bd` ; WF `20261009T115931_e9013b1a13`.
End-to-End chronologique : `20261010T001622_e7f734b61a` ; WF `20261010T001623_73fb23b91f`.

## Comparabilité vérifiée

- 1,814 candidats ; 12,362 fenêtres candidat ; 765,078 observations appariées par direction (1,530,156 lignes directionnelles). Aucune sélection sur admissibilité ou qualification.
- Zéro doublon de clé (candidat, fenêtre, date), zéro observation ou fenêtre non appariée, labels Up/Down strictement égaux. Les CSV de non-appariement sont présents même vides.
- Les deux snapshots préparés sont égaux (valeurs, index, colonnes et types). Même candidat et ordre des variables, mêmes retards/exclusions, seuils de labels, frontières et effectifs train/test, mêmes hyperparamètres hors sélection des tours. Les diagnostics chronologiques concordent avec les features et paramètres de référence.
- Même préfiltre hérité : `20261009T024818_3ce61407de`. Il ne constitue pas une validation externe indépendante ; sa sélection scientifique préalable peut limiter cette indépendance.
- 2 cohortes distinctes de calendrier complet. Les périodes réellement évaluées sont conservées dans les CSV, aucune comparaison de simples numéros de fenêtre entre calendriers.
- Commits enregistrés : fixe `8a86fc75daac699d267e9011e9c9306402cbd3fa`, chronologique `9e4da0dd0aa3c07f70653b9aeb9324b420c0074d`. Le changement de code reste un facteur de confusion possible ; le diff est joint si disponible. Une empreinte de commit ne capture pas d’éventuelles modifications locales historiques.

## Résultats descriptifs

Δ = chronologique moins fixe. Δ négatif favorable pour log loss et Brier ; positif favorable pour AUC.

|Direction|Statut|Fenêtres|Observations|LL fixe pondérée|LL chrono pondérée|Δ LL pondérée|Δ Brier pondéré|Δ AUC moyenne par fenêtre|
|---|---|---:|---:|---:|---:|---:|---:|---:|
|Up|toutes|12362|765078|0.536581|0.537812|0.001231|0.000494|-0.008197|
|Up|optimise|10548|650796|0.541809|0.543256|0.001448|0.000581|-0.009606|
|Up|repli|1814|114282|0.506809|0.506809|0.000000|0.000000|0.000000|
|Down|toutes|12362|765078|0.519929|0.515092|-0.004838|-0.001356|0.001103|
|Down|optimise|10548|650796|0.523582|0.517895|-0.005687|-0.001594|0.001293|
|Down|repli|1814|114282|0.499128|0.499128|0.000000|0.000000|0.000000|

Pour toutes les fenêtres, les valeurs Brier fixe → chronologique et AUC moyenne fixe → chronologique sont :
- Up : Brier 0.175751 → 0.176245 ; AUC moyenne par candidat/fenêtre 0.565345 → 0.557148.
- Down : Brier 0.167405 → 0.166050 ; AUC moyenne par candidat/fenêtre 0.500573 → 0.501677.

### Périodes réelles et cohortes

C01 : 1 702 candidats, 7 fenêtres chacun ; C02 : 112 candidats, 4 fenêtres chacun. Les deux modes suivent exactement le même calendrier à l’intérieur de chaque candidat. La différence de calendrier concerne les cohortes, pas un désalignement fixe/chronologique.

|Cohorte|Début test|Fin test|Direction|Statut|Fenêtres|Observations|Δ LL pondérée|Δ Brier pondéré|Δ AUC moyenne|
|---|---|---|---|---|---:|---:|---:|---:|---:|
|C01|2024-07-12|2024-10-09|Down|repli|1702|107226|0.000000|0.000000|0.000000|
|C01|2024-07-12|2024-10-09|Up|repli|1702|107226|0.000000|0.000000|0.000000|
|C01|2024-10-10|2025-01-10|Down|optimise|1702|107226|-0.005903|-0.001602|0.002083|
|C01|2024-10-10|2025-01-10|Up|optimise|1702|107226|0.001742|0.000433|-0.009612|
|C01|2025-01-13|2025-04-11|Down|optimise|1702|107226|-0.007762|-0.001916|0.004461|
|C01|2025-01-13|2025-04-11|Up|optimise|1702|107226|0.002306|0.001118|-0.009071|
|C01|2025-04-14|2025-07-15|Down|optimise|1702|107226|-0.005163|-0.001705|-0.002575|
|C01|2025-04-14|2025-07-15|Up|optimise|1702|107226|0.001671|0.000686|-0.013133|
|C01|2025-07-16|2025-10-13|Down|optimise|1702|107226|-0.005450|-0.001571|0.000061|
|C01|2025-07-16|2025-10-13|Up|optimise|1702|107226|0.001475|0.000500|-0.008964|
|C01|2025-10-14|2026-01-13|Down|optimise|1702|107226|-0.004526|-0.001325|-0.001199|
|C01|2025-10-14|2026-01-13|Up|optimise|1702|107226|0.001514|0.000597|-0.010080|
|C01|2026-01-14|2026-04-02|Down|optimise|1702|93610|-0.005109|-0.001283|0.006579|
|C01|2026-01-14|2026-04-02|Up|optimise|1702|93610|-0.000029|0.000168|-0.007569|
|C02|2025-04-03|2025-07-03|Down|repli|112|7056|0.000000|0.000000|0.000000|
|C02|2025-04-03|2025-07-03|Up|repli|112|7056|0.000000|0.000000|0.000000|
|C02|2025-07-07|2025-10-02|Down|optimise|112|7056|-0.001341|-0.001015|-0.019495|
|C02|2025-07-07|2025-10-02|Up|optimise|112|7056|-0.000500|-0.000153|-0.000083|
|C02|2025-10-03|2026-01-02|Down|optimise|112|7056|-0.006350|-0.002244|0.006505|
|C02|2025-10-03|2026-01-02|Up|optimise|112|7056|0.000894|0.000248|-0.010321|
|C02|2026-01-05|2026-04-02|Down|optimise|112|6944|-0.011521|-0.003371|-0.008233|
|C02|2026-01-05|2026-04-02|Up|optimise|112|6944|0.001174|0.000585|-0.006402|

### Couverture et replis

- Up : 10,548/12,362 fenêtres optimisées (85.33 %) ; 1,814 replis. Tours optimisés min/médiane/max : 1/34/500. Raisons de repli : {'insufficient_history': 1814}.
  Replis à 80 : 0 probabilités différentes ; écart absolu maximal 0.
  Optimisées : 10 sélections atteignent le plafond ; 10538 early stopping déclenchés.
- Down : 10,548/12,362 fenêtres optimisées (85.33 %) ; 1,814 replis. Tours optimisés min/médiane/max : 1/8/500. Raisons de repli : {'insufficient_history': 1814}.
  Replis à 80 : 0 probabilités différentes ; écart absolu maximal 0.
  Optimisées : 8 sélections atteignent le plafond ; 10540 early stopping déclenchés.

- Minimum : 315 séances distinctes utilisables après retards/exclusions = 252 apprentissage interne + 63 validation interne. Les frontières et effectifs de chaque direction ont été contrôlés sur le snapshot. Validation strictement après apprentissage interne et avant test ; refit sur le train complet. Les fenêtres insuffisantes utilisent explicitement 80 tours.

### Moyennes, AUC et référence constante

|Direction|Statut|Δ LL moyenne candidat/fenêtre|Δ Brier moyenne candidat/fenêtre|Fenêtres AUC définie|LL constante train pondérée|LL fixe − constante|LL chrono − constante|Brier constante train pondéré|Brier fixe − constante|Brier chrono − constante|
|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
|Up|toutes|0.001209|0.000488|12362|0.538603|-0.002022|-0.000791|0.176722|-0.000971|-0.000477|
|Up|optimise|0.001417|0.000572|10548|0.544001|-0.002193|-0.000745|0.179115|-0.001066|-0.000486|
|Up|repli|0.000000|0.000000|1814|0.507861|-0.001052|-0.001052|0.163098|-0.000430|-0.000430|
|Down|toutes|-0.004843|-0.001355|12362|0.510840|0.009090|0.004252|0.164872|0.002533|0.001178|
|Down|optimise|-0.005676|-0.001588|10548|0.514551|0.009031|0.003344|0.166318|0.002502|0.000908|
|Down|repli|0.000000|0.000000|1814|0.489704|0.009424|0.009424|0.156635|0.002714|0.002714|

Les moyennes CW donnent le même poids à chaque candidat/fenêtre. Les pertes pondérées donnent le même poids à chaque observation directionnelle (N dans les CSV). Les AUC sont calculées séparément par candidat/fenêtre avec les deux classes ; les fenêtres mono-classe sont NA et exclues uniquement du dénominateur AUC. Aucune AUC regroupant les prédictions de plusieurs modèles n’est utilisée.

La référence constante est la prévalence du train complet propre au candidat/fenêtre et à la direction, retrouvée dans les labels du snapshot après exclusions, puis vérifiée contre la somme des effectifs positifs apprentissage interne + validation. Elle n’utilise jamais la prévalence test.

## Conclusion et limites

Sur cette population, Up favorise le fixe pour la log loss, le Brier et l’AUC moyenne. Down favorise le chronologique en log loss et Brier, mais **les deux modes Down restent moins bons que la probabilité constante fondée sur le train** pour ces deux pertes. Un gain contre le fixe ne suffit donc pas à démontrer une valeur prédictive additionnelle. Les deux modes Up dépassent légèrement leur référence constante en log loss/Brier, le fixe davantage.

- Up : variation descriptive LL pondérée +0.001231, Brier pondéré +0.000494, AUC moyenne -0.008197. Voir les périodes/cohortes et les fenêtres optimisées avant de généraliser cette moyenne.
- Down : variation descriptive LL pondérée -0.004838, Brier pondéré -0.001356, AUC moyenne +0.001103. Voir les périodes/cohortes et les fenêtres optimisées avant de généraliser cette moyenne.

**Résultat exploratoire : aucune supériorité statistiquement démontrée.** Pas de test/IC naïf fondé sur l’indépendance des candidats/fenêtres. Les candidats partagent dates, titres, labels et variables ; les apprentissages sont expansifs et chevauchants. Les observations de candidats différents peuvent compter plusieurs fois une même réalisation économique. Les cohortes ont des calendriers différents et le préfiltre est partagé. La couverture de l’optimisation et les résultats globaux sont donc distingués.

Le run fixe ne persiste pas les diagnostics internes ni les tours effectivement observés : ses 80 tours sont ceux du contrat enregistré, pas une mesure instrumentée par fenêtre. Sa version runtime XGBoost historique n’est pas identifiée dans les sources utilisées ; le chronologique enregistre la version dans son contrat. Les commits différents empêchent d’attribuer sans réserve tout écart au seul choix des tours. Les métriques recalculées concordent avec les LL/Brier persistés chronologiques et les AUC persistées des deux runs (tolérance absolue 1e-12).

## Reproduction et conventions

Depuis C:\Dev\RStock : `python reports/analyse_xgb_20261010_fixe_chronologique/analyse.py` (Python, numpy, pandas). Le script n’importe pas RStock ou XGBoost ; il lit uniquement les artefacts existants et écrit exclusivement dans son dossier.

- Clé brute : Set JSON ordonné (cible en premier, prédicteurs dans l’ordre), Window, Date normalisée ; Direction ajoutée après appariement. Identité courte SHA256(Set) à des fins de lecture, Set demeure la clé scientifique.
- Log loss : probabilités bornées à [1e-15, 1−1e-15], même convention pour les deux runs et le comparateur constant ; logarithme naturel. Brier sur probabilités non bornées. AUC par rangs moyens pour les ex æquo.
- `metriques_par_candidat_direction_fenetre.csv` : métriques individuelles, deltas, prévalence train, frontières et diagnostics. `synthese_globale.csv` : toutes/optimisées/replis. `synthese_par_periode_statut.csv` : cohortes, dates réelles, direction et statut.
- `predictions_appariees.csv.gz` : les deux probabilités, label, clé, statut et prévalence train. Les chemins complets des deux CSV sources et leurs empreintes SHA256 sont dans `sources_et_verifications.json` (ainsi que tous les autres fichiers lus).
- Tous les fichiers sources hachés au début ont été rehachés en fin de lecture et sont inchangés. Les ZIP/grilles ne sont pas nécessaires : les CSV locaux complets existent. Aucun nouvel export applicatif ; aucun entraînement.

Le temps de sélection/refit enregistré est agrégé dans les CSV ; il s’agit de sommes de temps par modèle et non de durée murale du job parallèle. Les résultats de périodes doivent être lus à calendrier comparable, sans considérer les cohortes comme des réplications indépendantes.

Audit supplémentaire des commits : les AST des fonctions `prepare_dataset`, `predictor_columns`, `training_parameters`, `fit_booster_matrix` et `predict_probabilities_matrix` sont identiques. Le code ajouté comprend la sélection interne, le refit après sélection et les diagnostics. L’identité parfaite des replis constitue un contrôle empirique utile, sans prouver l’absence de toute différence historique non enregistrée. Tous les tests WF se terminent avant le début du holdout final (6 avril 2026).
