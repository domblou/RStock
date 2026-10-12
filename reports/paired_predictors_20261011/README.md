# Bilan apparié des quatre familles — EXP-010

Analyse du 11 octobre 2026 sur les prédictions déjà sauvegardées, sans entraînement,
nouveau run, calibration ou changement des paramètres. Les sources historiques
ont été hachées avant/après et sont inchangées (`summary.json`).

## Population et méthode

- 20 cibles, cutoff 2026-07-06 ; 433 dates communes de développement du
  2024-07-12 au 2026-04-02, avant qualification. Holdout et Forward non rescorrés.
- Constante `20261011T031914_c00cea6f22`, cible seule `20261011T031942_217eeee2c1`,
  mixte `20261011T032130_4742d2f509`, externes `20261011T032201_17ece38f7b`.
- Quatre snapshots préparés égaux. Calendriers train/test, données complètes,
  labels et clés de toutes les prédictions contrôlés ; aucun doublon candidat/date,
  aucune exclusion supplémentaire. Sept fenêtres : six de 63 séances et une de 55.
- Les trois XGBoost ont arbres profondeur 4, eta 0,20, 20 tours, lambda 5,
  trois retards, sans préfiltre ni volatilité cible. Les paramètres scientifiques
  pertinents sont vérifiés identiques hors type de modèle.
- Les familles externes/mixte ont les mêmes 3 800 combinaisons, 190 par cible.
  Cible seule et constante ont un candidat par cible. Ne pas confondre profondeur
  des combinaisons et profondeur des arbres.
- Constante alignée réutilisée d'EXP-011, avec vérification des prévalences de
  l'apprentissage commun à chaque fenêtre, antérieur au test. Ses probabilités
  ne proviennent jamais des labels test. La constante native non alignée n'est
  pas utilisée pour le classement.
- Fonctions existantes `rstock.evaluation.probability_losses` et `_roc_auc`.
  Scores recalculés séparément par candidat ; puis moyenne égale des candidats
  au sein d'une cible et poids égal des 20 cibles. Chaque date a le même poids.
  **Ce n'est pas un ensemble de prédictions ni la performance d'un sélecteur.**
- Les scores de chaque fenêtre XGBoost concordent avec ceux enregistrés, tolérance
  absolue 1e-10. L'AUC est moyennée par candidat, jamais calculée sur un mélange
  de prédictions de plusieurs modèles. La constante change entre fenêtres : son
  AUC sur les 433 dates peut différer de 0,5, sans classement au sein d'une fenêtre.

## Résultats mesurés

Log loss et Brier : plus bas = meilleur. AUC : plus haut = meilleur.

| Direction | Famille | Log loss | Brier | AUC moyenne par candidat |
|---|---|---:|---:|---:|
| Up | Constante alignée | 0.542410 | 0.178392 | 0.494782 |
| Up | Cible seule | 0.571065 | 0.187094 | 0.509884 |
| Up | Externes seuls | 0.575665 | 0.187884 | 0.506343 |
| Up | Cible + externes | 0.577120 | 0.188310 | 0.511478 |
| Down | Constante alignée | 0.522167 | 0.169866 | 0.466605 |
| Down | Cible seule | 0.552629 | 0.178864 | 0.500418 |
| Down | Externes seuls | 0.557106 | 0.179593 | 0.493634 |
| Down | Cible + externes | 0.559458 | 0.180397 | 0.497581 |

En Up, chaque famille XGBoost est derrière la constante en log loss moyenne pour
**20 cibles sur 20**. Externes devant cible seule pour 5/20 cibles ; mixte devant
externes pour 6/20. L'ajout de la cible aux mêmes combinaisons externes améliore
1 665/3 800 candidats, mais dégrade leur perte moyenne. En Down, familles externes/
mixtes derrière constante pour 20/20 ; cible seule la bat sur BNY uniquement.

La constante devance aussi les familles dans chacun des trois tiers temporels :
2024-07-12→2025-02-07 (145 dates), 2025-02-10→2025-09-05 (144),
2025-09-08→2026-04-02 (144). Découpage descriptif, pas réplications indépendantes.
Les résultats par titre et période sont dans les CSV associés.

En Up, **17/3 800 externes** et **10/3 800 mixtes** battent individuellement la
constante en log loss sur la période complète. En Down : 4 externes, 17 mixtes.
Ce sont des gagnants rétrospectifs parmi des milliers d'essais ; aucune règle de
sélection indépendante ne les valide ici. Ne pas fabriquer un nouveau sélecteur
ou un univers à partir de ces seuls résultats.

## Conclusion et décision scientifique

Résultat négatif descriptif pour les familles et configurations examinées :
aucun apport probabiliste moyen des externes face à la constante ; l'enrichissement
mixte ne compense pas cette faiblesse. Les nombres qualifiés d'EXP-010 ne constituent
plus une raison suffisante d'élargir les combinaisons sous cette formulation.

Des AUC légèrement favorables aux XGBoost, mais des pertes défavorables, laissent
ouverte une information de classement faible ou une mauvaise calibration. Ces
chiffres ne démontrent ni que recalibrer sauverait les modèles ni l'absence
universelle d'information externe. Les valeurs Up seules ne contredisent pas
EXP-011, dont les scores cités agrégeaient Up et Down.

Aucun test de significativité ni intervalle indépendant calculé. Dates et candidats
dépendants, univers de 20 cibles choisi, réglages et périodes déjà consultés :
aucune preuve de généralisation, sélection fiable, rentabilité ou rendement futur.

Avis du directeur scientifique : suspendre l'élargissement des combinaisons et
les sweeps équivalents ; avant une validation supplémentaire, demander si les
rares gagnants pourraient être identifiés par une règle entièrement antérieure
à l'évaluation. Leur trajectoire et qualification déjà persistées peuvent éclairer
cette question, mais seule une période indépendante validerait un tel sélecteur.
Cette recommandation n'est ni un changement de protocole ni une nouvelle campagne autorisée.

## Artefacts et reproduction

- `summary.json` : sources SHA-256, contrôles et résultats globaux.
- `candidate_metrics.csv`, `target_metrics.csv`, `overall_metrics.csv` : scores
  individuels, moyennes par cible et direction.
- `paired_mixed_external.csv` : appariement exact des mêmes combinaisons, deltas.
- `window_metrics.csv`, `period_metrics.csv`, `daily_target_losses.csv` :
  trajectoires par fenêtre, tiers temporel et cible/date, avec pondérations explicites.
- `analyse.py` : lecture par blocs des fichiers sources, sorties dans ce dossier
  uniquement ; aucun import de service applicatif ni ajustement de booster.

Reproduction depuis la racine : `python -B reports/paired_predictors_20261011/analyse.py`.
Les modèles et fichiers historiques ne sont jamais écrasés. Cette commande
recalcule les scores et remplace uniquement les sorties d'analyse de ce dossier.
