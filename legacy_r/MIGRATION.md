# Migration R vers Python

## Portée de la phase 1

Cette phase porte le comportement fonctionnel existant en séparant acquisition,
features, combinaisons, entraînement, évaluation, persistance, prédiction et
validation. Les scripts R restent archivés comme référence. Les modèles sont
recréés en Python; les fichiers `.rda` ne sont pas convertis.

## Portée de la phase 2

La phase 2 archive l'implémentation R et modernise un seul aspect
méthodologique : l'évaluation des modèles. L'erreur calculée contre la cible réelle
est désormais le comportement par défaut. Le split aléatoire 70/30 et tous les
autres comportements décrits dans le contrat de compatibilité restent inchangés.

## État actuel après modernisation méthodologique

Le code Python actif ne contient plus de mode de compatibilité R. L'historique se
trouve exclusivement dans `legacy_r/`. Le split est temporel, les données
manquantes ne sont plus imputées à zéro, les dates sont typées et les prochaines
séances proviennent d'abord des observations disponibles puis du calendrier déclaré
pour chaque titre. Les modèles antérieurs à ce schéma doivent être réentraînés.

## Archive de l'implémentation R originale

Le dossier `legacy_r/` contient, sans modification de contenu, l'implémentation R
originale ayant servi de référence à la migration Python : `CreateModels.R`,
`Functions.R`, `GetSymbols.R`, `Predict.R` et `Settings.R`. Les appels `source(...)`
internes restent relatifs à ce dossier; pour consulter ou réexécuter le projet
historique, il faut donc utiliser `legacy_r/` comme répertoire de travail.

Aucun workflow Python actif ne charge ces fichiers. Le fichier
`R Stock ML project.Rproj`, qui est un descripteur de projet et non un script, reste
à la racine.

## Dépendances Python

- `pandas` pour les tables, séries temporelles et CSV;
- `numpy` pour les tableaux et la reproduction du recyclage de la métrique R;
- `xgboost` pour l'entraînement, la sauvegarde et l'inférence;
- `yfinance` pour les cours Yahoo;
- `pytest` pour les tests automatisés (extra `dev`).

## Correspondance des fichiers

| Source R | Cible Python |
|---|---|
| `legacy_r/Settings.R` | `rstock/config.py` |
| `legacy_r/GetSymbols.R` | `rstock/symbols.py`, `scripts/get_symbols.py` |
| `legacy_r/Functions.R`: téléchargement | `rstock/data.py` |
| `legacy_r/Functions.R`: préparation | `rstock/features.py` |
| `legacy_r/Functions.R`: ensembles | `rstock/combinations.py` |
| `legacy_r/Functions.R`: historique | `rstock/history.py` |
| `legacy_r/CreateModels.R` | `rstock/training.py`, `rstock/evaluation.py`, `rstock/persistence.py`, `scripts/train_models.py` |
| `legacy_r/Predict.R` | `rstock/prediction.py`, `rstock/validation.py`, `scripts/predict_daily.py` |

## Contrat fonctionnel actuel

- La cible Python active est le rendement intraday exploitable
  `(Close_J / Open_J) - 1`, classé positif lorsqu'il est supérieur ou égal au
  seuil `intraday_target_threshold` (0,01 par défaut).
- Les valeurs absentes restent manquantes; chaque modèle exclut uniquement les
  lignes incomplètes pour sa cible et ses predictors.
- Les predictors intraday utilisent les observations boursières strictement
  antérieures J-1 à J-`lag_depth` (3 par défaut), sans jour civil intermédiaire.
- Les données préparées et les partitions d'entraînement sont ordonnées par une
  vraie date croissante.
- Les paires de titres sont ordonnées; les ensembles plus grands utilisent des
  combinaisons non ordonnées par cible.
- Les 70 % observations complètes les plus anciennes forment l'entraînement et les
  30 % les plus récentes forment le test, sans mélange.
- La prédiction binaire emploie `probabilité > 0.5`.
- Une prédiction réussie comprend les vrais positifs et les vrais négatifs.
- La date cible est la prochaine séance observée, ou la prochaine séance du
  calendrier déclaré si elle n'est pas encore observée.
- Les dates et les valeurs historiques sont comparées avec leurs types réels.

`yfinance` remplace `quantmod::getSymbols`. La récupération de la liste de titres
utilise les répertoires publics Nasdaq Trader comme remplacement de
`stockSymbols()`. Cette source doit être validée si une parité exacte de l'univers
de titres historique est requise.

## Évaluation : historique, correction et compatibilité

### Comportement historique

Le code R calcule :

```r
mean(binary_predictions != as.matrix(test[, predictorNames]))
```

Il compare donc chaque prédiction aux features et non à `test$outcome`. Avec
plusieurs features, le recyclage vectoriel de R compare la prédiction à chacune
d'elles. Ce calcul n'existe plus dans le code Python actif; il demeure uniquement
dans `legacy_r/CreateModels.R` à titre historique.

### Comportement corrigé par défaut

`RStockConfig.error_metric` vaut désormais `outcome`. `Err` compare les classes
prédites à la cible réelle et équivaut à `1 - accuracy`. C'est cette erreur qui
détermine si un modèle respecte `keep_predictor_under` et doit être sauvegardé.

Chaque ensemble évalué expose également :

- la matrice de confusion sous forme de colonnes `TN`, `FP`, `FN`, `TP`;
- `Accuracy`, `Precision`, `Recall` et `F1`;
- `ROCAUC`, calculé à partir des probabilités XGBoost lorsque les deux classes sont
  présentes dans le test.

Lorsque precision, recall ou F1 a un dénominateur nul, sa valeur est fixée à zéro.
Lorsque ROC-AUC n'est pas calculable parce qu'une classe manque, sa valeur est
absente (`None` dans les métadonnées JSON, `NaN` dans les tables pandas).

Ces métriques sont écrites dans `SymbolsToSurvey.csv` et dans les métadonnées des
modèles sauvegardés.

### Compatibilité historique

Aucun mode legacy n'est disponible dans l'application Python. Pour examiner le
calcul antérieur, consulter l'archive R.

## Autres faiblesses conservées ou rendues explicites

- Fallback de lag 1 à 5 jours inopérant dans le code R.
- L'explosion combinatoire demeure intrinsèque à l'objectif, mais une estimation
  préalable et `max_generated_sets` empêchent désormais sa matérialisation
  accidentelle.
- Sélection par regex non échappée dans R. Le portage utilise les noms exacts des
  titres; c'est une correction structurelle documentée nécessaire pour garantir
  le contrat des métadonnées.
- Le batching R est erroné au-delà d'un symbole. Le portage conserve le réglage
  effectif d'un appel par symbole et isole chaque échec.

## Phase suivante (hors portage initial)

1. Recalibrer `keep_predictor_under` avec la métrique corrigée et le split temporel.
2. Évaluer une validation walk-forward à plusieurs fenêtres au-delà du split
   temporel unique actuel.
3. Évaluer la fraîcheur maximale acceptable des predictors lorsque plusieurs
   calendriers de marché sont combinés.
