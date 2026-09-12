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

## Contrat de compatibilité

- La variation est toujours `1 - Open / Close` et le seuil UPDW vaut `0.01`.
- Les valeurs absentes deviennent toujours zéro.
- `DAY_MINUS_1` est l'observation boursière précédente.
- Les données préparées sont en ordre décroissant et les composantes de date
  gardent la numérotation R à partir de zéro.
- Les paires de titres sont ordonnées; les ensembles plus grands utilisent des
  combinaisons non ordonnées par cible.
- Le mélange et le split aléatoire 70/30 restent en phase 1.
- La prédiction binaire emploie `probabilité > 0.5`.
- Une prédiction réussie reste uniquement un vrai positif.
- La date cible reste le prochain jour civil.
- La comparaison du résultat historique relu comme texte reste lexicographique;
  `legacy_history_value_comparison=False` permet la future correction explicite.

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
d'elles. `rstock.evaluation.legacy_predictor_error` reproduit toujours exactement
ce calcul.

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

### Mode de compatibilité legacy

Le mode `legacy_predictors` n'est plus activé implicitement. Il doit être demandé :

```powershell
python scripts/train_models.py --legacy-error-metric
```

Dans ce mode, seule la colonne `Err` et donc la décision de conserver un modèle
reprennent le calcul historique. Les métriques standards restent correctement
calculées contre la cible réelle afin de ne pas leur donner une signification
trompeuse. L'ancien argument `--correct-error-metric` reste accepté comme alias
déprécié et masqué; il sélectionne simplement le nouveau comportement par défaut.

## Autres faiblesses conservées ou rendues explicites

- Split aléatoire et fuite temporelle potentielle.
- Mélange global non déterministe par défaut, puis même split positionnel pour
  chaque ensemble.
- Valeur absente assimilée à zéro.
- Fallback de lag 1 à 5 jours inopérant dans le code R.
- Découpage historique au premier point, incorrect pour les tickers comme
  `BRK.B`; `market_data_to_history(..., legacy_field_split=False)` expose la
  future correction sans changer le défaut.
- Ajout à l'historique seulement après la date maximale, sans comblement des trous.
- Date de prédiction pouvant tomber un week-end/jour férié.
- Suppression préalable des modèles existants.
- Vrais négatifs non comptés comme prédictions réussies.
- Explosion combinatoire très importante avec 500 titres et une profondeur de 3.
- Valeurs historiques comparées lexicalement au seuil après lecture CSV en mode
  `character`; ce comportement devient visible dans la configuration.
- Noms legacy ambigus avec `-` et `NA`; les modèles Python utilisent donc des noms
  techniques sûrs et des métadonnées JSON tout en conservant le champ `Set`.
- Sélection par regex non échappée dans R. Le portage utilise les noms exacts des
  titres; c'est une correction structurelle documentée nécessaire pour garantir
  le contrat des métadonnées.
- Le batching R est erroné au-delà d'un symbole. Le portage conserve le réglage
  effectif d'un appel par symbole et isole chaque échec.

## Phase suivante (hors portage initial)

1. Recalibrer `keep_predictor_under` avec la métrique corrigée.
2. Remplacer le split aléatoire par une validation walk-forward temporelle.
3. Corriger la gestion des calendriers de marché et des tickers ponctués.
4. Distinguer valeurs manquantes et classe négative.
5. Publier les nouveaux modèles de manière atomique après un entraînement réussi.
