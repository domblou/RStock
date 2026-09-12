# Migration R vers Python

## Portée de la phase 1

Cette phase porte le comportement fonctionnel existant en séparant acquisition,
features, combinaisons, entraînement, évaluation, persistance, prédiction et
validation. Les scripts R restent présents comme référence. Les modèles sont
recréés en Python; les fichiers `.rda` ne sont pas convertis.

## Dépendances Python

- `pandas` pour les tables, séries temporelles et CSV;
- `numpy` pour les tableaux et la reproduction du recyclage de la métrique R;
- `xgboost` pour l'entraînement, la sauvegarde et l'inférence;
- `yfinance` pour les cours Yahoo;
- `pytest` pour les tests automatisés (extra `dev`).

## Correspondance des fichiers

| Source R | Cible Python |
|---|---|
| `Settings.R` | `rstock/config.py` |
| `GetSymbols.R` | `rstock/symbols.py`, `scripts/get_symbols.py` |
| `Functions.R`: téléchargement | `rstock/data.py` |
| `Functions.R`: préparation | `rstock/features.py` |
| `Functions.R`: ensembles | `rstock/combinations.py` |
| `Functions.R`: historique | `rstock/history.py` |
| `CreateModels.R` | `rstock/training.py`, `rstock/evaluation.py`, `rstock/persistence.py`, `scripts/train_models.py` |
| `Predict.R` | `rstock/prediction.py`, `rstock/validation.py`, `scripts/predict_daily.py` |

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

## Bug de calcul d'erreur isolé

Le code R calcule :

```r
mean(binary_predictions != as.matrix(test[, predictorNames]))
```

Il compare donc chaque prédiction aux features et non à `test$outcome`. Avec
plusieurs features, le recyclage vectoriel de R compare la prédiction à chacune
d'elles. `rstock.evaluation.legacy_predictor_error` reproduit ce calcul et
`RStockConfig.error_metric` vaut `legacy_predictors` par défaut.

La métrique correcte existe sous le mode `outcome` et peut être activée
explicitement avec `scripts/train_models.py --correct-error-metric`. Ce choix est
une modification méthodologique séparée et ne fait pas partie du portage initial.

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

1. Basculer la métrique sur `outcome` et recalibrer le seuil de conservation.
2. Remplacer le split aléatoire par une validation walk-forward temporelle.
3. Corriger la gestion des calendriers de marché et des tickers ponctués.
4. Distinguer valeurs manquantes et classe négative.
5. Publier les nouveaux modèles de manière atomique après un entraînement réussi.
