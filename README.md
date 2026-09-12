# RStock

Le dépôt contient le portage Python actif et, dans `legacy_r/`, le projet R
historique archivé sans modification. Le contrat de compatibilité et les problèmes méthodologiques sont détaillés dans
[`MIGRATION.md`](MIGRATION.md).

## Installation

```powershell
python -m pip install -e ".[dev]"
```

## Vérification

```powershell
python -m pytest
```

## Workflows

À lancer depuis la racine du dépôt :

```powershell
python scripts/get_symbols.py
python scripts/train_models.py
python scripts/predict_daily.py
python scripts/walk_forward_evaluate.py
```

`get_symbols.py` produit `data/symbols/symbols.csv`, contenant `Symbol`, `ProviderSymbol`,
`Exchange` et `Calendar`. Un univers personnalisé doit fournir ces quatre colonnes;
les noms de calendrier sont ceux d'`exchange-calendars`. Il faut régénérer les
anciens fichiers d'univers à une seule colonne avant d'entraîner ou prédire.

Les paramètres et chemins sont centralisés dans `rstock/config.py`. L'entraînement
utilise les 70 % observations complètes les plus anciennes et teste sur les 30 %
les plus récentes, sans mélange. La génération s'arrête avant matérialisation si
elle dépasse `max_generated_sets`; la valeur par défaut de `max_symbols` est donc
limitée à 25.

Les modèles ne remplacent le dossier actif qu'après la réussite complète de
l'entraînement. Les anciens bundles doivent être régénérés afin d'inclure leur
calendrier et leurs bornes temporelles.

## Évaluation walk-forward

Le walk-forward utilise une fenêtre d'entraînement expansive et plusieurs fenêtres
de test futures successives. Les valeurs par défaut sont 252 observations minimales
d'entraînement, 63 observations de test et un pas de 63. Elles sont configurables
avec `--min-train-size`, `--test-size` et `--step-size`.

Pour évaluer explicitement un petit univers américain :

```powershell
python scripts/walk_forward_evaluate.py --symbols AAPL MSFT JPM XOM --calendar XNYS --permutation-depth 1
```

Les résultats détaillés et agrégés sont écrits dans `WalkForward/`. Cette commande
n'enregistre ni ne remplace les modèles de prédiction quotidienne.

## Cache local des données de marché

Les workflows d'entraînement, de prédiction quotidienne et de walk-forward passent
tous par la même couche de cache. Le stockage initial utilise un fichier Parquet
par symbole :

```text
data/
├─ market/
│  └─ <SYMBOL>.parquet
├─ symbols/
│  └─ symbols.csv
└─ metadata/
   └─ market_cache.json
```

À la première lecture, la période Yahoo Finance demandée est téléchargée en entier.
Les exécutions suivantes lisent le Parquet et demandent seulement les dates situées
avant ou après sa couverture connue. Les lignes sont fusionnées, triées et
dédupliquées par date. Un fichier Parquet inchangé n'est pas réécrit.

`market_cache.json` contient uniquement les bornes de couverture, l'identité du
symbole interne et Yahoo, le marché, le calendrier, la source et les dates de
rafraîchissement. Il ne duplique pas les valeurs OHLCV.

Les trois commandes acceptent les options suivantes :

- `--force-refresh` retélécharge la période complète de tous les symboles demandés;
- `--force-symbol SYMBOL` force un seul symbole et peut être répété.

Exemple :

```powershell
python scripts/walk_forward_evaluate.py --symbols AAPL MSFT JPM XOM --calendar XNYS --force-symbol AAPL
```

Les lectures et mises à jour sont isolées par symbole et exécutées avec au plus
`market_cache_workers` workers. Une erreur conserve le Parquet existant du symbole
et n'annule pas les autres téléchargements. Le backend est exposé par l'interface
`MarketDataStore`, afin de pouvoir adopter DuckDB ultérieurement sans modifier les
workflows.
