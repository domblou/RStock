# RStock

Le dépôt contient le projet R historique et son portage Python phase 1. Le contrat
de compatibilité et les problèmes méthodologiques sont détaillés dans
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
```

Les paramètres et chemins sont centralisés dans `rstock/config.py`. Le portage
utilise volontairement la métrique d'erreur legacy par défaut. L'option
`--correct-error-metric` est un changement méthodologique explicite.

