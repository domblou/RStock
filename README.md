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
```

Les paramètres et chemins sont centralisés dans `rstock/config.py`. Le portage
évalue désormais les prédictions contre leur cible réelle et publie matrice de
confusion, accuracy, precision, recall, F1 et ROC-AUC. Le calcul R historique reste
disponible uniquement en mode de compatibilité explicite :

```powershell
python scripts/train_models.py --legacy-error-metric
```
