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

`get_symbols.py` produit un `Symbols.csv` contenant `Symbol`, `ProviderSymbol`,
`Exchange` et `Calendar`. Un univers personnalisé doit fournir ces quatre colonnes;
les noms de calendrier sont ceux d'`exchange-calendars`. Il faut régénérer les
anciens fichiers `Symbols.csv` à une seule colonne avant d'entraîner ou prédire.

Les paramètres et chemins sont centralisés dans `rstock/config.py`. L'entraînement
utilise les 70 % observations complètes les plus anciennes et teste sur les 30 %
les plus récentes, sans mélange. La génération s'arrête avant matérialisation si
elle dépasse `max_generated_sets`; la valeur par défaut de `max_symbols` est donc
limitée à 25.

Les modèles ne remplacent le dossier actif qu'après la réussite complète de
l'entraînement. Les anciens bundles doivent être régénérés afin d'inclure leur
calendrier et leurs bornes temporelles.
