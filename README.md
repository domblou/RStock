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

## Cible et features temporelles

La cible binaire est basée sur le rendement exploitable entre l'ouverture et la
clôture de la séance cible : `(Close_J / Open_J) - 1`. Elle vaut 1 lorsque ce
rendement est supérieur ou égal à `intraday_target_threshold` (0,01 par défaut).
Les rendements overnight et close-to-close sont conservés uniquement comme
diagnostics.

Une cible baissière indépendante vaut 1 lorsque le rendement intraday est
inférieur ou égal à `-intraday_down_threshold` (-1 % par défaut). Le walk-forward
entraîne séparément les modèles haussier et baissier; aucune classe directionnelle
unique ni règle de trading n'est créée. L'option `--intraday-down-threshold` est
disponible sur les trois workflows.

Chaque symbole predictor fournit par défaut ses rendements intraday observés à
J-1, J-2 et J-3. Les retards avancent selon les séances réellement observées et
non selon les jours civils. `--lag-depth` et `--intraday-target-threshold` sont
disponibles sur les commandes d'entraînement, de prédiction et de walk-forward.
Les modèles créés avant ce changement doivent être réentraînés.

## Évaluation walk-forward

Le walk-forward utilise une fenêtre d'entraînement expansive et plusieurs fenêtres
de test futures successives. Les valeurs par défaut sont 252 observations minimales
d'entraînement, 63 observations de test et un pas de 63. Elles sont configurables
avec `--min-train-size`, `--test-size` et `--step-size`.

Pour évaluer explicitement un petit univers américain :

```powershell
python scripts/walk_forward_evaluate.py --symbols AAPL MSFT JPM XOM --calendar XNYS --permutation-depth 1
```

Les 63 dernières séances sont réservées par défaut comme holdout final et ne
participent ni aux fenêtres walk-forward, ni à la qualification. Elles servent
uniquement à confirmer puis à classer les modèles déjà admissibles.
La taille est configurable avec `--final-holdout-size`.

La qualification de stabilité utilise des seuils de calibration déclarés avant
l'évaluation finale : nombre minimal de fenêtres, AUC médian, proportion des
fenêtres au-dessus de 0,50, pire AUC, nombre de résultats positifs et écart-type
maximal. Ils sont configurés dans `rstock/config.py` et exposés par
`--min-windows`, `--min-median-auc`, `--min-pct-windows-above-random`,
`--min-worst-window-auc`, `--min-positive-observations` et `--max-auc-std`.
Après qualification et confirmation holdout, un score final explicable sur 100
classe les modèles admissibles sans les promouvoir. Ses composantes séparées
mesurent la qualité prédictive, la stabilité walk-forward, le holdout, l'adéquation
de l'échantillon et, lorsqu'elles existent, la qualité et la stabilité des signaux
calibrés. Les pondérations sont configurables dans `RStockConfig`; une composante
absente est exclue et les poids disponibles sont renormalisés.

Les résultats détaillés sont écrits dans `WalkForward/`, notamment
`qualification.csv`, `final_holdout.csv`, `final_holdout_predictions.csv`,
`selection_results.csv` et `run_configuration.json`. Cette commande n'enregistre
ni ne remplace les modèles de prédiction quotidienne.

L'analyse de risque est séparée des métriques de classification dans
`risk_by_window.csv`, `risk_by_set.csv`, `risk_global.csv` et
`final_holdout_risk.csv`. Elle contient la distribution des rendements, leur
espérance, les gains et pertes moyens, ainsi que MFE (`High / Open - 1`) et MAE
(`Low / Open - 1`). Le ratio gain/perte divise le gain moyen par la valeur absolue
de la perte moyenne. Les statistiques conditionnelles utilisent uniquement les
prédictions positives au seuil de classification inchangé de 0,5.

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

## Calibration contrôlée XGBoost

La calibration des hyperparamètres est un workflow séparé qui lit exclusivement
les fichiers Parquet déjà présents dans le cache et ne remplace aucun modèle actif :

```powershell
python scripts/calibrate_xgboost.py
```

Par défaut, elle conserve les 63 dernières séances comme holdout final, utilise
les mêmes fenêtres expansives et les mêmes cibles/features que le walk-forward,
et teste une grille réduite de 12 configurations. Pour borner le calcul sur
l'univers de 15 symboles, trois combinaisons de profondeur 1 sont retenues par
cible. Leur choix est reproductible : tri par SHA-256 de la graine XGBoost et de
l'identifiant de combinaison, avec un quota identique pour chaque cible.

La hausse et la baisse sont classées séparément. Le score documenté combine les
médianes ROC-AUC, PR-AUC et F1, les pires ROC-AUC/PR-AUC, la dispersion entre
fenêtres, les fenêtres catastrophiques, la variabilité des probabilités et la
production de classes positives. La configuration retenue est figée et hachée
avant l'unique évaluation du holdout. Les critères de qualification des
combinaisons existants sont seulement consignés et ne sont pas recalibrés.

Les sorties destinées à l'analyse ou à une future interface sont écrites dans
`WalkForward/Calibration/` : paramètres testés, métriques par configuration et
par fenêtre, comparaison baseline/calibrée, configurations retenues, prédictions
et métriques finales sur holdout, échantillon de combinaisons et protocole complet.

## RStock Laboratory

L'interface locale repose sur une couche applicative indépendante de Streamlit.
Elle soumet les expériences à des workers Python séparés et persiste chaque run
dans `runs/<run_id>/`. Pour la démarrer :

```powershell
python -m streamlit run rstock/application/streamlit_app.py
```

Les jobs walk-forward, calibration XGBoost et calibration des seuils sont
disponibles. Une combinaison qualifiée peut être promue depuis l'historique,
entraînée comme nouvel artefact production, puis activée dans la page `Modèles`.
La calibration des seuils utilise uniquement les probabilités hors entraînement
des fenêtres walk-forward. Elle sélectionne un seuil haussier par combinaison,
en imposant un nombre minimal de signaux et une couverture temporelle suffisante;
le holdout final mesure ensuite ce seuil figé sans participer à son choix. Lors
de la promotion, le seuil calibré et ses métriques sont conservés avec le modèle;
sans calibration associée, le seuil global `prediction_threshold` reste utilisé.
Le walk-forward peut activer un pré-filtrage univarié des prédicteurs avant les
combinaisons de profondeur 2 et 3. Il applique des seuils de performance et de
stabilité, un Top N par cible, puis retire les séries de features dont la
corrélation absolue dépasse le seuil configuré. Le classement utilise le score
explicable `AUC médiane + part des fenêtres > 0,50 - dispersion - pénalité de
Worst AUC`. Les minima de fenêtres valides et d'observations positives restent
ceux de la qualification générale. Les diagnostics complets sont
publiés dans `predictor_prefilter.csv` et `predictor_prefilter.json`. Lorsque
l'option est désactivée, la génération historique des combinaisons est conservée.
Dans `Expériences`, l'univers principal comme les univers de contexte peuvent
être utilisés au complet ou échantillonnés selon la même méthode : `Top N` ou
échantillon reproductible avec seed. Les symboles de contexte restent uniquement
des prédicteurs.
La page `Surveillance` dérive automatiquement l'univers opérationnel des modèles
actifs et permet de lancer la mise à jour marché, les prédictions Up/Down, le
screening et le rattachement ultérieur des résultats réalisés. Aucun ordre de
bourse n'est produit.

Les données opérationnelles restent séparées des runs expérimentaux :

```text
production/
├── model_registry.json
├── artifacts/<model_id>/
│   ├── up.ubj
│   ├── down.ubj
│   └── production.metadata.json
└── history/
    ├── predictions.csv
    ├── signals.csv
    └── realized_results.csv
```

Le registre JSON, les deux artefacts directionnels et les historiques CSV sont
publiés atomiquement. Une exécution opérationnelle complète publie ensemble ses
prédictions, signaux et résultats réalisés, afin qu'une annulation ne laisse pas
un historique partiel.

La CLI utilise les mêmes services applicatifs :

```powershell
python scripts/laboratory_cli.py create-config --job-type walk_forward --symbols AAPL MSFT --output experiment.json
python scripts/laboratory_cli.py submit --config experiment.json
python scripts/laboratory_cli.py list
```

Pour les jobs opérationnels, la CLI peut omettre `--symbols`; elle fige alors
l'univers dérivé des modèles actifs dans le fichier de configuration. Pour un
entraînement production, elle dérive les symboles du candidat choisi :

```powershell
python scripts/laboratory_cli.py create-config --job-type production_training --model-id model_abc123 --output production-training.json
python scripts/laboratory_cli.py create-config --job-type operational_run --output daily-operation.json
```

Les processus en attente partagent des slots locaux atomiques; un seul job lourd
s'exécute par défaut. Une annulation crée une demande persistante vérifiée entre
les grandes unités de travail. Les résultats restent dans `_working` en cas
d'échec ou d'annulation et ne sont publiés sous `results/` qu'après réussite.
