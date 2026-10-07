# Diagnostic descriptif du contexte de marché — V1

Activation dans Paramètres → Diagnostic de contexte de marché — paramètres avancés.
La V1 est facultative, désactivée par défaut et sans effet sur les décisions
d'entraînement, de qualification ou de production. L'absence des six champs
`market_context_*` dans un ancien snapshot signifie explicitement désactivé.
Une valeur enregistrée est prioritaire, y compris lors d'une duplication.

## Protocole SPY

Le benchmark officiel est **SPY**, indépendamment du benchmark d'univers ou des
prédicteurs du modèle. Le fournisseur RStock existant télécharge les OHLC bruts
avec `auto_adjust=False` et conserve `Adj Close` sous le nom `Adjusted`.
Le diagnostic utilise uniquement ce champ ajusté, jamais un remplacement par
`Close` brut. Yahoo ajuste cette clôture pour splits et distributions ; elle
sert d'indice de rendement pour éviter les ruptures mécaniques ex-dividende.
Sources : [historique SPY Yahoo](https://sg.finance.yahoo.com/quote/SPY/history/),
[convention d'ajustement yfinance](https://github.com/ranaroussi/yfinance/blob/main/yfinance/utils.py).

Les rendements des signaux RStock restent ceux persistés par les étapes
scientifiques : le contexte SPY décrit l'environnement et ne remplace pas le
rendement négocié. Pour une même séance, un facteur d'ajustement commun à Open
et Close s'annule dans Close/Open ; les rendements historiques close-to-close
bruts RStock ne sont pas modifiés par ce diagnostic.

Pour une observation de la séance J, seuls les prix jusqu'à J−1 sont utilisés :

| Variable | Formule connue avant J | Standard |
|---|---|---|
| Tendance | A[J−1] / A[J−1−n] − 1 | n = 63 séances |
| Drawdown positif | 1 − A[J−1] / maximum des n clôtures se terminant à J−1 | n = 252 |
| Volatilité annualisée | écart-type échantillonnal des n log-rendements × √252 | n = 21 |

Le calendrier de contexte est XNYS. Aucun remplissage de séance manquante,
aucune fenêtre raccourcie : l'indicateur concerné est indisponible en cas de
trou ou de warmup insuffisant. Les prix non positifs, dates dupliquées et
absences de clôture ajustée sont rejetés. Les séries ajustées du fournisseur
sont rétrospectives ; J−1 garantit le décalage du calcul, pas l'existence d'une
archive fournisseur point-in-time. Cette limite est inscrite au manifest.

Les fenêtres et l'activation des terciles sont avancées. Elles sont enregistrées
dans `RStockConfig`, dans les snapshots et dans le manifest de protocole.
`spy_adjusted_context_v1` identifie les formules ; une empreinte de tous les
paramètres identifie chaque variante, affichée comme personnalisée dès qu'elle
diffère du standard. Aucune optimisation des fenêtres selon les modèles.

## Terciles et agrégats

Les variables continues constituent la référence scientifique. Les bandes
`low/intermediate/high` sont des terciles descriptifs ; **elles ne sont pas des
régimes économiques**. Leurs coupures sont calculées sur la première période
de développement WF : `[premier TrainStart, premier TestStart)`, décalée à J−1,
puis figées. Il faut au moins 60 valeurs utilisables et deux coupures distinctes.
On ne recalcule ni ces coupures sur un fold futur, ni sur Holdout ou Forward.

Le post-traitement réutilise `predictions.csv` WF, les probabilités de
développement et les probabilités Holdout. Les observations ne sont pas
recopiées. Les agrégats sont séparés par identité canonique, fenêtre, axe,
bande et période : observations, dates, épisodes contigus, couverture, AUC,
précision, recall/F1, Brier, nombre/taux de signaux, rendement directionnel.
WF natif utilise `UpPrediction` persisté ; développement et Holdout comparables
utilisent Up ≥ seuil Up **et** Down < seuil Down figés. Ces règles sont
explicitement distinguées. Les résultats avec seuils calibrés sur le
développement ne constituent pas une nouvelle évaluation indépendante.
En l'absence des seuils, AUC/Brier restent calculables mais précision et taux
de signaux sont indisponibles. Les signaux combinés concernent les modèles
haussiers actuellement évalués par le diagnostic Forward.

Forward réutilise ses périodes existantes : cumul et intervalles à +21/+42/+63.
Le modèle est relié par le snapshot scientifique vérifié, et l'origine
`normal/common/additional/removed` par la référence T0 existante, si disponible.
Les modèles retirés n'ont aucune observation Forward inventée. Les références
d'agrégats WF/Holdout sont lues pour affichage T0, sans recalcul dans l'UI.

Une AUC est étayée à partir de 30 observations, 10 positives et 10 négatives ;
la précision/rendement à partir de 10 signaux. Les valeurs brutes restent
visibles avec leurs indicateurs de support. La robustesse descriptive comprend
la pire AUC médiane entre bandes, sa dispersion, la concentration des signaux
et le rendement hors bande dominante. Le score AUC exige trois bandes étayées
et, en WF/développement, au moins deux fenêtres par bande ; sinon il est
indisponible. Les fenêtres chevauchantes et épisodes ne sont pas indépendants.
Ce score ne sert pas à la qualification et n'est pas une preuve causale.

## Persistance et reprise

Le run **WF physique** possède le magasin commun
`runs/<wf>/diagnostics/market_context/<protocol_id>/revisions/<revision>/` :

- `spy_adjusted_snapshot.csv` : acquisition dédiée SPY, warmup inclus ;
- `market_context.csv` : contexte par séance ;
- `market_context_manifest.json` : paramètres, dates, coupures, fournisseur,
  date d'acquisition, révision parent et empreintes SHA-256.

Chaque étape possède dans ses résultats `context_metrics.csv`,
`context_robustness.csv` et `context_diagnostic_manifest.json`. Ce dernier
référence la révision partagée et les agrégats amont par chemin/empreinte.
Le contexte n'est pas dupliqué dans chaque résultat. Les dérivés de qualification
héritant du même WF et protocole partagent le même magasin.

Une extension Holdout/Forward conserve le préfixe figé et ajoute une révision
immuable. La nouvelle acquisition chevauche le snapshot précédent : un facteur
uniforme d'ajustement est raccordé sans changer les rendements ; une révision
des rendements historiques est rejetée. Le contexte ancien et les coupures
sont conservés. Le cache de marché courant n'est jamais consulté silencieusement.
Les calculs dérivés sont publiés sous mutex ; le manifest est écrit en dernier
et relu avant publication. Un manifest absent marque une publication incomplète,
recalculable. Les lecteurs vérifient les empreintes. Un échec du diagnostic est
signalé indisponible et ne fait pas échouer les résultats scientifiques.

L'affichage est un panneau repliable dans le détail des runs : série continue,
agrégats par contexte et synthèse de robustesse. Les anciens runs ne sont pas
rétroactivement recalculés. Si leurs probabilités, fenêtres ou identités
manquent, le diagnostic est indisponible. Aucun enrichissement silencieux au
moment de l'affichage et aucune nouvelle mécanique de comparaison multi-runs.

## Validation descriptive des fenêtres

`scripts/validate_market_context_protocol.py` accepte uniquement un snapshot
SPY explicite. Il compare les 12 variantes 42/63/84 × 126/252 × 21/42 sur
des plages datées prédéfinies, sans accès aux résultats des modèles. Il fige
la copie de validation et produit `window_variants.csv` et
`validation_manifest.json`, avec disponibilité et empreintes.

L'exécution V1 dans `diagnostics/context_protocol_validation/` utilise le
snapshot local explicite du 2023-01-03 au 2026-10-07. Le téléchargement d'une
série 2017–2026 était inaccessible dans cet environnement. La validation est
donc **partielle**, sans couverture 2018/2020/2022 ni warmup 252 complet en
2023. Il faudra compléter ces périodes avec une acquisition explicite ; aucune
validation intégrale de ces épisodes n'est revendiquée.

Sur les plages couvertes, le standard décrit notamment février–avril 2025
avec tendance médiane −6,50 %, drawdown maximal 18,76 % et volatilité maximale
52,40 % annualisée. En juillet–août 2024 la tendance 63 reste positive
(médiane +7,38 %) malgré un drawdown de 8,41 % : les axes doivent être lus
ensemble. En mai–juin 2025 la tendance médiane 42 est +5,66 %, alors que 63 et
84 restent légèrement négatives ; c'est un effet de mémoire attendu pendant
un rebond, pas un motif d'optimisation. La volatilité 42 lisse le pic et persiste
plus longtemps. Le standard 63/252/21 conserve donc un rôle de tendance
intermédiaire, mémoire annuelle de drawdown et réactivité mensuelle de volatilité,
avec ces limites explicitement documentées.

La classification économique versionnée (normal, forte hausse, stress, bear
important, reprise), ses priorités/hystérésis, le drift, breadth/dispersion et
les tests statistiques indépendants restent hors V1.
