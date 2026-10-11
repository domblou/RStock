# Comparaison WF avec constante alignée — 11 octobre 2026

Recalcul local uniquement. Aucun entraînement XGBoost, WF complet, E2E, préfiltre ou
Forward historique n’a été lancé. Les SHA-256 des configurations, statuts,
snapshots et fichiers de résultats lus sont vérifiés avant/après dans `summary.json`.

## Cause et alignement

Le moteur WF applique `dropna` aux labels et aux colonnes du modèle avant de construire
ses fenêtres. La constante utilise seulement les labels ; les variantes cible utilisent
aussi trois retards. Dans ces runs, les trois premières lignes sont exclues par les retards
(aucun autre manque observé). La première fenêtre constante va du 7 juillet 2023 au
8 juillet 2024, test le 9 juillet. Les variantes commencent leur apprentissage le
12 juillet 2023, le terminent le 11 juillet 2024, test le 12 juillet.

Le recalcul utilise l’intersection des dates où les labels et toutes les entrées des
variantes comparées sont disponibles. Le générateur WF existant construit les fenêtres.
Chaque entraînement/test reconstruit est vérifié contre les fenêtres et prédictions
historiques XGBoost : dates exactes, labels, effectifs et frontières. Les variantes
profondeur 2 et 4 sont alignées. La constante est la prévalence Up/Down de l’entraînement
commun, sans lissage, figée pendant chaque bloc test. Le test synthétique couvre aussi
une observation manquante et ses trois retards affectés : aucun décalage fixe n’est utilisé.

## Résultats

Même population avant/après : 20 titres × 2 directions, 433 dates par titre/direction,
du 12 juillet 2024 au 2 avril 2026. Moyenne à poids égaux des scores par titre/direction.
Sept fenêtres par titre : apprentissages de 252 à 630 observations ; six blocs de 63
observations et un dernier de 55. Aucun holdout utilisé.

| Modèle | Log loss | Brier |
| --- | ---: | ---: |
| Constante historique sur dates communes, entraînement non aligné | 0,5322472613 | 0,1741275777 |
| Constante recalculée, entraînement aligné | 0,5322886317 | 0,1741286965 |
| Cible profondeur 2, probabilités historiques inchangées | 0,5457329165 | 0,1783023505 |
| Cible profondeur 4, probabilités historiques inchangées | 0,5618472613 | 0,1829790514 |

La correction augmente la log loss constante de 0,0000413704 et le Brier de
0,0000011188. Le classement moyen reste **constante, profondeur 2, profondeur 4**
(plus petit = meilleur). Écart profondeur 2 moins constante alignée : +0,0134442847
en log loss, +0,0041736540 en Brier. Profondeur 4 : +0,0295586296 et +0,0088503549.
Ce classement est descriptif sur ce développement, sans conclusion de supériorité
statistique ou indépendante.

## Artefacts et reproduction

- `alignment_windows.csv` : dates/effectifs, empreintes des dates d’apprentissage/test,
  empreintes et effectifs des labels positifs d’apprentissage, probabilités constantes.
- `aligned_constant_20261011T031914_c00cea6f22.csv` : probabilités recalculées.
- `historical_windows.csv` : copie d’analyse des fenêtres historiques, jamais modifiées.
- `same_test_dates_metrics.csv` : scores avant/après par titre et direction.
- `full_controlled_metrics.csv` : résultat du mécanisme de comparaison de l’application.
- `summary.json` : résultats et empreintes des entrées historiques vérifiées inchangées.

```powershell
python -m scripts.analyze_aligned_constant 20261011T031914_c00cea6f22 20261011T031942_217eeee2c1 20261011T115541_2a0e278563 --output diagnostics/aligned_constant_20261011
```

Si les variantes n’ont pas les mêmes observations d’apprentissage ou frontières de
réentraînement, l’application conserve seulement la comparaison sur dates test communes
et affiche explicitement que la constante historique n’est pas parfaitement alignée.
La constante autonome continue à utiliser son propre calendrier ; aucun nouveau champ
de configuration, paramètre scientifique ou écran n’est introduit.
