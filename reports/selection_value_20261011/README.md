# Valeur de la sélection WF — investigation historique du 11 octobre 2026

**Résultat : les règles WF enregistrées ne démontrent pas la sélection de
probabilités Up meilleures qu'une référence constante sur les observations
ultérieures examinées.** Ce constat concerne les populations et modèles ci-dessous,
pas l'impossibilité universelle d'un avantage intersymboles.

Cette investigation approfondit EXP-005/EXP-010 et STR-015 sans modifier leurs
registres. Aucun entraînement, booster, lancement WF/E2E/Forward, configuration,
code applicatif ou roadmap n'a été modifié. Les nouveaux fichiers sont exclusivement
dans ce rapport. Les prédictions historiques sont rescorrées ; les nouvelles
décisions du contrôle de préfixe sont des décisions analytiques, pas de nouveaux runs.

## 1. Question, populations et contrats vérifiés

La question est la valeur **après décision** de la qualification WF Up existante,
sans prendre les rares gagnants rétrospectifs comme univers et sans filtrer sur
la confirmation du Holdout évalué.

Sources principales sans préfiltre, 20 titres :

- constante `20261011T031914_c00cea6f22` ;
- cible seule `20261011T031942_217eeee2c1` ;
- mixte `20261011T032130_4742d2f509` ;
- externes `20261011T032201_17ece38f7b`.

Les snapshots préparés sont égaux ; le rapport
`reports/paired_predictors_20261011/` vérifie les prédictions et fenêtres des
familles complètes. Les XGBoost principaux ont profondeur 4, eta 0,20, 20 tours,
lambda 5 et trois retards. La qualification s'appuie sur l'AUC Up, la stabilité,
le nombre de fenêtres et le support positif ; elle ne qualifie pas une log loss
supérieure à une référence.

Contrôle supplémentaire distinct : `20261011T133446_bab008d129`, 150 titres,
22 350 combinaisons externes, profondeur 2, eta 0,20, 20 tours, lambda 5.
Il n'est pas agrégé aux populations de 20 titres et n'est pas une réplication
indépendante : mêmes dates ultérieures et titres possiblement communs.

### Couverture Holdout

| Population | Candidats WF | Qualifiés avant Holdout | Évalués Holdout | Confirmés sur ce Holdout | Titres évalués |
|---|---:|---:|---:|---:|---:|
| Constante 20 | 20 | 0 | 0 | 0 | 0 |
| Cible seule 20 | 20 | 1 | 1 | 0 | 1 |
| Externes 20 | 3 800 | 107 | 107 | 39 | 19 |
| Mixte 20 | 3 800 | 121 | 121 | 56 | 15 |
| Externes 150 | 22 350 | 674 | 674 | 342 | 141 |

Tous les qualifiés ont des prédictions disponibles ; **aucun rejeté de ces runs
n'en a sur Holdout**. Une absence n'est pas remplacée par une métrique zéro.
Les diagnostics de rejet `20260921T000147_5c03cbdeae` et
`20260920T233545_d0b5f9cb79` concernent d'autres populations de septembre,
des candidats forcés et d'autres paramètres (notamment profondeur 6, eta 0,5,
8 tours). Ils ne complètent pas ces populations et ne sont pas fusionnés.

## 2. Résultat des sélectionnés sur Holdout ultérieur

Pour chaque candidat, les frontières, effectifs, labels et clés de prédiction
sont contrôlés contre son snapshot : entraînement terminé le **2 avril 2026**,
test du **6 avril au 6 juillet 2026**, 63 séances. La constante comparatrice
est la prévalence des labels de **ce même train**, figée durant le test. Elle ne
provient jamais du Holdout. Le calcul de cette référence ne réentraîne aucun booster.

Poids égaux entre titres évalués, puis entre candidats sélectionnés d'un titre,
puis entre dates. Ce sont des scores de modèles distincts, pas un ensemble de
prédictions. Les familles ayant 19/15 titres et des candidats différents ne sont
pas classées causalement entre elles ; chacune est confrontée à sa propre constante.

| Up | Log loss XGBoost | Log loss constante | Delta défavorable | Brier XGBoost / constante | AUC moyenne |
|---|---:|---:|---:|---:|---:|
| Externes 20 | 0,618844 | 0,590359 | +0,028486 | 0,207804 / 0,199170 | 0,509166 |
| Mixte 20 | 0,625564 | 0,582120 | +0,043444 | 0,208917 / 0,195695 | 0,481274 |
| Externes 150 | 0,604310 | 0,587984 | +0,016325 | 0,203678 / 0,198744 | 0,499702 |
| Cible seule, un sélectionné | 0,560532 | 0,513329 | +0,047204 | 0,184820 / 0,165283 | 0,450769 |

La moyenne Up des sélectionnés bat la constante pour 3/19 titres externes,
0/15 mixtes et 14/141 titres dans le contrôle à 150. Certains candidats gagnent
individuellement (15/107, 9/121, 166/674) ; ils sont découverts après consultation
du Holdout et ne constituent pas une nouvelle sélection validée.

Les 17 gagnants externes du bilan de développement comprennent 7 qualifiés WF :
**aucun de ces 7 ne bat ensuite la constante en log loss Holdout**. Pour les
10 gagnants mixtes, 6 sont qualifiés et aucun de ces 6 ne la bat ensuite. Ce
sous-ensemble rétrospectif reste un diagnostic de persistance, pas notre univers.

Le classement n'offre pas d'appui descriptif fort : Spearman AUC médiane WF /
AUC Holdout vaut -0,0719 (107 externes), -0,0570 (121 mixtes), +0,0372
(674 du contrôle à 150). Ces associations entre candidats dépendants ne
disposent d'aucun test de significativité ; la restriction aux qualifiés limite
l'étendue des scores et interdit une conclusion universelle sur le classement.

### Incertitude et confirmations

Des intervalles **exploratoires**, conditionnels à ces populations, rééchantillonnent
les dates en blocs circulaires communs aux titres/candidats. Deux longueurs
conventionnelles (5 et 10 séances), 2 000 tirages chacune, vérifient une sensibilité
limitée ; elles ne sont ni paramètres de sélection ni critères d'acceptation approuvés.
Les enveloppes 95 % des deux conventions pour le delta de log loss Up sont :

- externes 20 : environ [0,019858 ; 0,037401] ;
- mixte 20 : [0,032144 ; 0,054592] ;
- contrôle externes 150 : [0,014076 ; 0,018651].

Cela étaye le caractère défavorable des agrégats sous ces conventions, sans
constituer un test indépendant de toute la recherche. Seulement 63 dates,
6,3 à 12,6 blocs approximatifs, hypothèses de stabilité temporelle non garanties,
essais antérieurs et périodes consultées : couverture exacte et généralisation
ne sont pas revendiquées. Aucun bootstrap indépendant par candidat n'est utilisé.

`confirmed_partition_descriptive.csv` montre la partition confirmés/non confirmés,
avec poids égaux candidats. Elle est **sélectionnée sur ce même Holdout** ; ses
meilleurs scores ne valident pas la confirmation. `NativeSignals` utilise le seuil
WF enregistré, pas les seuils combinés et calibrés d'un E2E. Ce rapport ne mesure
donc pas une stratégie de trading ni un portefeuille et n'attribue aucun nouveau P&L.

## 3. Contrôle de sélection sur un préfixe, avec rejetés disponibles

Pour éviter le manque de rejetés sur Holdout, un seul contrôle analytique utilise
les prédictions WF complètes déjà sauvegardées. Le préfixe est **le plus court
satisfaisant le minimum enregistré de quatre fenêtres**, pas le meilleur découpage
observé. Les critères complets de qualification restent inchangés ; leur application
aux sept fenêtres reproduit exactement toutes les décisions historiques des deux runs.

Décision après les quatre premières fenêtres, dernier test le **15 juillet 2025** ;
évaluation des trois suivantes, **16 juillet 2025 → 2 avril 2026**, 181 séances.
Les modèles des fenêtres suivantes utilisent leurs entraînements expansifs déjà
sauvegardés, toujours antérieurs au test. Pas de nouveau fit ni de seuil ajusté.
Les populations sont celles de la décision de préfixe, et non les qualifiés des sept
fenêtres. Le contrôle couvre les 3 800 candidats de chaque famille, sans préfiltre.

| Up, évaluation après décision | Externes | Mixte |
|---|---:|---:|
| Retenus après quatre fenêtres | 345 | 473 |
| Titres avec au moins un retenu | 20 | 18 |
| Delta log loss retenus / constante, poids égaux titres | +0,033117 | +0,030825 |
| Delta des rejetés sur **les mêmes titres/dates** | +0,028195 | +0,028892 |
| Retenus moins rejetés (positif = retenus pires) | +0,004922 | +0,001933 |
| Retenus gagnants ensuite face à la constante | 12/345 | 14/473 |
| Rejetés gagnants ensuite, mêmes titres | 249/3 455 | 180/2 947 |
| Titres dont les retenus moyens battent la constante | 0/20 | 0/18 |

CSCO et REGN n'ont aucun retenu mixte au préfixe ; ils sont exclus de la comparaison
retenus/rejetés mixte et ne reçoivent aucune performance artificielle. Le score de
tous les candidats sur les titres comparables est aussi sauvegardé : delta +0,028700
pour externes et +0,029139 pour mixte.

La moyenne pondérée des AUC **par fenêtre ultérieure** des retenus est environ
0,5036 / 0,5104. Elle n'est pas une AUC recalculée en fusionnant les 181 dates.
Il existe donc au plus un faible indice de classement dans ces agrégats, tandis
que les pertes sont défavorables. Aucun intervalle spécifique n'a été calculé
pour les petites différences retenus/rejetés ; on ne prétend pas démontrer une
infériorité statistique de la sélection. Elle ne montre cependant ici ni enrichissement
en qualité probabiliste ni avantage des retenus face à la référence.

Ce contrôle sépare chronologiquement décision et évaluation **dans l'analyse**.
L'univers, les réglages et les dates ont déjà été consultés : il reste rétrospectif.
Les snapshots révisés du fournisseur ne garantissent pas une disponibilité PIT
historique. Ce contrôle ne restaure pas une preuve prospective.

## 4. Conclusion, statut et prochaine décision

**Investigation historique complétée, résultat négatif borné.** L'hypothèse selon
laquelle la qualification AUC/stabilité actuelle suffit à identifier des modèles
apportant un avantage probabiliste futur ne reçoit pas d'appui dans ces populations.
Le résultat n'est pas limité à une moyenne des 3 800 candidats non sélectionnés :
les sélectionnés échouent aussi après décision, et le contrôle préfixe n'enrichit
pas leur qualité probabiliste comparée aux rejetés.

Pour EXP-005/STR-015 : arrêter les optimisations équivalentes de cette règle et ne
plus la présenter comme preuve de valeur prédictive. Le principe de qualification,
de seuils figés et de séparation chronologique **n'est pas abandonné** ; ses critères
doivent être réévalués. Ce rapport ne modifie aucun statut du registre ni aucune
configuration opérationnelle. La validation indépendante reste non acquise.

Prochaine étape recommandée, soumise à adoption : **préparer un seul challenger
parsimonieux de XGBoost**, avant d'optimiser les seuils ou de relancer une découverte.
L'AUC future proche de 0,5 ne justifie pas de parier d'abord sur une recalibration
qui n'améliore pas un classement par simple transformation monotone. Un comparateur
simple régularisé peut tester si la complexité de l'estimation détruit un éventuel
signal ; il ne garantit pas qu'un signal existe.

Plan concret de préparation : mêmes entrées et identités préspecifiées, Up,
référence constante du même train, une seule famille simple et une régularisation
définie en amont, aucune expansion des combinaisons. Distinguer comparaison de
modèles à population figée et test de la procédure entière. Définir avant lancement
l'effet minimum utile, le support, la durée et les critères de poursuite/arrêt,
actuellement **non approuvés**. Identifier les périodes déjà consultées et réserver
une période réellement indépendante pour une confirmation. Une éventuelle acquisition,
implémentation scientifique ou tout entraînement exige une mission distincte.

Si le modèle simple ne montre pas d'avantage utile, suspendre la formulation actuelle
des retards intraday intersymboles et revoir variables/horizon. S'il gagne de manière
crédible, préparer ensuite une validation indépendante de sélection, sans déclarer
la stratégie validée sur ce diagnostic historique.

## Reproduction et provenance

`python -B reports/selection_value_20261011/analyse.py` écrit uniquement dans ce
dossier. Le script réutilise `probability_losses` et `_roc_auc`, n'importe aucun
service applicatif et ne charge aucun booster. Il vérifie sources SHA-256 avant/après,
contrats des snapshots, labels, dates, frontières, couverture et décisions de qualification.
Les sources historiques restent inchangées. Les sorties incluent `summary.json`,
les scores individuels/par titre, couverture, diagnostics de rang, intervalles
exploratoires et les trois fichiers `prefix_selection_*`. Les scripts et rapports
antérieurs sont référencés, jamais remplacés.
