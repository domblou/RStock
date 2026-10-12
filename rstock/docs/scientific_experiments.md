# Registre des expériences scientifiques

Audit documentaire du 11 octobre 2026. Synthèse et priorités :
[roadmap scientifique](scientific_roadmap.md).

## Portée et lecture

350 dossiers de runs ont été examinés par leurs configurations et statuts ;
les résumés, manifests et rapports pertinents ont été lus, sans exécution métier.
Ce registre regroupe les questions étudiées, pas chaque exécution technique.
18 investigations sont identifiées : 13 avec résultats sauvegardés (dont plusieurs
partielles), 1 constat méthodologique vérifié, 4 comparaisons sans preuve primaire suffisante.
Le complément utilisateur confirme historiquement EXP-003, sans ajouter d’artefact primaire.
Un run `completed` ne signifie pas une expérience concluante. Les résultats
ci-dessous sont ceux des artefacts conservés, sans recalcul de métriques ni
nouvelle vérification exhaustive des empreintes de tous les runs.

Les conversations accessibles sont celles de cette session et les décisions
consignées dans le dépôt ; aucune autre conversation ChatGPT n'a été consultée.
Les fichiers sous `runs/` et `simulations/` sont locaux et peuvent disparaître
après purge. Les chemins relatifs ci-dessous sont résolus depuis la racine du dépôt.

## Expériences, stratégies et statuts

Une stratégie est une méthode ; une expérience étudie une question ; un run est
une exécution concrète. Les approches et décisions sont dans le
[registre des stratégies](scientific_strategies.md). Les 18 IDs EXP-001–EXP-018 restent
inchangés ; le détail existant conserve paramètres, sources, mesures et limites.

Les statuts autorisés sont : **Planifiée**, **En cours**, **Réalisée — concluante**,
**Réalisée — non concluante**, **Réalisée — preuves incomplètes**, **Abandonnée**.
« Non concluante » ne signifie pas « jamais exécutée » ni « aucun résultat utile ».
Aucune entrée n’est déclarée concluante sur la généralisation indépendante.
Pour EXP-004, **Planifiée** désigne la comparaison contrôlée : la préférence
historique pour Expanding est attestée, mais aucune preuve documentaire suffisante
ne confirme l'exécution de cette comparaison. EXP-003 est désormais identifiée comme réalisée sur la base du récit
historique fourni ; ses artefacts primaires restent à retrouver.
EXP-014 planifie la quantification du biais ; le constat méthodologique déjà vérifié
est conservé et n’est pas transformé en expérience quantitative exécutée.
EXP-018 reste un ensemble de variantes, sans multiplier artificiellement les IDs.

| Expérience | Objectif / hypothèse à tester | Stratégies | Statut |
|---|---|---|---|
| [EXP-001](#exp-001) | Les seuils de préfiltre améliorent-ils la sélection ? | [STR-005](scientific_strategies.md#str-005), [STR-009](scientific_strategies.md#str-009) | Réalisée — non concluante |
| [EXP-002](#exp-002) | Quelle méthode temporelle sélectionne les candidats les plus robustes ? | [STR-001](scientific_strategies.md#str-001), [STR-002](scientific_strategies.md#str-002), [STR-003](scientific_strategies.md#str-003) | Réalisée — preuves incomplètes |
| [EXP-003](#exp-003) | Une séance et le changement de train expliquent-ils l’instabilité ? | [STR-001](scientific_strategies.md#str-001), [STR-002](scientific_strategies.md#str-002), [STR-003](scientific_strategies.md#str-003), [STR-004](scientific_strategies.md#str-004) | Réalisée — preuves incomplètes |
| [EXP-004](#exp-004) | Expanding ou rolling généralise-t-il mieux ? | [STR-004](scientific_strategies.md#str-004) | Planifiée |
| [EXP-005](#exp-005) | La qualification et les seuils se maintiennent-ils au Holdout ? | [STR-015](scientific_strategies.md#str-015), [STR-010](scientific_strategies.md#str-010) | Réalisée — non concluante |
| [EXP-006](#exp-006) | La découverte se maintient-elle à différentes origines ? | [STR-013](scientific_strategies.md#str-013), [STR-015](scientific_strategies.md#str-015) | Réalisée — preuves incomplètes |
| [EXP-007](#exp-007) | Les candidats issus du Holdout atteignent-ils le MVP en Forward ? | [STR-010](scientific_strategies.md#str-010), [STR-009](scientific_strategies.md#str-009), [STR-005](scientific_strategies.md#str-005) | Réalisée — non concluante |
| [EXP-008](#exp-008) | Quels réglages XGBoost apportent un gain stable ? | [STR-009](scientific_strategies.md#str-009) | Réalisée — non concluante |
| [EXP-009](#exp-009) | La sélection chronologique bat-elle le fixe et la référence ? | [STR-009](scientific_strategies.md#str-009), [STR-007](scientific_strategies.md#str-007) | Réalisée — non concluante |
| [EXP-010](#exp-010) | Quelle famille de variables apporte une valeur additionnelle ? | [STR-006](scientific_strategies.md#str-006), [STR-007](scientific_strategies.md#str-007) | Réalisée — non concluante |
| [EXP-011](#exp-011) | Le classement cible/constante survit-il à l’alignement du train ? | [STR-006](scientific_strategies.md#str-006), [STR-007](scientific_strategies.md#str-007) | Réalisée — non concluante |
| [EXP-012](#exp-012) | La volatilité passée ajoute-t-elle une valeur prédictive ? | [STR-008](scientific_strategies.md#str-008), [STR-006](scientific_strategies.md#str-006) | Réalisée — non concluante |
| [EXP-013](#exp-013) | Enrichir les combinaisons et l’univers améliore-t-il la généralisation ? | [STR-006](scientific_strategies.md#str-006) | Réalisée — non concluante |
| [EXP-014](#exp-014) | Quelle part du gain WF provient de la sélection sur ses périodes test ? | [STR-011](scientific_strategies.md#str-011), [STR-005](scientific_strategies.md#str-005) | Planifiée |
| [EXP-015](#exp-015) | Les fenêtres SPY décrivent-elles convenablement les épisodes connus ? | [STR-014](scientific_strategies.md#str-014) | Réalisée — preuves incomplètes |
| [EXP-016](#exp-016) | Combien de temps l’avantage des modèles figés persiste-t-il ? | [STR-010](scientific_strategies.md#str-010), [STR-012](scientific_strategies.md#str-012), [STR-013](scientific_strategies.md#str-013) | Réalisée — preuves incomplètes |
| [EXP-017](#exp-017) | Le réentraînement prolonge-t-il la valeur des mêmes modèles ? | [STR-012](scientific_strategies.md#str-012) | Réalisée — preuves incomplètes |
| [EXP-018](#exp-018) | Les variantes de définition d’événement et de préfiltre apportent-elles un gain ? | [STR-005](scientific_strategies.md#str-005), [STR-006](scientific_strategies.md#str-006), [STR-015](scientific_strategies.md#str-015) | Planifiée |

## Découverte, préfiltre et sélection

| ID / objectif | Variantes et sources | Résultat, statut et limite |
|---|---|---|
| <a id="exp-001"></a>EXP-001 — Sensibilité du préfiltre | WF du 13 septembre : `20260913T201916_7b87aaacf1` à `20260913T214614_fa29f85b8c`. Configurations : Top N 10/12/20, Worst AUC 0,35/0,40 ; médiane AUC 0,50, proportion 0,50 et écart-type 0,15 dans les snapshots retrouvés. | Explorations réalisées, non concluantes sur la généralisation : populations et nombres admissibles changent. Les configurations changent aussi XGBoost ; aucun effet isolé de chaque seuil n'est démontré. Pas de preuve retrouvée d'un balayage des trois valeurs fixes. |
| <a id="exp-002"></a>EXP-002 — Single Origin / stabilité temporelle / consensus | Consensus exécutés `20261008T040635_3b1b0e8ae6`, `20261009T024818_3ce61407de` ; 4 origines 2026-07-06, 06-03, 05-04, 04-02, pas 21 séances, minimum 3 occurrences. | Consensus réalisé ; comparaison entre les trois méthodes non confirmée par les artefacts retrouvés. L’utilisateur rapporte une préférence historique pour Temporal Consensus dans certaines comparaisons ; références et chiffres originaux restent à retrouver. Cette observation favorable n’efface pas les limites ni les résultats Forward négatifs EXP-007. Les deux WF associés rapportent 1 814 combinaisons et 746 admissibles. Cela ne démontre ni gain Forward ni correction du biais de sélection. |
| <a id="exp-003"></a>EXP-003 — Décalage d'une séance / composition du train | Analyse contrôlée J→J+1 rapportée par l’utilisateur ; sept fenêtres, six de 63 séances et une de 55. | Réalisée — preuves incomplètes : mesures et décomposition historique ci-dessous, sans paire de runs ni artefact primaire retrouvé. Ne pas remplacer ce manque par les comparaisons offset 63. Retrouver les sources avant de proposer une répétition. |
| <a id="exp-004"></a>EXP-004 — Expanding contre rolling | Expanding retenu dans les décisions existantes ; ajout rolling au commit `e727014`. Les snapshots inspectés ont expanding ou un champ historique absent. | Décision conservée ; comparaison scientifique chiffrée non retrouvée. L'ajout du mode et ses tests ne prouvent pas son évaluation empirique. |
| <a id="exp-005"></a>EXP-005 — Qualification, calibration et Holdout | Runs WF/calibrations/E2E de septembre et octobre ; ex. `20261011T115541_2a0e278563` et `20261011T130921_392c045368`. | Exécutions réalisées : dans ces deux variantes cible seule, 1/20 admissible WF, 0 confirmé Holdout. Les critères de qualification/promotion sont figés par run ; aucun seuil universel optimal n'est démontré. Les candidats Holdout diffèrent (BMY/COST) : pas de comparaison Holdout appariée. |
| <a id="exp-006"></a>EXP-006 — Univers et redécouverte à dates décalées | Comparaisons persistées sous `runs/<ID>/results/temporal_validation_comparison.json`, tableau ci-dessous ; univers de 15, 165 et 12 symboles déclarés. | Partiellement réalisée : gates positifs et négatifs, parfois aucune identité commune. Ni indépendance prospective ni garanties fournisseur PIT démontrées. |
| <a id="exp-008"></a>EXP-008 — Paramètres XGBoost et régularisation | WF septembre : profondeur arbres 4/6, eta 1/0,50/0,25 et 4/8/16 tours ; calibrations conservées. Octobre : configurations 4/0,20/20 et 2/0,05/80. | Réalisée mais non concluante comme optimum stable : changements de population/préfiltre et sélections multiples. Réutiliser les calibrations et snapshots ; ne pas relancer une grille équivalente sans hypothèse nouvelle. |
| <a id="exp-013"></a>EXP-013 — Richesse des combinaisons/univers | `20260916T040549_c16bf64fd1` : profondeur 3, 65 symboles, 14 431 combinaisons, 1 895 admissibles. `20261011T133446_bab008d129` : profondeur 1, 150 symboles, sans préfiltre, 22 350 combinaisons, 674 admissibles, 342 confirmées. | Réalisée, non concluante sur l'intérêt de profondeur 3 : univers et réglages différents, absence d'une comparaison contrôlée profondeur 1/2/3 et d'un Forward correspondant vérifié ici. Ne pas confondre ces profondeurs avec celle des arbres. |
| <a id="exp-014"></a>EXP-014 — Biais de sélection du préfiltre | [Protocole XGBoost](CHRONOLOGICAL_XGBOOST.md), [diagnostic de sélection](selection_diagnostic.md), code de sélection avant WF. | Constat méthodologique confirmé : des périodes sélectionnant les prédicteurs chevauchent les tests WF. Son ampleur n'est pas quantifiée ici. Consensus ou héritage figé ne prouvent pas son élimination. |
| <a id="exp-018"></a>EXP-018 — Autres variantes retenues | Seuils d'événement 1 %, 0,75 %, 0,5 % ; préfiltre enrichi de volatilité ; roadmap antérieure. | À faire ou preuve d'exécution à retrouver pour ces comparaisons précises. Ne pas confondre volatilité du modèle final et volatilité dans le préfiltre. |

### Complément historique EXP-001–EXP-005 et EXP-013

Source : informations fournies par l’utilisateur dans cette session, issues de
ses discussions historiques. Aucun nouvel audit des 350 runs ni calcul n’a été
réalisé. Les chiffres non reliés à une source primaire sont **historiques rapportés** ;
ils ne remplacent pas les résultats vérifiés ci-dessus.

**EXP-002 — Appartenance des populations de préfiltre (environ 388–398 candidats).**

| Mesure historique rapportée | Résultat |
|---|---:|
| Conservation à J+1 | 77,6 % |
| Conservation après environ 21 séances | 60,7–63,8 % |
| Recouvrement du 16 juin au 27 juillet | 57 % |
| Présents aux quatre cutoffs | 155 |
| Présents à trois cutoffs sur quatre | 140 |

Comparaisons de stabilité rapportées comme réalisées ; année, IDs, méthode exacte
associée à chaque chiffre et dénominateurs restent non retrouvés. Ne pas attribuer
ces cinq mesures exclusivement au consensus ni les assimiler aux 1 814 combinaisons
WF vérifiées. Préférence historique pour Temporal Consensus dans certaines
comparaisons ; Temporal Stability contre Single Origin non concluant. Aucun gain
prédictif Forward robuste démontré : stabilité d’appartenance ≠ rentabilité.

**EXP-003 — Décomposition du décalage J→J+1, sept fenêtres.**

| Relation | AUC initiale | AUC décalée | Effet train rapporté | Effet test rapporté |
|---|---:|---:|---:|---:|
| AMAT / UBER | 0,5369 | 0,4719 | ≈ −0,0654 | ≈ +0,0004 |
| MU / QCOM | 0,4822 | 0,5164 | ≈ +0,0355 | ≈ −0,0013 |
| ABNB / CRWD | 0,5303 | 0,5328 | Non fourni | Non fourni |

Conclusion historique : le changement de composition du train explique l’essentiel
des deux écarts décomposés ; ABNB/CRWD est un contrôle relativement stable.
De petits changements d’entraînement peuvent affecter certaines relations :
conclusion limitée à ces relations/fenêtres. Dates, directions, méthode d’agrégation
AUC et IDs restent à retrouver. Les sept fenêtres ne sont pas présumées indépendantes.

**EXP-005/EXP-013 — Classement WF → Holdout, univers de 150 symboles.**

Le run déjà identifié `20261011T133446_bab008d129` correspond aux caractéristiques
vérifiées : 22 350 relations, 674 qualifiées WF, arbres profondeur 2, 20 tours,
sans préfiltre ; le cutoff conservé est 2026-07-06. Ce rapprochement ne suffit pas
à vérifier l’origine des métriques d’analyse supplémentaires rapportées :

| Mesure historique rapportée | Valeur |
|---|---:|
| AUC Holdout moyenne | ≈ 0,502 |
| Corrélation de classement WF/Holdout | ≈ 0,037 |
| Log loss moyenne XGBoost | 0,6032 |
| Log loss moyenne constante | 0,5877 |

Résultat négatif historique : cette sélection WF ne démontre pas l’identification
fiable des meilleurs candidats Holdout ; perte XGBoost supérieure à la constante.
Population exacte des moyennes, direction, pondérations, type de corrélation et
alignement de la constante restent non vérifiés. Les 342 confirmées déjà
consignées utilisent la règle de confirmation du run : ce compte n’invalide pas
la faiblesse rapportée du classement et n’est pas une preuve de rentabilité.
Ne pas comparer ces pertes aux pertes globales WF ni aux moyennes alignées EXP-011.

**EXP-001/EXP-004/EXP-005 — Décisions de paramètres historiques.**

| Décision rapportée | Statut et justification disponible |
|---|---|
| Expanding privilégié à rolling | Retenue provisoirement ; décision déjà conservée, preuve comparative chiffrée toujours manquante. |
| Worst AUC préfiltre 0,30 rejeté ; plancher 0,35 conservé | Variante rejetée / plancher retenu historiquement ; 0,35 vérifié dans des snapshots. Motif comparatif précis du rejet 0,30 non fourni, optimum non démontré. |
| `threshold_calibration_min_robust_signals = 6` | Valeur historique retenue, justification comparative non retrouvée. |
| Précision minimale Holdout 55 % | Valeur historique retenue, périmètre/version du critère à retrouver. |
| Minimum total de signaux de qualification 25 | Valeur historique retenue ; nom technique et unité/périmètre à retrouver, sans assimilation automatique à un autre minimum. |
| Temporal Stability / Single Origin | Comparaisons historiquement non concluantes ; pas de répétition sans élément nouveau. |

**Divergences de configuration, conservées sans correction applicative :** les
faits actuels sont distincts de ces décisions historiques. `rstock/config.py`
a pour defaults Worst AUC préfiltre 0,40, robust signals 10, précision promotion
Holdout 0,40 et minimum signaux promotion 20. Le snapshot du run 150 symboles
persiste respectivement 0,35, **25**, **0,52** et **10**. Ces différences ne
prouvent pas que les décisions historiques sont fausses, mais empêchent de les
présenter comme les valeurs de ce run ou les defaults actuels. Les seuils de
qualification, calibration et promotion ont des rôles distincts. Aucun optimum
ni justification absente n’est inventé ; aucun paramètre n’est modifié.

<a id="parametres-correspondance"></a>

### Paramètres : correspondance historique, snapshots et rôles

Vérification ciblée de `rstock/config.py`, `qualification.py`,
`threshold_calibration.py`, `application/history_analysis.py` et
`application/production_services.py`. Les trois snapshots consultés —
`20261011T133446_bab008d129`, `20261009T115931_e9013b1a13`,
`20261011T115541_2a0e278563` — ont les mêmes valeurs ci-dessous ; cela ne
préjuge pas des autres runs ni des réglages globaux enregistrés sur une installation.

| Décision historique / rôle | Nom technique | Default actuel | Snapshots consultés | Usage opérationnel vérifiable |
|---|---|---:|---:|---|
| Robustesse de calibration : 6 retenu | `threshold_calibration_min_robust_signals` | 10 | 25 | Sélection du seuil en développement ; Production utilise le seuil figé, pas ce minimum comme filtre quotidien. |
| Précision Holdout : 55 % retenu, périmètre à confirmer | `promotion_min_holdout_precision` est le critère actuel de promotion ; attribution historique non certaine | 0,40 | 0,52 | Qualification de promotion selon le snapshot ; pas un seuil de probabilité du screening. |
| Minimum total de qualification : 25 rapporté, champ inconnu | Attribution non confirmée | Non attribuable | Non attribuable | Ne pas substituer calibration/promotion/observations positives à cette décision. |
| Observations positives WF, distinctes des signaux | `qualification_min_positive_observations` | 20 | 20 | Règle WF conservée dans les métadonnées du modèle promu ; pas un minimum de trades. |
| Signaux Holdout pour promotion, distincts du minimum historique ci-dessus | `promotion_min_holdout_signals` | 20 | 10 | Règle de qualification promotion, pas quota quotidien. |
| Worst AUC préfiltre : 0,30 rejeté / 0,35 conservé | `predictor_prefilter_min_worst_auc` | 0,40 | 0,35 | Sélection des prédicteurs avant WF ; pas recalculé dans le screening. |
| Fenêtres WF AUC > 0,50 : 66,67 % / 75 % rapportés | `qualification_min_pct_windows_above_random` | 2/3 | 2/3 | Qualification WF ; pas une mesure Holdout ni une proportion de signaux rentables. |
| Proportion de fenêtres du préfiltre, critère différent | `predictor_prefilter_min_pct_above_random` | 0,50 | 0,50 | Préfiltre seulement. |
| Écart-type AUC WF | `qualification_max_auc_std` | 0,10 | 0,10 | Qualification WF. |
| Écart-type AUC préfiltre | `predictor_prefilter_max_auc_std` | 0,15 | 0,15 | Préfiltre seulement. |

EXP-001/EXP-005 : les variantes 66,67 % et 75 % sont désormais conservées comme
investigation historique rapportée ; 66,67 % est confirmé dans les snapshots
ciblés, exécution de 75 % et résultats comparatifs non retrouvés. L’écart-type
0,10 WF et 0,15 préfiltre est confirmé, mais ces deux rôles ne constituent pas
une comparaison de variantes d’un même critère. Autres valeurs d’écart-type et
effets isolés non disponibles : ne pas en inventer. Le rejet de Worst AUC 0,30
reste une décision historique dont la justification chiffrée manque.

Les écarts 6/10/25 et 55 %/40 %/52 % peuvent refléter des décisions non appliquées,
des versions ou des périmètres différents ; ils ne sont pas déclarés bogues.
La configuration effective et les seuils du modèle promu font autorité pour son
usage ; aucun registre Production global n’a été réaudité. Le screening vérifié
utilise `P(Up) >= seuil Up` et `P(Down) < seuil Down`, figés par modèle : il ne
réapplique pas ces critères statistiques comme paramètres globaux modernes.
Aucun réglage n’est modifié. Aucun optimum de ces seuils n’est démontré ; ne pas
répéter les comparaisons non concluantes sans hypothèse ou preuve supplémentaire.

### Protocoles futurs rattachés aux investigations existantes

- **EXP-013 — Trois prédicteurs :** tester si les profondeurs de combinaison 1/2/3
  améliorent réellement la généralisation. Même univers, cutoff, calendrier,
  snapshot, géométrie WF, qualification/calibration et paramètres XGBoost ; mesures
  WF, Holdout et Forward comparables. La profondeur des combinaisons compte les
  externes ; la profondeur des arbres XGBoost est un autre paramètre. Un supplément
  de candidats admissibles n’est pas un gain prédictif. Comparaison contrôlée
  future, distincte des runs de profondeurs/univers différents déjà conservés.
- **EXP-014 — Biais :** risque identifié, ampleur non quantifiée, problème non
  réputé corrigé. Consensus n’est pas une correction automatique. Distinguer
  population figée et nouvelle découverte ; toute future mesure préserve une
  séparation temporelle rigoureuse de sélection et tests.
- **EXP-006 — Redécouverte PIT :** cutoffs indépendants des décisions de sélection/
  optimisation autant que possible ; configurations et snapshots figés, même
  découverte/qualification, Forward comparable. Mesurer recouvrement, nouveaux
  candidats et candidats conservés séparément ; tracer disponibilité historique
  fournisseur et limites PIT. Les comparaisons sans candidat commun ci-dessous
  restent des résultats, pas une preuve de renouvellement performant.
- **EXP-016/EXP-017 — Vieillissement et réentraînement :** protocole jumelé et
  intervalles 1–21, 22–42, 43–63, 64–84, 85–126 décrits sous
  [STR-012](scientific_strategies.md#str-012). Les simulations actuelles ne clôturent
  pas cette comparaison contrôlée et aucun nouvel horizon n’est déclaré observé.

Ces compléments ne créent aucun nouvel ID et n’autorisent aucun lancement.
EXP-004 reste **Planifiée** : préférence Expanding conservée, exécution contrôlée
non suffisamment attestée ; Rolling n’est pas scientifiquement rejeté.

### EXP-006 — Résultats temporels sauvegardés

| E2E de référence | Décision persistée | Connaissance utile |
|---|---|---|
| `20260919T013519_3e1ce32657` | failed | 0 candidat ; AUC Holdout médiane 0,51985 → 0,45833 ; autres gates inconclusive. |
| `20260919T020736_28fa866cd2` | passed | 16 → 16 candidats, 0 commun ; precision edge 0,13523, IC 95 % [0,09764 ; 0,17673] ; rendement directionnel 0,005857, IC [0,003073 ; 0,008847]. |
| `20260919T213755_20f36d8467` | passed | 8 → 6 candidats, 0 commun ; succès des gates historiques, sans preuve de maintien des mêmes modèles. |
| `20260922T023754_ac0dc582e8`, `20260922T025100_96bd4d23b4` | failed | 0 candidat de part et d'autre ; gates précision/rendement inconclusive. Même fingerprint et digests retrouvés : répétitions regroupées, pas réplications indépendantes. |

Les IC et `passed` sont ceux du protocole historique. Les candidats sont déjà
sélectionnés sur les résultats et les dates peuvent avoir été consultées : ces
succès ne valent pas preuve indépendante du système complet.

## Performances hors échantillon et sources prédictives

<a id="exp-007"></a>

### EXP-007 — Holdout → Forward figé : résultat négatif pour le MVP étudié

Cinq runs terminés, quatre résultats distincts, cutoff 2026-07-06, 63 séances
du 2026-07-07 au 2026-10-02. Sources : `runs/<ID>/summary.json`, configurations
et résultats ; politique FROZEN. Aucun nouvel entraînement pendant cet audit.

| Forward | Modèles / signaux | Précision | Rendement directionnel moyen | P&L brut à 10 000 $ par signal |
|---|---:|---:|---:|---:|
| `20261008T094711_66d01fb94e`, `20261009T093338_02102575c4` | 8 / 110 | 0,26364 | −0,000880 | −967,79 $ |
| `20261009T151834_9d02adfc6d` (fixe 80) | 15 / 261 | 0,29885 | −0,002513 | −6 558,05 $ |
| `20261010T001623_22e2343d22` (chronologique) | 12 / 197 | 0,34010 | −0,000307 | −604,53 $ |
| `20261011T031609_c98f33d319` (sans préfiltre, 20 titres) | 1 / 24 | 0,25000 | −0,008102 | −1 944,47 $ |

Réalisation confirmée, résultat brut négatif : ces configurations n'atteignent
pas le rendement positif visé sur cette période. Les populations sélectionnées
diffèrent : aucun classement causal fixe/chronologique ou avec/sans préfiltre
ne découle de leurs P&L. Fenêtre commune rétrospective, sans coûts, exposition
par signal et dépendance entre modèles : pas de portefeuille ni réplications
indépendantes. Ne pas additionner les deux résultats identiques de 8 modèles.

<a id="exp-009"></a>

### EXP-009 — Tours fixes contre sélection chronologique

[Rapport détaillé](../../reports/analyse_xgb_20261010_fixe_chronologique/SYNTHESE.md),
E2E `20261009T115931_8bf594b3bd` / `20261010T001622_e7f734b61a`,
WF `20261009T115931_e9013b1a13` / `20261010T001623_73fb23b91f`.
1 814 candidats, 12 362 fenêtres, 765 078 observations par direction appariées ;
80 tours fixes contre sélection interne chronologique, même préfiltre hérité.

Chronologique moins fixe : Up Δ log loss +0,001231, Brier +0,000494, AUC moyenne
−0,008197 ; Down −0,004838, −0,001356 et +0,001103 respectivement.
Up favorise descriptivement le fixe ; Down améliore le fixe mais reste moins bon
que la constante train en log loss/Brier. Réalisée, non concluante statistiquement :
préfiltre commun, observations dépendantes, commits différents et absence de
preuve prospective. Ne pas refaire cette paire sans changement justifié.

### EXP-010/EXP-011 — Familles de variables et constante correctement alignée

<a id="exp-010"></a>

EXP-010 : quatre WF terminés sans préfiltre, 20 titres, cutoff 2026-07-06 :
`20261011T031914_c00cea6f22` constante (0/20 admissible),
`20261011T031942_217eeee2c1` cible seule profondeur arbres 4 (1/20),
`20261011T032130_4742d2f509` cible + externes (121/3 800),
`20261011T032201_17ece38f7b` externes (107/3 800).
Le [bilan apparié terminé](../../reports/paired_predictors_20261011/README.md)
contrôle toutes les clés/labels et l'apprentissage/test commun sur 433 dates,
avant qualification. Up : log loss constante **0,542410**, cible seule **0,571065**,
externes **0,575665**, mixte **0,577120** ; Brier respectivement **0,178392**,
**0,187094**, **0,187884**, **0,188310**. Poids égaux par cible, puis candidats.
Chaque famille XGBoost reste derrière la constante pour 20/20 cibles et les trois
tiers temporels. Dix-sept externes et dix mixtes la battent individuellement,
mais sont des gagnants identifiés après comparaison de milliers d'essais.
Résultat négatif descriptif, sans preuve d'absence universelle de signal ou de
significativité ; moyenne des scores, pas ensemble ni sélecteur. Aucun nouvel
entraînement, Holdout ou Forward. Statut non concluant pour la généralisation ;
les effectifs qualifiés ne classent pas la valeur prédictive.

<a id="exp-011"></a>

EXP-011 : [comparaison alignée sauvegardée](../../diagnostics/aligned_constant_20261011/README.md),
avec cible seule arbres profondeur 2 `20261011T115541_2a0e278563`.
20 titres × 2 directions, 433 dates, 2024-07-12 à 2026-04-02, avant qualification.
Moyennes à poids égaux titre/direction : log loss constante alignée **0,532289**,
cible profondeur 2 **0,545733**, profondeur 4 **0,561847** ; Brier **0,174129**,
**0,178302**, **0,182979**. Alignement réalisé : la correction du calendrier ne
change pas le classement défavorable aux variantes cible seule. Résultat négatif
descriptif ; ni significativité ni invalidité universelle des retards démontrées.

<a id="exp-012"></a>

### EXP-012 — Volatilité passée de la cible

[Préparation historique](../../diagnostics/target_volatility_mvp_20261011/README.md)
initialement sans lancement ; exécution ultérieure confirmée :
contrôle `20261011T115541_2a0e278563` et dérivé `20261011T130921_392c045368`,
`target_only`, 20 titres, cutoff 2026-07-06, trois retards, arbres profondeur 2,
eta 0,20, 20 tours, lambda 5 ; ajout N=21 uniquement dans les overrides enregistrés.
Données rétrospectives ; les résumés rapportent 8 660 observations dans chaque run.

Scores globaux persistés : Up log loss 0,554595 → 0,555338 et Brier
0,182101 → 0,182266 ; Down 0,536870 → 0,534465 et 0,174504 → 0,174113.
Un candidat qualifié dans chaque run, aucun confirmé Holdout. Réalisée, non
concluante : aucun gain Up observé dans ces agrégats, amélioration descriptive
Down ; AUC globale non utilisée comme preuve appariée. Masques complets et pertes
par candidat/fenêtre restent à comparer en réutilisant les artefacts. Pas de
Forward indépendant de cette variante retrouvé.

## Vieillissement, simulations et contexte

| ID | Sources et résultats | Statut / limite |
|---|---|---|
| <a id="exp-015"></a>EXP-015 — Fenêtres de contexte SPY | [Validation descriptive](market_context_protocol.md), `diagnostics/context_protocol_validation/validation_manifest.json` : 12 variantes 42/63/84 × 126/252 × 21/42, snapshot 2023-01-03 → 2026-10-07. Février–avril 2025 : drawdown max 18,76 %, volatilité max 52,40 %. | Partiellement réalisée : 2018/2020/2022 absents, warmup 2023 incomplet. Décrit le marché, ne valide pas la prédictivité par régime ; modèles par régime reportés. |
| <a id="exp-016"></a>EXP-016 — Durée de vie | Les Forward EXP-007 ont 63 séances et des diagnostics temporels sauvegardés. | Partiellement réalisée : comportement négatif sur l'horizon agrégé, sans estimation validée de durée de vie ni preuve de renouvellement à 126 séances. Les outils de jalons ne prouvent pas des observations à chaque horizon. |
| <a id="exp-017"></a>EXP-017 — Figé contre réentraîné / simulations | `simulations/20260924T161827_1e6ea729a6/simulation.json` : Historique, 2026-03-24 → 09-21, 1 117 trades, +42 896,66 $. `20260924T164308_aac8071489` : frozen_at_start, mêmes dates, 1 137 trades, +37 560,73 $. Autres simulations : `20260918T164229_4364fbe664`, `20260923T122131_3d1956d693`. | Exécutions rétrospectives réalisées, comparaison scientifique partielle : nombre de signaux différent, population active choisie après coup, appariement des modèles et provenance du mode Historique ancien à contrôler avant attribution au réentraînement. Rendements bruts à notionnel par signal, sans preuve indépendante ni capital limité. |

## Réutilisation avant toute nouvelle proposition

EXP-007, EXP-009, EXP-011, EXP-012 et les grilles exploratoires EXP-001/EXP-008 ont déjà des résultats
négatifs ou non concluants. Toute répétition exige une hypothèse nouvelle,
un changement méthodologique justifié ou une preuve supplémentaire attendue.
Pour EXP-002/EXP-003/EXP-004, chercher d'abord les anciens rapports ou paires de runs ;
un manque documentaire ne signifie pas que le test n'a jamais eu lieu.
Les expériences futures retenues et leur ordre restent dans la roadmap.
