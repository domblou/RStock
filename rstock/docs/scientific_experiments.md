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
Ce plan est historique : la décision utilisateur du 13 octobre 2026 accepte le
chevauchement préfiltre/WF pour la qualification selon la
[convention permanente](scientific_roadmap.md#convention-scientifique-permanente--préfiltre-et-walk-forward).
Il ne constitue pas une intervention à lancer ni un défaut architectural à
corriger; sa réouverture exige l'élément nouveau défini par cette convention.
EXP-018 reste un ensemble de variantes, sans multiplier artificiellement les IDs.
EXP-019 ajoute l'étude de formulation autorisée et terminée le 12 octobre 2026,
après l'audit historique; aucune nouvelle performance prédictive n'y est calculée.
EXP-020 est la comparaison exploratoire overnight autorisée à la suite de cette étude.
EXP-021 ajoute le diagnostic unique de rendement continu ridge autorisé le
13 octobre 2026, avec protocole figé avant calcul; aucun changement applicatif.
EXP-022 évalue l'ajout du contexte SPY 5 séances / volatilité 21 séances,
autorisé le même jour sur la population figée du dernier WF logistique.
EXP-023 reprend le même diagnostic avec tendance SPY 21 séances à la demande
de l'utilisateur, sans changer la volatilité, les supports ou les critères.

| Expérience | Objectif / hypothèse à tester | Stratégies | Statut |
|---|---|---|---|
| [EXP-026](#exp-026) | Ajouter systématiquement les trois retards intraday SPY améliore-t-il les combinaisons ? | [STR-014](scientific_strategies.md#str-014), [STR-006](scientific_strategies.md#str-006), [STR-007](scientific_strategies.md#str-007) | Réalisée — non concluante |
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
| [EXP-019](#exp-019) | Les informations préouverture correspondent-elles à la cible et à l'horizon prédits ? | [STR-006](scientific_strategies.md#str-006) | Réalisée — preuves incomplètes |
| [EXP-020](#exp-020) | Les rendements overnight passés ajoutent-ils un gain probabiliste aux retards intrajournaliers ? | [STR-006](scientific_strategies.md#str-006), [STR-007](scientific_strategies.md#str-007) | Réalisée — non concluante |
| [EXP-021](#exp-021) | Prévoir directement le rendement intrajournalier permet-il de battre sa moyenne train et d'apporter un gain économique ? | [STR-016](scientific_strategies.md#str-016), [STR-006](scientific_strategies.md#str-006), [STR-007](scientific_strategies.md#str-007) | Réalisée — non concluante |
| [EXP-022](#exp-022) | Un contexte SPY récent améliore-t-il les externes au-delà du marché seul et de la constante ? | [STR-014](scientific_strategies.md#str-014), [STR-006](scientific_strategies.md#str-006), [STR-007](scientific_strategies.md#str-007) | Réalisée — non concluante |
| [EXP-023](#exp-023) | Le même ajout avec une tendance SPY de 21 séances apporte-t-il le gain recherché ? | [STR-014](scientific_strategies.md#str-014), [STR-006](scientific_strategies.md#str-006), [STR-007](scientific_strategies.md#str-007) | Réalisée — non concluante |
| [EXP-024](#exp-024) | Les retards J?1 ? J?5 am?liorent-ils le diagnostic SPY 21 face ? J?1 ? J?3 ? | [STR-006](scientific_strategies.md#str-006), [STR-014](scientific_strategies.md#str-014), [STR-007](scientific_strategies.md#str-007) | R?alis?e ? non concluante |
| [EXP-025](#exp-025) | Retirer J?3 et conserver seulement J?1/J?2 avec SPY 21 am?liore-t-il la configuration ? | [STR-006](scientific_strategies.md#str-006), [STR-014](scientific_strategies.md#str-014), [STR-007](scientific_strategies.md#str-007) | R?alis?e ? non concluante |
| [EXP-011](#exp-011) | Le classement cible/constante survit-il à l’alignement du train ? | [STR-006](scientific_strategies.md#str-006), [STR-007](scientific_strategies.md#str-007) | Réalisée — non concluante |
| [EXP-012](#exp-012) | La volatilité passée ajoute-t-elle une valeur prédictive ? | [STR-008](scientific_strategies.md#str-008), [STR-006](scientific_strategies.md#str-006) | Réalisée — non concluante |
| [EXP-013](#exp-013) | Enrichir les combinaisons et l’univers améliore-t-il la généralisation ? | [STR-006](scientific_strategies.md#str-006) | Réalisée — non concluante |
| [EXP-014](#exp-014) | Quelle part du gain WF provient de la sélection sur ses périodes test ? | [STR-011](scientific_strategies.md#str-011), [STR-005](scientific_strategies.md#str-005) | Planifiée |
| [EXP-015](#exp-015) | Les fenêtres SPY décrivent-elles convenablement les épisodes connus ? | [STR-014](scientific_strategies.md#str-014) | Réalisée — preuves incomplètes |
| [EXP-016](#exp-016) | Combien de temps l’avantage des modèles figés persiste-t-il ? | [STR-010](scientific_strategies.md#str-010), [STR-012](scientific_strategies.md#str-012), [STR-013](scientific_strategies.md#str-013) | Réalisée — preuves incomplètes |
| [EXP-017](#exp-017) | Le réentraînement prolonge-t-il la valeur des mêmes modèles ? | [STR-012](scientific_strategies.md#str-012) | Réalisée — preuves incomplètes |
| [EXP-018](#exp-018) | Les variantes de définition d’événement et de préfiltre apportent-elles un gain ? | [STR-005](scientific_strategies.md#str-005), [STR-006](scientific_strategies.md#str-006), [STR-015](scientific_strategies.md#str-015) | Réalisée — non concluante (seuils Up 1 % / 0,5 %; autres variantes non exécutées ici) |

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
| <a id="exp-014"></a>EXP-014 — Biais de sélection du préfiltre | [Protocole XGBoost](CHRONOLOGICAL_XGBOOST.md), [diagnostic de sélection](selection_diagnostic.md), code de sélection avant WF. | Constat conservé : les périodes sélectionnant les prédicteurs chevauchent les tests WF; ampleur de l'optimisme non quantifiée. Décision du 13 octobre 2026 : chevauchement intentionnel accepté pour la qualification, sans validation indépendante du processus complet par ces scores WF; appliquer la convention permanente. |
| <a id="exp-018"></a>EXP-018 — Autres variantes retenues | Seuils d'événement 1 %, 0,75 %, 0,5 % ; préfiltre enrichi de volatilité ; roadmap antérieure. | Up 1 % / 0,5 % exécutés et comparés le 12 octobre 2026 : résultat négatif face à chaque constante train. 0,75 % et les autres variantes restent sans exécution attestée ici. Ne pas confondre volatilité du modèle final et volatilité dans le préfiltre. |

### EXP-018 — Comparaison logistique Up 1 % / 0,5 % du 12 octobre 2026

Runs exécutés par l'utilisateur : `20261012T225925_80a439ee64` (1 %) et
`20261012T220820_1ca10163f4` (0,5 %), tous deux terminés. Analyse demandée ensuite
au directeur : [rapport et artefacts](../../reports/adhoc/up_threshold_comparison_20261012/README.md).
20 cibles, 3 800 combinaisons externes, sept fenêtres, 433 séances avant
qualification; logistique L2 pénalité 0,05, cutoff 2026-07-06. Le seuil Down a
également changé (1 % / 0,5 %), écart au plan Up seul : modèles Up/Down séparés,
données hors labels et disponibilité identiques, mêmes géométries/masques vérifiés.
Le bilan probabiliste Up reste comparable; aucun nouveau run correctif nécessaire.

Période principale 2025-07-25→2026-04-02, 174 séances; 259 précédentes descriptives.
Chaque cible est comparée à sa propre prévalence train exacte, à poids égaux entre
candidats d'une cible, cibles et dates. Ce bilan n'est ni un ensemble ni un sélecteur.

| Seuil Up | Log loss modèle | Log loss constante propre | Gain relatif |
|---|---:|---:|---:|
| 1 % | 0,551720 | 0,548740 | −0,543 % |
| 0,5 % | 0,658791 | 0,655674 | −0,475 % |

Les trois segments sont négatifs contre la constante pour chaque cible d'événement;
Brier également défavorable. HAC L/2L unilatéral d'amélioration, p ajustées Holm
sur les deux tests = 1; ce n'est pas un test interseuils ni un test bilatéral de
dégradation. Supports minimaux par cible : 23 positifs / 105 négatifs à 1 %,
47 / 90 à 0,5 %. Les critères de poursuite discutés ne sont pas atteints.
Les 26 600 fits Up et 26 600 Down sont convergés par run, maximum Up 10 itérations.
Chaque perte fenêtre concorde avec l'archive à 1e-12, agrégats complets concordants;
empreintes des sources vérifiées avant/après. Analyse 133,4 secondes, aucun fit,
aucun lancement métier. Calendrier rétrospectif déjà consulté et candidats
dépendants : aucune preuve indépendante Holdout/Forward, ni de rentabilité.

Avis final du directeur : terminer cette comparaison, ne pas adopter 0,5 %,
conserver 1 % comme référence provisoire sans optimum démontré; aucun nouveau
test suffisamment justifié. Le déficit relatif un peu moindre à 0,5 % ne prouve
pas une supériorité entre cibles différentes. Ces recommandations ne modifient
pas les paramètres globaux ni les priorités adoptées de l'utilisateur.

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
- **EXP-014 — Plan historique de mesure du biais :** ampleur non quantifiée;
  aucun calcul exécuté par la décision documentaire du 13 octobre 2026.
  Le chevauchement préfiltre/WF est accepté dans le processus de qualification,
  sans nécessité de le supprimer. Distinguer population figée et nouvelle
  découverte; protéger les périodes Holdout/Forward de la sélection et de la
  calibration qu'elles sont censées évaluer. Ne rouvrir cette question que
  selon la convention permanente, sans investigation automatique.
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

Suivi méthodologique du 12 octobre 2026 : la fonctionnalité « Persistance des
relations rares » implémente maintenant une sélection préalable Up sur 60 % des
séances et une évaluation sur les 40 % restants, avec référence train exacte,
HAC/Holm, support et couverture complets, et contrôle des tiers d'évaluation.
Les règles sont détaillées dans le [guide](user_guide.md); les seuils restent
non validés empiriquement. L'analyse `20261012T151456_bc632969d3`, terminée sur
le WF `20261011T133446_bab008d129`, examine 22 350 relations : zéro sélection
sur 259 séances (2024-07-12→2025-07-24), aucune persistance évaluée sur les
174 séances suivantes (2025-07-25→2026-04-02). Le résumé signale 594 couvertures
incomplètes. Zéro sélection ne démontre pas l'absence universelle de signal;
statut non concluant pour la généralisation conservé.

Suivi autorisé du 14 octobre 2026, **combinaison 3 exacte** : analyse
`20261014T202859_284f76180d` terminée en **88,2 s**, source E2E
`20261014T122421_09e82c5bbf` / WF `20261014T122426_43f5d94809`.
Définition 3 et découpage 259/174 conservés, sans entraînement.
**1 506 relations**, **1 348 admissibles**, 158 exclusions couverture/support
et inférence indisponible; 545 admissibles gagnent ≥ 1 %, 81 ont p brute ≤ 5 %,
**aucune ne passe Holm** (minimum ajusté 0,1798). Zéro sélection : persistance
sur les 174 séances suivantes **non évaluée**. Les sept candidats Holdout ne
passent pas cette sélection; HPE est non admissible (77/259 séances).
Boosters WF source avant calibration/réajustement final : aucune attribution
causale de la chute Forward ni validation indépendante des boosters finaux.
Contrôles sources, Holm et pertes des sept effectivement passés.
Avis réel du Directeur : terminer ce diagnostic, ne pas imposer automatiquement
ce filtre ni assouplir ses règles; efficacité de sélection des survivants non
démontrée. Statut non concluant pour la généralisation conservé.
[Rapport](../../reports/adhoc/persistence_exact3_20261014/README.md).

Diagnostic logistique également déjà réalisé :
[rapport](../../reports/adhoc/simple_regularized_20261011/README.md), même source
externe EXP-010 `20261011T032201_17ece38f7b`, 3 800 candidats, 26 600 fits Up.
Log loss moyenne constante 0,542410, logistique L2 0,546616, XGBoost 0,575665.
La logistique améliore XGBoost mais reste derrière la constante dans les trois
tiers temporels. Durée mesurée 225,8 secondes. Diagnostic historique négatif,
sans preuve indépendante, sans nouvelle qualification ni adoption de modèle.

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

Suivi contrôlé d'**EXP-017**, autorisé et terminé le 14 octobre 2026 :
E2E `20261014T122421_09e82c5bbf`, Holdout `20261014T122426_66888d7346`,
Forward `20261014T184054_82746da7b1`. Reconstruction de **14 boosters** jusqu'au
2 avril; reproduction des 882 probabilités Holdout et des décisions avant
évaluation sur les mêmes 441 observations Forward que les boosters réentraînés
au 6 juillet. Candidats, variables, paramètres et seuils identiques.
Log loss anciens/réentraînés/constante commune train qualification :
**0,638350 / 0,634129 / 0,582521**; AUC médiane **0,44583 / 0,48559**.
57/68 signaux, rendements bruts par signal **+0,1703 % / +0,1418 %**.
Trois contrastes préétablis (anciens meilleurs que réentraînés, meilleurs que
constante, rendement positif), HAC 4/8 sur 63 moyennes journalières puis Holm :
**p ajustées 1 / 1 / 0,9281**, aucun rétablissement démontré; pas preuve d'équivalence.
Contrôles indépendants passés sans fit supplémentaire. Calcul 2,12 s, contrôle 0,35 s.
Résultat négatif pour le rétablissement de l'avantage, non concluant sur la cause
complète de l'échec; le réentraînement ne suffit pas à l'expliquer. Ce contrôle
unique ne remplace pas la comparaison future de cadences de réentraînement.
Avis réel du Directeur : clore ce contrôle, recommander de suspendre les nouvelles
variantes quotidiennes; aucune décision utilisateur de réorientation ou activation.
[Rapport et protocole figé](../../reports/adhoc/refit_control_exact3_20261014/README.md).

<a id="exp-019"></a>

### EXP-019 — Adéquation variables–cible–horizon

**Statut : Réalisée — preuves incomplètes.** Étude autorisée le 12 octobre 2026,
terminée par lecture du code, des résultats et de sources scientifiques primaires;
aucun entraînement, acquisition, nouveau run ni calcul prédictif. Ce statut décrit
l'absence de preuve d'efficacité, pas une étude restée à exécuter.
[Rapport et références](../../reports/adhoc/variables_cible_horizon_20261012/README.md),
empreintes des sources dans `source_manifest.json` du même dossier.

Sources réutilisées : EXP-010/EXP-012, diagnostic logistique
`reports/adhoc/simple_regularized_20261011/`, sélection
`20261012T151456_bc632969d3`, snapshots des WF `20261011T032201_17ece38f7b`
et `20261011T133446_bab008d129`. Empreintes des deux snapshots vérifiées;
751 observations, 20/150 colonnes overnight et clôture→clôture, aucune colonne
volume. Aucun volume brut lié par empreinte identifié dans les dossiers examinés.

Résultat : pas de justification suffisante pour changer l'horizon ouverture→clôture
ou l'événement Up ≥ 1 %. La segmentation overnight/intraday suggère une information
distincte omise, mais la littérature ne démontre pas un avantage RStock à trois
retards. Volume et horizons courts demanderaient des données supplémentaires.

Recommandation non adoptée : une ablation appariée intraday versus intraday +
trois observations overnight strictement antérieures, mêmes 3 800 combinaisons
externes et sept fenêtres, logistique avec pénalité identique, constante train.
Support commun obligatoire; si les masques changent, refaire le contrôle et la
constante sur le même support. Critères détaillés section 6 du rapport, proposés
et non approuvés : gain relatif ≥ 1 % contre contrôle et constante, HAC/Holm 5 %,
stabilité temporelle et support explicite. Aucun optimum empirique démontré.
Estimation vérifiée techniquement : 3 points, 5–15 minutes indicatives, valeur
scientifique moyenne, confiance moyenne; référence mesurée du diagnostic antérieur
225,8 secondes. Réalisation ad hoc proposée, sans intégration applicative justifiée.
L'étude n'autorisait pas son exécution; l'autorisation distincte suivante a permis
la comparaison [EXP-020](#exp-020), désormais terminée et négative.

## Réutilisation avant toute nouvelle proposition

<a id="exp-020"></a>

### EXP-020 — Ablation exploratoire intraday + overnight

**Statut : Réalisée — non concluante.** Comparaison autorisée le 12 octobre 2026,
après EXP-019, terminée avec résultat négatif pour l'apport overnight étudié.
Artefacts : `reports/adhoc/overnight_ablation_20261012/`; source figée
`20261011T032201_17ece38f7b`, contrôle logistique historique
`reports/adhoc/simple_regularized_20261011/`. Même 20 cibles, 3 800 candidats,
sept fenêtres et 433 séances test. Population réelle : 380 candidats à un externe,
3 420 à deux externes; les colonnes passent respectivement de 3 à 6 ou de 6 à 12.
Cette précision corrige la description du protocole préparatoire sans changer
la population. Première tentative arrêtée avant tout fit; son manifeste est préservé.

Cible Up ≥ 1 %, horizon ouverture→clôture, pénalité logistique 0,05 × somme beta²
et solveur historiques conservés. Retards overnight strictement antérieurs via
`previous_observed_values`; aucune source mutable, aucun téléchargement. Contrôle
et constante refaits sur le train commun si ses masques diffèrent. La période
principale est 2025-07-25→2026-04-02 (174 séances), historique déjà consulté;
259 séances précédentes descriptives, aucun Holdout ni Forward.

Critères préspecifiés du protocole final du directeur : gain relatif log loss ≥1 %
contre contrôle et constante, HAC L/2L et Holm 5 % sur deux contrastes globaux,
deux tiers positifs et gain positif hors meilleur tiers, support/couverture complets.
Pas de recherche de gagnants individuels, ni de réglage après observation.
Manifeste, audits, checkpoints et résultats sont conservés dans le dossier ad hoc.
Estimation préalable : 3 points, 5–15 minutes, valeur et confiance moyennes.

Résultats principaux (log loss, plus bas est meilleur) : constante **0,548681**,
contrôle intraday **0,551669**, enrichi **0,553528**. L'enrichissement dégrade la
perte de 0,337 % face au contrôle et de 0,883 % face à la constante. Gains négatifs
dans les trois segments pour les deux contrastes; p unilatérales Holm = 1 pour
une amélioration. Les critères préspecifiés échouent. Brier : 0,181619, 0,182514,
0,183023 respectivement. Pas de preuve d'apport probabiliste sous cette formulation,
ni d'absence universelle d'information overnight; aucune validation indépendante.

Les 26 600 masques train perdent exactement leur première observation, commune
aux deux variantes; aucun contrôle historique réutilisé, calendrier test intact.
53 200 fits exécutés et convergés, maximum 12 itérations. Durée observée **154,07 s**,
optimisation cumulée 25,49 s; mémoire résidente observée jusqu'à 326 Mo dans la
télémétrie échantillonnée (pas une mesure continue du pic). Sources hachées avant/après
et inchangées. Vérifications indépendantes : pertes/Brier depuis les probabilités,
masques, convergence et reprise des 20 checkpoints sans fit, résultats identiques,
sorties originales inchangées; `validation.json`. Deux tentatives techniques
initiales arrêtées sont tracées par leurs manifests; elles ne constituent pas
des expériences scientifiques supplémentaires.

[Rapport scientifique final](../../reports/adhoc/overnight_ablation_20261012/README.md).
Avis du directeur : terminer EXP-020, ne pas adopter l'enrichissement et ne pas
répéter des variantes équivalentes sans élément nouveau; aucune nouvelle
intervention suffisamment justifiée. La suspension d'autres avenues est une
recommandation, pas une décision utilisateur adoptée. Le remplacement intraday
par clôture→clôture n'est pas testé ici. Les p unilatérales d'amélioration ne
constituent pas un test bilatéral de dégradation.

<a id="exp-021"></a>

### EXP-021 — Rendement continu ridge et décision long/comptant

Diagnostic unique autorisé le 13 octobre 2026 et terminé le même jour, avec
résultat négatif selon les critères de poursuite; généralisation non validée.
Source réutilisée : `20261011T032201_17ece38f7b`, snapshot vérifié, 20 cibles,
3 800 combinaisons externes, 26 600 fenêtres. Rendements continus présents dans
le snapshot; masques des variables et des deux labels, supports et frontières
train/test hérités. Aucun préfiltre ni sélection sur la qualification source.

Ridge : moyenne de l'erreur quadratique + 0,05 × somme des coefficients au carré,
intercept non pénalisé, standardisation train seulement. Une configuration sans
balayage; moyenne uniforme des 190 prévisions par cible/date, décidée avant calcul.
Référence : moyenne des rendements des trains exacts correspondants. Position Up
si prévision > 0, sinon comptant; même règle pour la référence. Notionnel fixe par
cible/date, rendement nul au comptant, poids égaux entre les 20 cibles et les dates;
publier exposition et résultat par occasion, sans les confondre avec un portefeuille
à capital limité ou une performance après coûts.

Période principale : 174 séances, 2025-07-25→2026-04-02. Les 259 séances antérieures
sont descriptives. Critères figés : gains prédictif (erreur quadratique) et économique
strictement positifs, HAC unilatéral aux retards 6/12 (plus grande p), Holm 5 % pour
les deux contrastes globaux, gains positifs dans au moins deux tiers et après retrait
du meilleur tiers, rendement brut de la stratégie positif. Ce sont des conventions
adoptées pour ce diagnostic, pas des optima validés; aucune généralisation indépendante
sur ces dates déjà consultées. Aucun Holdout ou Forward nouveau.

Mode : script ad hoc; fichiers et progression dans
`reports/adhoc/ridge_returns_20261013/`. Effort estimé 3 points, calcul estimé
5–15 minutes, valeur scientifique attendue moyenne, confiance moyenne;
référence de coût : diagnostic L2 comparable de 225,8 secondes.
Le [manifeste](../../reports/adhoc/ridge_returns_20261013/manifest.json) conserve
sources, empreintes, versions, règle économique et conditions d'arrêt avant les fits.
L'autorisation ne couvre aucune intégration applicative ou campagne supplémentaire.

Résultats principaux : MSE ridge **0,000356274594** contre moyenne train
**0,000354636790**, gain relatif **−0,461826 %**, négatif dans les trois segments.
Rendement brut par cible/date, comptant compris : **+0,0369475 %** contre
**+0,0310983 %**, soit +0,00584923 point de pourcentage; 2 102 positions contre
2 499 sur 3 480 occasions (exposition 60,40 % / 71,81 %). Gain économique non
significatif : p Holm **0,785513**; négatif après retrait du meilleur tiers.
La p Holm du gain MSE vaut 0,898679; aucun test inverse de dégradation n'est ajouté.
Les 259 séances antérieures et les 433 complètes restent également derrière
la moyenne train en MSE. Ce test ne classe pas causalement la ridge et le
pipeline L2 élargi, aux populations et règles différentes.

26 600 fits, 1 645 400 lignes source vérifiées, contrôles numériques et
agrégations/inférence indépendantes passés; empreintes source inchangées.
Durée observée : **436,8 secondes** traitement/publication, pic mémoire
**196,0 Mio**. Erreur de sérialisation JSON corrigée par publication séparée
depuis les artefacts calculés, sans nouveaux fits ni changement de protocole.
[Rapport](../../reports/adhoc/ridge_returns_20261013/README.md), manifeste,
summary et validation conservent la provenance et les résultats détaillés.
Avis du Directeur : terminer EXP-021, ne pas retenir la configuration évaluée
de STR-016; aucune intégration ou nouvelle intervention justifiée par ces
résultats, sans rejet universel de la prévision du rendement continu.

<a id="exp-022"></a>

### EXP-022 — Contexte SPY 5 séances sur le WF logistique récent

Diagnostic unique autorisé et terminé le 13 octobre 2026. Source : WF
`20261013T000704_e4e3de3a0c` du E2E `20261013T000659_510f8395e2`.
150 cibles demandées + 15 symboles de contexte; 148 cibles effectivement
représentées dans les 2 517 combinaisons figées (GEV et SNDK sans combinaison).
Préfiltre conservé, aucune qualification source utilisée comme sous-sélection.

Logistique L2 Up ≥ 1 %, paramètres source conservés. Contexte : rendement
cumulé SPY sur 5 séances et volatilité des log-rendements clôture→clôture sur
21 séances (`ddof=1`, annualisation √252), toujours jusqu'à J−1. La demande de
5 séances remplace la tendance de 63 proposée; la volatilité reste sur 21.
Source physique : rendements SPY figés du snapshot, pas le contrat de prix
ajustés du contexte descriptif. Aucun téléchargement ni source mutable substituée.

Comparateurs : constante train exacte, marché seul, externes seuls, externes +
contexte. Mêmes masques et frontières archivées; 2 433 premières fenêtres
exclues des quatre comparateurs car le démarrage du contexte laisse seulement
233 observations train, sous le minimum 252. 14 934 fenêtres conservées sur les
17 367 originales; les 2 517 combinaisons restent présentes. Les contrôles
externes sont réentraînés sur les supports communs; aucun minimum assoupli.

Période principale : 174 séances, 2025-07-25→2026-04-02, contrôle de couverture
complète avant verdict. Poids égaux entre candidats d'une cible/date, entre
cibles représentées et entre dates; moyenne des pertes, pas ensemble de prévisions.
Critères figés avant fits : gain relatif de log loss ≥ 1 % contre chacun des
trois comparateurs, HAC unilatéral conservateur 6/12, Holm 5 % sur les trois
contrastes globaux, gain positif dans deux tiers et sans meilleur tiers.
Conventions non validées empiriquement. Signaux Up au seuil 0,5 et rendements
descriptifs uniquement : aucun veto Down, calibrage ou verdict MVP nouveau.

Mode ad hoc dans `reports/adhoc/spy_context_5_20261013/`, manifeste figé avant
les fits, coefficients et checkpoints par cible. Pas d'intégration applicative,
modèle par régime, nouveau préfiltre, Holdout ou Forward. Estimation préalable :
3 points, 10–25 minutes machine pour la population présélectionnée, valeur
attendue moyenne, confiance moyenne; aucune nouvelle découverte exhaustive.

Résultats principaux, 174 séances complètes : log loss constante **0,544856867**,
marché **0,543241996**, externes **0,541075678**, enrichi **0,540038598**.
Gains relatifs enrichi contre constante / marché / externes : **+0,884318 % /
+0,589682 % / +0,191670 %**. Positifs dans les trois segments et sans le meilleur;
aucun n'atteint 1 %. P Holm respectives 0,000028234 / 0,000814027 / **0,052972547**;
le gain additionnel ne satisfait pas non plus le critère statistique de 5 %.
Sur les 196 séances antérieures et les 370 évaluables complètes, l'enrichi reste
derrière les externes seuls. Aucune contribution durable démontrée.

Signaux descriptifs Up 0,5 : externes **931**, précision **42,96 %**, rendement
par signal **+0,622 %**; enrichi **916**, **43,45 %**, **+0,637 %**. Aucune
calibration, validation économique indépendante ou inférence portefeuille.
Constante et marché sans signaux : précision/rendement par signal indisponibles.

**30 816 fits convergés**, 1 074 573 lignes source et 21 084 prédictions de contrôle
à train inchangé vérifiées; validation indépendante des pertes/HAC/Holm/supports/
signaux passée, sources inchangées. Durée **154,2 secondes**, pic mémoire
**227,7 Mio**. [Rapport](../../reports/adhoc/spy_context_5_20261013/README.md),
manifest, summary et validation conservent les détails et les limites.
Avis du Directeur : terminer EXP-022, ne pas adopter cet enrichissement ni
recommander automatiquement une variante; pas de rejet universel du contexte.

<a id="exp-023"></a>

### EXP-023 — Contexte SPY 21 séances, variante demandée d'EXP-022

Autorisé et terminé le 13 octobre 2026. Seule variable modifiée : tendance SPY
de 5 à 21 séances; volatilité conservée à 21 séances. Mêmes sources physiques,
2 517 combinaisons, 148 cibles représentées, 14 934 fenêtres admissibles et
2 433 exclusions communes que [EXP-022](#exp-022); préflight contrôlé identique.
La demande explicite de variante remplace l'absence de variante automatique
recommandée après EXP-022, sans modifier ses conclusions ou critères.

Protocole figé avant fits dans `reports/adhoc/spy_context_21_20261013/manifest.json`.
Logistique L2 Up 1 %, paramètres et frontières identiques. Réutiliser les
14 934 contrôles externes d'EXP-022 après vérification des sources, paramètres,
empreintes, dates et supports; recalculer marché seul et externes + contexte.
Constantes train exactes identiques vérifiées. Pas de préfiltre refait, autre
qualification, calibration, acquisition, changement applicatif, Holdout/Forward.

Même période principale de 174 séances, mêmes pondérations et critères : gain
relatif log loss ≥ 1 % contre constante/marché/externes, HAC conservateur 6/12,
Holm 5 % sur les trois contrastes, stabilité dans deux tiers et sans meilleur.
Rendements Up 0,5 descriptifs seulement. Le choix de 21 après consultation de
5 reste exploratoire; les résultats ne deviennent pas une preuve indépendante
de généralisation ou une validation de l'horizon optimal.

Résultats : log loss enrichi **0,539905184**, contre **0,544856867** constante,
**0,543348378** marché et **0,541075678** externes identiques à EXP-022.
Gains relatifs +0,908804 % / +0,633699 % / +0,216327 %, positifs dans les trois
segments et sans meilleur. P Holm 0,000014116 / 0,000491294 / **0,023039235**;
statistique et stabilité satisfaites, mais aucun gain n'atteint le minimum 1 %.
Sur les 196 séances antérieures et les 370 évaluables complètes, l'enrichi reste
derrière les externes, davantage qu'à 5. Aucun test de supériorité entre horizons.

Signaux descriptifs enrichis : **1 132**, précision **44,26 %**, rendement moyen
**+0,543 %**, contre 916 / 43,45 % / +0,637 % à 5 séances; populations différentes,
aucune validation de portefeuille ou Forward. **15 882 nouveaux fits convergés**
et 14 934 contrôles réutilisés; validation indépendante passée, sources et
contrôles inchangés. Durée **96,0 secondes**, pic **174,2 Mio**.
[Rapport](../../reports/adhoc/spy_context_21_20261013/README.md).
Avis du Directeur : terminer EXP-023 et ne pas adopter SPY 21; aucune autre
fenêtre automatique, sans rejet universel du contexte.

<a id="exp-024"></a>

### EXP-024 ? Retards J?1 ? J?5 avec SPY 21

Autoris? et termin? le 13 octobre 2026, ? la suite de l'estimation 3 points,
3?6 minutes machine, valeur scientifique moyenne et confiance moyenne.
Source WF `20261013T000704_e4e3de3a0c`, parent `20261013T000659_510f8395e2`;
variante d'EXP-023 : profondeur externe 3?5 seulement, tendance et volatilit?
SPY conserv?es ? 21. Population pr?filtr?e fig?e : 2 517 combinaisons,
148 cibles repr?sent?es sur 150 demand?es et 15 symboles de contexte.
Calcul ad hoc, aucune modification applicative ni pr?filtre refait.

Retards J?4/J?5 construits depuis les observations intraday r?elles strictement
ant?rieures. Fronti?res archiv?es conserv?es; supports communs aux comparateurs.
14 850 fen?tres admissibles, 2 517 exclusions dont 84 suppl?mentaires; minimum
train 252 respect?. Couverture compl?te sur les 174 s?ances principales.
Contr?le J?3 + SPY r?utilis? pour 14 598 fen?tres et r?ajust? pour 252 supports
modifi?s; aucune comparaison silencieuse avec l'ancien contr?le non align?.
M?me protocole : gains ? 1 % contre constante/march?/externes, HAC 6/12,
Holm 5 % sur ces trois contrastes et stabilit? chronologique. Comparaison
J?5/J?3 descriptive seulement. Dates d?j? consult?es, diagnostic exploratoire
WF de qualification; convention Pr?filtre/WF conserv?e, aucun Holdout/Forward.

Log loss principale J?5 + SPY **0,541521612**, contre J?3 + SPY align?
**0,539906986** : d?gradation descriptive **0,299056 %**. J?5 ?galement moins
bon sur les 196 s?ances ant?rieures et les 370 s?ances compl?tes.
Gains contre constante/march?/externes J?5 : **+0,610966 % / +0,335084 % /
+0,211192 %**, tous inf?rieurs ? 1 %; p Holm **0,010229 / 0,054319 / 0,048805**.
Le contexte reste l?g?rement utile r?cemment face aux externes J?5, ce qui
ne d?montre aucun apport des nouveaux retards. Signaux descriptifs J?3/J?5 :
1 131/1 493, rendement brut moyen +0,549191 %/+0,491192 %; populations de signaux
diff?rentes, aucune validation ?conomique ind?pendante.

30 885 nouveaux fits converg?s et 14 598 contr?les r?utilis?s. Validation
ind?pendante pass?e : empreintes, pertes, HAC/Holm/stabilit?, signaux, supports,
standardisation et 37 296 pr?dictions ?chantillonn?es reconstruites depuis
coefficients. Dur?e calcul/publication **148,8 secondes**, validation **7,5 secondes**,
pic m?moire **177,5 Mio**. Journal avec total h?rit? 14 934, r?concili? ? 14 850
sans alt?ration du script fig? ni nouveau fit.
[Rapport et artefacts](../../reports/adhoc/spy_context_j5_20261013/README.md).
Avis r?el du Directeur : terminer cette variante, ne pas adopter J?5; aucune
poursuite automatique vers J?10. Ni optimum J?3 ni rejet universel des m?moires
plus longues d?montr?s. Aucun changement de priorit? ou nouveau lancement.

<a id="exp-025"></a>

### EXP-025 ? Seulement J?1 et J?2 avec SPY 21

Autoris? et termin? le 13 octobre 2026, ? la demande explicite de l'utilisateur.
Variante d'EXP-023 : profondeur externe 3?2 seulement, contexte SPY tendance et
volatilit? 21 conserv?. Source WF `20261013T000704_e4e3de3a0c`, E2E parent
`20261013T000659_510f8395e2`; 2 517 combinaisons, 148 cibles repr?sent?es sur
150 demand?es et 15 contextuels. Population pr?filtr?e fig?e, aucun pr?filtre refait.
Protocole et empreintes fig?s avant fits dans le dossier ad hoc.

J?3 est absent des pr?dicteurs de la variante; masque historique J?1 ? J?3,
labels et contexte conserv? pour tous les comparateurs afin d'isoler son retrait.
14 934 fen?tres admissibles, 2 433 exclusions comme EXP-023, train minimum 252,
174 s?ances principales compl?tement couvertes. Les 14 934 contr?les J?3 + SPY
sont r?utilis?s apr?s v?rification des supports, param?tres et empreintes.
M?me crit?res contre constante/march?/externes J?2 : gain relatif ? 1 %, HAC 6/12,
Holm 5 % sur trois contrastes, stabilit? chronologique. Comparaison directe J?2/J?3
descriptive seulement. Dates d?j? consult?es, WF de qualification selon la
convention permanente; aucun Holdout/Forward ni changement applicatif.

Log loss principale J?2 + SPY **0,541504666**, contre J?3 + SPY **0,539905184** :
d?gradation descriptive **0,296252 %**. J?2 ?galement moins bon sur les 196 s?ances
ant?rieures et les 370 s?ances compl?tes. Gains contre constante/march?/externes :
**+0,615244 % / +0,339324 % / +0,253640 %**, tous inf?rieurs ? 1 %;
p Holm **0,000138 / 0,016299 / 0,016299**. Statistique et stabilit? internes
satisfaites, crit?re d'amplitude non atteint. Aucun test inverse de d?gradation.
Signaux descriptifs J?3/J?2 : **1 132/895**, pr?cision **44,258 %/41,229 %**,
rendement brut moyen **+0,543491 %/+0,395191 %**; populations diff?rentes,
aucun avantage ?conomique ind?pendant ?tabli.

30 816 nouveaux fits converg?s et 14 934 contr?les r?utilis?s. Validation
ind?pendante pass?e : sources/checkpoints, pertes/Brier, HAC/Holm/stabilit?,
signaux/supports et 37 296 pr?dictions ?chantillonn?es reconstruites depuis
standardisation et coefficients. Calcul/publication **116,4 secondes** et
validation **5,9 secondes**, environ **2 min 2 s**, pic m?moire **174,7 Mio**.
[Rapport et artefacts](../../reports/adhoc/spy_context_j2_20261013/README.md).
Avis r?el du Directeur : terminer cette variante et ne pas adopter J?2; crit?res
de poursuite non satisfaits. Aucun optimum J?3
ni rejet universel des autres m?moires d?montr?; aucune suite automatique.

EXP-007, EXP-009, EXP-011, EXP-012 et les grilles exploratoires EXP-001/EXP-008 ont déjà des résultats
négatifs ou non concluants. Toute répétition exige une hypothèse nouvelle,
un changement méthodologique justifié ou une preuve supplémentaire attendue.
Pour EXP-002/EXP-003/EXP-004, chercher d'abord les anciens rapports ou paires de runs ;
un manque documentaire ne signifie pas que le test n'a jamais eu lieu.
Les expériences futures retenues et leur ordre restent dans la roadmap.


<a id="exp-026"></a>

### EXP-026 ? Trois retards intraday SPY syst?matiques

**Statut : R?alis?e ? non concluante.** Analyse autoris?e et termin?e le
14 octobre 2026; r?sultat n?gatif pour l'apport ?tudi?, sans preuve ind?pendante.
WF `20261013T000704_e4e3de3a0c`, E2E `20261013T000659_510f8395e2` :
2 517 combinaisons/148 cibles avant qualification, 17 367 fen?tres conserv?es,
aucun masque train/test modifi?. Les trois rendements SPY J?1/J?2/J?3 sont
ajout?s s?par?ment, sans moyenne 5/21 ni volatilit?. Six combinaisons contiennent
d?j? SPY et restent identiques par d?duplication.

Logistique L2 source, p?nalit? 0,05 et standardisation train seulement;
contr?les constante train exacte, SPY seul trois retards, externes seuls.
P?riode principale 174 s?ances 2025-07-25?2026-04-02, crit?res inchang?s
(gain relatif ? 1 % contre les trois contr?les, HAC 6/12, Holm 5 %, stabilit?).
Poids ?gaux candidats?cibles?dates; pas d'ensemble ou de s?lection de gagnants.

Log loss principale constante/SPY/externe/enrichi : **0,545047 / 0,544575 /
0,541222 / 0,541870**. Gains relatifs enrichis **+0,583 % / +0,497 % /
?0,120 %**, p Holm **0,024362 / 0,000057534 / 0,877222**. Aucun gain n'atteint
1 %; ajout d?favorable descriptivement dans les trois tiers et sur l'ant?rieur
et le complet. Aucun test inverse de d?gradation ajout?. 949/1 123 signaux
externes/enrichis, rendements bruts moyens **+0,681 % / +0,737 %** sur des
populations de signaux diff?rentes, sans validation ?conomique ind?pendante.

17 325 fits enrichis et 1 096 march? converg?s; 17 367 contr?les et 42 fen?tres
enrichies identiques r?utilis?s. Validation ind?pendante pass?e : sources,
contr?les, pertes, HAC/Holm, causalit?, supports, coefficients et 18 648 pr?dictions
reconstruites. Calcul **120,8 s**, v?rification environ **4,9 s**, pic m?moire
237,5 Mio. P?riodes d?j? consult?es, pr?filtre/WF de qualification accept?,
aucun nouveau Holdout/Forward ni modification de l'application.
Avis r?el du Directeur : terminer EXP-026 et ne pas adopter cet ajout;
aucune nouvelle analyse justifi?e par ce r?sultat. Aucun rejet universel de SPY.
[Rapport, protocole et validation](../../reports/adhoc/spy_lags3_20261014/README.md).
