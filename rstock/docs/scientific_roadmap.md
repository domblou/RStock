# RStock — Feuille de route scientifique

## Objectif et connaissances

Découvrir des relations prédictives sur des données futures inconnues, puis mesurer
leur vieillissement et leur renouvellement. MVP : **Up, qualité supérieure à une
référence simple et rendement intrajournalier brut positif maintenus en Forward**.
Coûts et stratégie Down restent ultérieurs ; leurs capacités existantes sont conservées.

Les quatre références de pilotage sont regroupées ici :
[roadmap applicative](rstock_roadmap.md), cette roadmap,
[stratégies](scientific_strategies.md) et [expériences](scientific_experiments.md).
Le registre conserve les 18 investigations et les preuves de l’audit du 11 octobre 2026.

- [EXP-007](scientific_experiments.md#exp-007) : quatre résultats Forward distincts, tous à
  rendement brut négatif sur les 63 séances étudiées ; le MVP n’est pas atteint.
- [EXP-009](scientific_experiments.md#exp-009) : fixe favorisé descriptivement en Up ; gain
  chronologique Down insuffisant face à la constante. Pas de supériorité statistique établie.
- [EXP-011](scientific_experiments.md#exp-011) et [EXP-012](scientific_experiments.md#exp-012) : cible
  seule derrière constante alignée ; aucun gain Up indépendant de volatilité N=21 démontré.
- [EXP-006](scientific_experiments.md#exp-006) : gates historiques parfois positifs sans
  identités candidates communes ; ni maintien des mêmes modèles ni garantie PIT prouvés.
- [EXP-014](scientific_experiments.md#exp-014) : risque de sélection sur les périodes test
  identifié, ampleur non quantifiée. Les autres résultats et limites restent dans le registre.

- [EXP-003](scientific_experiments.md#exp-003) : analyse J→J+1 historiquement réalisée ;
  changement du train prépondérant pour deux relations, contrôle plus stable.
  Sources primaires manquantes, conclusion limitée aux fenêtres examinées.
- [EXP-005/EXP-013](scientific_experiments.md#exp-005) : sur 150 symboles, classement WF peu
  prédictif du Holdout et XGBoost derrière constante selon les chiffres historiques
  rapportés ; détails d’agrégation non vérifiés.

- [EXP-010](scientific_experiments.md#exp-010) : bilan apparié terminé ; pertes Up
  moyennes des trois familles XGBoost derrière la constante sur les 20 cibles.
  Quelques gagnants rétrospectifs ne démontrent pas une sélection fiable.

## Stratégies actuellement retenues ou en évaluation

Retenues provisoirement : [expanding](scientific_strategies.md#str-004),
[constante alignée](scientific_strategies.md#str-007),
[Holdout → Forward figé](scientific_strategies.md#str-010) et
[qualification/calibration](scientific_strategies.md#str-015).
[Temporal Consensus](scientific_strategies.md#str-002) conserve une préférence
historique provisoire, dont les preuves comparatives restent à retrouver.

[Single Origin](scientific_strategies.md#str-001) et
[Temporal Stability](scientific_strategies.md#str-003) restent en évaluation,
avec les [sources de variables](scientific_strategies.md#str-006),
[XGBoost](scientific_strategies.md#str-009) et
[volatilité cible](scientific_strategies.md#str-008). Ni implémentation ni préférence
méthodologique ne valent validation scientifique.

Décisions historiques conservées en [EXP-001/EXP-004/EXP-005](scientific_experiments.md#exp-001) :
expanding privilégié ; Worst AUC 0,30 rejeté / 0,35 conservé ; robust signals 6,
précision Holdout 55 %, minimum de qualification 25. Ce ne sont pas des optima
prouvés ni nécessairement les defaults actuels ; les divergences sont explicitées.
Temporal Stability contre Single Origin reste historiquement non concluant.

## Priorités et investigations justifiées

1. **Stabiliser découverte et sélection.** Réutiliser [EXP-010/EXP-011](scientific_experiments.md#exp-010)
   et [EXP-012](scientific_experiments.md#exp-012) : la comparaison commune EXP-010
   est terminée et négative en moyenne. La question restante est l'identification
   des rares gagnants par une règle antérieure à l'évaluation ; pas un nouveau
   balayage. Une analyse manquante ne justifie pas de nouveaux entraînements.
   Pour l'analyse de persistance sur archives WF, la décision utilisateur du
   12 octobre retient une coupure commune en séances : 60 % de sélection par
   défaut (arrondi inférieur), puis toutes les séances restantes en évaluation ;
   un nombre de séances explicite peut remplacer la proportion. Calendrier et
   dates sont hérités des prédictions évaluables du source, sans ajustement par
   relation ni optimisation du découpage. Cette règle n'est pas un optimum prouvé.
   Les règles finalisées par le directeur puis autorisées pour l'implémentation
   le 12 octobre sont disponibles en définition 3 : gain relatif log loss ≥ 1 %,
   HAC aux retards L/2L, Holm 5 %, support de 100 séances/20 observations par classe,
   au moins 10 blocs de longueur 2L et couverture complète; évaluation avec gain
   positif dans deux tiers et après retrait du meilleur tiers. Voir le
   [guide](user_guide.md). Ces seuils ne sont pas validés empiriquement.
   Implémentation vérifiée sur fixtures synthétiques seulement; aucun nouveau
   run scientifique, résultat de persistance ou preuve Forward. Les archives
   déjà examinées ne constituent pas une confirmation indépendante.
   La gestion applicative de protocoles est reportée et retirée du code ; aucun
   résultat scientifique nouveau n'est établi par cette correction technique.
2. **Démontrer la généralisation.** Expliquer l’écart WF/Holdout/Forward ; définir
   référence, seuils, durée et période indépendante avant une nouvelle validation
   du parcours [STR-010](scientific_strategies.md#str-010), déjà disponible.
3. **Évaluer le préfiltre après stabilisation WF.** [EXP-001/EXP-002](scientific_experiments.md#exp-001)
   et [EXP-014](scientific_experiments.md#exp-014) : compromis coût/qualité et mesure du biais,
   en distinguant population figée et redécouverte.
4. **Redécouverte, durée de vie et renouvellement.** Prolonger les questions
   [EXP-006](scientific_experiments.md#exp-006), [EXP-016](scientific_experiments.md#exp-016) et
   [EXP-017](scientific_experiments.md#exp-017) après les fondamentaux, avec populations et
   calendriers vérifiés. Un replay rétrospectif ne devient pas une preuve prospective.

Variantes complémentaires [EXP-018](scientific_experiments.md#exp-018) conservées : seuils
d’événement 1 % / 0,75 % / 0,5 %, volatilité dans le préfiltre, richesse des
combinaisons. Les types expérimentaux restent en l’état.
[Régimes](scientific_strategies.md#str-014) reportés ; diagnostics existants conservés.

Les protocoles futurs de profondeurs 1/2/3, redécouverte PIT et mesure du biais
sont rattachés aux expériences existantes. Le protocole jumelé figé/réentraîné et
les cinq intervalles de vieillissement restent sous
[STR-012](scientific_strategies.md#str-012), sans annonce d’exécution contrôlée.
Les [paramètres historiques et effectifs](scientific_experiments.md#parametres-correspondance)
restent distincts ; la constante alignée est un contrôle méthodologique, pas une
stratégie de trading à faire gagner.

## Décisions ouvertes et non-répétition

Restent ouverts : preuves comparatives historiques EXP-002/EXP-003/EXP-004, critères chiffrés
de stabilisation, indépendance des périodes déjà consultées, garanties fournisseur
PIT, durée de vie et règle de renouvellement. Expanding reste retenu sans rejet
scientifique documenté de rolling ; aucune approche n’est validée pour le système complet.

Ne pas répéter à l’identique EXP-001/EXP-008, EXP-007, EXP-009, EXP-011 ou EXP-012 sans hypothèse nouvelle,
changement méthodologique justifié ou preuve supplémentaire attendue. Retrouver
les sources EXP-002/EXP-003/EXP-004 avant de proposer une répétition. Conserver les résultats
positifs, négatifs et non concluants. Chaque proposition précise décision éclairée,
bénéfice et coût ; aucun plafond chiffré ni lancement n’est autorisé par cette roadmap.
