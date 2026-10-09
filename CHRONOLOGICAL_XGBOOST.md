# Sélection chronologique des tours XGBoost

Le mode `fixed` reste le défaut. Le WF utilise `xgb_rounds` ; les étapes aval
conservent les tours fixes de la configuration sélectionnée par la calibration.
Les anciens snapshots sans les nouveaux champs restent en mode fixe et
conservent leur nombre de tours enregistré, y compris lors d'une duplication.

## Première comparaison

Le parcours End-to-End utilise son bouton **Créer une expérience dérivée**
existant. Choisir le point **Walk-forward**, puis **Early stopping chronologique**
dans cette section. Les paramètres internes sont configurables dans le même
formulaire. Le pipeline recalcule ensuite la calibration XGBoost, les paramètres
de seuils, les seuils, le holdout et la qualification. Il n'existe aucune portée
« WF uniquement » ajoutée au pipeline. La promotion et le Forward conservent
leurs options et contrôles existants.

Dans un dérivé au WF, le préfiltre est hérité. Les candidats d'entrée et le
snapshot préparé sont vérifiés par empreintes avec le contrat de dérivation WF
partagé. Un parent dépourvu de ces artefacts ne permet pas une comparaison
chronologique reproductible et est refusé. Les étapes aval utilisent les
candidats admissibles selon les règles existantes : leurs populations peuvent
différer entre deux modes. L'IC de la comparaison appariée concerne le WF ;
les métriques aval ne démontrent pas à elles seules une supériorité statistique.

La procédure suivante reste disponible pour comparer deux jobs WF autonomes :

1. Utiliser un Walk-forward de référence terminé, explicitement configuré avec
   `xgb_rounds=80` pour cette expérience.
2. Dans son détail, choisir **Créer une expérience dérivée**.
3. Choisir `xgb_round_selection_mode=chronological`, désactiver l'évaluation du
   holdout final et conserver tous les autres paramètres scientifiques.
4. Exécuter manuellement le WF dérivé, puis sélectionner les deux runs dans
   **Comparer**, onglet Métriques.

La dérivation réutilise les candidats d'entrée et le snapshot préparé vérifiés
par empreintes. Elle ne recalcule ni le préfiltre ni la génération des
combinaisons. La qualification reste calculée, mais ne filtre pas la comparaison.
Les dates réservées au holdout restent exclues du développement même lorsque
son évaluation est désactivée.

## Protocole `chronological_v1`

| Paramètre | Défaut |
|---|---:|
| Tours internes maximum | 500 |
| Validation interne, dernières observations utilisables | 63 |
| Apprentissage interne minimum | 252 |
| Patience | 30 |
| Métrique minimisée | Log loss |
| Repli | `xgb_rounds` de la référence |

La sélection utilise uniquement le train fourni par chaque entraînement, après
les exclusions existantes. Up et Down sont sélectionnés séparément. Le modèle
final est entraîné à nouveau sur tout ce train avec `best_iteration + 1` tours.
En rolling, l'historique reste limité au train glissant configuré.

Il faut au moins **315 observations utilisables** avec les paramètres par défaut.
Un rolling de 252 observations est donc entièrement en repli. Les observations
utilisables peuvent couvrir plus de séances calendaires à cause des données
manquantes et des retards.

Historique insuffisant, validation/apprentissage interne mono-classe ou résultat
de sélection inexploitable donnent un repli explicite. Atteindre 500 tours avec
une sélection valide reste une optimisation réussie. Les annulations, erreurs
de ressources et erreurs du modèle final restent des interruptions ou échecs.

Les résultats `windows.csv` et les checkpoints conservent les dates, effectifs,
classes, nombres de tours et raisons de repli par direction. La configuration
effective conserve la version du protocole, les paramètres et la version XGBoost.
La couverture compte les fenêtres candidat-direction : nombre et pourcentage
optimisés/replis pour Up et Down, avec distribution des tours retenus.

## Comparaison et validité

La comparaison vérifie toutes les identités, l'ordre des features, les dates et
labels test. Les candidats non admissibles sont conservés. Les pertes d'anciens
runs sont recalculées en lecture depuis les prédictions, sans réécrire le run.
La log loss d'évaluation utilise un clipping commun à `1e-15`; le Brier utilise
les probabilités originales. Une AUC mono-classe reste indisponible.

La métrique principale est le delta de log loss, chronologique moins fixe,
moyenné entre fenêtres par candidat puis entre candidats, avec poids égal des
directions. Une valeur négative est favorable. Le rapport distingue toutes les
fenêtres comparables et celles réellement optimisées. Le second sous-ensemble
dispose d'un historique et de conditions de classes suffisants et ne représente
pas nécessairement toute la population.

L'IC à 95 % rééchantillonne des blocs temporels communs à tous les candidats et
directions. La taille minimale couvre le chevauchement observé, est au moins
la racine cubique arrondie du nombre d'origines et au moins deux origines.
Une sensibilité à une taille doublée est vérifiée; l'intervalle publié est
l'enveloppe conservatrice des deux estimations, sur 2 000 rééchantillonnages.
Il faut au moins 20 blocs effectifs à la taille initiale, 10 à la taille doublée,
80 % de couverture temporelle par candidat-direction et 95 % de tirages
exploitables. Une conclusion changeant avec la taille des blocs rend l'IC
indisponible. Sinon ces conditions sont des contrôles d'adéquation, pas une
garantie d'indépendance ou de couverture exacte.

Lorsque ces conditions échouent, le rapport indique **résultat exploratoire**
et ne conclut jamais à une supériorité démontrée. Ajouter des candidats corrélés
ne crée pas artificiellement de nouvelles observations temporelles indépendantes.

## Pipeline complet et compatibilité

Le protocole E2E XGBoost `e2e_xgboost_protocol_version=2` réserve l'évaluation
du holdout à son étape dédiée, aussi bien en mode fixe que chronologique.
La calibration XGBoost ne l'évalue plus. Les anciens snapshots sans ce champ
restaurent explicitement la version 1, conservée en reprise et duplication avec
la configuration du run. Une nouvelle dérivation qui recalcule la calibration
utilise la version 2. La calibration autonome conserve son évaluation explicite
du holdout. Les anciens jobs WF chronologiques restent lisibles.

En calibration XGBoost chronologique, les candidats sont dédupliqués selon les
paramètres structurels : les tours nominaux ne participent pas à la recherche.
Chaque fenêtre, candidat et direction sélectionne ses propres tours. Le repli
utilise toujours `xgb_rounds` de la configuration de référence, même si le
candidat de calibration comporte un autre nombre nominal. Les paramètres
sélectionnés conservent la politique chronologique pour les consommateurs aval.

La calibration des paramètres de seuils et celle des seuils génèrent des
probabilités hors fenêtre à l'aide de cette politique. Elles ne sélectionnent
pas les tours à partir des scores de seuils et ne constituent pas une calibration
statistique des probabilités. La recherche des seuils réutilise ses probabilités
avec une identité incluant données, politique, paramètres, échantillon, seed et
géométrie. Le holdout réentraîne sur le développement complet, avec les seuils
et paramètres structurels figés ; sa validation interne reste antérieure au
holdout. La qualification et la promotion ne réentraînent aucun modèle.

Le snapshot Forward sélectionne les tours à son cutoff, puis persiste deux
boosters figés. Les prédictions Forward ne réentraînent pas ces modèles. La
création du snapshot intervient après la qualification : elle peut utiliser
l'historique désormais connu, y compris l'ancien holdout, pour prédire après
ce cutoff. Ces boosters ne servent jamais à recalculer le score du holdout.
La production possède une politique figée indépendante des réglages globaux :
un réentraînement ou le replay `DAILY_RETRAIN` sélectionne de nouveau à chaque
origine, uniquement avec les observations disponibles auparavant. Le replay
`FROZEN_AT_START` sélectionne une seule fois. Une politique ou un audit
chronologique absent/incompatible bloque la reprise ou l'utilisation d'un
artefact ; il n'est jamais converti silencieusement en mode fixe.

Les décisions sont conservées dans les attributs du booster, les métadonnées
Forward/production et `round_selection_training.csv` pour les calibrations et
le holdout. Le fichier porte une ligne par candidat, direction, configuration
et origine, avec les tours effectifs, dates/effectifs/classes internes, raison
de repli, politique et empreinte des données d'entraînement. Les onglets et
comparaisons existants affichent la couverture Up/Down et les audits.

Toute modification de données, origine, paramètres ou politique impose une
nouvelle sélection. Aucune médiane des tours WF n'est réutilisée en aval. Seuls
un checkpoint ou une prédiction déjà calculée avec une identité identique et
un booster figé validé peuvent être réutilisés.

Pour une origine optimisable, le coût est un entraînement interne et un nouveau
modèle final par direction. Avec une limite de 500 tours, le plafond de volume
est 1 000 tours par direction contre 80 pour la référence, soit jusqu'à 12,5 fois
le volume de tours ; ce n'est pas une estimation du temps réel. Un repli conserve
un seul entraînement. Le Forward figé ne paie ce coût qu'au snapshot ; le replay
quotidien le paie à chaque origine. Le nombre de workers et threads est conservé.
Les tests utilisent des boosters simulés ; aucun benchmark ni entraînement réel
n'a été lancé pour cette intégration.

Critères proposés pour justifier une validation supplémentaire : gain relatif
de log loss d'au moins 1 %, borne supérieure IC négative, dégradation AUC au plus
0,01 et Brier au plus 0,005, gain dans deux des trois tiers temporels et delta
restant favorable après retrait du tiers le plus favorable.

Le préfiltre peut avoir utilisé les labels des périodes de développement ensuite
évaluées en WF. Le filtre de corrélation utilise aussi le développement complet.
Cette expérience compare donc deux méthodes **sur une population déjà
sélectionnée**. Elle ne valide pas indépendamment toute la chaîne de découverte.

## Périmètre et extensions futures

Le préfiltre conserve ses paramètres XGBoost dédiés et son entraînement fixe,
même si la configuration WF est chronologique. Son appel désactive explicitement
la sélection de tours dans l'évaluateur partagé. Le composant de sélection et
réentraînement est réutilisable, mais toute future activation du préfiltre devra
avoir sa propre politique persistée, versionner ses contrats et invalider les
résultats de sélection correspondants. Aucune activation n'est implémentée ici.

Holdout, calibration, E2E, snapshots Forward et réentraînements de production
réutilisent le composant commun selon les frontières décrites ci-dessus.
Aucune promotion automatique ni expérience n'est lancée par cette modification.

Les entraînements supplémentaires restent séquentiels dans chaque worker, sans
augmenter les threads ni le nombre de workers. Aucun booster interne ni courbe
complète d'apprentissage n'est persisté. La reprise conserve les lots terminés;
une interruption avant commit peut refaire le lot incomplet. Les manifests sont
rechargés/réconciliés avant leur finalisation, selon le mécanisme existant.
