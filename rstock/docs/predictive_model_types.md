# Types de modèles prédictifs

Chaque job Walk-forward ou End-to-End utilise un seul `predictive_model_type`, figé dans son snapshot.
Les quatre types se comparent en exécutant des jobs distincts, puis en sélectionnant leurs résultats dans Comparer.

| Type | Entrées | Préfiltre |
| --- | --- | --- |
| `external_only` (défaut, y compris snapshots historiques sans champ) | Prédicteurs actuels | Optionnel |
| `constant_probability` | Aucune | Non applicable |
| `target_only` | Retards intrajournaliers du titre cible | Non applicable |
| `target_and_external` | Retards du titre cible et des externes sélectionnés | Optionnel, sur les externes |

Les variables calendaires explicitement autorisées restent disponibles dans le mode historique `external_only`.
Les nouveaux types n'en ajoutent pas. La profondeur des combinaisons compte les externes ; les retards de la cible
sont ajoutés systématiquement dans le mode mixte. Constant et cible seule produisent un candidat par cible.
Le `Set` reste une clé d'exécution locale au run ; les identités directionnelles versionnées incluent le type.

## Labels et entraînement

### Volatilité passée optionnelle du titre cible

`target_volatility_enabled` (défaut `False`) ajoute une seule variable à chaque modèle
XGBoost final, sans changer ses combinaisons. `target_volatility_sessions` (défaut `21`,
minimum `2`) est le nombre de rendements logarithmiques clôture ajustée/clôture ajustée.
L'écart-type `ddof=1` est annualisé par `sqrt(252)` et décalé d'une séance : pour J,
les N rendements se terminent à J−1. N+1 clôtures valides et consécutives sur le calendrier
du marché sont exigées ; aucune valeur manquante n'est remplie. La constante ignore
cette option, et le préfiltre conserve exclusivement ses variables historiques.

Le réglage se trouve dans Paramètres → Modèles et combinaisons, dans les lancements
WF/E2E et leurs dérivations. `external_only` enrichi utilise aussi la volatilité cible,
ce que la liste des colonnes et Comparer rendent explicite. Les snapshots historiques
sans ces champs restent désactivés avec N=21 inactif.

Le préfixe nécessaire au calcul est acquis séparément puis retiré de la période scientifique.
Un dérivé fige les clôtures compatibles du cache dans un nouvel artefact référencé par SHA-256,
en conservant les labels, retards et dates du parent. Il ne consulte plus le cache mutable lors
de la reprise. Les colonnes, définition, couverture et provenance accompagnent les résultats.
La liste ordonnée des variables et les réglages propres au modèle restent figés en Forward/
Production pour les types actuellement autorisés ; les autres types restent bloqués.

Les données Yahoo sont **rétrospectives, sans garantie point-in-time**. Le décalage J−1
empêche l'usage de la clôture J mais ne recrée pas les millésimes historiques d'ajustement.
Les sources sont marquées `retrospective_not_pit` : le test historique est exploratoire.
Une conclusion prospective exige un protocole arrêté avant les nouveaux labels et des
entrées/prédictions archivées à leur date d'utilisation. Aucun changement de N ne doit être
choisi a posteriori selon les performances.

Up et Down conservent leurs définitions binaires et leurs seuils actuels. Ils ne sont pas complémentaires.
Les décisions conservent la convention strictement supérieure au seuil de probabilité.

Le constant estime séparément chaque prévalence sur les labels connus de la fenêtre d'apprentissage.
Il la fige pendant le bloc de test, puis la réestime à l'origine suivante selon la géométrie expanding/rolling.
Il n'utilise ni observations futures ni lissage. Les prévalences 0 et 1 sont valides ; un apprentissage sans label
connu est refusé. Aucun XGBoost n'est chargé pour ce modèle. L'étape de calibration XGBoost E2E porte la raison
`constant_probability` et le statut `not_applicable`, sans paramètres sélectionnés fictifs.
Les calibrations de seuils utilisent les probabilités de développement ; le holdout reste séparé.

Les trois types XGBoost réutilisent les moteurs et politiques de sélection des tours existants.
Le préfiltre conserve ses paramètres dédiés et sa méthode scientifique, indépendamment du type du modèle final.

## Comparaison commune

Comparer recalcule les métriques à partir des prédictions individuelles disponibles, sur le développement
avant qualification et, lorsque les fichiers existent pour tous les runs, sur le holdout.
Pour chaque cible et direction, les dates doivent être présentes dans tous les runs et tous leurs candidats concernés.
Chaque candidat est évalué séparément sur ces mêmes dates : aucune moyenne de scores ne crée un modèle combiné.
Les scores, labels et décisions doivent être valides ; les labels doivent correspondre entre les runs.

En cas de fenêtres WF chevauchantes, une seule prédiction est conservée par candidat et date : celle de l'origine
d'apprentissage la plus récente, strictement antérieure à la prédiction. Des prédictions contradictoires à une même
origine rendent la comparaison indisponible. Les dates et origines d'apprentissage peuvent différer entre modèles ;
la comparaison décrit une population de test commune et ne démontre pas à elle seule une supériorité statistique.
Les décisions holdout sont celles obtenues avec les seuils figés, jamais des décisions reconstruites avec le seuil global.

Lorsqu’une constante est comparée à des variantes XGBoost en développement, Comparer reconstruit
un calendrier et un masque de disponibilité communs à partir des snapshots locaux et des colonnes
requises par chaque variante. Les fenêtres sont produites par le mécanisme WF existant. Leurs dates,
effectifs et labels sont vérifiés contre les fenêtres et prédictions XGBoost enregistrées, sans entraînement.
La constante est alors recalculée sur les seuls labels de cet apprentissage, strictement antérieur au test,
et figée pendant chaque bloc test. Les diagnostics exposent les dates, effectifs et empreintes des
observations/labels d’apprentissage et des dates test. Les runs historiques restent inchangés.

Si les entraînements ou frontières diffèrent, ou si les snapshots nécessaires sont absents, cette
reconstruction est refusée : les métriques historiques sur dates test communes restent disponibles,
avec un avertissement explicite indiquant que l’apprentissage n’est pas aligné. Le fonctionnement
autonome de la constante et la comparaison holdout conservent leurs probabilités enregistrées.

Une intersection vide, des labels incohérents ou des prédictions absentes sont signalés explicitement.
Aucune métrique agrégée n'est présentée comme une comparaison sur observations communes.

## Résultats, dérivation et opérations

La qualification et l'exécution holdout sont deux états distincts, indépendants de la disponibilité des données.
`no_qualified_candidates`, `not_executed` et `unavailable` ne sont jamais remplacés par une métrique zéro.
Un E2E sans candidat WF qualifié conserve les résultats WF et termine normalement avec les étapes aval non exécutées.
Les seuils de qualification ne sont pas assouplis pour favoriser le constant.

Un changement de type exige de recalculer le WF et ses consommateurs. Les points de dérivation aval verrouillent le type.
Le snapshot et les univers restent figés ; les candidats sans externes sont reconstruits, tandis que les populations
externes existantes sont réutilisables entre les deux types qui les consomment. Un snapshot privé des colonnes requises
est refusé avant lancement. Les snapshots historiques absents du nouveau champ conservent explicitement `external_only`.

Les nouveaux types sont destinés aux jobs de recherche. Leur promotion et leur activation Forward/Production
sont bloquées aux frontières de contrat. Les opérations existantes restent en `external_only`, même si le défaut
choisi dans les paramètres de recherche change. Les checkpoints restent liés au fingerprint du snapshot ; les batches
committés sont réconciliés lors de la reprise et les manifests E2E sont rechargés avant les mises à jour finales.
