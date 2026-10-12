# Indices des modèles promus — bilan du 12 octobre 2026

**Investigation complétée ; résultat partiellement concluant.** Des réussites
individuelles persistent dans les prédictions archivées avant ouverture, mais
leur support est trop petit pour valider des relations prédictives rares. La
population complète reste à rendement brut agrégé négatif. Ces résultats ne
réfutent pas l'existence de relations rares sur un autre univers ou horizon.

## 1. Périmètre et provenance

L'analyse porte sur les **24 modèles** du registre provenant de deux décisions :

- `20260919T020736_28fa866cd2` : 16 modèles, promotion le 19 septembre 2026 ;
  les 16 identifiants sont vérifiés contre `results/promotion_results.json` ;
- `20260919T213755_20f36d8467` : 8 modèles créés le 21 septembre 2026,
  identifiés par leur provenance persistée dans le registre et les lignées.

Les cinq modèles de la capture sont des membres de ces cohortes, pas un univers
choisi parce qu'il gagne. Le registre contient 22 actifs, un Watching
(KKR/SBUX,ELV, version 2) et un inactif (INTC/GS). Ils sont tous inclus dans
l'inventaire et les analyses de couverture. Cette population est déjà sélectionnée
par les décisions historiques ; les non-promus ne disposent pas ici des mêmes
prédictions de production. On ne conclut donc pas à l'efficacité du sélecteur sur
la population complète de découverte.

Les données proviennent du registre, des métadonnées de production, des observations
Parquet, du journal CSV original et de la génération de qualité
`generation_daily_20261011T023042075028_8e6a1181`. Les probabilités, versions, dates
et horodatages sont vérifiés contre `production/history/predictions.csv`. Aucun
service applicatif n'est appelé. Aucun écran de l'application n'a été parcouru,
afin d'éviter la récupération automatique de Forward pending.

Les paramètres des cinq exemples sont différents du diagnostic de 20 cibles :
XGBoost profondeur 6, eta 0,5, 8 tours, trois retards ; leurs cohortes proviennent
d'une découverte avec préfiltre. Les rapports antérieurs ne sont pas fusionnés.

### Temporalité

884 lignes d'observation sont conservées, dont 553 `operational_backfill`,
235 `scheduled_live`, 88 `legacy_inferred_live` et 8 `legacy_unknown`.
Une grande partie des backfills concerne des dates antérieures à la promotion,
produites après coup avec des modèles entraînés plus tard : aucune preuve future.

Après décision, les observations évaluées couvrent **21 septembre → 9 octobre 2026**.
Le 12 octobre reste pending ; aucune évaluation ou prédiction supplémentaire n'est
lancée pour le compléter.

Le périmètre pré-ouverture vérifie simultanément :

- création de la prédiction après la décision et avant 09:30 New York de la séance ;
- origine `scheduled_live` ou `legacy_inferred_live` ;
- `as_of_date` et fin d'apprentissage strictement antérieurs à la séance ;
- version de la prédiction correspondant aux métadonnées de production accessibles ;
- entraînement de cette version antérieur à la création de la prédiction.

Les métadonnées de 23 modèles version 1 se terminent au 18 septembre. Leurs
prédictions postérieures sont conservées dans le périmètre principal. La version 1
historique de KKR/SBUX,ELV manque de métadonnées d'apprentissage accessibles ; ses
cinq lignes antérieures à la version 2 restent séparées, jamais remplacées par zéro.
La version 2, entraînée le 27 septembre sur données jusqu'au 25 septembre, est
analysée séparément : sept observations pré-ouverture, deux signaux et +21,28 $.

24 lignes taguées live après décision sont créées en cours de séance : elles ne
peuvent soutenir une entrée à l'ouverture connue à l'avance et sont exclues du
périmètre pré-ouverture. Ce n'est pas une preuve que leurs variables utilisent
une observation future. Exemple : AMD le 25 septembre, création 16:23 UTC,
après l'ouverture 13:30 UTC, avec as_of antérieur. Le code `_predict_model`
enregistre `created_at=utc_now()` après calcul ; la qualité recopie ce champ,
distinct de `quality_ingested_at`. Une preuve indépendante d'une inférence
antérieure à cet horodatage ne figure pas dans les fichiers utilisés.

Ces vérifications concernent les archives disponibles. Elles ne garantissent
ni l'immuabilité passée des octets des boosters, ni les millésimes fournisseur PIT.

## 2. Résultats des cohortes et interprétation de la capture

| Périmètre | Modèles | Observations | Dates | Signaux | P&L brut à 10 000 $ par signal |
|---|---:|---:|---:|---:|---:|
| Après décision, toutes origines | 24 | 357 | 15 | 107 | -1 263,83 $ |
| Après décision, tags live | 24 | 300 | 13 | 88 | -1 884,00 $ |
| Pré-ouverture, version 1 vérifiée | 23 | 264 | 12 | 77 | **-666,58 $** |

Les deux premières lignes mélangent deux versions de KKR et des couvertures
différentes ; elles sont descriptives et ne sont pas un portefeuille. Le périmètre
principal compte **10 modèles à P&L positif et 13 négatifs**. La cohorte du 19 septembre
a 43 signaux et -715,40 $ ; celle du 21 septembre a 34 signaux et +48,82 $.
Cela montre une hétérogénéité, sans permettre de retirer a posteriori les déficitaires.

**Les pourcentages verts de la capture sont des rendements strictement positifs,
pas la précision du label Up >= 1 %.** Les calculs retrouvent ses nombres de signaux
et pourcentages pour les cinq exemples :

| Modèle, pré-ouverture | Signaux | Trades à rendement positif | Précision Up >= 1 % | P&L brut |
|---|---:|---:|---:|---:|
| VLO <- TMO, MMM | 7 | 71,4 % | 42,9 % | +454,48 $ |
| TMUS <- APH, GOOGL | 3 | 66,7 % | 66,7 % | +307,74 $ |
| DDOG <- REGN | 3 | 66,7 % | 33,3 % | +526,38 $ |
| DE <- SCHW, XOM | 5 | 60,0 % | 40,0 % | **-105,15 $** |
| AMD <- AXP, KKR | 7 | 57,1 % | 42,9 % | +641,35 $ |

Sur les 77 signaux principaux, win rate rendement > 0 = **49,35 %**, mais précision
de l'événement Up >= 1 % = **24,68 %**. La fréquence de gains ne tient pas compte de
leur amplitude : DE illustre qu'une majorité de trades positifs peut laisser un P&L
négatif. Une bonne AUC sur toutes les observations ne garantit pas non plus le
rendement au seuil combiné enregistré.

Le pire modèle principal, INTC/GS aujourd'hui inactif, perd **863,13 $ sur cinq signaux**.
Les trois variantes DE sont déficitaires. Les pires journées communes sont le
9 octobre (**-974,01 $**) et le 2 octobre (**-892,96 $**), compensées partiellement
par d'autres journées. Les dépendances sont visibles : cibles répétées, prédicteurs
partagés et dates communes. Seulement **sept dates** sont disponibles dans
l'intersection stricte des 23 modèles. Aucun candidat/date n'est traité comme
une réplication indépendante.

La notion utilisée est **10 000 $ par signal**, avec jusqu'à dix signaux sur une même
journée, donc jusqu'à **100 000 $ de notionnels additionnés**. Ce n'est pas un
portefeuille disposant de 10 000 $ de capital. Les coûts, contraintes de positions
et exécution réelle ne sont pas inclus. Le fichier de transactions manuelles
contient une transaction, aucune pour ces cohortes : aucun P&L réel de courtage
de ces 24 modèles n'est validé ici.

## 3. Qualité probabiliste, référence et portée scientifique

La constante Up est reconstruite dans le snapshot de découverte, aux bornes
d'apprentissage des métadonnées de production, avec contrôle des colonnes, dates
et effectifs. Cette reconstruction est disponible pour les 23 modèles version 1.
**Le contenu exact du train de production n'est pas identifié par un hash de sa
matrice dans ces archives.** Des révisions fournisseur entre découverte et
entraînement ne peuvent être exclues : la comparaison est indicative, pas une
preuve d'appariement parfait des labels d'apprentissage.

Sur le périmètre principal, poids égaux entre les 15 cibles, puis entre modèles
du même titre et dates propres à chaque modèle : log loss Up **0,672904**, Brier
**0,222858** ; deltas face à la constante reconstruite **+0,081594** et **+0,022464**.
Dix modèles sont devant cette référence en log loss, treize derrière. Ces chiffres
ne sont pas une moyenne de probabilités constituant un nouveau modèle.

Les indications sont hétérogènes : TMUS, VLO et AMD ont des pertes plus faibles
que leur référence reconstruite sur leur petit échantillon ; DDOG, malgré un P&L
positif, a une AUC de 0,4583 et une log loss défavorable. À l'inverse, certaines
bonnes AUC locales ont un P&L négatif. Avec 7 à 12 observations par modèle et
souvent 1 à 7 signaux, ces exemples ne sont pas des preuves de relation rare.

Le contrôle économique `DailyBuyMeanReturnSameDates` compare le rendement moyen
des jours signal au rendement open-close moyen de toutes les dates évaluables
du **même modèle**, sans acheter un nouveau portefeuille. Les nombres de trades
diffèrent : aucune différence de P&L non apparié n'est attribuée causalement à la
sélection. La référence persistée de qualité est une référence Holdout économique,
pas une probabilité constante train ; son maintien ne prouve pas à lui seul un edge.

L'investigation distingue donc : résultats locaux observés, indices positifs,
preuve indépendante encore manquante et limites de provenance. Les décisions et
dates ont déjà été consultées. Aucun test de significativité, intervalle individuel
ou correction complète de toute la recherche passée n'est revendiqué.

### Mécanismes et hypothèse rare

Un état commun du marché, la volatilité, une transmission d'information entre
entreprises ou le hasard après sélection peuvent produire ces indices. Aucune
relation causale entreprise A -> B n'est établie ; la démontrer n'est pas exigée
pour une prévision valide. L'obligation permanente est la disponibilité réelle
des variables au moment de la prédiction. Les combinaisons ne sont pas toutes des
voisins économiques évidents : une explication doit être préspecifiée et testable,
pas inventée pour chaque gagnant. Les mécanismes connus dans la littérature
intraday ou clients-fournisseurs ne se transfèrent pas automatiquement à J-1 et
au rendement open-close ; voir la revue scientifique précédente.

## Conclusion scientifique et prochaine intervention

- **Statut :** investigation complétée, partiellement concluante. La provenance
  et les résultats locaux sont établis ; avantage rare reproductible non validé.
- **Décision :** conserver l'hypothèse des relations rares et ces indices, mais
  suspendre leur présentation comme preuve de succès et les nouveaux sweeps.
  Ne pas invalider cette hypothèse par le seul P&L agrégé négatif, ni retenir les
  dix gagnants après coup comme nouveau portefeuille validé.
- **Décision utilisateur du 12 octobre 2026 :** suivi prospectif figé reporté hors
  MVP, sans abandon de l'hypothèse scientifique. Sa mise en place sera réexaminée
  lorsque des preuves statistiques plus probantes de notre capacité prédictive
  seront disponibles. Aucun seuil chiffré de réexamen n'est fixé ici.
- **Plan conservé :** le plan technique et l'UI envisagée sont archivés ci-dessous
  pour une éventuelle mise en place ultérieure ; aucun développement ni lancement
  n'est autorisé par cet archivage.
- **Critères envisagés pour une validation future :** poursuivre un indice s'il apporte une qualité additionnelle
  et un rendement brut positif sur données réellement postérieures ; suspendre
  localement en cas de fuite connue ou d'échec documenté ; support insuffisant ou
  archives ambiguës = non concluant, sans changer les critères après consultation.
- **Justification du report :** privilégier les preuves prédictives utiles au MVP
  avant d'investir dans cette fonctionnalité de suivi. Les résultats de la présente
  investigation restent conservés ; aucun suivi supplémentaire n'est lancé.

## Plan d'implémentation différé — hors MVP

**Statut : plan archivé, non implémenté.** Le périmètre envisagé conserve les
24 versions examinées (23 versions 1 et KKR version 2), sans sélectionner après
coup les dix gagnants. Les composants et estimations devront être revalidés lors
de la reprise.

1. **Archive de cohorte.** Étendre `ProductionRepository` et
   `ProductionQualityRepository` pour figer boosters, métadonnées, seuils Up/Down,
   labels, variables ordonnées, calendrier, versions logicielles et SHA-256.
   Les artefacts actuels représentent environ 3,72 Mo. Leur copie établira une
   référence pour le futur, sans prouver l'immuabilité de leur passé.
2. **Observation isolée.** Étendre `DailyPredictionService` et les workflows pour
   lire les artefacts figés ; réutiliser repository, workers, leases et slots.
   Inclure INTC/GS sans activation ni publication de ses signaux opérationnels.
   Les nouvelles versions restent hors cohorte ; ne pas créer d'orchestrateur parallèle.
3. **Publication avant ouverture.** Archiver atomiquement les entrées consommées,
   leur provenance, probabilités, seuils, signaux, empreintes et heure de publication
   durable. Vérifier le calendrier de New York et la deadline à la publication,
   pas seulement `created_at`. Utiliser une clé cohorte/version/séance ; conserver
   les conflits sans écrasement. Prévoir explicitement le déclenchement quotidien,
   aucun ordonnanceur complet n'ayant été retrouvé dans l'étude technique.
4. **Évaluation séparée.** Après clôture, joindre les résultats aux prédictions
   archivées avec les labels de la version figée, plutôt que le modèle courant
   dans `RealizedResultService`. Distinguer absences, erreurs, publications tardives,
   backfills et révisions fournisseur ; réutiliser la réconciliation de qualité.
5. **Validation technique.** Couvrir paramètres et labels figés, versions et statuts,
   modèle inactif, horaires/DST/jours fériés, backfills, doublons, conflits et
   corruption d'empreintes. Tester les interruptions et la réconciliation de
   l'état persistant pour éviter les écrasements concurrents.

**Référence scientifique à résoudre avant une évaluation décisive :** la matrice
exacte de l'ancien apprentissage n'est pas archivée dans les dossiers contrôlés.
Retrouver une archive contemporaine vérifiable, ou faire décider une comparaison
limitée avec cette référence explicitement indisponible. Une reconstruction n'est
pas l'original. Un réentraînement changerait les versions et constituerait une
mission distincte. Durée, support et critères chiffrés restent non approuvés.

**UI envisagée :** une vue de cohorte dans l'interface existante, avec protocole
figé et références disponibles, état quotidien (pré-ouverture, absence, retard,
erreur), résultats par modèle et cohorte, puis détail des entrées, versions,
horodatages et origines. Séparer précision Up, trades gagnants, rendement et P&L
au notionnel indiqué. Réutiliser la grille commune `render_grid` ; consulter les
résultats persistés sans lancement ni recalcul automatique. Cette UI est proposée,
pas implémentée.

| Envergure indicative de l'implémentation future | Estimation |
|---|---|
| Développement et validation | 4 à 8 jours d'ingénierie ; confiance moyenne |
| Collecte quotidienne | 24 observations, 48 inférences directionnelles ; aucun entraînement |
| Hors estimation | Récupération du train historique et éventuelle planification système |

Les 3 points annoncés précédemment concernaient la préparation, pas le développement.
Toute reprise exigera une décision de priorité, l'autorisation de développement
puis de lancement et les critères scientifiques arrêtés en amont.
L'analyse déjà réalisée a demandé environ **1,9 seconde machine** ; aucun calcul
supplémentaire n'est effectué pour archiver ce plan.

## Artefacts et reproduction

`python -B reports/promoted_models_evidence_20261012/analyse.py` lit les archives
et écrit uniquement dans ce dossier. Les répertoires de production peuvent
nécessiter une permission de lecture ; leur inspection a été autorisée par le
mécanisme d'approbation automatique. Aucun booster n'est chargé ou réentraîné.

- `summary.json` : périmètres, sources SHA-256 et limites ;
- `population.csv` : 24 modèles, versions, statuts, dates et références ;
- `observations_classified.csv` : classification temporelle et provenance ;
- `model_metrics.csv`, `cohort_metrics.csv` : qualité et signaux distincts ;
- `daily_signal_contributions.csv` : dépendance et concentration des P&L ;
- `equal_target_probability_metrics.csv`, `common_calendar_probability_metrics.csv` : pondérations et contrôle du calendrier ;
- `persisted_quality_snapshot_comparison.csv` : références des vues persistées.

Les hashes des sources avant/après concordent. Aucun fichier historique, réglage,
run, roadmap ou code applicatif n'a été changé ; aucune application, campagne,
activation, prédiction du 12 octobre, entraînement ou téléchargement n'a été lancé.
