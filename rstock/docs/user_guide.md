# Documentation RStock

# 1. Vue d’ensemble / Processus

RStock est une application d’analyse et de sélection de modèles de prévision boursière.

L’objectif n’est pas de produire une prédiction unique, mais de construire un processus reproductible permettant de :

1. définir les titres et données utilisés;
2. générer et tester plusieurs combinaisons de prédicteurs;
3. mesurer leur robustesse dans le temps;
4. ajuster les paramètres du modèle;
5. calibrer les seuils qui déclenchent les signaux;
6. valider les résultats sur une période indépendante;
7. sélectionner les modèles jugés suffisamment robustes;
8. promouvoir ces modèles;
9. suivre ensuite leurs signaux et leurs résultats.

Le processus général est :

**Univers → Walk-forward → Calibration XGBoost → Calibration des seuils → Holdout → Promotion → Signaux → Simulation**

---

# 2. Univers

## 2.1 Rôle d’un univers

Un univers définit les symboles qui peuvent être utilisés dans une expérience.

RStock distingue deux types d’univers :

### Univers principal

L’univers principal contient les titres qui peuvent devenir des **cibles de prédiction**.

Exemples :

- actions américaines;
- univers technologique;
- univers diversifié;
- leaders sectoriels.

Les symboles de l’univers principal peuvent également servir de prédicteurs.

### Univers de contexte

Un univers de contexte contient des variables utilisées comme prédicteurs mais qui ne deviennent pas elles-mêmes des cibles.

Exemples :

- SPY;
- QQQ;
- IWM;
- GLD;
- TLT;
- HYG;
- UUP;
- ETF sectoriels.

L’objectif est de fournir au modèle de l’information sur l’environnement de marché.

## 2.2 Population d’une expérience

Une expérience peut utiliser :

- l’univers principal complet;
- un sous-échantillon Top N;
- un ou plusieurs univers de contexte;
- tout l’univers de contexte ou un Top N.

Les symboles sont figés au moment du lancement de l’expérience afin qu’un run puisse être reproduit exactement plus tard.

---

# 3. Walk-forward

## 3.1 Pourquoi utiliser un walk-forward?

Un modèle financier ne doit pas être évalué uniquement sur les données qui ont servi à son entraînement.

Le walk-forward reproduit une utilisation plus réaliste :

1. le modèle apprend sur une période historique;
2. il est testé sur une période suivante qu’il n’a pas vue;
3. la fenêtre avance dans le temps;
4. le processus est répété plusieurs fois.

On obtient ainsi plusieurs fenêtres de validation temporelle.

Un modèle intéressant doit fonctionner dans plusieurs périodes différentes, et non seulement dans une période particulièrement favorable.

---

## 3.2 Paramètres principaux

### Historique

Détermine la profondeur historique chargée pour l’expérience.

Exemple :

`1095 jours`

Ce paramètre ne correspond pas directement au nombre de séances de marché.

---

### Train minimal

Nombre minimal d’observations utilisées avant qu’un premier test puisse être effectué.

Exemple :

`252`

Cela correspond approximativement à une année de séances de marché.

---

### Taille test

Nombre d’observations utilisées pour chaque fenêtre de test.

Exemple :

`63`

Cela correspond approximativement à un trimestre.

---

### Step

Nombre de séances utilisées pour déplacer le walk-forward entre deux fenêtres.

Exemple :

`63`

Avec un test de 63 et un step de 63, les fenêtres de test ne se chevauchent pas.

Un step inférieur à la taille du test crée des fenêtres qui se chevauchent.

---

### Holdout final

Période finale complètement séparée du développement du modèle.

Exemple :

`63`

Le holdout ne doit pas être utilisé pour choisir les paramètres du modèle.

Il sert uniquement à vérifier si les décisions prises sur les données de développement généralisent sur une période jamais utilisée auparavant.

---

### Décalage de fin

Permet de rejouer exactement la même méthodologie dans le passé.

Exemple :

`63`

RStock recule alors la date de fin de 63 séances de marché et reconstruit la même profondeur historique relativement à cette nouvelle date.

Ce paramètre est particulièrement utile pour tester la robustesse temporelle de la méthodologie.

`0` signifie : utiliser les données les plus récentes.

---

# 4. Génération et qualification

RStock teste plusieurs ensembles de prédicteurs pour chaque cible.

Exemple :

**Cible : MU**

Combinaisons possibles :

- MU;
- MU + CAT;
- MU + QQQ;
- MU + CAT + XLI;
- etc.

La profondeur maximale détermine le nombre de variables pouvant entrer dans une combinaison.

Une profondeur plus grande :

- augmente le nombre de possibilités;
- augmente fortement le temps de calcul;
- augmente également le risque de trouver des relations accidentelles.

La profondeur doit donc rester raisonnable.

---

## 5. Préfiltrage

Avant d'exécuter les analyses les plus coûteuses, RStock peut éliminer certaines combinaisons peu prometteuses.

Le préfiltrage utilise notamment :

- la performance AUC sur plusieurs fenêtres;
- la stabilité;
- les corrélations;
- différents critères minimums.

L’objectif n’est pas de décider qu’un modèle est bon.

L’objectif est de réduire le nombre de combinaisons qui doivent passer dans les étapes plus coûteuses.

---

## 6. Qualification walk-forward

Après le walk-forward, chaque combinaison est évaluée sur plusieurs fenêtres temporelles.

Des critères peuvent notamment porter sur :

- nombre minimum de fenêtres;
- AUC médiane;
- proportion de fenêtres supérieures au hasard;
- pire fenêtre;
- stabilité de l’AUC;
- nombre d’observations positives.

Une combinaison qui passe ces critères devient une combinaison **qualifiée**.

La qualification walk-forward indique qu’un modèle semble relativement stable pendant la période de développement.

Elle ne garantit pas encore que le modèle fonctionnera sur le holdout final.

---

# 7. Calibration XGBoost

## 7.1 Rôle

La calibration XGBoost sert à rechercher de meilleurs hyperparamètres pour le modèle.

Exemples :

- profondeur des arbres;
- learning rate;
- nombre de rounds;
- min child weight;
- subsample;
- colsample;
- gamma;
- alpha;
- lambda.

L’objectif est d’améliorer le compromis entre :

- capacité prédictive;
- stabilité;
- risque de surapprentissage.

---

## 7.2 Principe important

La calibration doit être faite uniquement sur les données de développement.

Le holdout final ne doit jamais servir à choisir les hyperparamètres.

Sinon le holdout cesse d’être une validation indépendante.

---

# 8. Calibration des seuils

## 8.1 Pourquoi calibrer un seuil?

Le modèle produit une probabilité.

Exemple :

`P(Up) = 0,37`

Cela ne signifie pas automatiquement qu’un signal doit être déclenché.

La calibration des seuils cherche le niveau de probabilité à partir duquel le signal devient suffisamment intéressant.

---

## 8.2 Grille de quantiles

RStock génère différents seuils candidats à partir de quantiles des probabilités produites pendant les périodes de calibration.

Exemple :

`0.50, 0.60, 0.70, 0.75, 0.80, 0.85, 0.90, 0.925, 0.95, 0.975, 0.99`

Ces valeurs sont des quantiles, et non directement des seuils de probabilité.

---

## 8.3 Signaux minimums par fenêtre

Un seuil doit générer suffisamment de signaux dans les fenêtres de calibration.

Si un seuil est tellement élevé qu’il produit seulement un ou deux signaux, les statistiques deviennent trop fragiles.

Ce paramètre contrôle le nombre minimal de signaux requis dans une fenêtre.

---

## 8.4 Fraction minimale de fenêtres

Un seuil doit fonctionner sur une proportion minimale des fenêtres de calibration.

Exemple :

`0,75`

signifie qu’environ 75 % des fenêtres doivent satisfaire les critères requis.

Ce mécanisme évite de choisir un seuil qui fonctionne uniquement pendant une période particulière.

---

## 8.5 Minimum total pour un seuil robuste

Un minimum global de signaux est également utilisé afin d’éviter de considérer comme robuste un seuil reposant sur trop peu d’observations.

Ce paramètre est distinct du minimum par fenêtre.

---

## 8.6 Tolérance de précision — sélection Up

Lorsque plusieurs seuils ont une précision très proche, il n’est pas toujours pertinent de choisir automatiquement celui qui obtient quelques dixièmes de point de précision supplémentaires.

RStock peut considérer comme équivalents les seuils situés dans une bande de tolérance autour de la meilleure précision.

Exemple :

`0,01`

signifie une tolérance absolue de **1 point de pourcentage de précision**.

À l’intérieur de cette bande, les seuils sont départagés selon des critères économiques et de robustesse, notamment :

- rendement directionnel;
- fréquence de mouvement opposé;
- stabilité de la précision;
- stabilité du rendement;
- nombre de signaux;
- F1.

La précision demeure donc le filtre principal, mais une différence minuscule n’écrase plus automatiquement les autres critères.

---

## 9. Diagnostic du choix du seuil

RStock permet d’afficher les seuils candidats évalués pendant la calibration.

Pour chaque seuil, on peut notamment consulter :

- admissibilité;
- nombre de signaux;
- fraction de fenêtres admissibles;
- précision;
- stabilité de la précision;
- rendement directionnel;
- stabilité du rendement;
- fréquence de mouvement opposé;
- F1;
- raison du rejet ou de la sélection.

Cette vue sert à comprendre **pourquoi** le moteur a choisi un seuil particulier.

Elle ne doit pas être utilisée pour modifier manuellement le seuil après avoir observé le holdout.

---

# 10. Holdout et sensibilité

Le holdout représente la dernière période indépendante.

Une fois :

- la combinaison choisie;
- les hyperparamètres fixés;
- le seuil calibré;

le modèle est évalué sur le holdout.

Les principales métriques incluent :

### Signaux holdout

Nombre de signaux générés.

Un résultat basé sur 5 signaux est beaucoup moins fiable qu’un résultat basé sur 30 ou 40 signaux.

### Précision

Proportion des signaux ayant réellement produit le mouvement recherché.

### Recall

Proportion des mouvements réels qui ont été détectés.

### F1

Compromis entre précision et recall.

### AUC

Mesure la capacité générale du modèle à classer les observations.

Une AUC de 0,50 correspond approximativement au hasard.

### Rendement directionnel moyen

Rendement moyen obtenu dans la direction prédite.

### Rendement médian

Permet de vérifier si le rendement moyen est dominé par quelques observations extrêmes.

### MFE

Maximum Favorable Excursion.

Mesure le meilleur mouvement favorable observé pendant la période du signal.

### MAE

Maximum Adverse Excursion.

Mesure le pire mouvement défavorable observé.

### Fréquence de mouvement opposé

Proportion des signaux où le marché s’est déplacé significativement dans la direction contraire.

---

## 11. Analyse de sensibilité au seuil

L’analyse de sensibilité permet de voir ce qui se serait produit sur le holdout avec d’autres seuils.

Elle est strictement diagnostique.

Elle permet notamment d’identifier :

- un seuil calibré situé dans une zone stable;
- un seuil très sensible à de petites variations;
- un modèle dont les performances augmentent seulement avec des seuils extrêmement élevés;
- un modèle pour lequel le seuil calibré semble bien généralisé.

## Règle fondamentale

**Ne jamais remplacer automatiquement le seuil calibré par le meilleur seuil observé sur le holdout.**

Faire cela reviendrait à optimiser le modèle sur le holdout.

Le holdout ne serait alors plus indépendant.

---

## 12. Synthèse de sensibilité

La synthèse permet de comparer plusieurs modèles simultanément.

Elle peut notamment indiquer :

- seuil calibré;
- meilleur seuil robuste observé sur le holdout;
- différence entre les deux;
- différence de précision;
- différence de rendement;
- nombre de signaux;
- diagnostic.

Exemples de diagnostics :

- `near_optimal`
- `higher_threshold_better`
- `lower_threshold_better`
- `unstable`

Cette information sert à améliorer la **méthodologie générale de calibration**, pas à ajuster individuellement chaque modèle après coup.

---

# 13. Promotion des modèles

Un modèle peut être promu lorsqu’il a franchi les différentes étapes de validation.

La promotion signifie que le modèle devient disponible pour produire des signaux opérationnels.

La promotion doit tenir compte de plusieurs dimensions :

- performance walk-forward;
- stabilité;
- holdout;
- nombre de signaux;
- rendement;
- comportement économique;
- cohérence du seuil;
- qualité générale du modèle.

Une métrique unique ne devrait pas décider à elle seule de la promotion.

---

# 14. Signaux

Les modèles promus peuvent produire des signaux Up ou Down selon leur configuration.

Le signal est généré à partir :

- des données disponibles au moment du calcul;
- du modèle entraîné;
- du seuil calibré.

Un signal ne signifie pas qu’un mouvement est certain.

Il signifie que les conditions observées correspondent suffisamment aux situations historiques jugées intéressantes par le modèle.

---

# 15. Simulation

La page Simulation permet d’évaluer les conséquences économiques des signaux.

## Historique

Le mode Historique rejoue les modèles sur des données passées.

Il permet d’obtenir une vue plus longue de leur comportement.

Il s’agit toutefois d’une simulation rétrospective.

Elle peut contenir certains biais de sélection puisque les modèles analysés aujourd’hui ont eux-mêmes été sélectionnés à partir de données historiques.

## Résultats réalisés

Le mode Résultats réalisés utilise les signaux réellement produits et persistés par l’application.

Cette partie devient progressivement la source la plus importante pour mesurer la performance opérationnelle réelle.

## Simulation combinée

Une évolution naturelle consiste à combiner :

**Historique → date de mise en production → Résultats réalisés**

L’historique permet d’obtenir davantage de profondeur, tandis que les résultats réalisés représentent la validation opérationnelle la plus forte.

Les deux composantes doivent cependant demeurer clairement identifiées.

---

# 16. Historique des expériences

La page Historique permet de retrouver les runs exécutés.

Chaque run conserve notamment :

- configuration;
- univers;
- symboles;
- paramètres;
- provenance;
- résultats;
- date d’exécution;
- offset temporel;
- date de fin effective lorsque disponible.

Les expériences peuvent être dupliquées afin de comparer différentes méthodologies tout en conservant la même population de symboles.

Cette approche est préférable à la reconstruction manuelle d’une expérience.

---

# 17. Fine-tuning des paramètres

## 17.1 Principe général

Ne jamais modifier plusieurs dimensions importantes simultanément.

Une bonne démarche expérimentale consiste à :

1. conserver un run de référence;
2. modifier un seul groupe de paramètres;
3. relancer l’expérience;
4. comparer les résultats;
5. conserver ou rejeter la modification;
6. documenter la décision.

Sinon, il devient impossible de savoir quelle modification a réellement produit l’amélioration.

---

## 18. Fine-tuning du walk-forward

Les paramètres principaux sont :

- historique;
- train minimal;
- taille test;
- step;
- holdout final;
- décalage de fin.

## Train minimal

Un train plus grand :

- fournit davantage de données;
- peut améliorer la stabilité;
- mais peut diluer les régimes récents.

Un train plus petit :

- s’adapte davantage aux changements récents;
- mais augmente la variance.

## Taille test

Une fenêtre plus grande :

- donne davantage d’observations;
- produit des métriques plus stables.

Une fenêtre plus petite :

- fournit davantage de points de contrôle temporels;
- mais rend chaque mesure plus bruitée.

## Step

Un step égal à la taille test produit des fenêtres indépendantes.

Un step plus petit permet davantage de fenêtres mais introduit du chevauchement.

## Holdout

Le holdout doit être suffisamment grand pour être informatif, mais assez petit pour conserver suffisamment de données de développement.

## Décalage de fin

Utiliser différents offsets permet de vérifier si la méthodologie fonctionne aussi sur des périodes antérieures.

Exemples :

- `0`
- `63`
- `126`
- `189`

Une méthodologie robuste devrait produire des résultats raisonnablement cohérents sur plusieurs périodes.

---

## 19. Fine-tuning XGBoost

Le fine-tuning XGBoost doit rester limité et contrôlé.

## Profondeur des arbres

Une profondeur élevée augmente la capacité du modèle mais également le risque de surapprentissage.

## Learning rate

Un learning rate élevé apprend rapidement mais peut être moins stable.

Un learning rate plus faible nécessite généralement davantage de rounds.

## Nombre de rounds

Trop peu de rounds peuvent sous-apprendre.

Trop de rounds peuvent augmenter le surapprentissage.

## Subsample / Colsample

Ces paramètres peuvent améliorer la régularisation en empêchant chaque arbre d’utiliser toutes les observations ou toutes les variables.

## Alpha / Lambda

Ils contrôlent la régularisation.

Ils peuvent être utiles lorsque le modèle devient trop complexe.

## Méthode recommandée

Ne pas rechercher des centaines de combinaisons simplement pour maximiser l’AUC.

Privilégier :

- stabilité entre fenêtres;
- performance holdout;
- simplicité;
- reproductibilité.

---

## 20. Fine-tuning de la calibration des seuils

Le fine-tuning du seuil doit chercher un compromis entre :

- précision;
- nombre de signaux;
- rendement;
- stabilité;
- mouvements opposés.

## Minimum de signaux par fenêtre

Trop élevé :

- exclut les seuils sélectifs;
- peut pousser automatiquement la calibration vers des seuils plus faibles.

Trop faible :

- accepte des résultats statistiquement fragiles.

## Fraction minimale de fenêtres

Une valeur élevée exige qu’un seuil fonctionne dans presque toutes les périodes.

Cela améliore la robustesse mais peut exclure des stratégies plus sélectives.

Une valeur plus faible accepte davantage de variation temporelle.

## Minimum total robuste

Protège contre les conclusions reposant sur trop peu de signaux.

## Tolérance de précision

Permet d’éviter qu’un avantage minuscule de précision élimine un seuil économiquement supérieur.

Une tolérance trop grande peut toutefois diminuer l’importance de la précision.

Il faut donc la modifier avec prudence.

---

## 21. Comment évaluer une modification de paramètres

Lorsqu’un paramètre est modifié, ne regarder pas uniquement le meilleur modèle.

Comparer plutôt :

- nombre total de modèles qualifiés;
- distribution des AUC;
- stabilité;
- nombre de signaux;
- précision holdout;
- rendement directionnel;
- fréquence de mouvement opposé;
- nombre de modèles réellement promotables.

Une bonne modification méthodologique devrait améliorer **la population globale**, pas seulement quelques cas particuliers.

---

# 22. Bonnes pratiques / prévention du surapprentissage

Il est possible de surajuster non seulement un modèle, mais également tout le processus de recherche.

Exemple :

1. observer le holdout;
2. modifier les paramètres;
3. relancer;
4. observer le même holdout;
5. modifier encore les paramètres.

Après suffisamment d’itérations, le processus finit lui-même par être optimisé pour cette période.

Pour limiter ce risque :

- utiliser plusieurs périodes avec le décalage de fin;
- comparer plusieurs holdouts historiques;
- conserver des règles générales;
- éviter d’ajuster manuellement chaque modèle;
- privilégier les résultats reproduits sur plusieurs périodes.

---

## 23. Processus recommandé avant promotion

Un modèle candidat devrait idéalement avoir :

1. passé les critères walk-forward;
2. montré une stabilité raisonnable entre fenêtres;
3. utilisé des paramètres XGBoost validés sans sur-optimisation;
4. obtenu un seuil calibré uniquement sur les données de développement;
5. produit suffisamment de signaux sur le holdout;
6. obtenu une AUC et une précision raisonnables;
7. produit un rendement directionnel positif;
8. évité une fréquence excessive de mouvements opposés;
9. montré une sensibilité raisonnable autour de son seuil;
10. idéalement reproduit son comportement sur plusieurs périodes historiques.

La promotion demeure une décision contrôlée, et non une conséquence automatique d’une métrique unique.

---

## 24. Lecture recommandée des résultats

Un modèle présentant :

- AUC élevée;
- précision élevée;
- rendement positif;
- beaucoup de signaux;
- faible fréquence opposée;
- stabilité élevée;

est évidemment intéressant.

Dans la pratique, ces métriques entrent souvent en conflit.

Par exemple :

- augmenter le seuil améliore souvent la précision mais réduit le nombre de signaux;
- un rendement moyen élevé peut provenir de quelques observations extrêmes;
- une AUC élevée ne garantit pas une bonne précision au seuil choisi.

Il faut donc rechercher un **ensemble cohérent de métriques**, plutôt qu’un chiffre parfait.

---

## 25. Principe directeur

RStock doit favoriser :

**robustesse > optimisation maximale**

L’objectif n’est pas de trouver le modèle qui aurait été parfait dans le passé.

L’objectif est de trouver des modèles dont le comportement historique est suffisamment cohérent pour justifier leur utilisation sur des données futures.
