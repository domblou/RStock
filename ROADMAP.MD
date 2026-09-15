Oui. Je pense que le plus simple est de voir RStock comme ayant éventuellement **deux grands modes d’utilisation** :

1. **Laboratoire** : tu expérimentes, compares et choisis une configuration.
2. **Opérationnel** : tu utilises cette configuration pour produire des signaux chaque jour.

Aujourd’hui, tu es surtout en train de bâtir la première moitié.

> État de `feature/production-workflow` : la V1 du passage laboratoire →
> production est maintenant implémentée. Elle couvre la promotion, le registre
> versionné, l'entraînement Up/Down sur l'historique admissible, l'activation,
> l'univers opérationnel dérivé, la mise à jour marché, les prédictions, le
> screening et le suivi séparé des résultats réalisés. L'automatisation horaire,
> le portefeuille et le courtage restent volontairement hors scope.

### Flux complet que je viserais

Imaginons que tu ouvres RStock un lundi matin.

#### 1. Tu définis ton univers

Aujourd’hui, tu inscris encore tes symboles dans le champ :

```text
AAPL, AMZN, BAC, CVX, ...
```

À terme, je remplacerais ou compléterais ça par notre idée récente :

```text
Univers : Actions US
Filtres :
  Prix > 5 $
  Volume moyen > X
  Marchés : NYSE / Nasdaq

Taille du bassin :
  profondeur 1 : 300
  profondeur 2 : 100
  profondeur 3 : 30

Seed : 1234
```

RStock sauvegarde **la liste exacte retenue**, de sorte qu'une expérience reste reproductible.

---

### 2. Tu fais ton premier type d'expérience : Walk-forward

C'est ton test principal.

Tu dis par exemple :

```text
Historique : 1095 jours
Permutation depth : 2
Lag depth : 3
Target : hausse intraday >= 1 %
Workers : 4
```

Puis tu lances **Walk-forward**.

RStock :

```text
récupère/cache les prix
        ↓
fabrique les features J-1/J-2/J-3
        ↓
génère les combinaisons
        ↓
entraîne et teste chaque combinaison
        ↓
mesure leur stabilité dans le temps
        ↓
qualifie certaines combinaisons
        ↓
teste les survivantes sur le holdout final
        ↓
produit les résultats
```

Ça répond essentiellement à :

> **« Y a-t-il réellement des relations prédictives qui persistent dans le temps? »**

C'est là que tu vois tes 1 575 ou 7 035 combinaisons, les AUC, les fenêtres, les combinaisons admissibles, etc.

---

### 3. Deuxième expérience : calibration XGBoost

Supposons que le walk-forward montre qu'il existe quelque chose de suffisamment intéressant.

Tu ne veux pas nécessairement continuer avec des paramètres XGBoost choisis arbitrairement.

Tu lances alors :

**Calibration XGBoost**

Ça répond à :

> **« Quelle forme de modèle XGBoost fonctionne le mieux pour ce problème? »**

Il teste des configurations comme :

```text
max_depth
eta
rounds
min_child_weight
subsample
colsample_bytree
gamma
alpha
lambda
```

Sur un sous-ensemble contrôlé de combinaisons.

À la fin, tu peux obtenir quelque chose comme ce qu'on avait trouvé :

```text
max_depth = 1
eta = 0.05
rounds = 120
subsample = 0.8
...
```

Puis cette configuration devient un **candidat à utiliser comme configuration standard**.

---

### 4. Troisième expérience : calibration des seuils

Même avec un modèle qui classe correctement les probabilités, il reste une question :

> **« À partir de quelle probabilité est-ce que je considère qu'un signal est suffisamment fort? »**

Le fameux `0.50` n'a aucune raison d'être automatiquement optimal.

Tu lances donc :

**Threshold calibration**

Elle analyse les distributions de :

```text
P(hausse >= 1 %)
P(baisse <= -1 %)
```

et cherche séparément un seuil Up et un seuil Down qui donnent suffisamment de signaux tout en conservant une certaine qualité.

Par exemple, tu pourrais éventuellement arriver à quelque chose comme :

```text
Up threshold   = 0.34
Down threshold = 0.31
```

Je donne ces valeurs uniquement comme illustration, pas comme résultat attendu.

---

# Là où les trois expériences se rejoignent

À terme, je voudrais que l'application puisse dire :

```text
EXPÉRIENCE WALK-FORWARD
        ↓
relations suffisamment robustes

CALIBRATION XGBOOST
        ↓
architecture modèle retenue

CALIBRATION SEUILS
        ↓
règles de déclenchement retenues

        ↓

CONFIGURATION CANDIDATE
```

Et là, plutôt que toi-même recopier les paramètres à la main, tu pourrais éventuellement cliquer sur quelque chose comme :

**« Promouvoir comme configuration candidate »**

Ça devient alors une version identifiable :

```text
Model configuration v1
Universe version : 2026-09-12
Features : lag 3
XGB params : ...
Up threshold : ...
Down threshold : ...
Qualification rules : ...
```

Ça, on ne l'a pas encore réellement construit comme mécanisme complet.

---

# Ensuite commence la partie opérationnelle

C'est principalement cette portion qui reste à construire.

Une fois une configuration choisie, RStock devrait pouvoir faire :

### 5. Entraînement complet

Tu demandes :

**Entraîner les modèles de production**

Le système prend tout l'historique disponible jusqu'à aujourd'hui et entraîne les combinaisons qui ont survécu à ton processus expérimental.

Pas de holdout ici : le holdout avait servi à valider la méthode.

Maintenant, tu veux exploiter toute l'information disponible pour produire les modèles actuels.

---

### 6. Mise à jour quotidienne des marchés

Chaque soir ou chaque matin :

```text
Yahoo
  ↓
mise à jour du cache
  ↓
nouvelles séances seulement
```

On a déjà une bonne partie de cette infrastructure avec `MarketDataService` et le cache Parquet.

Ce qui manque surtout est le **workflow utilisateur complet** autour de cette opération.

---

### 7. Calcul des prédictions du jour

Après la fermeture du marché, par exemple :

```text
AAPL J
NVDA J
XOM J
...
```

deviennent les features qui servent à prédire la séance suivante.

RStock charge les modèles et produit quelque chose comme :

```text
Cible   Predictors        P(Up)   P(Down)

MSFT    NVDA + XOM        0.67    0.18
GOOGL   META              0.61    0.21
AMZN    JPM + NVDA        0.39    0.42
...
```

---

### 8. Screening

Ensuite RStock applique tes règles.

Par exemple :

```text
P(Up) >= seuil Up
P(Down) faible
combinaison qualifiée
modèle actif
données récentes valides
```

Et il transforme potentiellement 10 000 prédictions en seulement :

```text
3 signaux intéressants demain
```

C'est probablement là que l'application commence réellement à devenir agréable à utiliser au quotidien.

---

### 9. Tableau de surveillance

Ton écran principal pourrait finalement devenir quelque chose comme :

```text
RStock — Surveillance

3 signaux actifs

MSFT
Probabilité hausse : 67 %
Probabilité baisse : 18 %
Modèle : NVDA + XOM
Confiance : élevée

GOOGL
Probabilité hausse : 63 %
Probabilité baisse : 19 %
...
```

avec possibilité de voir :

- pourquoi le signal existe;
- historique du modèle;
- performance passée;
- MFE/MAE;
- fréquence des +1 %;
- risque de -1 %;
- date du dernier entraînement.

---

### 10. Puis suivi réel

Après la séance suivante, RStock récupère le résultat :

```text
Signal MSFT : +1.27 %
→ succès

Signal GOOGL : -0.32 %
→ échec du seuil +1 %, mais pas forte baisse
```

Et commence à accumuler une véritable **performance hors expérience**.

C'est extrêmement important.

À terme tu pourras distinguer :

```text
Backtest
Walk-forward
Holdout
Production réelle
```

et voir si le comportement réel reste cohérent avec ce qu'on avait observé historiquement.

---

# Ce qui existe déjà versus ce qui reste

Je placerais ton projet approximativement ici :

```text
✅ Données historiques + cache
✅ Features temporelles
✅ Combinaisons exhaustives
✅ XGBoost
✅ Walk-forward
✅ Qualification/stabilité
✅ Holdout
✅ Modèle Up / Down
✅ Calibration XGBoost
✅ Calibration seuils — moteur construit
✅ Exécution asynchrone
✅ Parallélisme
✅ Progression / logs améliorés
✅ Laboratoire Streamlit

────────────────────────────

✅ Sélection V1 d'univers de titres (manuel, sauvegardé, échantillon reproductible)
Sélecteur de titres pour créer des univers
⬜ Bassins différents selon profondeur
✅ Comparaison conviviale des expériences
✅ Promotion d'une expérience/configuration
✅ Registre des modèles/configurations
✅ Entraînement complet production
✅ Prédiction quotidienne Up/Down
✅ Screening V1 des signaux
✅ Dashboard Surveillance V1
✅ Historique réel des signaux
✅ Suivi descriptif rendement / risque des signaux
Test sur les prefiltres qui aapliquée un prédcateur à la fois peut éliminer des combinaisons intéressantes. Voir fichier dans les notes.
✅ Traiment des combinaisons de walk-forward en batch
A. plusieurs petits univers cohérents → B. variation du contexte → C. horizons différents → D. targets différents → E. régimes → F. nouvelles familles de modèles.
Option encore plus robuste : séparer l’UI et le worker.
⬜ Promotion et disqualification automatique de modèle
⬜ Calculer les profits non réalisés sur signaux positifs
⬜ Positions / portefeuille éventuel
⬜ Automatisation quotidienne
```

Il y a donc encore du travail, mais **le moteur de recherche expérimental, qui était probablement la partie la plus délicate méthodologiquement, est déjà très avancé**.

Et je changerais légèrement notre ordre de travail maintenant. Avant d'ajouter encore beaucoup d'algorithmes, je ferais probablement les prochaines briques dans cet ordre :

**Universe Manager → comparaison/résultats des expériences → promotion d'une configuration → entraînement complet → prédiction quotidienne → screening → dashboard.**

À ce moment-là, RStock commence à passer d'un laboratoire de recherche à une **application qu'on peut réellement ouvrir chaque jour et utiliser**.
