
# RStock — Feuille de route scientifique

## 1. Notre hypothèse fondamentale

Nous avons progressivement changé notre façon d'aborder RStock.

Au départ, nous cherchions surtout à découvrir des modèles présentant de bonnes performances Walk-forward et Holdout.

Nos expériences nous ont amenés à une hypothèse différente :

**Les relations prédictives sur les marchés pourraient être temporaires. Notre objectif n'est donc pas nécessairement de trouver des modèles permanents, mais de développer un système capable de découvrir régulièrement des modèles performants, de mesurer leur durée de vie et de les remplacer lorsqu'ils perdent leur avantage.**

C'est cette hypothèse que notre feuille de route cherche à vérifier.

Le système devra démontrer trois capacités distinctes :

1. **Découvrir** des modèles ayant une véritable valeur prédictive.
2. **Exploiter** ces modèles pendant leur période de validité, avec des signaux rentables après coûts.
3. **Renouveler** la population lorsqu'ils deviennent moins performants.

### Périmètre actuel du MVP

L'objectif immédiat est de maintenir en Forward une qualité prédictive supérieure
à une référence simple **et un rendement intrajournalier brut positif**. Les coûts
de transaction, le spread et le slippage ne sont pas intégrés aux critères de
réussite du MVP ; la rentabilité après coûts reste un objectif ultérieur.

Nous nous concentrons sur **Up**. Down reste disponible dans le système existant,
notamment comme filtre, mais sa valeur comme stratégie directionnelle sera
explorée plus tard, après démonstration de notre capacité à prédire Up.

Un modèle individuel peut échouer après quelques semaines sans que toute l'approche soit invalide. En revanche, le système de découverte et de renouvellement doit démontrer sa valeur de façon répétée.

## 2. Où nous en sommes : stabiliser notre moteur de découverte

**État : En cours — Phase actuelle**

Avant de mesurer la durée de vie ou le renouvellement des modèles, nous avions décidé de stabiliser les paramètres et les mécanismes qui déterminent quels candidats sont découverts.

C'est précisément cette phase qui nous occupe depuis plusieurs semaines.

Nous avons notamment travaillé sur :

- La stabilité du préfiltre lorsque le cutoff se déplace d'une seule séance.
- Les critères de sélection : Worst AUC, proportion de fenêtres positives, écart-type, nombre de signaux.
- La séparation entre préfiltre et Walk-forward.
- Les modes de sélection temporelle et le consensus entre origines.
- Les paramètres XGBoost, notamment la régularisation et le nombre de tours.
- La recherche d'éventuels biais temporels dans la sélection des prédicteurs.

**Pourquoi cette phase?**

Parce que nous avions observé que déplacer un cutoff pouvait changer fortement la population de candidats.

Nous voulions déterminer si ces différences traduisaient de véritables changements du marché ou une instabilité artificielle de notre méthode de sélection.

### Ce que nous avons établi

Le Walk-forward expanding a été retenu plutôt que rolling. Plusieurs seuils ont été révisés et certaines hypothèses ont été rejetées.

Nous avons également identifié un problème scientifique important : le préfiltre peut sélectionner des prédicteurs à partir de périodes qui chevauchent ensuite les fenêtres de test du WF.

Cela peut rendre la qualification WF trop optimiste.

**Ce qui reste à résoudre :** démontrer que notre mécanisme de sélection identifie réellement des candidats qui généralisent, et non simplement des candidats favorisés par le processus de sélection.

Les expériences récentes sans préfiltre, avec différents types de modèles et avec la volatilité cible, constituent des investigations complémentaires à cette phase fondamentale.

## 3. Les prochaines phases convenues

### Phase 1 — Stabilisation scientifique des paramètres et de la sélection

**État : En cours**

Terminer les expériences ciblées sur les paramètres influents, vérifier la contribution réelle des variables et clarifier les biais de sélection. Éviter les nouveaux balayages massifs tant que leur utilité n'est pas démontrée.

**Stabilisation signifie ici : prédictabilité maintenue en Forward**, avec une
qualité supérieure à une référence simple et un rendement brut positif pour le
MVP. Les seuils quantitatifs et la durée minimale seront précisés dans le protocole
concerné ; ils ne sont pas fixés par cette clarification.

La priorité est de stabiliser le WF. Le préfiltre demeure important : l'objectif
à terme est un WF utile économiquement et un préfiltre qui ne diminue pas sa
qualité. Son compromis entre coût de calcul et qualité sera étudié en temps utile ;
aucune tolérance de dégradation n'est encore décidée.

### Phase 2 — Redécouverte Point-in-Time (PIT)

**État : Partiellement réalisée**

Exécuter des découvertes indépendantes à plusieurs dates historiques, avec le même protocole, pour déterminer si RStock est capable de découvrir régulièrement de nouvelles populations.

Mesurer le nombre de candidats, leur recouvrement, leur renouvellement et leur comportement Forward. Une faible répétition des mêmes modèles n'est pas nécessairement un échec.

La définition précise de Point-in-Time et les garanties exigées pour les données
et les univers seront arrêtées lors de cette phase. Les limites des séries
rétrospectives restent documentées ; cette clarification ne les résout pas.

### Phase 3 — Durée de vie et vieillissement des modèles

**État : Partiellement réalisée**

Mesurer les performances des modèles figés par périodes de 1–21, 22–42, 43–63, 64–84 et 85–126 séances.

Vérifier si leur pouvoir prédictif disparaît progressivement, brutalement ou demeure stable, et établir à quel moment il devient pertinent de renouveler la population.

### Phase 4 — Modèles figés contre réentraînés

**État : À valider**

Prendre les mêmes modèles, au même instant, et comparer leurs performances sur les mêmes journées selon différentes politiques de réentraînement.

L'objectif est de déterminer si le réentraînement prolonge réellement leur durée de vie ou s'il vaut mieux découvrir de nouveaux modèles.

### Phase 5 — Régimes de marché

**État : Reportée volontairement**

Mesurer si les modèles se comportent différemment dans les marchés haussiers, baissiers, calmes ou volatils.

Les diagnostics de contexte existent déjà, mais nous avions décidé de ne pas développer immédiatement des modèles propres à chaque régime.

Les régimes restent un axe à explorer ultérieurement selon cette roadmap ; leur
présence dans les diagnostics existants ne change pas la priorité actuelle du WF.

## 4. Les expériences complémentaires à conserver

Nous avions également ajouté plusieurs axes, sans nécessairement les placer avant les validations fondamentales.

| Expérience | Question scientifique |
|---|---|
| Quatre types de modèles | L'historique de la cible, les externes ou leur combinaison apportent-ils une valeur prédictive? |
| Volatilité cible sur 21 séances | La volatilité passée améliore-t-elle les prédictions? |
| Combinaisons de trois prédicteurs | Augmenter la richesse des variables améliore-t-il la qualité Forward? |
| Seuils de mouvement 1 %, 0,75 %, 0,5 % | Notre définition d'un événement Up/Down est-elle trop restrictive? |
| Préfiltre tenant compte de la volatilité | Permet-il de mieux sélectionner les prédicteurs externes? |

Ces tests sont utiles, mais ils ne doivent pas nous faire perdre de vue la validation du mécanisme complet de découverte et de renouvellement.

Les types de modèles expérimentaux et leurs règles applicatives sont laissés
en l'état pour le moment.

## 5. Ce que nous cherchons véritablement à démontrer

Notre raisonnement repose sur une succession de questions.

### 1. Pouvons-nous découvrir un avantage prédictif réel?

Au-delà de la constante et du hasard de sélection.

### 2. Cet avantage survit-il sur des données futures?

Le holdout intervient selon les règles de sélection et de qualification déjà
implémentées. Le parcours vers le Forward figé est également en place : candidats
et seuils sont figés, les boosters sont préparés à la source, puis évalués après
le cutoff sans réentraînement pendant le Forward.

Il faut mesurer le maintien de la qualité dans ce parcours existant. Une période
Forward utilisée pour ajuster les choix ne devient pas une nouvelle preuve
indépendante pour ces mêmes choix.

### 3. Combien de temps cet avantage persiste-t-il?

Vieillissement et remplacement.

### 4. Pouvons-nous renouveler cet avantage régulièrement?

Redécouverte PIT indépendante.

### 5. Le système produit-il un rendement positif?

Pour le MVP : rendement brut positif, avec suivi du risque et du drawdown.
La rentabilité après coûts sera étudiée dans une étape ultérieure.

## 6. Ma lecture de notre position actuelle

Nous avons déjà construit une bonne partie des infrastructures nécessaires : expériences dérivées, préfiltre, WF, Holdout, Forward figé, comparaison des modèles, surveillance et diagnostics temporels.

Mais **nous sommes encore principalement entre les questions 1 et 2**.

Nous ne sommes pas encore rendus à optimiser le cycle de renouvellement comme si l'avantage statistique avait été démontré.

C'est pourquoi je considère que notre prochaine priorité doit être de terminer la validation des sources de prédiction et du processus de sélection, puis reprendre les expériences PIT et de durée de vie avec un protocole suffisamment stable.

Et je préserverais une décision importante que nous avions prise : **ne pas refaire des expériences déjà non concluantes sans élément nouveau.**

Le budget signifie d'abord concentrer notre temps et notre argent sur les travaux
ayant le plus de potentiel pour le MVP. Éviter de tourner autour d'un cas avec une
succession de microtests à faible chance de succès. Avant de proposer une étude,
expliciter la décision qu'elle peut éclairer, son bénéfice attendu et son coût ;
réutiliser les résultats existants. Aucun plafond chiffré n'est fixé à ce stade.

Le véritable objectif demeure de vérifier si RStock peut devenir une **machine à découvrir et renouveler des avantages prédictifs temporaires**, plutôt qu'une machine à générer beaucoup de modèles qui réussissent leurs tests historiques.
