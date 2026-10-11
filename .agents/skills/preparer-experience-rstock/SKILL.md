---
name: preparer-experience-rstock
description: Préparer un protocole, une comparaison ou une campagne RStock avec sources, paramètres effectifs, faisabilité et coût pour le MVP. Ne couvre pas les corrections UI ordinaires et ne déclenche pas un lancement.
---

# Préparer une expérience RStock

Skill partagée : le directeur définit la question et la décision scientifique ;
l'ingénieur vérifie données, contrats, faisabilité et coût. Lire les
[instructions communes](../../../AGENTS.md) et la
[roadmap scientifique](../../../SCIENTIFIC_ROADMAP.md). Les chemins ci-dessous
sont relatifs à cette skill ; travailler depuis la racine du dépôt.

## Préparation

- Rattacher la question à l'axe actuel de la roadmap. Expliquer quelle décision
  l'expérience peut changer et pourquoi son potentiel justifie l'effort MVP.
  Réutiliser les résultats existants ; une nouvelle série de microtests peu
  prometteurs n'est pas une réponse par défaut.
- Identifier les sources : run IDs, rapports, commit et modifications pertinentes,
  snapshots, manifests et empreintes disponibles. Une configuration préparée
  n'est pas un résultat. Vérifier les artefacts nécessaires avant de promettre
  une comparaison ou une reprise.
- Décrire la référence simple, la population, les paramètres effectifs et les
  différences étudiées, en distinguant defaults, réglages globaux et snapshot.
  Figer les données, univers, cutoff demandé/résolu et sources supplémentaires
  nécessaires ; signaler toute acquisition ou préparation encore requise.
- Vérifier calendriers et disponibilité des variables, apprentissage, sélection,
  calibration, tests, holdout et Forward. Identifier les périodes déjà consultées,
  biais du préfiltre, sélection des candidats et essais multiples. Documenter les
  limites PIT sans imposer un nouveau protocole, dont la définition est reportée.
- Choisir le parcours existant dans RStock : lancement, dérivation à l'étape
  appropriée, comparaison, holdout ou Forward figé. Préserver les contrats de
  population héritée et d'invalidation aval. Les études à population figée et les
  redécouvertes complètes répondent à des questions différentes.
- Estimer la charge avec les informations disponibles : candidats, fenêtres,
  entraînements, mémoire, stockage et durée indicative. Donner l'incertitude,
  sans benchmark ou entraînement supplémentaire implicite. Le budget concerne
  aussi le coût d'opportunité en temps et argent ; aucun plafond chiffré n'est fixé.

## Livrable et autorisation

Présenter un protocole court et reviewable : hypothèse, décision attendue,
référence/population, sources et paramètres, séparation temporelle, métriques,
critères de poursuite/arrêt proposés, coût, parcours/commandes et livrables
reproductibles (config, manifests, résultats et synthèse référencés).

Le MVP vise Up, qualité supérieure à une référence simple et rendement brut
positif maintenus en Forward. Ne pas imposer les coûts de transaction ni inventer
un horizon ou seuil déjà approuvé. Préfiltre, Down et régimes suivent la roadmap ;
les types expérimentaux restent en l'état.

Préparer n'autorise pas le lancement. Appliquer les autorisations existantes dans
leur périmètre ; demander uniquement ce qui manque avant une opération dépendante.
Ne pas soumettre un job, préparer des boosters, lancer un rejeu, télécharger des
données ou modifier un protocole sous couvert d'une simple lecture. Une étude
technique peut être déléguée si elle apporte une valeur réelle, sans multiplier
les agents ni partager l'historique complet.

## Références selon le besoin

- Parcours et paramètres : [guide utilisateur](../../../rstock/docs/user_guide.md).
- Batching/reprise : [contrats WF/E2E](../../../rstock/docs/WF_BATCHES.md).
- Variantes : [types de modèles](../../../rstock/docs/predictive_model_types.md),
  [sélection chronologique](../../../rstock/docs/CHRONOLOGICAL_XGBOOST.md).
- Cartographie technique : [recensement](../../../reports/application_inventory_20261011/RECENSEMENT.md).
  Vérifier les contrats actuels dans le code ; ce rapport est daté.
