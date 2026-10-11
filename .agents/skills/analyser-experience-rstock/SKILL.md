---
name: analyser-experience-rstock
description: Interpréter les résultats existants d'une expérience ou comparaison RStock dans l'application, notamment WF, holdout, Forward et vieillissement, avec vérification de provenance et décision scientifique. Ne lance pas de nouvelle expérience.
---

# Analyser les résultats d'une expérience RStock

Skill partagée : le directeur interprète et replace la décision dans la roadmap ;
l'ingénieur vérifie sources et calculs lorsque nécessaire. Lire les
[instructions communes](../../../AGENTS.md) et la
[roadmap scientifique](../../../SCIENTIFIC_ROADMAP.md).

## Lire les résultats existants

Partir de la question utilisateur et des vues pertinentes de RStock : Historique,
Comparer, détails WF/E2E, holdout, Forward, diagnostic de sélection ou qualité
modèle. Privilégier leur interprétation, sans transformer une demande de lecture
en campagne de tests ou en développement de scripts.

Si l'interface est inaccessible, utiliser ses artefacts persistés et préciser la
limite. Ne pas prétendre avoir parcouru un écran non observé. Avant navigation,
tenir compte de la récupération automatique des Forward pending : une consultation
n'autorise pas une relance. Ne pas démarrer l'application sur des données réelles
comme contournement sans vérifier ces effets de bord.

## Vérifications et interprétation

- Identifier runs, sources physiques, snapshots, paramètres effectifs, période,
  versions et empreintes disponibles. Vérifier l'état terminé et la disponibilité
  des artefacts ; distinguer non exécuté, non applicable, indisponible et zéro.
- Vérifier la comparabilité : identités, labels, dates test, frontières et
  observations d'apprentissage lorsque la question les exige. Employer la
  constante train alignée lorsque possible, ou signaler l'alignement manquant.
  Ne pas assimiler un chevauchement de dates test à un entraînement identique.
- Distinguer population complète, qualifiée et finale, Up/Down, règles de signal
  WF et seuils combinés figés, holdout de qualification et holdout comparable.
  Préfiltre, modèles et identités directionnelles ont des unités différentes.
- Examiner qualité prédictive vs référence, rendement brut, nombre de signaux,
  couverture, exclusions et évolution Forward selon la question. Donner les
  pondérations et ne pas additionner intervalles et cumuls. Un notionnel par
  modèle/signal n'est pas un portefeuille à capital limité ; backfill et rejeu
  historique ne sont pas des observations live indépendantes.
- Signaler sélection sur périodes observées, essais multiples, dépendance entre
  dates/cibles/modèles et faibles effectifs. Utiliser les contrôles et intervalles
  existants adaptés, sans inventer de p-value ou exiger un nouveau calcul par
  défaut. Une période ayant servi à ajuster les choix ne valide pas indépendamment
  ces mêmes choix. Le parcours holdout → Forward figé existe déjà : vérifier son
  contrat pour les runs concernés plutôt que proposer de le reconstruire.
- Distinguer observation descriptive, hypothèse et preuve manquante. Évaluer le
  maintien de la prédictabilité en Forward et le rendement brut positif pour Up ;
  la rentabilité après coûts et la stratégie Down sont des étapes ultérieures.
  Les diagnostics de régimes existants ne déclenchent pas une nouvelle exploration.

Réutiliser exports, rapports et scripts existants pour une vérification manquante
seulement si elle change la conclusion. Vérifier leurs effets de bord, estimer le
coût et rester dans l'autorisation donnée. Ne pas réentraîner, modifier les seuils,
réécrire les runs sources, promouvoir ou activer pour compléter une analyse.

## Livrable

Présenter une synthèse adaptée : sources/périodes et comparabilité, résultats
observés avec effectifs, interprétation, limites et décision proposée dans la
roadmap (poursuivre, arrêter, réutiliser ou attendre une preuve précise). Si une
vérification technique est déléguée, demander un retour court et borné.

Appliquer l'obligation de mise à jour documentaire d'AGENTS.md : référencer les
expériences, résultats observés et conclusions réellement établies dans la roadmap
et le rapport existant approprié, selon leur pertinence, sans autorisation
supplémentaire ni copie des artefacts ou conversations. Une analyse n'autorise pas
la prochaine expérience.

## Références selon le besoin

- Vues et définitions : [guide utilisateur](../../../rstock/docs/user_guide.md).
- Sélection : [diagnostic scientifique](../../../rstock/docs/selection_diagnostic.md).
- Comparaison des tours : [protocole chronologique](../../../rstock/docs/CHRONOLOGICAL_XGBOOST.md).
- Contexte et limites : [protocole SPY](../../../rstock/docs/market_context_protocol.md).
- Sources techniques : `rstock/application/common_prediction_comparison.py`,
  `aligned_constant_comparison.py`, `forward_diagnostic.py` et
  `forward_temporal_analysis.py` depuis la racine du dépôt, selon la question.
