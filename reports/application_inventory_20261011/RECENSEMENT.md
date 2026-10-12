# Recensement RStock — 11 octobre 2026

## Portée et niveau de preuve

Ce document prépare la prochaine révision des instructions et de la documentation.
Il ne modifie ni `AGENTS.md`, ni les roadmaps, ni l'application.

- État analysé : arbre de travail, incluant les modifications non committées ; commit de base `8dd923806e9ab177ca9c28fefe9ca0072eaa8788`.
- Inventaire structurel : 276 fichiers, 86 modules Python applicatifs, 34 modules Python du moteur, 18 scripts, 116 fichiers de tests et 126 champs de `RStockConfig`.
- Les 1 353 définitions de tests recensées sont un inventaire statique, pas un nombre de tests exécutés ni un résultat de validation. La paramétrisation peut modifier le nombre de cas.
- Lecture ciblée des routes d'écran, contrats, services, mécanismes scientifiques et documents ; extraction AST de toutes les sources Python inventoriées. Ce recensement ne constitue pas une revue ligne par ligne de chaque fonction.
- Aucun entraînement, expérience, téléchargement de marché, import/exécution du moteur, test, réparation ou migration n'a été lancé. Aucun agent n'a été créé.
- **Tournée visuelle non réalisée** : aucun navigateur connecté disponible ; l'ouverture dans le navigateur intégré a échoué (`Browser is not available: iab`). Aucun listener détecté sur les ports locaux 8501 à 8510 lors de la vérification. Cela ne prouve pas l'absence d'une instance sur une autre machine ou un autre port.
- Le démarrage de l'application sur les données réelles n'a pas été utilisé comme contournement : `_service()` peut relancer des Forward pending via `recover_pending_forward_dispatches()`. Une consultation des écrans n'est donc pas nécessairement sans effet de bord.

[inventory.json](inventory.json) contient les chemins, empreintes SHA-256, définitions et lignes, appels UI déclarés et champs de configuration. Les empreintes permettent d'identifier les sources de ce recensement ; elles ne certifient pas leur validité scientifique. Aucun artefact volumineux de run n'a été dupliqué.

## 1. Surfaces fonctionnelles de l'application

La navigation actuelle déclare neuf pages dans `rstock/application/streamlit_app.py::_primary_pages`.

| Page | Capacités recensées dans le code | Contrats à préserver | Références principales |
|---|---|---|---|
| Surveillance | Univers opérationnel, mise à jour quotidienne, signaux haussiers de prochaine séance, résultats de dernière séance, modèles en observation, historique, saisie de transactions et progression des jobs | Séparer observation/activation et contexte historique au moment de la prédiction ; actualisation conditionnelle ; ne pas confondre résultat de signal et transaction exécutée | `surveillance.py`, `surveillance_refresh.py`, `production_services.py`, `real_trades.py` |
| Rendement | Transactions réelles saisies, filtres date/cible/modèle, modification, annulation/suppression et synthèses | Les transactions manuelles restent distinctes des prédictions et rendements théoriques ; leur modification ne réécrit pas un run scientifique | `real_trades.py`, `_returns_page` |
| Modèles | Catalogue, statuts, univers/source/santé, fenêtres 20/63/126 séances, KPI et détail, entraînement, observation, activation, désactivation et retrait | Registre Production autoritaire ; statut à la prédiction figé ; qualité dérivée ; notionnel théorique explicite ; sélection par identité stable | `production_domain.py`, `production_quality*.py`, `model_ui.py`, `_models_page` |
| Historique | Expériences, Holdout et Production réelle ; filtres, sélection, détail, comparaison, duplication, dérivation, reprise, annulation, purge et suppression | Runs techniques masqués ; sources physiques résolues ; sélection côté session ; graphe de dépendances revérifié avant destruction ; comparabilité contrôlée | `history_*.py`, `run_*.py`, `batch_delete.py`, `batch_purge.py`, `dependency_index.py` |
| Expériences | WF, préfiltre autonome, calibrations XGBoost/paramètres de seuils/seuils et E2E ; univers, cutoff, géométrie, type de modèle, volatilité, préfiltre et options aval | Configuration locale immuable au lancement, preview combinatoire, confirmation, héritage de préfiltre explicite ; options aval enregistrées | `_experiments`, `experiment_launch.py`, `domain.py`, `prefilter_contract.py` |
| Univers | Création manuelle ou à partir des sources existantes, distinction standard/contexte, sauvegarde/modification, aperçu et échantillonnage Top N ou seed | Contexte prédicteur seulement ; listes réellement résolues figées dans le run ; modifier un univers sauvegardé ne change pas un snapshot | `universes.py`, `universe_ui.py` |
| Paramètres | Univers/données, modèles/combinaisons, préfiltre, WF, calibrations, holdout/promotion, Forward/production, avancé | Distinguer defaults modernes, paramètres sauvegardés, paramètres locaux et compatibilité historique ; enregistrer ne modifie pas un run existant | `config.py`, `_settings`, `tests/test_settings_tabs.py`, `tests/test_user_settings.py` |
| Simulation | Prédictions évaluées ; rejeu historique avec réentraînement quotidien ou modèles figés au début ; notionnel par signal, benchmark SPY, résultats sauvegardés et détail | Rejeu historique d'une population active choisie aujourd'hui distinct d'une découverte PIT ; calcul historique peut entraîner ; simulation financière haussière théorique | `simulation.py`, `simulation_repository.py`, `production_services.py::HistoricalReplayMode` |
| Documentation | Affichage des sections du guide utilisateur intégré | Le guide affiché ne représente pas à lui seul tous les contrats spécialisés ; les documents de protocoles doivent être référencés | `_documentation_page`, `rstock/docs/user_guide.md` |

Une branche `_legacy_models_page` existe mais n'est pas la page enregistrée dans la navigation actuelle. Elle ne doit pas servir à conclure que toutes ses actions sont accessibles aujourd'hui. Ne pas supprimer les branches historiques dans le cadre de cet audit.

### Détails et comparaisons

- WF : résumé, analyse, combinaisons, validation, batches techniques et technique, avec ressources lorsque disponibles.
- E2E : résumé, WF, XGBoost, paramètres de seuils, seuils, évaluation holdout, qualification promotion, diagnostic de sélection, validation temporelle, promotion et technique.
- Revalidation forcée : WF sur candidats imposés, évaluation à seuils figés et qualification ; contrat de période verrouillé pour les nouveaux runs concernés.
- Diagnostic holdout des rejets WF : résultats, sensibilité Worst AUC et candidat. Ce diagnostic révèle des labels supplémentaires ; il ne réhabilite pas automatiquement les candidats.
- Comparaisons distinctes pour WF, préfiltre, E2E et Forward ; la comparaison de runs passe par une sélection de 2 à 6 runs dans la navigation Historique.
- Détail modèle : baseline de promotion, signaux et technique ; qualité persistée, fenêtres temporelles et provenance.
- Le registre `run_detail_tabs.py` charge le rendu de l'onglet actif. Réutiliser les vues autonomes des étapes scientifiques dans le pipeline plutôt que dupliquer leur logique.

## 2. Chaîne scientifique et contrats

### Données, calendriers et univers

- Acquisition Yahoo/yfinance, cache Parquet incrémental, métadonnées de fournisseur et calendrier. Les requêtes tiennent compte des séances clôturées ; une acquisition incomplète ne doit pas effacer des valeurs valides.
- Calendriers de marché explicites, dates ordonnées, retards en séances ; séparation cible/contexte/prédicteurs.
- Cibles Up et Down binaires indépendantes fondées sur le rendement Open→Close. Leur définition et les opérateurs de décision doivent venir du contrat de l'étape ; ne pas généraliser une convention depuis une autre vue.
- Matrice propre aux colonnes requises par chaque modèle ; exclusions et effectifs doivent rester traçables. Ne pas combler silencieusement les données manquantes.
- Données préparées, cutoff demandé et résolu, univers et digests persistés. Les sources supplémentaires de volatilité et SPY ont leur propre provenance et leur chauffe.
- Une série ajustée téléchargée rétrospectivement n'est pas une archive fournisseur PIT. Décaler à J−1 et figer les données garantit une partie de la causalité/reproductibilité, pas l'absence de révisions fournisseur ou de biais d'univers.

Références : `data.py`, `market_cache.py`, `calendars.py`, `features.py`, `target_volatility.py`, `traceability.py`, `prepared_dataset_hash`, `universes.py`.

### Préfiltre et découverte

- Préfiltre univarié avec paramètres XGBoost propres, performance/stabilité, Top N et déredondance par corrélation.
- Modes recensés : origine unique, stabilité temporelle et consensus d'occurrences sur sélections autonomes complètes. Le consensus porte sur le statut retenu, pas sur une moyenne arbitraire de rangs.
- Contrat public versionné entre sélection des prédicteurs et WF : sources, snapshot, paramètres et digests ; héritage explicite lors de la création d'un dérivé.
- Préfiltre autonome forcé en `external_only`, sans volatilité cible au lancement. Les modèles constante/cible seule ne consomment pas de référence de préfiltre.
- Plan combinatoire déterministe, paresseux et indexable ; cibles principales et symboles de contexte ont des rôles distincts. Le batch technique change l'exécution, pas la population scientifique.
- Biais identifié dans la roadmap : une sélection de prédicteurs utilisant des périodes qui chevauchent les tests WF peut favoriser la qualification. La présence de plusieurs origines ne corrige pas à elle seule ce biais.

Références : `predictor_prefilter.py`, `prefilter_experiments.py`, `prefilter_stability.py`, `prefilter_consensus.py`, `prefilter_contract.py`, `combination_planning.py`, `combinations.py`, `rstock/docs/scientific_roadmap.md`.

### Types et entraînement

- Quatre types : `external_only`, `constant_probability`, `target_only`, `target_and_external`.
- Constante : prévalences Up/Down du train, sans XGBoost ; référence d'apprentissage alignée reconstruisible en lecture pour les comparaisons compatibles.
- XGBoost : tours fixes ou sélection chronologique interne, par direction, suivie d'un refit ; raisons de repli explicites et versions persistées.
- Volatilité cible facultative, profondeur de retards et combinaisons configurables ; ces variantes sont des investigations de la roadmap, pas une validation du système complet.
- WF expanding ou rolling ; expanding est le choix scientifique actuellement indiqué dans la roadmap, pas une suppression du mode rolling dans le logiciel.
- Les types expérimentaux sont acceptés pour la recherche WF/E2E dans `ExperimentSpec`. Promotion automatique et Forward sont refusés pour les types autres que `external_only` ; `PromotionService` bloque aussi leur promotion.

Références : `modeling.py`, `walk_forward.py`, `streaming_walk_forward.py`, `parallel.py`, `domain.py`, `production_services.py`, `predictive_model_types.md`, `CHRONOLOGICAL_XGBOOST.md`.

### Sélection, calibrations et pipeline

- Qualification WF, calibration XGBoost, calibration des paramètres de seuils et calibration des seuils ont des fonctions et populations différentes.
- Sampling historique `per_target_v1` et moderne `global_stratified_v2` ; manifests déterministes. Ne pas échantillonner à nouveau silencieusement la calibration finale exhaustive sous la politique concernée.
- Les probabilités de développement servent au choix des seuils ; ne pas remplacer un seuil par l'optimum observé sur holdout.
- Pipeline moderne : préfiltre lorsqu'applicable → WF → XGBoost → paramètres seuils → seuils → évaluation holdout → qualification promotion ; étapes optionnelles de validation temporelle, revalidation forcée, promotion et Forward selon le contrat/version.
- Les versions historiques ont des graphes différents. Dériver une étape invalide ses dépendants et conserve les étapes physiques héritées ; ne pas résoudre une nouvelle source de remplacement pendant la reprise.
- La constante a un statut de calibration XGBoost non applicable ; « non applicable », « non exécuté », « indisponible », « aucun candidat » et « échec » ne sont pas équivalents.
- Le classement composite WF et la décision de qualification promotion ont des usages distincts. Une règle bannissant globalement tout score composite serait incorrecte.

Références : `end_to_end.py`, `derivation.py`, `derived_experiments.py`, `calibration_sampling.py`, `calibration.py`, `threshold_parameter_calibration.py`, `threshold_calibration.py`, `qualification.py`, `model_selection.py`, `promotion_pipeline.py`.

### Validation, comparaison et vieillissement

- Comparaison sur prédictions individuelles, dates/labels et identités vérifiés. Définir la population et les pondérations ; ne pas substituer des scores agrégés à des observations communes.
- Comparaison appariée fixe/chronologique avec bootstrap temporel conjoint ; contrôle de l'alignement et de la couverture des replis. Les candidats ne sont pas des réplications indépendantes.
- Validation temporelle E2E versionnée : source référence et enfant offset 63 ; contrôles de rendement de découverte, AUC, precision edge et rendement directionnel, statuts `passed/failed/inconclusive/invalid`.
- Revalidation forcée des identités de référence et diagnostic des rejets : procédures séparées de la redécouverte d'une population libre.
- Forward : `forward_policy=FROZEN` uniquement dans `ExperimentSpec`. Les boosters sont préparés une fois à la source ; `run_forward_simulation` ne les réentraîne pas. Préparer le snapshot peut donc entraîner, même si l'évaluation Forward n'entraîne pas.
- Horizons descriptifs 21/42/63/84/126 séances, cumuls et intervalles, cohortes T0 communes/additionnelles/retirées ; périodes non additionnables et modèles sans observations explicitement signalés.
- La Simulation historique possède déjà des modes figé/réentraîné. Leur population active courante ne constitue pas une campagne indépendante de découverte PIT ni une validation prospective.

Références : `common_prediction_comparison.py`, `aligned_constant_comparison.py`, `round_selection_comparison.py`, `temporal_validation.py`, `forced_candidate_validation.py`, `qualification_holdout_diagnostic.py`, `forward_simulation.py`, `forward_temporal_analysis.py`, `forward_diagnostic.py`, `simulation.py`.

### Diagnostics de sélection et contexte

- Entonnoir avec unités distinctes : couples cible/prédicteur, combinaisons et identités directionnelles. Motifs de rejet chevauchants distincts des motifs exclusifs.
- Agrégats WF/développement/holdout/Forward, détails par modèle et export ZIP préparé à la demande ; gros CSV lus par blocs et partitions sur demande.
- SPY ajusté dédié, variables continues, terciles figés sur la référence de développement ; régimes/épisodes `rstock_spy_regimes_v1` déjà implémentés comme diagnostics descriptifs.
- Le default global de contexte reste désactivé ; le nouveau formulaire E2E propose la capture activée. L'absence historique reste désactivée. Ces trois comportements ne sont pas interchangeables.
- Seuils de support, couverture et indisponibilité explicites ; un régime unique ne démontre pas de généralisation. Les diagnostics ne changent pas automatiquement entraînement, sélection ou qualification.

Références : `selection_diagnostic.py`, `selection_diagnostic_ui.py`, `market_context*.py`, `market_regimes.py`, `selection_diagnostic.md`, `market_context_protocol.md`.

## 3. Production, transactions et simulations

- Cycle candidat → entraîné → observation/actif → inactif/retrait, avec transitions et artefacts vérifiés. L'observation n'est pas l'activation.
- Registre versionné, baseline de promotion immuable, artefacts Up/Down, seuils figés et origine scientifique. Promotion idempotente par identité et checkpoints.
- Screening haussier opérationnel combiné : Up ≥ seuil Up et Down < seuil Down ; ce contrat est distinct du seuil natif des prédictions WF. Ne pas harmoniser les opérateurs sans protocole explicite.
- Historique opérationnel, résultats réalisés, transactions saisies, qualité canonique et simulations sont des collections distinctes.
- Qualité : origines `scheduled_live`, `operational_backfill`, `legacy_inferred_live`, `legacy_unknown` ; statut modèle au moment de prédiction. Les KPI live excluent backfills et origines temporellement ambiguës, tout en conservant leurs observations.
- Fenêtres de qualité définies en séances calendaires, pas en derniers N trades ; snapshots compacts, partitions et reconstructions explicites. Le registre reste autoritaire en présence d'une qualité orpheline.
- Le P&L de monitoring/Forward utilise un notionnel théorique de 10 000 $ par signal. Le moteur de simulation consulté calcule montant × (Close/Open − 1), sans frais/slippage dans cette formule ; ce résultat brut ne démontre pas la rentabilité après coûts.
- Plusieurs modèles peuvent exposer la même réalisation économique. Un agrégat modèle/signal n'est pas automatiquement un portefeuille avec capital limité et positions dédupliquées.
- Scripts de rebuild/backfill/réparation existent ; ils écrivent ou recalculent des artefacts. Ils ne sont pas des opérations de lecture neutres.

Références : `production_domain.py`, `production_repository.py`, `production_services.py`, `production_quality*.py`, `real_trades.py`, `simulation*.py`, `scripts/repair_*`, `scripts/backfill_*`.

## 4. Architecture, persistance et présentation

| Contrat durable candidat aux instructions | Détail à conserver dans la documentation |
|---|---|
| Réutiliser les services et workflows existants | `RunRepository`, `RunService`, `WorkflowRegistry`, workers, leases, slots et heartbeat ; pas de moteur de jobs parallèle |
| Réutiliser la grille générique | `grid.py`, `grid_dataframe.py`, frontend commun ; IDs stables, tri brut/affichage formaté, aides, sélection, pagination, export ; aucune science dans le frontend |
| Rendu borné et paresseux | Onglet actif seulement, résolution légère des étapes, détails sur demande ; gros artefacts hors rendu, agrégats persistés |
| Autorité des états persistés | Status/progress enfant autoritaires ; réconciliation avant écriture finale ; index Historique/dépendances/qualité reconstruisibles |
| Reprise sans changement de science | Même run, snapshot et sources ; réutilisation des étapes/batches vérifiés ; fingerprint historique autoritaire ; annulation propagée |
| Publication cohérente | Écritures atomiques, verrous, staging et manifests ; protéger les publications concurrentes et distinguer résultats incomplets/committés |
| Concurrence bornée | Leases Run/Slot, queue équitable et heartbeat ; pas de décision fondée sur un PID seul ; pas de deadlock parent/enfant avec un slot |
| Compatibilité versionnée | Defaults historiques explicites pour config, `ExperimentSpec`, graphes E2E, sampling, origine de prédiction et politiques de qualité |
| Suppression/purge contrôlée | Prévisualisation, dépendances des dérivés/Production/simulations, revalidation à l'exécution ; purge ≠ suppression ; quarantaine transitoire ≠ corbeille utilisateur |
| Provenance des exports | Sources physiques, digests et couverture ; exporter ne doit pas télécharger, réentraîner ou réinventer une valeur manquante |

La sélection Historique est autoritaire côté session : événements visibles, contextes et révisions sont réconciliés. Les événements d'une ancienne page ou d'un ancien filtre ne doivent pas sélectionner un autre run.

Les durées observées de worker et les sommes de calcul par modèle ne mesurent pas la même chose. La télémétrie ressources n'est pas une preuve de validité scientifique.

Références : `repository.py`, `runner.py`, `worker.py`, `atomic_io.py`, `persistence.py`, `checkpoints.py`, `run_integrity.py`, `history_index.py`, `history_selection.py`, `dependency_index.py`, `run_storage.py`, `run_delete.py`, `resource_telemetry.py`, `run_timing.py`, `forward_export.py`.

## 5. Documentation, scripts et tests réutilisables

| Source | Usage futur | Vigilance |
|---|---|---|
| `AGENTS.md` | Invariants communs, routage et autorisations | Garder court ; référencer les contrats plutôt qu'énumérer les 126 paramètres |
| `rstock/docs/scientific_roadmap.md` | Axes, approches, techniques, expériences, conclusions et décisions | Les affirmations historiques et statuts « partiellement réalisée » demandent des références aux runs/rapports |
| `rstock/docs/rstock_roadmap.md` (anciennement `ROADMAP.MD`) | Roadmap produit | Mélange de souhaits et réalisations ; technologies/statuts à clarifier |
| `rstock/docs/WF_BATCHES.md` (anciennement à la racine) | Contrats WF/E2E et reprise | Ancien pipeline, phases de développement et valeurs d'introduction ; ne pas les appliquer à toutes les versions modernes |
| `README.md`, `MIGRATION.md` | Entrée technique et histoire du portage | Certains parcours, métriques et phases sont historiques ; `MIGRATION.md` n'est pas le protocole actuel global |
| `grid_standardization_plan.md`, `grid_migration_report.md` | Grille et procédure d'intégration | L'inventaire avant migration n'est pas l'état actuel ; le compte rendu distingue les fonctions livrées |
| `CHRONOLOGICAL_XGBOOST.md`, `predictive_model_types.md` | Contrats de variantes | Versions, couverture et restrictions ; divergence avec la note UI sur les types expérimentaux |
| `market_context_protocol.md`, `selection_diagnostic.md` | Diagnostics et interpretation | Terciles, régimes, defaults, support et règles de signal distincts ; ajouter des liens plutôt que fusionner les définitions |
| `reports/analyse_xgb_20261010_fixe_chronologique/` | Exemple d'analyse reproductible et limites | Résultat exploratoire ; commits et versions runtime historiques différents ; ne pas convertir en conclusion générale |
| `diagnostics/aligned_constant_20261011/` | Référence constante et alignement | Développement observé, pas validation indépendante |
| `diagnostics/target_volatility_mvp_20261011/` | Préparation A/B/C réutilisable | Configurations préparées, pas résultats ni autorisation de lancement |
| `diagnostics/e2e_recovery/`, `reports/history-lot1/`, `reports/dependency-index/` | Reprise, performances et index dérivés | Mesures locales/historiques, pas engagements de performances actuelles |
| `scripts/` | CLI et outils existants | Séparer analyse, préparation, benchmark, migration/réparation, entraînement et opérationnel avant exécution |
| `tests/` | Carte des contrats et validations ciblées | Présence d'un test ≠ réussite actuelle ; aucune suite exécutée dans cet audit |

Les tests recensés couvrent notamment calendriers/features, compatibilité, préfiltre/consensus, types, sélection chronologique, constantes alignées, batching/reprise, leases, dérivations, holdout/validation temporelle, Forward, contexte/régimes, publication, qualité Production, transactions, simulations, dépendances, purge/suppression, grilles et navigation. L'inventaire structurel contient les noms et lignes de chaque définition.

## 6. Questions et décisions à confirmer

### Décisions issues des réponses de l'utilisateur

Mise à jour du 11 octobre 2026. Les décisions suivantes remplacent les propositions
initiales correspondantes. Elles mettent à jour les orientations documentaires,
sans modifier les contrats exécutables ou lancer de calcul.

| Sujet | Décision / état |
|---|---|
| D01 — Types expérimentaux | Laisser les types et règles applicatives en l'état. La divergence documentaire recensée reste un constat, pas une demande de correction actuelle. |
| D02 — Régimes | À explorer plus tard selon la roadmap scientifique. Conserver les diagnostics existants ; priorité actuelle au WF. |
| D03 — Stabilisation | Prédictabilité maintenue en Forward : qualité supérieure à une référence simple et rendement brut positif. Seuils et durée à préciser au moment du protocole, sans nouvelle question bloquante pour les instructions. |
| D04 — Holdout → Forward | Parcours déjà implémenté et vérifié dans le code : candidats/seuils figés, début après cutoff et absence de réentraînement pendant l'évaluation Forward. Pas de nouveau développement requis pour cette séparation. L'implémentation ne constitue pas un résultat de validation scientifique. |
| D05 — Préfiltre | Important à terme : rechercher un WF utile économiquement et un préfiltre qui n'en diminue pas la qualité. Stabiliser d'abord le WF ; compromis et éventuelle tolérance non décidés. |
| D06 — PIT | Définition et garanties à définir plus tard ; les limites fournisseur/univers restent documentées. |
| D07/D08 — Critères MVP | Qualité prédictive supérieure à une référence simple et rendement positif brut. Ne pas imposer des coûts de transaction, spread ou slippage au MVP. Étude après coûts ultérieure ; choix détaillé de la référence selon le protocole. |
| D12 — Budget | Prioriser les efforts en temps et argent selon leur potentiel pour le MVP. Éviter les séries de microtests à faible bénéfice et les tentatives non concluantes sans élément nouveau. Aucun plafond chiffré ni autorisation générale d'expérimenter n'a été donné. |
| D13 — Forward pending | L'utilisateur considère la relance automatique injustifiée. Une consultation n'autorise pas une relance ; l'écart avec le mécanisme existant reste à examiner dans une tâche technique distincte. Aucune correction applicative effectuée ici. |
| D16 — Up/Down | Focus Up pour le MVP ; Down reste une possibilité ultérieure. Conserver ses capacités et son usage comme filtre existants. |
| D09/D10/D11/D14/D15 | Sujets de protocole, traçabilité, rétention et documentation à traiter au besoin ; aucune décision détaillée supplémentaire fournie. Ils ne bloquent pas l'ajustement des orientations communes. |
| D17 — Tournée visuelle | Toujours non réalisée ; limitations d'accès de la section Portée inchangées. |

Références de la vérification D04 :
`forward_simulation.py::build_forward_model_snapshot`,
`forward_simulation.py::run_forward_simulation`,
`end_to_end.py::run_end_to_end` et
`tests/test_forward_simulation.py::test_forward_execution_contains_no_booster_fit_path`.
Les sources et tests ont été lus, pas exécutés pour cette vérification.

`inventory.json` reste le snapshot de sources pris lors du recensement initial.
Ses empreintes pour les documents modifiés depuis ne décrivent donc pas leur
version actuelle ; il n'a pas été réécrit pour effacer cette provenance.

### Questions initiales archivées

Les propositions ci-dessous conservent le contexte de l'audit. Elles ne sont pas
des demandes en attente lorsque la décision ci-dessus les a résolues ou reportées.

| ID | Question ou décision | Constat motivant la question | Proposition / destination |
|---|---|---|---|
| D01 | Les types expérimentaux sont-ils autorisés dans toute la chaîne E2E de recherche, ou veut-on désormais les limiter au WF ? | Note UI restrictive ; contrat `ExperimentSpec`, workflows et doc décrivent WF/E2E, avec blocages promotion/Forward | Confirmer le périmètre avant correction de texte ou du moteur. Contrat spécialisé + aide UI, puis référence courte dans AGENTS |
| D02 | Que signifie précisément « régimes reportés » ? | Régimes/épisodes descriptifs déjà implémentés ; roadmap reporte la phase et les modèles spécifiques | Conserver les diagnostics disponibles, reporter campagnes d'interprétation approfondie et modèles conditionnés aux régimes. Roadmap scientifique |
| D03 | Quelle preuve et quels seuils ferment la phase de stabilisation ? | Roadmap claire sur la priorité, sans critères d'acceptation mesurables ni budget fixé | Définir contribution vs référence simple, validation temporelle indépendante, couverture et coût avant nouvelle campagne. Roadmap/protocole |
| D04 | Quel usage du holdout accepte-t-on pour sélectionner ou promouvoir, et quelle période reste indépendante après cette sélection ? | Holdout utilisé en confirmation, classement et qualification promotion ; diagnostic des rejets et sensibilité exposent aussi ses labels | Tracer les périodes déjà consultées ; réserver une validation future distincte. Ne pas modifier les décisions historiques. Roadmap + skill d'analyse |
| D05 | Quelle priorité pour corriger/mesurer le biais préfiltre→tests WF ? | Risque explicite dans la roadmap ; consensus multi-origines et dérivés à population héritée ne prouvent pas une correction | Distinguer étude de contribution à population figée et validation complète de redécouverte. Protocole scientifique à approuver |
| D06 | Quelle définition de PIT utilise-t-on : coupure causale historique ou archives réellement disponibles à chaque origine ? | Yahoo ajusté rétrospectif ; snapshots figés ; population d'univers potentiellement choisie après coup | Nommer les limites fournisseur, révisions et univers, puis classer chaque expérience. Roadmap et préparation |
| D07 | Quelle référence simple est obligatoire, et sur quelle population ? | Constante alignée disponible ; calendriers/train/features et populations retenues peuvent différer | Référence train alignée lorsque possible ; signaler les comparaisons non alignées, distinguer population complète et sélectionnée. Skill d'analyse |
| D08 | Quelle métrique et quelle politique de coûts prouvent l'exploitabilité ? | Rendements Open→Close et P&L à notionnel théorique ; frais/slippage absents de la formule de simulation consultée | Décider frais, spread/slippage, exécution, exposition simultanée et capital avant de qualifier un résultat de rentable après coûts. Roadmap/protocole financier |
| D09 | Compare-t-on des modèles, des signaux ou un portefeuille ? | Plusieurs modèles peuvent partager cible/date ; les simulations cumulent un notionnel par signal | Définir unité d'analyse, pondération et déduplication économique ; préserver les métriques par modèle existantes. Protocoles d'analyse |
| D10 | Comment rattacher les comparaisons figé/réentraîné existantes à la roadmap ? | Simulation possède ces modes ; Forward recherche accepte seulement FROZEN ; redécouverte renouvelée est une autre expérience | Réutiliser les outils, mais approuver une population commune et une validation future identique ; aucun rejeu automatique. Roadmap scientifique |
| D11 | Quels runs étayent les choix déjà « établis » et les phases « partiellement réalisées » ? | Roadmap : expanding retenu, hypothèses rejetées, PIT/vieillissement partiels, sans liste complète de sources | Ajouter références, dates, statut de preuve et décision ; ne pas rejouer sans élément nouveau. Roadmap et rapports |
| D12 | Quel budget nécessite une autorisation supplémentaire et comment reconnaître une campagne déjà autorisée ? | 1 slot lourd par défaut ; plusieurs actions de préparation/rejeu/diagnostic entraînent ou calculent ; coût dépend des candidats et fenêtres | Autorisation par protocole, population, plafond de coût et opérations aval ; estimer durée/mémoire/stockage. AGENTS + skill de préparation |
| D13 | Autorise-t-on une récupération automatique de Forward pendant une simple consultation ? | `_service()` appelle périodiquement `recover_pending_forward_dispatches()` et le runner peut lancer un worker | Documenter le comportement et prévoir une tournée sur environnement sans dispatch pending, ou une future option explicite de consultation. Décision produit/technique |
| D14 | Quelle politique de rétention garantit les analyses futures après purge ? | Purge peut retirer des artefacts lourds ; comparateurs et baselines exigent certaines probabilités/snapshots | Définir les éléments minimaux par usage ; contrôler la disponibilité et afficher une limite plutôt que recalculer silencieusement. Documentation stockage |
| D15 | Où placer les règles de rendu, provenance live/backfill et maintenance ? | Ces contrats existent mais sont peu explicites dans AGENTS ; scripts de migration et qualité dérivée disponibles | Ajouter seulement les invariants durables à AGENTS ; procédures détaillées dans doc d'architecture/maintenance et skills selon répétition |
| D16 | Le périmètre intrajournalier reste-t-il principalement haussier pour le MVP ? | Up/Down sont modélisés ; screening, monitoring et moteur financier recensés sont orientés signaux haussiers | Clarifier le périmètre produit sans assimiler la modélisation Down à une stratégie short déjà complète. Roadmap produit |
| D17 | Comment réaliser et clôturer la tournée visuelle restante ? | Aucun navigateur connecté ; consultation susceptible de dispatch automatique | Donner accès à une instance/browser contrôlable avec environnement connu ; parcourir les neuf pages et détails représentatifs sans actions métier. Complément de ce rapport |

Les réponses ci-dessus suffisent à ajuster les orientations communes. Les détails
de protocole reportés seront précisés lors d'une expérience concrète ; ne pas
réouvrir systématiquement toutes les questions archivées.

## 7. Réutilisation et prochaine étape

- **AGENTS.md** : règles communes de réutilisation, rendu paresseux, autorités de persistance, provenance, compatibilité, validation ciblée et périmètres autorisés. Ne pas y mettre toutes les fonctionnalités ni les seuils scientifiques.
- **Profil ingénieur** : carte des modules/contrats, préflight des effets de bord, tests ciblés, coût et livrables techniques.
- **Profil directeur** : statut des preuves, biais, unité économique, décisions de poursuite/arrêt et rattachement à la roadmap.
- **Skills partagées** : préparation avec paramètres effectifs/sources/coût et analyse d'abord depuis les résultats applicatifs, vérifications techniques complémentaires si nécessaires.
- **rstock/docs/scientific_roadmap.md** : axes et expériences tracés, preuves/conclusions, études complémentaires ; aucune duplication intégrale des rapports.
- **rstock/docs/rstock_roadmap.md et documentation** : carte fonctionnelle, architecture et limites produit. Le renommage de la roadmap produit et le déplacement de WF_BATCHES dans `rstock/docs/` ont été réalisés le 11 octobre 2026 ; l'inventaire JSON conserve les chemins historiques du recensement.

Après réponses, ajuster les fichiers concernés à leur niveau approprié. Ce recensement constitue un support réutilisable ; il ne promet pas que tous les comportements de l'application ont été validés à l'écran ou en exécution.

## 8. Complément — audit documentaire final du 11 octobre 2026

Ce complément actualise les références sans réécrire l'inventaire historique.
`WF_BATCHES.md` a été supprimé : les contrats utiles sont dans
[Jobs, lots et reprise durable](../../rstock/docs/job_orchestration.md), le
[guide](../../rstock/docs/user_guide.md) et les
[calibrations](../../rstock/docs/CHRONOLOGICAL_XGBOOST.md).

Le [registre scientifique](../../rstock/docs/scientific_experiments.md) regroupe
18 investigations après lecture des métadonnées de 350 runs et des rapports/
résultats pertinents. Treize ont des résultats sauvegardés, une est un constat
méthodologique, quatre comparaisons restent sans preuve suffisante. Cela ne signifie
pas treize expériences concluantes. Les simulations rétrospectives et les gates
historiques positifs ne compensent pas les Forward bruts négatifs examinés.

Les neuf pages, chaînes expérimentales, dérivations/comparaisons, snapshots,
batching/reprise, Active/Watching, qualité, simulations, maintenance et grille
commune ont des chemins et contrats documentés. Le guide a été complété pour les
types/volatilité, ressources et index de suppression. Aucune tournée visuelle,
aucun test ni calcul scientifique n'a été exécuté. La présence de tests n'est
pas assimilée à leur réussite actuelle.

### Lacunes documentaires restantes

| Importance | Sujet / manque | Emplacement | Pourquoi non complété |
|---|---|---|---|
| Haute | Comparaisons Single Origin/stabilité, décalage d'une séance et expanding/rolling : sources et conclusions chiffrées manquantes. | Registre EXP-002/EXP-003/EXP-004 | Paires/rapports non identifiés ; autres conversations et éventuels artefacts supprimés inaccessibles. Ne pas relancer pour combler la mémoire. |
| Haute | Contributions appariées des quatre familles et volatilité : synthèse indépendante et calendrier complet de comparaison. | Registre EXP-010/EXP-012 | Résultats bruts disponibles, mais aucune synthèse appariée persistée retrouvée ; non remplacée par les métriques globales. |
| Haute | Preuve prospective indépendante, critères quantifiés de stabilisation et garanties PIT. | Roadmap scientifique et protocole de l'expérience retenue | Aucun résultat indépendant ni seuil/horizon approuvé retrouvé ; les inventer modifierait le protocole. |
| Moyenne | Comparabilité du replay ancien Historique et figé ; durée de vie/renouvellement validés. | Registre EXP-016/EXP-017 | Provenance/modes historiques et populations à réconcilier ; outils existants distincts de preuve scientifique. |
| Moyenne | Validation en usage des écrans et navigation des liens Markdown intégrés. | Présent rapport | Audit de code seulement ; application non ouverte pour éviter la récupération automatique de Forward pending. |

Divergences encore présentes dans le produit : aide des types trop restrictive
par rapport à `ExperimentSpec`, récupération automatique des Forward pending.
Elles sont documentées ; le code n'a pas été modifié. Les anciennes listes et
empreintes de l'inventaire restent datées, sans prétention de représenter les
nouveaux documents ou la validation runtime actuelle.

## 9. Références de pilotage regroupées

Les roadmaps ont été déplacées, noms conservés, sous `rstock/docs/` :
[application](../../rstock/docs/rstock_roadmap.md),
[science](../../rstock/docs/scientific_roadmap.md),
[stratégies](../../rstock/docs/scientific_strategies.md),
[expériences](../../rstock/docs/scientific_experiments.md).
Les 18 IDs et leurs résultats sont conservés ; 15 approches distinctes sont reliées
aux expériences. Les noms de fichiers et empreintes de l'inventaire JSON daté
restent des données historiques, pas des liens actifs ni un nouvel audit des runs.
