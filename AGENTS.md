# Instructions communes — RStock

## Objectif et références

RStock vise à découvrir des relations prédictives entre symboles, exploitables
en transactions intrajournalières sur des données futures inconnues, après coûts.
Ces avantages peuvent être temporaires : leur découverte, leur vieillissement
et leur renouvellement doivent être évalués au niveau du système complet.

Pour le MVP, viser une qualité prédictive supérieure à une référence simple et
un rendement intrajournalier brut positif maintenus en Forward. Les coûts de
transaction seront étudiés ultérieurement. Priorité à la stabilisation du WF et
aux signaux Up ; l'évaluation du préfiltre et l'exploration Down viendront ensuite,
sans supprimer les capacités existantes.

- [SCIENTIFIC_ROADMAP.md](SCIENTIFIC_ROADMAP.md) est la source de vérité pour les
  hypothèses, approches, techniques, expériences réalisées ou prévues, conclusions
  et priorités scientifiques. La priorité actuelle est la stabilisation de la
  découverte et de la sélection ; les régimes sont reportés.
- [RSTOCK_ROADMAP.md](RSTOCK_ROADMAP.md) décrit l'évolution du logiciel.
- Charger les références techniques selon la mission, sans recopier leur contenu
  ni l'historique des conversations. Référencer les runs et rapports existants.
- Certains documents contiennent des plans ou états historiques. Vérifier le
  chemin de code et les contrats applicables ; signaler les contradictions avant
  l'opération concernée, sans modifier silencieusement le protocole.

## Mise à jour continue de la documentation

Toute intervention qui modifie l'état fonctionnel, technique ou scientifique de
RStock doit également maintenir sa documentation à jour. Cette obligation couvre
les développements, les corrections fonctionnelles et les expériences scientifiques.

- Avant de considérer une tâche comme terminée, vérifier et effectuer les mises
  à jour nécessaires des roadmaps, du guide utilisateur et de la documentation
  technique concernée. Ces mises à jour font partie de la tâche initiale et ne
  nécessitent aucune demande ni autorisation supplémentaire de l'utilisateur.
- Utiliser les sources officielles existantes : [RSTOCK_ROADMAP.md](RSTOCK_ROADMAP.md)
  pour l'évolution fonctionnelle et technique, [SCIENTIFIC_ROADMAP.md](SCIENTIFIC_ROADMAP.md)
  pour les hypothèses, expériences, résultats, conclusions et prochaines validations,
  le [guide utilisateur](rstock/docs/user_guide.md) et les documents pertinents de
  `rstock/docs/` ou d'un autre emplacement officiel déjà utilisé. Ne pas créer de doublons.
- Après chaque expérience scientifique ou modification pertinente de l'application,
  actualiser la roadmap concernée : états à faire, en cours, terminé, abandonné ou
  reporté ; travaux réalisés marqués terminés ou retirés de la liste des travaux
  à faire ; décisions et conclusions réellement établies ; nouvelles expériences
  retenues et leurs objectifs ; prochaines priorités selon les résultats obtenus.
  Référencer les runs et rapports existants. Distinguer résultat observé, hypothèse
  et conclusion validée ; ne jamais déclarer une expérience concluante sans données
  qui le démontrent, ni inventer de résultat scientifique.
- Après chaque modification fonctionnelle, vérifier les impacts documentaires sur
  les fonctionnalités et paramètres, l'interface et la navigation, le comportement
  des jobs et expériences, les règles de validation, calibration et qualification,
  les procédures de lancement, d'utilisation et de reprise, ainsi que les fonctions
  supprimées ou remplacées. Le guide et la documentation technique décrivent le
  fonctionnement réellement implémenté ; les fonctionnalités planifiées restent
  explicitement identifiées comme telles dans les roadmaps.
- Privilégier les modifications ciblées. Ne pas modifier artificiellement une
  roadmap pour une correction purement technique sans changement d'état ou de
  décision, ni ajouter de documentation inutile pour un changement interne sans
  conséquence. Éviter les réécritures complètes et les nouveaux fichiers non nécessaires.
- Cette obligation documentaire n'autorise aucun lancement, entraînement,
  changement de protocole scientifique, promotion ou activation supplémentaire.

## Responsabilités et autorisations

Ces rôles définissent les responsabilités ; ils ne créent aucun agent actif.

Profils versionnés : [.codex/agents/directeur-scientifique.toml](.codex/agents/directeur-scientifique.toml)
et [.codex/agents/ingenieur-logiciel-donnees.toml](.codex/agents/ingenieur-logiciel-donnees.toml).
Pour choisir l'interlocuteur principal, demander explicitement son rôle dans la
conversation. Si le client ne sélectionne pas les profils, demander de lire le
fichier correspondant et d'en appliquer `developer_instructions` ; pour une
délégation, transmettre une mission bornée et le profil plutôt que le seul nom.

Skills partagées : `$preparer-experience-rstock` pour préparer un protocole et
`$analyser-experience-rstock` pour interpréter les résultats existants, d'abord
dans l'application. Leurs sources sont dans `.agents/skills/`. Elles ne sont pas
requises pour une correction UI ordinaire et n'autorisent aucun lancement.

- **Directeur scientifique** : interlocuteur pour la vision, les hypothèses,
  protocoles, campagnes et interprétations. Maintient la roadmap scientifique,
  les conclusions et les critères de poursuite ou d'arrêt. Distingue résultats
  observés, hypothèses et preuves manquantes ; évalue bénéfice attendu et coût.
- **Ingénieur logiciel et données** : maîtrise l'architecture, vérifie données,
  calendriers, causalité et reproductibilité. Prépare les calculs, propose les
  plans techniques, implémente et valide dans le périmètre autorisé. Signale les
  écarts entre protocole demandé et possibilités réelles avant leur exécution.
- Une fonctionnalité substantielle demandée au directeur donne lieu à une mission
  d'étude de l'ingénieur et à un plan technique présenté à l'utilisateur avant
  implémentation. Si la délégation est disponible, confier une mission bornée avec
  sources, périmètre autorisé et livrable ; sinon fournir une mission transférable
  manuellement. Ne pas supposer qu'un nom de tâche charge un profil d'agent.
- L'ingénieur retourne une synthèse courte : faisabilité, changements,
  validations, limites et décision requise. Le directeur replace la décision
  dans la roadmap. Éviter les écritures concurrentes sur les mêmes fichiers.
- Les demandes techniques peuvent être adressées directement à l'ingénieur.
  Une correction simple déjà autorisée s'exécute directement, sans délégation
  ni revue scientifique systématique.
- Ne déclencher aucun entraînement coûteux, campagne, changement de protocole
  scientifique, promotion ou activation sans autorisation correspondante.
  Une autorisation existante reste valable dans son périmètre ; ne pas la
  redemander. Préparer ou analyser une expérience n'autorise pas son lancement.
- Réutiliser scripts, snapshots et résultats avant de proposer un recalcul.
  Estimer la charge avant un calcul coûteux. Ne pas ajouter d'outil, service
  ou orchestrateur sans nécessité démontrée.
- Prioriser les travaux selon leur potentiel pour le MVP et leur coût en temps
  et en argent. Éviter les séries de microtests à faible bénéfice attendu et les
  nouvelles tentatives non concluantes sans élément nouveau. Aucun plafond
  chiffré de calcul n'est convenu ; proposer un périmètre utile avant exécution.
- Une simple consultation n'autorise pas la relance d'un Forward pending. Le
  mécanisme automatique existant est un écart à examiner séparément ; ne pas
  modifier son comportement dans une tâche de documentation.

## Architecture et grilles

- Étendre les services, repository, workers, leases, workflows et checkpoints
  existants ; ne pas créer de framework de jobs parallèle. Voir les contrats
  de [WF_BATCHES.md](rstock/docs/WF_BATCHES.md).
- Toute grille de l'application réutilise le socle
  `rstock/application/grid.py::render_grid`, via `grid_dataframe.dataframe`
  pour les DataFrames ou l'adaptateur métier existant. Compléter le composant
  commun si nécessaire plutôt que créer un rendu indépendant.
- Préserver identifiants et sélection, tri sur valeurs brutes, formats, aides,
  pagination et actions. Aucun calcul scientifique dans le frontend.
  Référence actuelle : [grid_migration_report.md](rstock/docs/grid_migration_report.md) ;
  le [plan de standardisation](rstock/docs/grid_standardization_plan.md) contient
  aussi l'inventaire antérieur à la migration.

## Données et validité scientifique

- Respecter les calendriers et la causalité des variables : seules les
  observations disponibles à l'instant de prédiction peuvent être utilisées.
  Préserver les règles explicites de retards, valeurs manquantes et exclusions.
- Figer et identifier données, univers, paramètres effectifs, versions et sources
  au lancement. Un changement des paramètres globaux ne modifie pas un run existant.
- Séparer apprentissage, sélection/calibration, test et holdout selon le protocole.
  Ne pas remplacer un seuil calibré par le meilleur seuil observé sur le holdout.
  Signaler les biais de sélection, notamment les chevauchements préfiltre/test.
- Réutiliser le parcours holdout → Forward figé déjà implémenté : candidats et
  seuils figés, évaluation après le cutoff, sans réentraînement dans le Forward.
  Sa présence ne démontre pas à elle seule le maintien de la qualité prédictive.
- La stabilisation recherchée est le maintien de la prédictabilité en Forward,
  avec rendement brut positif pour le MVP. Les seuils et horizons d'acceptation
  seront précisés dans les protocoles ; ne pas en inventer ni changer les règles
  des types de modèles expérimentaux, laissées en l'état.
- Un snapshot reproductible ou un calcul décalé à J−1 ne prouve pas une
  disponibilité historique Point-in-Time ; documenter les limites du fournisseur.
- Comparer sur des populations et calendriers vérifiés ; distinguer références
  simples, résultats descriptifs, preuve de généralisation et rentabilité après
  coûts. Ne pas traiter des fenêtres ou candidats dépendants comme indépendants.
- Références selon le besoin : [guide utilisateur](rstock/docs/user_guide.md),
  [sélection chronologique](rstock/docs/CHRONOLOGICAL_XGBOOST.md),
  [types de modèles](rstock/docs/predictive_model_types.md) et
  [contexte de marché](rstock/docs/market_context_protocol.md).

## Compatibilité historique des configurations

Tout nouveau champ ajouté à `RStockConfig` doit définir explicitement
son comportement lorsqu’il est absent d’un snapshot historique.

- Ne jamais appliquer silencieusement un default moderne si cela change
  la sémantique d’un ancien run.
- Une valeur explicitement enregistrée dans le snapshot est toujours prioritaire.
- Les nouveaux runs utilisent les defaults actuels.
- Si l’absence historique d’un champ correspond à une valeur différente
  du default moderne, ajouter une compatibilité rétroactive explicite.
- Ajouter un test couvrant la désérialisation et/ou la duplication du run historique.

## Reproductibilité et checkpoints

- Un état persistant ne doit jamais être écrasé à partir d’un objet en mémoire potentiellement périmé.
- Avant toute écriture finale d’un manifest/checkpoint, recharger ou réconcilier l’état persistant si plusieurs composants peuvent l’avoir modifié.
- Les mécanismes de reprise doivent être couverts par des tests d’interruption et de réconciliation.
- Réutiliser les étapes et batches terminés après vérification des contrats et
  empreintes ; préserver les fingerprints historiques persistés lorsqu'ils font
  autorité. Le statut d'un enfant se lit depuis son état persistant, pas depuis
  une copie potentiellement périmée du manifest parent.
- Le batching doit préserver les résultats scientifiques d'un mono-run équivalent
  sur les mêmes données et paramètres. Voir [WF_BATCHES.md](rstock/docs/WF_BATCHES.md).

## Portée et validation des modifications

- Limiter les changements à la demande autorisée ; éviter les refactors annexes.
- Choisir les validations pertinentes au changement. Pour une petite correction
  de présentation, une vérification ciblée suffit ; ne pas imposer un entraînement
  ou une revue systématique. Les tests de compatibilité et de reprise ci-dessus
  restent obligatoires lorsqu'ils s'appliquent.
- Rapporter brièvement ce qui a changé, les vérifications effectuées et les
  limites restantes. Ne pas présenter une capacité implémentée comme une
  hypothèse scientifiquement validée.
