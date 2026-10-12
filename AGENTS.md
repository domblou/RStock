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

- [scientific_roadmap.md](rstock/docs/scientific_roadmap.md) est la source de vérité pour les
  hypothèses, approches, techniques, expériences réalisées ou prévues, conclusions
  et priorités scientifiques. La priorité actuelle est la stabilisation de la
  découverte et de la sélection ; les régimes sont reportés.
- [rstock_roadmap.md](rstock/docs/rstock_roadmap.md) décrit l'évolution du logiciel.
- Charger les références techniques selon la mission, sans recopier leur contenu
  ni l'historique des conversations. Référencer les runs et rapports existants.
- Les registres et roadmaps sont une mémoire et un guide de priorités, pas une
  limite à la recherche scientifique indépendante. Le directeur peut rechercher
  au-delà des sources internes, remettre en question les approches retenues et
  proposer une révision justifiée. Une idée n'est ni ajoutée automatiquement à la
  roadmap ni transformée en expérience/développement : l'utilisateur décide de
  son adoption et de sa priorité, dans les autorisations actives.
- Certains documents contiennent des plans ou états historiques. Vérifier le
  chemin de code et les contrats applicables ; signaler les contradictions avant
  l'opération concernée, sans modifier silencieusement le protocole.

## Mise à jour continue de la documentation

Toute intervention qui modifie l'état fonctionnel, technique ou scientifique de
RStock doit également maintenir sa documentation à jour. Cette obligation couvre
les développements, les corrections fonctionnelles et les expériences scientifiques.

Exception prioritaire : une demande explicite d'analyse, vérification ou révision
sans modification n'autorise aucune écriture. Examiner les sources et présenter
conclusions/corrections recommandées sans modifier de fichier, registre ou roadmap.
Respecter les fichiers et répertoires désignés en lecture seule. Une lacune ne
transforme pas cette mission en édition ; l'obligation documentaire s'applique
uniquement lorsque la mission autorise les modifications, dans son périmètre.

- Avant de considérer une tâche comme terminée, vérifier et effectuer les mises
  à jour nécessaires des roadmaps, du guide utilisateur et de la documentation
  technique concernée. Ces mises à jour font partie de la tâche initiale et ne
  nécessitent aucune demande ni autorisation supplémentaire de l'utilisateur.
- Utiliser les sources officielles existantes : [rstock_roadmap.md](rstock/docs/rstock_roadmap.md)
  pour l'évolution fonctionnelle et technique, [scientific_roadmap.md](rstock/docs/scientific_roadmap.md)
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

## Stratégies, expériences et runs scientifiques

- Une stratégie est une approche scientifique ; une expérience évalue une question
  et peut comparer plusieurs stratégies ; un run est son exécution concrète avec
  données, configuration et paramètres figés. Ne pas créer une stratégie par réglage.
- Toute stratégie reçoit un identifiant `STR-XXX` et toute expérience un identifiant
  `EXP-XXX`, unique, stable et jamais réutilisé, même après rejet ou abandon.
  Conserver intégralement les identifiants techniques des runs. La normalisation
  initiale préserve l'ordre : stratégies `STR-001` à `STR-015`, expériences
  `EXP-001` à `EXP-018` ; attribuer ensuite un numéro inédit dans chaque registre.
- Les quatre documents de pilotage sont dans `rstock/docs/` :
  [roadmap applicative](rstock/docs/rstock_roadmap.md),
  [roadmap scientifique](rstock/docs/scientific_roadmap.md),
  [stratégies](rstock/docs/scientific_strategies.md) et
  [expériences](rstock/docs/scientific_experiments.md). Préserver leurs identifiants
  et leurs liens ; les runs et artefacts restent associés aux expériences.
- Avant de proposer une expérience, consulter les stratégies et expériences
  antérieures. Un manque d'archives ne justifie pas une répétition automatique :
  préciser hypothèse nouvelle, changement méthodologique ou validation supplémentaire.
- Après chaque investigation, mettre à jour l'expérience correspondante ; mettre
  à jour les stratégies si leur évaluation/statut change, et la roadmap scientifique
  si les décisions/priorités évoluent. Conserver les résultats positifs, négatifs et
  non concluants, les limites et les décisions de rejet/remplacement lorsqu'établies.
  Ces mises à jour restent dans le périmètre d'édition autorisé : une recommandation
  seule n'autorise aucun changement de priorité ou de statut documenté ; ces
  changements exigent une autorisation ou une tâche qui les prévoit explicitement.
- Utiliser les statuts définis dans chaque registre. Une stratégie implémentée ou
  retenue provisoirement n'est pas Validée. Une expérience non concluante n'est pas
  une expérience jamais exécutée. Signaler explicitement les réalisations historiques
  non confirmées sans inventer de run ou de conclusion.
- Une expérience contrôlée conserve les mêmes données, snapshot, cutoff résolu,
  calendrier et paramètres, sauf les variables explicitement étudiées. Identifier
  et documenter tout écart nécessaire ; vérifier cette comparabilité avant de
  conclure, y compris pour des variantes dérivées d'un même parent.
- Ne jamais présenter une décision méthodologique ou une fonctionnalité implémentée
  comme une preuve d'efficacité prédictive. Ne pas relancer une expérience non
  concluante sans hypothèse nouvelle, preuve manquante pertinente ou changement
  méthodologique justifié ; conserver aussi les décisions abandonnées.
- Un simple développement applicatif ne crée pas artificiellement une expérience
  scientifique. La maintenance documentaire n'autorise aucun lancement supplémentaire.

## Responsabilités et autorisations

Codex principal est directement responsable de toute la programmation, de
l'architecture, des données, des tests et de la qualité logicielle de RStock.
Le directeur scientifique est le seul agent spécialisé ; il reste responsable
des orientations, protocoles et validations scientifiques. Ces responsabilités
ne créent aucun agent actif. Les travaux logiciels ne sont pas délégués à un
agent ingénieur.

Profil spécialisé versionné : [.codex/agents/directeur-scientifique.toml](.codex/agents/directeur-scientifique.toml).
Adopter un rôle ne garantit pas le chargement natif de son profil personnalisé.
Sans profil natif, lire le TOML pertinent et appliquer `developer_instructions`
dans les limites des instructions et autorisations actives. Lorsque pertinent,
distinguer profil chargé nativement et instructions simplement lues ; ne pas
prétendre qu'un agent a été activé ou consulté sans intervention réelle.

Codex principal peut consulter le directeur pour les hypothèses, protocoles,
interprétations, sélection/qualification et décisions scientifiques lorsque
nécessaire, sans consultation systématique pour les corrections techniques.
Codex principal vérifie la faisabilité technique et les coûts des propositions
du directeur. Éviter les échanges circulaires et fournir une synthèse consolidée.
Ces règles ne garantissent ni routage automatique ni chargement natif ; sans
délégation disponible, appliquer le profil scientifique pertinent sans inventer
de consultation et signaler la limite seulement si elle affecte la mission.

Skills partagées : `$preparer-experience-rstock` pour préparer un protocole et
`$analyser-experience-rstock` pour interpréter les résultats existants, d'abord
dans l'application. Leurs sources sont dans `.agents/skills/`. Elles ne sont pas
requises pour une correction UI ordinaire et n'autorisent aucun lancement.

- **Directeur scientifique** : interlocuteur pour la vision, les hypothèses,
  protocoles, campagnes et interprétations. Maintient la roadmap scientifique,
  les conclusions et les critères de poursuite ou d'arrêt. Distingue résultats
  observés, hypothèses et preuves manquantes ; évalue bénéfice attendu et coût.
- **Codex principal** : maîtrise l'architecture, vérifie données,
  calendriers, causalité et reproductibilité. Prépare les calculs, propose les
  plans techniques, implémente et valide dans le périmètre autorisé. Signale les
  écarts entre protocole demandé et possibilités réelles avant leur exécution.
- Une mission d'étude uniquement produit analyse, faisabilité et plan technique,
  avec vérification technique par Codex principal si nécessaire, puis s'arrête avant implémentation.
  Si l'implémentation est déjà explicitement autorisée, préparer un plan proportionné
  et poursuivre dans ce périmètre sans redemander l'autorisation. Une décision est
  nécessaire pour un choix substantiel non couvert, changement de protocole, coût
  non autorisé ou écart bloquant ; une petite correction n'impose pas de revue scientifique.
- Une délégation transmet seulement objectif, question précise, profil pertinent,
  références nécessaires, contraintes scientifiques, périmètre autorisé et livrable.
  Référencer fichiers/runs sans recopier rapports volumineux ou historique complet.
  Ne pas multiplier les sous-agents si le travail se traite directement ; un nom
  de tâche ne garantit pas le chargement du profil.
- Lors d'une délégation, utiliser un nom d'agent stable correspondant à son rôle :
  « Directeur scientifique ». Présenter
  séparément le titre précis de la mission actuelle et la demande transmise.
  Éviter de nommer l'agent d'après une tâche ponctuelle, car ce nom peut devenir
  trompeur lors de sa réutilisation.
- Lorsqu'une demande est transmise à un agent spécialisé, toujours afficher
  à l'utilisateur le contenu de la demande transmise afin qu'il voie ce qui
  est communiqué, y compris pour les demandes de suivi. Restituer sa réponse
  à l'utilisateur, même si elle contient seulement une intention, une question
  ou une limite. Ne pas terminer l'échange au seul accusé de transmission.
  Si la réponse n'accomplit pas la demande, la relayer comme telle, puis poursuivre
  avec l'agent jusqu'au livrable attendu. Ne jamais laisser un retour reçu sans
  restitution.
- Ne pas déléguer une modification documentaire lorsque les décisions et le
  contenu sont déjà établis. L'agent principal effectue directement l'édition
  et les vérifications ciblées. Consulter un spécialiste uniquement si une
  nouvelle décision scientifique ou technique est nécessaire.
- Codex principal retourne une synthèse courte : faisabilité, solution proposée ou changements,
  validations, limites et décision requise. Le directeur replace la décision
  dans la roadmap. Éviter les écritures concurrentes sur les mêmes fichiers.
- Codex principal traite directement les demandes techniques.
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

- Avant de proposer des changements, Codex principal inspecte le code et les
  contrats actuels des mécanismes concernés : services, workflows, repositories,
  jobs, manifests, checkpoints, reprise, diagnostics et interface. Charger la
  roadmap applicative et les références spécialisées selon le besoin ; limiter
  l'exploration au périmètre utile et distinguer faits vérifiés et hypothèses.
- Une analyse technique ou proposition d'évolution fournit, lorsque pertinent,
  une solution concrète, cohérente et pérenne au-delà du diagnostic des écarts.
  Pour une évolution fonctionnelle ou architecturale, décrire le parcours
  utilisateur et le flux d'exécution de bout en bout, les entrées et résultats
  persistés, le suivi, les erreurs, les diagnostics et la reprise concernés.
  Distinguer existant, adaptations et créations nécessaires, avec chemins et
  contrats utiles ; présenter les compromis et validations pertinentes.
- Pour chaque nouvelle expérience ou investigation, évaluer l'approche :
  fonctionnalité RStock existante, nouvelle fonctionnalité intégrée ou script
  ad hoc, avec justification concise. Privilégier la réutilisation pour des
  besoins concrets de reproduction, comparaison et validation future ; réserver
  les scripts autonomes aux explorations ponctuelles de réutilisation incertaine.
  Préserver traçabilité et validation temporelle et éviter le couplage inutile
  à une hypothèse. Ne pas introduire de framework, abstraction ou mécanisme
  générique sans bénéfice concret justifiant son coût et sa maintenance.
- Proposer une réalisation incrémentale lorsque nécessaire, avec objectifs,
  dépendances, livrables et critères de validation par phase. Une question simple
  appelle une réponse ciblée, sans étude architecturale complète. Les orientations
  et critères scientifiques restent du ressort du directeur ; toute adoption
  et exécution respecte les autorisations actives.
- Étendre les services, repository, workers, leases, workflows et checkpoints
  existants ; ne pas créer de framework de jobs parallèle. Voir les contrats
  de [Jobs, lots et reprise durable](rstock/docs/job_orchestration.md).
- Toute grille de l'application réutilise le socle
  `rstock/application/grid.py::render_grid`, via `grid_dataframe.dataframe`
  pour les DataFrames ou l'adaptateur métier existant. Compléter le composant
  commun si nécessaire plutôt que créer un rendu indépendant.
- Préserver identifiants et sélection, tri sur valeurs brutes, formats, aides,
  pagination et actions. Aucun calcul scientifique dans le frontend.
  Référence actuelle : [grid_migration_report.md](rstock/docs/grid_migration_report.md) ;
  le [plan de standardisation](rstock/docs/grid_standardization_plan.md) contient
  aussi l'inventaire antérieur à la migration.
  Réutiliser le registre d'onglets et les vues partagées avec rendu paresseux.

## Estimation des interventions techniques

Pour chaque nouvelle intervention recommandée ou plan proposé, Codex principal
présente un petit tableau avec justification concise, proportionné à la demande :

- **Effort :** 1, 2, 3, 5, 8 ou 13 points relatifs, incluant développement et
  validation ; ne pas convertir ces points en jours ou heures de travail.
- **Durée de calcul machine :** fourchette en minutes ou heures, ou « aucun »
  sans traitement nécessaire ; la distinguer de l'effort humain.
- **Valeur scientifique attendue :** faible, moyenne ou élevée, selon l'incertitude
  résolue, y compris par un résultat négatif. Pour une tâche purement technique,
  indiquer « sans objet » et justifier sa valeur technique.
- **Confiance dans l'estimation :** faible, moyenne ou élevée.

Appuyer la durée machine sur les runs comparables, volumes, ressources et
architecture actuels ; citer les références et expliciter hypothèses et manques,
sans précision injustifiée ni benchmark implicite. Codex principal valide les
hypothèses techniques pour un développement significatif ou de nouveaux calculs
coûteux ; la valeur scientifique s'appuie sur le protocole ou l'analyse du directeur.
Préciser les principaux facteurs de variation du coût pour les interventions
importantes. À 13 points ou plus, tenter un découpage en phases plus petites pour
réduire le risque, avec objectif, estimation et critères de passage à la suite,
ou expliquer pourquoi aucun découpage utile n'est identifié. Une estimation
n'autorise aucune exécution.

## Données et validité scientifique

- Vérifier paramètres effectifs, snapshots, digests et sources physiques ;
  distinguer données figées et garanties PIT, live et backfills, Forward figé et
  rejeu historique. Préserver les contrats des dérivations sans source substituée.
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
  sur les mêmes données et paramètres. Voir [Jobs, lots et reprise durable](rstock/docs/job_orchestration.md).

## Portée et validation des modifications

- Tous les agents, y compris les agents spécialisés et délégués, enregistrent
  systématiquement les rapports et tous les fichiers générés pour une analyse
  ponctuelle (ad hoc) dans `reports/adhoc/`, de préférence dans un sous-répertoire
  identifiable par analyse. Cela inclut scripts ad hoc, données dérivées, tableaux,
  figures, exports et autres artefacts : ils ne font pas partie de l'application
  RStock. Ce répertoire est ignoré par Git. Cette règle ne déplace pas les archives
  existantes et ne remplace pas les emplacements officiels des runs, de la
  documentation ou des registres scientifiques. Elle n'autorise aucune écriture
  dans une mission en lecture seule ; transmettre ce périmètre aux agents délégués.
- Limiter les changements à la demande autorisée ; éviter les refactors annexes.
- Vérifier les effets de bord des scripts et services avant consultation ou
  exécution : une lecture seule n'autorise aucun traitement supplémentaire.
  Une préparation de boosters ou un rejeu historique peut entraîner des modèles.
- Choisir les validations pertinentes au changement. Pour une petite correction
  de présentation, une vérification ciblée suffit ; ne pas imposer un entraînement
  ou une revue systématique. Les tests de compatibilité et de reprise ci-dessus
  restent obligatoires lorsqu'ils s'appliquent.
- Rapporter brièvement ce qui a changé, les vérifications effectuées et les
  limites restantes. Ne pas présenter une capacité implémentée comme une
  hypothèse scientifiquement validée.
  Distinguer un test présent d'un test réellement passé.
