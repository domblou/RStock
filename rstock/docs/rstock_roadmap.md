# RStock — Roadmap applicatif

Révision du 11 octobre 2026, fondée sur une inspection ciblée du code et des tests
existants, sans exécution métier. Les capacités ci-dessous sont implémentées ;
leur présence ne démontre pas leur efficacité prédictive ni une validation complète en usage.
Les protocoles, expériences et conclusions restent dans [scientific_roadmap.md](scientific_roadmap.md).
Mise à jour du 12 octobre 2026 : mécanisme d'analyses programmées intégré et vérifié
sur fixtures techniques ; aucune nouvelle expérience scientifique exécutée.
Correction UX du 12 octobre 2026 : détails WF autonomes/E2E avec deux boutons
et une liste d'analyses rapprochés sur une ligne ; formulaire affiché dès la sélection.
Correction technique du 12 octobre 2026 : soumission et reprise légères ; préparation
approfondie checkpointée dans le worker, contrat résolu séparé et aperçus bornés
des résultats.
Implémentation du 12 octobre 2026 : persistance des relations rares débloquée
(définition 3), sélection log loss/HAC/Holm, support et couverture complets,
évaluation temporelle et sélection checkpointée. Validation sur fixtures synthétiques
uniquement; aucune analyse scientifique réelle lancée et aucun seuil validé empiriquement.

## A. État actuel de RStock

Régression logistique L2 — phases 1 et 2 terminées le 12 octobre 2026 :
moteur à standardisation train uniquement, configuration figée, artefacts JSON
versionnés, WF ordinaire/streaming/lots et Holdout final. Les jobs WF utilisent
les workers, checkpoints et mesures existants; XGBoost reste le défaut historique.
Les tests couvrent convergence, annulation, persistance et reprise sans recalcul
des lots/enfants terminés. Aucune expérience scientifique réelle exécutée pour
valider cette intégration. Phase 3 terminée : sélecteurs algorithme/variables,
paramètres logistiques, dérivations WF/E2E, calibration XGBoost sans objet,
calibration des seuils/Holdout logistiques et Comparer avec constante alignée.
Phase 4 terminée : gel Forward et inférence sans réentraînement, promotion,
entraînement Production, observation et prédictions avec les contrats L2 sources.
Reprise des modèles Forward terminés et réconciliation d'un entraînement Production
publié avant une interruption du registre; intégrité des artefacts vérifiée.
Les opérations restent limitées aux variables externes. Validation temporelle
automatique logistique intégrée : offset 63, comparaison et revalidation forcée
avec contrat L2 et seuils sources, sans calibration XGBoost active ni changement
des critères scientifiques. Reprise des enfants terminés et promotion conditionnelle.
Vérification visuelle du formulaire réel
sur fixture isolée, sans services scientifiques.
Voir [types de modèles](predictive_model_types.md#logistique-phases-1-2).

Préfiltre multi-algorithmes intégré : XGBoost ou L2 indépendant du WF final,
dérivations sur snapshots/paires figés, référence constante Up sans classement,
audits par fenêtre et comparaison contrôlée des observations et frontières.
Découpage Holdout existant conservé, sans double exclusion; protection du Holdout
principal en consensus et refus de reprise sur une entrée modifiée. Archives
compatibles, comparaisons historiques sans audit signalées non vérifiables.
Aucune nouvelle expérience réelle ni conclusion d'efficacité prédictive.

Analyses scientifiques : interprétation déterministe obligatoire dans Résumé,
avec explication des métriques/règles, constats et limites/verdict. Persistance des
relations rares prise en charge, y compris absence de sélection et résultats
indéterminés. Texte versionné et empreinte des entrées gelés avec les sorties;
compatibilité des anciens bilans sans réécriture ni recalcul. Critères fournis
par le directeur; aucun nouveau résultat scientifique établi par cette fonction.

Historique : encadré commun des jobs actifs ajouté hors des onglets, sans filtre
de type et indépendant des filtres de grille. Rafraîchissement et contrôles existants
réutilisés; l'encadré d'Expériences est conservé.

Détails de run : navigation uniformisée avec Technique toujours en dernière
position, puis Ressources/Configuration/Fichiers/Logs imbriqués et Provenance pour
les pipelines et analyses scientifiques. Registre explicite des sous-onglets,
composants existants et chargement paresseux réutilisés, sans migration des runs.

Analyses scientifiques : phases réelles reliées à la télémétrie CPU/mémoire et aux
durées, progression par octets/lignes/fenêtres/relations/fichiers avec écritures
bornées. Reprises et annulations distinguées, publication seule à 100 %. Règle
permanente ajoutée dans AGENTS.md pour tous les runs. Validation sur fixtures
techniques, sans expérience scientifique ni reconstruction des anciennes mesures.

| Domaine | Capacités disponibles |
|---|---|
| Données et univers | Cache marché, calendriers, variables retardées ; univers manuels et sauvegardés, échantillons reproductibles. |
| Laboratoire | WF, préfiltre et consensus, XGBoost Up/Down, calibration des modèles et seuils, qualification, Holdout et chaîne E2E. Types de variables expérimentaux et volatilité cible disponibles en recherche, avec restrictions selon le parcours. |
| Validation et analyse | Forward à candidats, modèles et seuils figés après cutoff ; comparaisons, référence constante alignée, diagnostics de sélection, vieillissement et contexte de marché. |
| Analyses programmées | Catalogue et jobs distincts des dérivés, sources WF autonomes/E2E, Historique et détail commun, provenance et reprise. Formulaire intégré sous les actions ; persistance disponible avec coupure commune 60/40 ou nombre de séances, sélection Up log loss/HAC/Holm et évaluation à liste figée. Gestion des protocoles reportée. |
| Maintenance et présentation | Grille commune et onglets chargés à la demande ; télémétrie des tentatives ; purge, suppression avec dépendances et index reconstructible. |
| Exécution et traçabilité | Jobs asynchrones, parallélisme, batches, checkpoints et reprises ; configurations figées, historique, duplication et dérivation de runs. |
| Opérationnel | Promotion manuelle et promotion automatique optionnelle E2E, registre versionné, entraînement Up/Down, activation, observation (`Watching`), désactivation et retrait ; mise à jour marché, prédictions, screening et résultats réalisés. |
| Rendement | Suivi de qualité avec distinction live/backfill, transactions Up saisies manuellement, simulations à notionnel par signal et rejeux figés ou réentraînés. Pas de portefeuille à capital limité ni de courtage confirmé. |

Les investigations déjà exécutées et leurs limites sont dans le
[registre scientifique](scientific_experiments.md). Aucun avantage
prédictif stable n’est démontré par cet inventaire.

Les procédures sont dans le [guide utilisateur](user_guide.md) et
les contrats de [batching et reprise](job_orchestration.md).

## B. Priorités actuelles

Le MVP vise d'abord **Up : qualité supérieure à une référence simple et rendement
intrajournalier brut positif maintenus en Forward**. La rentabilité après coûts
reste un objectif ultérieur. Les priorités applicatives soutiennent les validations
scientifiques ; elles ne constituent pas de nouveaux protocoles.

| Ordre | Travail restant | État / résultat attendu |
|---|---|---|
| 1 | Stabiliser la découverte WF et la sélection des candidats. | En cours : vérifier la contribution des variables et la robustesse des choix ; cibler les corrections sur les écarts constatés. |
| 2 | Établir la généralisation et comprendre l'écart WF → Holdout → Forward. | À valider scientifiquement sur le parcours existant : avantage reproductible hors échantillon, populations comparables et périodes indépendantes des choix. |
| 3 | Maîtriser les effets de bord de la consultation. | À corriger : l'interface peut récupérer automatiquement des Forward pending ; une consultation ne vaut pas autorisation de relance. |
| 4 | Réconcilier les paramètres actifs. | À faire : décisions scientifiques retenues, defaults, snapshots effectivement utilisés et valeurs proposées dans l’interface ; distinguer les étapes, sans appliquer automatiquement les décisions historiques. |
| 5 | Faciliter l'organisation des expériences. | À développer : tags de runs, sans urgence supérieure aux validations fondamentales ; fonctionnalité non retrouvée dans les chemins inspectés. |

L'évaluation du compromis coût/qualité du préfiltre vient après la stabilisation
du WF. Le chevauchement possible entre sa sélection et les tests WF reste une
limite scientifique ouverte, pas un problème réputé résolu par le consensus.

## C. Prochaines évolutions

- **À développer — sweeps de dérivés à un paramètre**, secondaires aux problèmes
  fondamentaux de validité prédictive. La dérivation unitaire existe ; la saisie
  multivaleur et la soumission en lot sont absentes du parcours inspecté
  (`streamlit_app.py::_render_derived_creation`, `ExperimentService.create_derived`).
  Depuis le formulaire existant, saisir plusieurs valeurs séparées par `;`
  (ex. `0.50;0.55;0.60`) : un dérivé distinct par valeur, même parent et état de
  référence, seul le paramètre ciblé change. Préserver snapshot, cutoff résolu,
  héritage, provenance et empreintes. Avant soumission, afficher valeurs et nombre
  de dérivés ; valider les valeurs, dédupliquer et prévenir les soumissions
  accidentelles. Comparer ensuite avec les outils existants. Première définition :
  un seul paramètre variable, sans grille multidimensionnelle ni produit cartésien.
  Réutiliser l’orchestration existante ; aucune implémentation dans cette intervention.

- **Après stabilisation** : soutenir la redécouverte à plusieurs dates, la mesure
  de durée de vie et la comparaison figé/réentraîné avec les outils existants.
  Le renouvellement et le retrait automatiques selon la dégradation restent à définir.
- **À préciser selon le bénéfice attendu** : sélection d'univers par prix, volume
  et marché, bassins par profondeur ; extension à d'autres algorithmes et calibration
  généralisée. Ces parcours complets n'ont pas été confirmés dans le code inspecté.
- **Reporté** : exploration Down comme stratégie, régimes et modèles spécifiques,
  étude des coûts de transaction et garanties Point-in-Time. Les diagnostics et
  capacités existants sont conservés.
- **Évolutions opérationnelles ultérieures** : automatisation quotidienne planifiée,
  positions ouvertes et profits non réalisés, portefeuille et éventuel courtage.
  Le suivi manuel et les simulations actuels ne couvrent pas ces fonctions complètes.

Les décisions historiques et les valeurs effectives sont distinguées dans le
[tableau des paramètres scientifiques](scientific_experiments.md#parametres-correspondance).
Les divergences ne déclenchent aucune modification des configurations.

## D. Points ouverts et décisions à prendre

- L’aide UI des types expérimentaux annonce encore un périmètre WF plus restreint
  que le contrat WF/E2E ; correction de texte à traiter sans changer les règles.
- Quels seuils, horizon et référence simple permettront de conclure à la
  stabilisation ? À préciser dans le protocole scientifique, sans en inventer ici.
- Quelle indépendance reste-t-il aux périodes déjà utilisées pour sélectionner,
  calibrer ou ajuster les choix ? Un snapshot figé ne garantit pas la disponibilité
  historique des données ou de l'univers chez le fournisseur.
- Quand retirer ou renouveler un modèle, et avec quelles preuves ? La promotion
  automatique E2E existe ; elle ne résout pas la politique de vieillissement.
- Quelle priorité donner aux tags, aux bassins par profondeur et aux extensions
  opérationnelles une fois les validations fondamentales obtenues ?

L'audit initial du 11 octobre confirme des chemins de code et des contrats présents
dans le dépôt, sans exécution de tests ni parcours visuels. L'implémentation du
12 octobre a été vérifiée par des tests techniques ciblés sur des fixtures ; aucun
WF, E2E, Forward, entraînement ou calcul scientifique sur les archives n'a été lancé.
