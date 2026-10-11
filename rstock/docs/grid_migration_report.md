# Migration des grilles RStock

## Périmètre livré

Les 82 anciens appels `st.dataframe` de `streamlit_app.py` utilisent l'adaptateur
commun `grid_dataframe.dataframe`. La grille principale Historique continue à
utiliser son adaptateur de provenance, sur le même frontend.

Les modifications des écrans portent sur le point de rendu, les identifiants
d'univers/de transaction et la conservation de la pagination externe des Modèles.
Aucun calcul scientifique, donnée de run, manifest, snapshot ou filtre métier
n'a été modifié.

## Fonctions conservées

- Sélection simple ou multiple; événements compatibles avec les lectures
  `event.selection.rows` existantes; correspondance avec les positions d'entrée
  après tri/recherche/pagination.
- Identifiants métier explicites pour Univers et trades, `model_id` pour les
  populations de modèles uniques; empreinte du contenu pour les autres tables.
  Un ancien indice ne peut pas sélectionner une autre ligne après changement de population.
- Tri des valeurs brutes; affichage formaté distinct. Tous les formats utilisés
  par l'application sont pris en charge, notamment les pourcentages signés et devises.
- Recherche locale, export CSV des données brutes (colonnes masquées incluses),
  copie des lignes ou d'une plage de cellules, consultation du texte complet.
- Colonnes masquables, déplaçables et redimensionnables; réglages conservés lors
  des rafraîchissements de la même grille; plein écran selon la permission du navigateur.
- Styles conditionnels pandas : couleurs, fonds, poids et style de police,
  alignement et décoration. Les styles non pris en charge échouent explicitement.
- Barres de progression et mini-graphiques, avec les valeurs, bornes et couleurs
  du `column_config` existant. Les trous des séries restent des trous.
- **Toutes les aides de colonnes sont transférées verbatim**. Elles restent
  disponibles au survol de l'en-tête et via un bouton ouvrant le texte complet,
  accessible sur écran tactile.
- Thème Streamlit, dimensions et bordures communes, scroll interne, état vide,
  pagination interne de 25 lignes par défaut. Historique et Modèles conservent
  leurs contrôles externes de pagination.

Le frontend ne recalcule aucun indicateur. Il reçoit les valeurs de présentation
et les séries déjà produites par les sources existantes.

## Validation par lot

| Lot | Migration | Contrôles |
|---|---|---|
| 1 | Univers | Identifiants, sélection/désélection, affichage du détail Streamlit, navigateur et capture |
| 2 | Détails Historique, WF, qualification, ressources et batches | Sélection, actions existantes, aides, formats, progression; navigateur et capture |
| 3 | Résultats, calibrations, validation temporelle et comparateurs | Aides et formats, tests existants, navigateur et capture |
| 4 | Trades réels | Identifiant de transaction, sélection, tests existants, navigateur et capture |
| 5 | Surveillance et audit | Styles conditionnels, aides, tests existants, navigateur et capture |
| 6 | Modèles et qualité détaillée | Couleurs, tendances et bornes, pagination externe, aides; navigateur et capture |
| 7 | Simulation et tableaux secondaires/historiques | Formats, sélection et navigateur avec capture |

Les captures de validation utilisent des données représentatives et les vrais
adaptateurs/aides de l'application. Les tests Streamlit complètent ces contrôles
pour la navigation, les imports en contexte script, les sélections et les IDs de
plusieurs composants dans une même page.

Résultat de la suite ciblée UI/adaptateurs : **305 tests réussis**, incluant
les huit scénarios navigateur (sept lots + outils/thème sombre/mobile), les tests
Streamlit et la non-régression des vues Historique, Surveillance, Univers,
qualification, Forward, trades réels et Modèles.

## Limites techniques

La pagination limite le DOM; le transfert initial des données reste complet,
comme avec les anciens appels. Le composant ne virtualise pas les lignes.
Les fonctionnalités de presse-papiers et plein écran dépendent des permissions
du navigateur; la copie propose un texte sélectionnable en cas de refus.
Les types/numéro-formats spécialisés non utilisés actuellement et non pris en
charge sont refusés explicitement, jamais remplacés silencieusement.

## Fichiers principaux

- `application/grid.py` : contrat commun et normalisation de présentation.
- `application/grid_dataframe.py` : adaptateur DataFrame/Styler/column_config.
- `application/history_grid_frontend/index.html` et `grid_tools.js` : frontend commun.
- `application/streamlit_app.py` : appels migrés et identifiants métier.
- `pyproject.toml` : inclusion des assets HTML/JS dans les distributions.
- Tests `test_grid*` et adaptation des tests UI dépendant du rendu natif.
