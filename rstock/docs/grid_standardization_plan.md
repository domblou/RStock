# Standardisation des grilles RStock

## Périmètre et état

Inventaire par analyse AST de `rstock/application/streamlit_app.py` et recherche dans
le dépôt : **82 appels `st.dataframe`**, dont **13 avec sélection**, et la grille
principale Historique. Aucun appel `st.data_editor` ou `st.table` dans l'application.
Les autres tableaux restent interactifs au sens du tri et du scroll natifs.
Plusieurs appels se trouvent dans une même vue ou une fonction réutilisée : ces
chiffres comptent les points de rendu, pas les écrans distincts.

Seul le socle de la grille Historique a été consolidé. Aucune autre grille n'a été
migrée. Les configurations scientifiques, populations, filtres et actions restent
hors du composant commun.

## Composant commun

`rstock/application/grid.py::render_grid` est l'API de présentation commune.
`history_grid.py` conserve les résolutions propres aux runs et un adaptateur
compatible avec le résultat de sélection existant. Le frontend reste dans
`history_grid_frontend/index.html` et garde son identité Streamlit pour éviter un
changement de packaging ou d'état de composant. Ce nom historique ne limite pas
son usage à Historique.

Contrat du socle :

- Enregistrements de présentation et colonne d'identifiant **unique et stable**.
- Colonnes déclarées : libellé, aide, largeur/min/max en pixels, alignement,
  clé de texte secondaire et clé de valeur formatée. Aucun HTML fourni par les données.
- Valeur brute pour le tri, valeur préformatée pour l'affichage. Null/non-fini → `—`;
  dates Python sérialisées en ISO. Formats métiers fournis par les adaptateurs.
- Sélection `none`, `single` ou `multi`; résultat avec IDs et positions **dans les
  données d'entrée**, indépendamment du tri. Clic, checkbox ou clavier sur la ligne.
- Sélection conservée entre pages internes; checkbox d'en-tête limitée à la page.
  Une nouvelle population réconcilie les IDs sélectionnés et réinitialise le tri/page.
- Pagination commune facultative (`page_size`); `None` signifie pagination externe.
  Historique conserve ses contrôles de pagination existants.
- Même thème Streamlit; police 13 px, sous-texte 11 px atténué; contenu de ligne
  34 px + padding vertical 10 px, en-tête 28 px + padding vertical 10 px.
- Bordures locales, en-tête sticky, scroll interne, hauteur plafonnée à 520 px
  par défaut; hauteur configurable. Aucun CSS global.
- Texte long tronqué, texte complet au survol; message d'état vide configurable.
- Mobile : padding horizontal réduit, scroll horizontal dans la grille, pagination
  pouvant revenir à la ligne. Pas de masquage automatique de données.

Exemple d'API (hors migration) :

```python
render_grid(records, columns=["Nom", "Score"], row_id="model_id", key="models",
            selection_mode="single", page_size=25,
            column_options={"Score": {"display_key": "score_display",
                                      "align": "right", "max_width": 120}})
```

## Écrans à migrer et ordre proposé

| Lot | Écrans/grilles | Adaptation nécessaire |
|---|---|---|
| 1 | Univers enregistrés | ID d'univers stable, sélection simple et actions actuelles |
| 2 | Historique : détail WF, qualification, combinaison, ressources/phases, batches, Forward Simulation | Identité métier par combinaison/phase/batch; événements au format commun; aides et formats |
| 3 | Historique : résultats Prefilter/consensus, calibrations, validation temporelle, comparateurs et exports affichés | Formats, colonnes dynamiques, gros artefacts; pagination adaptée |
| 4 | Rendement : trades réels | ID de trade stable; préserver sélection et actions |
| 5 | Surveillance : signaux, absence de signal, prédictions évaluées, en attente, audit et historique modèle | IDs techniques existants; préserver lien entre vue visible et données techniques; styles conditionnels |
| 6 | Modèles : grille principale, fenêtres de performance, comparaison promotion/actuel, signaux et exclusions | Mini-graphiques, styles directionnels, formats et actions |
| 7 | Simulation : trades et tableaux secondaires de duplication / anciens écrans | Formats monétaires/probabilités, vérification des branches historiques |

Les anciennes branches `_legacy_models_page` et `_history_page` sont incluses
dans l'inventaire; `_history_page` reste le point d'entrée de navigation et contient
des branches de présentation historiques. Vérifier leur accessibilité avant de
retirer ou remplacer un rendu. Aucun nettoyage de ces branches dans ce changement.

## Incompatibilités à résoudre avant chaque lot

1. **`column_config` natif** n'est pas directement transférable : convertir les
   aides, formats %, devises, précision et largeurs vers les options communes.
   Préserver les valeurs brutes pour le tri numérique/date; ne pas trier les textes
   formatés comme s'ils étaient les données scientifiques.
2. **Styler pandas** : styles de Surveillance/Modèles non interprétés par le socle.
   Ajouter une liste limitée de styles de cellule sémantiques, sans CSS arbitraire,
   puis vérifier le sens des couleurs et le thème sombre.
3. **`LineChartColumn`** (tendance 63 séances) et **`ProgressColumn`** (batches et
   pipeline) : rendus spécialisés absents du socle. Les porter avant les vues concernées;
   aucun remplacement silencieux par un texte moins informatif.
4. **Identités** : plusieurs écrans utilisent aujourd'hui des indices. Définir leurs
   clés métier avant migration; garder la correspondance avec les objets techniques.
   La vue Ressources lit directement `event.selection.rows` et exige une adaptation.
5. **Volume** : le tableau HTML ne virtualise pas les lignes. Activer la pagination
   pour les grandes populations; utiliser une pagination externe pour éviter de
   transmettre tout un gros CSV au navigateur. Mesurer sur des artefacts réels.
6. **Fonctions natives** : export CSV, recherche, copie et configuration des colonnes
   de Streamlit ne sont pas fournis par le composant actuel. Décider lesquelles
   conserver et les ajouter au composant commun avant de supprimer les rendus natifs.
7. **Accessibilité/mobile** : conserver la sélection au clavier, ajouter des libellés
   appropriés et vérifier le focus après rafraîchissement; les cellules tronquées
   doivent aussi être consultables sans survol sur écran tactile.

## Plan de migration après validation

1. Valider les fonctions natives à conserver, la pagination (25 lignes proposée,
   configurable) et les formats/couleurs spécialisés.
2. Compléter ces capacités une seule fois dans le composant et ses tests.
3. Migrer les lots ci-dessus par adaptateurs de présentation; conserver sources,
   filtres, données et actions. Aucun calcul métier dans le frontend.
4. Pour chaque lot, tester sélection → actions/détail, désélection, tri et pagination,
   changement de filtre/population, formats, états vides, thème sombre et mobile.
5. Vérifier les grandes populations et la navigation Streamlit réelle; adapter les
   tests qui supposent un `st.dataframe` à la nouvelle interface de sélection.
6. Retirer les anciens rendus seulement après validation des écrans concernés.

Tests du socle : sérialisation, formats séparés du tri, modes de sélection,
identifiants uniques, positions stables, pagination, état vide, scroll mobile;
test navigateur du frontend et non-régression de la grille Historique.

## Inventaire détaillé des points de rendu

Les lignes correspondent au fichier `streamlit_app.py` au moment de l'inventaire.
`Lecture` désigne un tableau natif avec tri/scroll mais sans sélection de ligne.

| Ligne | Fonction / vue | Interaction | Donn?es affich?es |
|---|---|---|---|
| 471 | `_render_locked_duplication_mode` | Lecture | `summary` |
| 2115 | `_render_walk_forward_promotion` | Sélection simple | `combinations` |
| 2295 | `_render_threshold_calibration_promotion` | Lecture | `sensitivity_summary` |
| 2337 | `_render_threshold_calibration_promotion` | Lecture | `choice_diagnostics` |
| 2473 | `_render_threshold_sensitivity_analysis` | Lecture | `sensitivity` |
| 2666 | `_render_qualification_decision_grid` | Sélection simple | `table` |
| 2693 | `_render_xgboost_calibration_selection` | Lecture | `xgboost_calibration_selection_display_table(table)` |
| 2737 | `_render_threshold_parameter_calibration_selection` | Lecture | `table` |
| 2795 | `_selected_combination` | Sélection simple | `table.drop(columns=['Eligible', 'Holdout confirmé', *detail_only], errors='ignore')` |
| 2979 | `_render_run_resources` | Sélection simple | `pd.DataFrame(phase_rows)` |
| 3091 | `_render_run_resources` | Lecture | `pd.DataFrame(subphase_rows)` |
| 3137 | `_render_run_resources` | Lecture | `batch_frame[['phase', 'batch_id', 'completed_items', 'duration_seconds', 'calculation_seconds', '...` |
| 3161 | `_render_standard_results` | Lecture | `ranking` |
| 3170 | `_render_standard_results` | Lecture | `origin_rows` |
| 3184 | `_render_standard_results` | Lecture | `pd.DataFrame([{'Occurrences': key, 'Candidats': count} for key, count in manifest['occurrence_dis...` |
| 3244 | `_render_standard_results` | Lecture | `exclusions.loc[:, columns].head(500)` |
| 3259 | `_render_standard_results` | Lecture | `pd.read_csv(path)` |
| 3295 | `_render_standard_results` | Lecture | `metrics` |
| 3441 | `_render_forward_temporal_results` | Lecture | `final_table` |
| 3461 | `_render_forward_temporal_results` | Lecture | `table` |
| 3487 | `_render_forward_temporal_results` | Sélection simple | `grid` |
| 3528 | `_render_forward_temporal_results` | Lecture | `table` |
| 3551 | `_render_forward_temporal_results` | Lecture | `table[columns]` |
| 3641 | `_render_walk_forward_summary` | Lecture | `prefilter_table` |
| 3716 | `_render_walk_forward_combinations` | Lecture | `diagnostic` |
| 3726 | `_render_walk_forward_combinations` | Lecture | `pd.DataFrame([{'Composante': label, 'Score / 100': selected.get(name, '-')} for label, name in su...` |
| 3771 | `_render_walk_forward_validation` | Lecture | `validation` |
| 3826 | `_render_walk_forward_batches` | Sélection simple | `table` |
| 4385 | `_render_pipeline_summary` | Lecture | `pd.DataFrame(rows)` |
| 4631 | `_render_candidate_identity_stability` | Lecture | `tables['common']` |
| 4639 | `_render_candidate_identity_stability` | Lecture | `lost_candidate_display_table(tables['lost'])` |
| 4653 | `_render_candidate_identity_stability` | Lecture | `tables['new']` |
| 4695 | `_render_temporal_validation` | Lecture | `temporal_validation_gate_table(gates)` |
| 4832 | `_render_temporal_validation` | Lecture | `table` |
| 4868 | `_render_temporal_validation` | Lecture | `pd.DataFrame(pipeline_stage_rows([stage]))` |
| 5042 | `render_results` | Lecture | `results.drop(columns=['AUC WF par fenetre'], errors='ignore')` |
| 5063 | `render_sensitivity` | Lecture | `table` |
| 5225 | `_render_end_to_end_comparison` | Lecture | `pd.DataFrame(comparison_rows)` |
| 5233 | `_render_end_to_end_comparison` | Lecture | `pd.DataFrame(details)` |
| 5249 | `_render_end_to_end_comparison` | Lecture | `pd.DataFrame([{'Étape': label, **{item.run_id: render(item) for item in analyses}} for label, ren...` |
| 5254 | `_render_end_to_end_comparison` | Lecture | `pd.DataFrame([{'Run': item.run_id, 'Validation globale': item.temporal_status or ('Non calculé' i...` |
| 5317 | `_render_end_to_end_comparison` | Lecture | `pd.DataFrame([{'Mesure': 'Qualification source', **{item.run_id: item.rejection_qualification_sou...` |
| 5329 | `_render_end_to_end_comparison` | Lecture | `pd.DataFrame([{'Run': item.run_id, 'Up évaluables': count((item.holdout_population or {}).get('co...` |
| 5343 | `_render_end_to_end_comparison` | Lecture | `pd.DataFrame([{'Run': item.run_id, 'Candidats': count(item.final_candidates), 'Candidate yield / ...` |
| 5378 | `_render_prefilter_comparison` | Lecture | `summary_grid` |
| 5394 | `_render_prefilter_comparison` | Lecture | `pd.DataFrame([{'Paramètre': field, **values} for field, values in differences.items()])` |
| 5401 | `_render_prefilter_comparison` | Lecture | `pd.DataFrame([{'Paramètre': field, **{run_id: profiles[run_id].get(field, ND) for run_id in run_i...` |
| 5418 | `_render_prefilter_comparison` | Lecture | `pd.DataFrame([{'Run ID': item.run_id, 'Candidats retenus': item.retained if item.retained is not ...` |
| 5438 | `_render_prefilter_comparison` | Lecture | `pd.DataFrame(candidate_rows)` |
| 5518 | `_render_run_comparison_view` | Lecture | `comparison_display_table(summary)` |
| 5538 | `_render_run_comparison_view` | Lecture | `quality[['Run', 'Delta dev→holdout', 'Date / heure']]` |
| 5561 | `_render_run_comparison_view` | Lecture | `top.drop(columns=['Eligible', 'Holdout confirmé'])` |
| 5564 | `_render_run_comparison_view` | Lecture | `validation` |
| 5574 | `_render_run_comparison_view` | Lecture | `differences` |
| 6226 | `_universes_page` | Sélection simple | `table` |
| 6313 | `_render_prediction_audit_details` | Lecture | `lagged_features` |
| 6317 | `_render_prediction_audit_details` | Lecture | `other_features` |
| 6323 | `_render_prediction_audit_details` | Lecture | `observations` |
| 6327 | `_render_prediction_audit_details` | Lecture | `other_observations` |
| 6861 | `_render_next_session_signals` | Lecture | `_styled_surveillance_table(table, ('P(Up)', 'Rendement moyen historique (63 séances)', 'Trades ga...` |
| 6878 | `_render_latest_session_results` | Lecture | `_styled_surveillance_table(view.table, ('P(Up)', 'Rendement de la séance (Open→Close)', 'P&L séan...` |
| 6917 | `_render_signals_card` | Sélection simple | `_styled_signal_table(displayed.signals.table)` |
| 6956 | `_render_signals_followup` | Sélection simple | `displayed.no_signal.table` |
| 7352 | `_evaluated_predictions_panel` | Sélection simple | `main_table` |
| 7365 | `_evaluated_predictions_panel` | Lecture | `build_predictions_view(displayed_view.pending, limit=len(displayed_view.pending)).table` |
| 7421 | `_render_watching_surveillance_section` | Lecture | `_styled_surveillance_table(upcoming.table, ('P(Up)', 'Rendement moyen historique (63 séances)', '...` |
| 7433 | `_render_watching_surveillance_section` | Lecture | `_styled_surveillance_table(latest.table, ('P(Up)', 'Rendement de la séance (Open→Close)', 'P&L sé...` |
| 7459 | `_render_surveillance_model_history` | Lecture | `history` |
| 7614 | `_returns_page` | Sélection simple | `display.loc[:, visible]` |
| 7694 | `_legacy_models_page` | Sélection simple | `table` |
| 8214 | `_render_model_quality_detail` | Lecture | `style_directional_columns(performance_windows_display_table(windows), ('Rendement moyen', 'Trades...` |
| 8227 | `_render_model_quality_detail` | Lecture | `style_directional_columns(model_phase_comparison_table(detail.observations, detail.series), ('Ren...` |
| 8299 | `_render_model_quality_detail` | Lecture | `style_directional_columns(comparison_table, ('À la promotion', 'Actuel', 'Écart'))` |
| 8324 | `_render_model_quality_detail` | Lecture | `style_directional_columns(signal_table[['Date', 'Période', 'Prob. Up', 'Prob. Down', 'Rendement',...` |
| 8335 | `_render_model_quality_detail` | Lecture | `excluded_observations_display_table(excluded)` |
| 8439 | `_models_page` | Sélection simple | `style_directional_columns(table, ('Rendement moyen', 'P&L cumulé', 'Drawdown')).map(winning_trade...` |
| 8518 | `_history_page` | Lecture | `[{'model_id': model.model_id, **model.holdout_metrics} for model in models]` |
| 8552 | `_history_page` | Lecture | `summary` |
| 8566 | `_history_page` | Lecture | `results.tail(100)` |
| 8568 | `_history_page` | Lecture | `predictions.tail(200)` |
| 8570 | `_history_page` | Lecture | `signals.tail(200)` |
| 8713 | `_render_simulation_results` | Lecture | `displayed_trades` |
| 5654 | `_history_runs_panel` / `render_history_grid` | Sélection multiple | Runs, lignée, univers, cutoff, dérivation |
