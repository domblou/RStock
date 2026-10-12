# Comparateur logistique régularisé — diagnostic historique

Protocole adopté avant les fits : population externe complète d'EXP-010,
run `20261011T032201_17ece38f7b`, 3 800 candidats sur 20 cibles, Up uniquement.
Sept fenêtres de développement, 433 séances du 2024-07-12 au 2026-04-02.
Aucun Holdout, Forward, candidat supplémentaire, changement de configuration
applicative ou acquisition. Ce diagnostic n'est pas une validation indépendante.

L'estimateur minimise `mean(logloss train) + 0.05 * sum(beta²)` ; intercept
non pénalisé. Standardisation moyenne/écart-type train exclusivement, `ddof=0`,
écart-type nul remplacé par 1. Initialisation coefficients nuls et intercept
logit de la prévalence train ; aucun poids de classes. SciPy L-BFGS-B avec
gradient analytique, `maxiter=1000`, `gtol=1e-8`, `ftol=1e-12` ; aucune grille,
réessai de régularisation ou substitution après non-convergence.

Les colonnes, labels, masques incluant les labels Up/Down, frontières train/test
et identités sont ceux du snapshot et de `windows.csv` historiques. La constante
utilise la prévalence du même train. Les prédictions XGBoost sont réutilisées,
jamais réentraînées. Les pertes et AUC utilisent les fonctions existantes de
`rstock/evaluation.py`. Moyenne des candidats au sein d'une cible, poids égal
des 20 cibles ; AUC calculée par candidat, jamais sur un mélange de modèles.
Les trois tiers sont ceux du rapport apparié existant, descriptifs et dépendants.

## Résultats mesurés

| Modèle | Log loss | Brier | AUC moyenne par candidat |
|---|---:|---:|---:|
| Constante du train | 0.542410 | 0.178392 | 0.494782 |
| Logistique L2 | 0.546616 | 0.179530 | 0.505006 |
| XGBoost historique | 0.575665 | 0.187884 | 0.506343 |

La logistique réduit la log loss moyenne face à XGBoost de **0.029049** et le
Brier de **0.008354**, mais reste derrière la constante de **0.004206** en
log loss et **0.001139** en Brier. Log loss : meilleure que XGBoost sur 20/20
cibles et 3 772/3 800 candidats ; meilleure que la constante sur 1/20 cibles
et 681/3 800 candidats. Brier : meilleure que XGBoost sur 20/20 cibles et
3 750/3 800 candidats ; meilleure que la constante sur 3/20 cibles et
888/3 800 candidats. Ces effectifs ne définissent pas une règle de sélection.

| Tiers chronologique | Constante LL | Logistique LL | XGBoost LL |
|---|---:|---:|---:|
| 2024-07-12 → 2025-02-07, 145 séances | 0.493007 | 0.496974 | 0.528745 |
| 2025-02-10 → 2025-09-05, 144 séances | 0.577049 | 0.581807 | 0.612234 |
| 2025-09-08 → 2026-04-02, 144 séances | 0.557517 | 0.561412 | 0.586343 |

Le challenger simplifié améliore nettement les pertes de XGBoost dans ce
diagnostic, sans battre la référence constante en moyenne ni dans les trois
tiers. Cela distingue la complexité de l'estimation de la présence d'un
avantage probabiliste : ce calcul ne démontre pas cet avantage, ni l'absence
universelle d'information externe. L'interprétation et les décisions scientifiques
restent au directeur ; aucune adoption de modèle, qualification ou priorité
opérationnelle n'est effectuée par ce script.

## Conclusion scientifique et prochaine intervention

Avis du directeur scientifique : diagnostic complété, négatif face à la référence
constante. Ne pas adopter le challenger comme stratégie validée ; suspendre les
sweeps de modèles/régularisation et l'extension des combinaisons sous cette
formulation. L'amélioration face à XGBoost ne sépare pas les effets de la forme
linéaire, de la normalisation et de la régularisation. Les gagnants individuels
restent rétrospectifs, sans sélecteur validé.

Prochaine intervention proposée, non exécutée : examiner quel mécanisme causal
justifie que les rendements intraday J−1/J−2/J−3 prédisent l'open→close suivant.
Préparer une étude courte des mécanismes, informations manquantes et contraintes
de disponibilité avant l'ouverture, à partir des preuves existantes, avant tout
nouvel entraînement. Si aucun mécanisme défendable n'émerge, suspendre cette
formulation plutôt que tester un troisième modèle. Les roadmaps, registres et
paramètres opérationnels restent inchangés.

## Validations et coût réellement observés

26 600 fits exécutés et convergés, maximum 10 itérations, sans fallback ni
réessai. Gradient analytique vérifié par différences finies (erreur maximale
`6.0e-11`), objectif stable aux logits extrêmes. Les 26 600 contrats de fenêtres
ont été contrôlés ; aucune fenêtre train monoclasse (prévalences 0.103175–0.355556).
La constante concorde avec l'archive alignée (erreur maximale `8.3e-17`).
Log loss/Brier/AUC des 3 800 XGBoost concordent exactement avec le rapport
apparié antérieur. Les empreintes des sources historiques ont été vérifiées
avant/après ; sources inchangées. Aucun test applicatif ou entraînement XGBoost.

Durée mesurée entre manifeste figé et résumé final : **225.8 secondes**, incluant
contrôles, fits et exports ; les appels d'optimisation cumulent **13.02 secondes**.
Environ **47 Mo** d'artefacts sur disque. Le snapshot occupait 1.22 Mo ; la lecture
par blocs des prédictions historiques évite leur chargement intégral. La mémoire
maximale du processus n'a pas été mesurée. Aucun coût de service externe.

## Reproduction et artefacts

Depuis la racine : `python -B reports/simple_regularized_20261011/fit_compare.py`
effectue les contrôles sans entraînement ni écriture. Ajouter `--execute`
entraîne le protocole adopté et écrit uniquement dans ce dossier. Les checkpoints
terminés par cible sont réutilisés uniquement après vérification du manifeste et
de leurs empreintes ; une non-convergence arrête sans retry automatique.

- `manifest.json` : protocole et paramètres figés avant entraînement, versions,
  configuration historique et SHA-256 des sources.
- `external_<CIBLE>.npz` : probabilités logistiques float64, clés candidats
  ordonnées, dates communes et labels Up ; lecture avec `allow_pickle=False`.
- `external_<CIBLE>_audit.csv` : scaler, coefficients, nombres d'observations,
  statut/critère d'arrêt, itérations, gradient et temps par fit.
- `external_<CIBLE>_complete.json` : empreintes du checkpoint et du manifeste.
- CSV `candidate_metrics`, `target_metrics`, `overall_metrics`,
  `paired_candidate_metrics`, `window_metrics` et `*_period_metrics` : scores
  et différences appariées ; plus bas est meilleur pour log loss/Brier.
- `summary.json` : résultats, convergence, coût mesuré et intégrité avant/après.

Sources et limites : `reports/paired_predictors_20261011/` et
`reports/selection_value_20261011/`. Données et dates déjà consultées, candidats
et fenêtres dépendants, snapshots sans garantie fournisseur PIT. Aucun seuil
de succès, sélecteur nouveau, promotion ou preuve de rendement futur.
