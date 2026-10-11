# Diagnostic scientifique de sélection et régimes SPY

## Utilisation et activation

Dans les résultats End-to-End : **Diagnostic de sélection**, après
**Qualification promotion** et avant **Validation temporelle**. Les quatre
sections présentent l’entonnoir, la stabilité WF, la généralisation et les
contextes. Les tableaux et une partition par modèle sont chargés sur demande.
Le bouton **Exporter le diagnostic scientifique (.zip)** prépare le ZIP à la
demande, indépendamment des filtres et du modèle affiché.

Dans le formulaire de lancement End-to-End, **Capturer le diagnostic SPY et
les régimes de marché** est coché pour une nouvelle demande. Le snapshot
persiste ce choix. Le default de la configuration générale reste désactivé ;
les scripts doivent explicitement activer `market_context_enabled`.
Les étapes filles et le Forward utilisent la configuration scientifique
persistée de leur source. Les anciens snapshots ne sont jamais réécrits :
l’absence historique de `market_context_regime_version` signifie `None`, donc
l’ancien protocole sans classification économique. Une valeur enregistrée
reste prioritaire. Les dérivés résolvent les étapes physiques héritées.

Le panneau de contexte existant des résultats WF, Holdout et Forward propose
aussi les axes `regime` et `episode` lorsqu’ils sont présents. Les périodes
Forward proviennent exclusivement de `forward_period_metrics.csv`, avec les
bornes propres à chaque modèle : plein run, cumul, intervalle, +84, +126 et
fins personnalisées incluses lorsqu’elles sont persistées. Aucun résultat
Forward n’est attribué aux candidats non simulés.

## Protocole économique `rstock_spy_regimes_v1`

Les seuils sont des conventions descriptives RStock V1, jamais optimisées sur
les performances des modèles. SPY ajusté est la seule référence. La machine
à états est évaluée à chaque clôture, puis décalée intégralement à J−1 avant
son association aux observations de J. Les fenêtres continues 63/252/21 et
leurs terciles demeurent distincts ; la volatilité garde ses propres bandes.

La chauffe exige 252 séances XNYS consécutives connues. Une clôture absente
réinitialise l’état et la chauffe, sans remplissage. Hors épisode, le sommet
est le maximum des 252 dernières clôtures, avec la date la plus récente en
cas d’égalité. L’épisode commence à un recul de 5 % et fige ce sommet jusqu’à
sa clôture, même après sa sortie de la fenêtre glissante.

| État | Condition et priorité |
|---|---|
| Indisponible | Historique insuffisant ou interrompu ; priorité absolue |
| Sortie d’épisode | Recul depuis le sommet figé inférieur à 5 % ; fermer avant tout test de reprise |
| Reprise | Épisode actif, rebond d’au moins 5 % depuis son creux et rendement 21 séances strictement positif |
| Bear market | Épisode actif hors reprise, recul d’au moins 20 % |
| Correction | Épisode actif hors reprise, recul de 10 % à moins de 20 % |
| Repli | Épisode actif hors reprise, recul de 5 % à moins de 10 % |
| Forte hausse | Hors épisode, rendement 63 séances au moins +10 % et recul inférieur à 5 % |
| Normal / neutre | Aucun état précédent applicable |

La reprise, une fois déclenchée, persiste jusqu’à un nouveau creux ou à la
sortie d’épisode (hystérésis). Un nouveau creux annule la reprise et rétablit
la gravité correspondant au recul courant. Le type d’origine de l’épisode
est celui de son entrée ; son amplitude maximale, sa gravité maximale, son
sommet, son creux et sa durée sont conservés pendant la reprise. Une tolérance
numérique de 1e−12 protège les frontières 5/10/20 %. Chaque séance possède
une seule catégorie principale. La ligne de fermeture garde l’identité de
l’épisode pour documenter sa clôture, avec une catégorie hors épisode.

La durée compte les séances depuis le premier franchissement observé.
Un épisode déjà commencé avant l’historique acquis est censuré à gauche :
son sommet ancien et sa durée réelle ne peuvent être garantis. La persistance
empêche la perte du sommet après le début suivi ; elle ne reconstitue pas
les épisodes antérieurs à l’acquisition. Les clôtures ajustées téléchargées
rétrospectivement ne constituent pas une archive fournisseur point-in-time.
J−1 exclut les observations futures du calcul, pas les révisions ultérieures
du fournisseur. Les extensions conservent le préfixe et rejettent une révision
non uniforme des rendements historiques.

## Persistance et provenance

Le magasin commun reste celui du WF physique :
`runs/<wf>/diagnostics/market_context/<protocol_id>/revisions/<revision>/`.
Il conserve `spy_adjusted_snapshot.csv`, `market_context.csv`,
`market_episodes.csv` et `market_context_manifest.json`. Ce dernier documente
les règles, la chauffe, la référence de développement, les frontières figées,
la provenance, la révision parent et les empreintes. Le contexte n’est pas
recopié par étape. Les identifiants de protocole historiques sont préservés.

Chaque étape conserve `context_metrics.csv`, `context_robustness.csv` et
`context_diagnostic_manifest.json`. L’agrégation ajoute les régimes et épisodes,
le nombre de séances, la part d’exposition, la part des observations/signaux,
et le ratio part des signaux / part des séances de marché. Le nombre
d’épisodes est descriptif et ne prouve pas leur indépendance.

`runs/<e2e>/results/` contient :

- `selection_funnel.csv` : effectifs, unités et conversions par étape ;
- `selection_rejections.csv` : motifs chevauchants et motifs seuls ;
- `selection_candidates.csv` : population WF, rangs, décisions, métriques initiales et dispersion ;
- `selection_wf_windows.csv` : distributions par fenêtre et population ;
- `selection_context_population.csv` : médianes descriptives des métriques modèle/fenêtre par contexte ;
- `selection_history_composition.csv` : séances historiques distinctes et épisodes par contexte ;
- `selection_details/<empreinte-identité>.json` : valeurs du candidat, fenêtres et contextes, à lecture différée ;
- `selection_diagnostic_manifest.json` : sources vérifiées, filiations, critères, couverture et limites ;
- `selection_forward_index.json` : index léger réconcilié des Forward associés.

Chaque Forward conserve `selection_generalization.csv`,
`selection_t0_associations.csv`, `selection_forward_context.csv`,
`selection_forward_composition.csv` et `selection_generalization_manifest.json`.
Les identités canoniques sont appariées aux snapshots existants ; origines
normal/common/additional/removed et références T0 restent celles du diagnostic
Forward existant. La comparabilité vérifie le protocole, le magasin SPY,
l’empreinte de la référence, ses dates et toutes les frontières statistiques.
Un protocole identique seul ne suffit pas. Une incompatibilité est signalée,
pas résolue par une nouvelle calibration.

Les calculs se font en post-traitement hors rendu Streamlit. Les gros CSV de
prédictions sont parcourus par blocs et groupes de modèles contigus ; aucun
doublon de prédictions n’est persisté. Un ancien CSV réordonné avec des groupes
non contigus est explicitement refusé. Les agrégats historiques sans les
nouveaux champs restent lisibles ; leur exposition absente reste indisponible.
La publication utilise les mutex et écrit le manifest en dernier, après
revérification des sources et relecture de l’état persistant. Une interruption
ou une partition manquante est réparée à la reprise. Les échecs diagnostiques
sont signalés sans invalider les résultats scientifiques d’origine.

## Lecture scientifique et limites

Le préfiltre compte des couples cible/prédicteur ; le WF compte des
combinaisons. Ces populations ne doivent pas être additionnées. Le Holdout
est une évaluation, sa décision de passage appartient à la qualification.
Les motifs chevauchants ne sont pas présentés comme une cascade fictive :
le compte « motif seul » mesure les rejets exclusivement attribuables à ce
motif dans les décisions persistées, sans recalcul contrefactuel de sélection.

La part des bonnes fenêtres WF utilise le repère descriptif AUC > 0,5,
uniquement parmi les AUC disponibles ; elle ne remplace pas les critères
de qualification enregistrés. Le graphique distingue les décisions WF et
les décisions de qualification.

Les AUC par contexte sont étayées à partir de 30 observations dont 10 par
classe ; précision et rendement exigent 10 signaux. Les valeurs brutes restent
disponibles dans les détails, avec indicateurs de support. Les graphiques
agrégés masquent les valeurs insuffisamment étayées. La robustesse des terciles
exige trois bandes étayées et deux fenêtres WF par bande ; celle des régimes
exige au moins deux régimes observés, tous étayés, avec le même minimum de
fenêtres. Un seul régime observé ne démontre pas une généralisation aux autres.

WF initial, développement calibré, Holdout de qualification et Holdout
comparable gardent des règles de signal et des définitions propres.
Les deltas Forward existants utilisent le Holdout comparable. Les médianes
modèle/fenêtre ne sont pas des métriques recalculées sur des observations
fusionnées. Les intervalles, cumuls et plein run ne sont pas additionnés.

Les corrélations de rang T0/Forward sont exploratoires, sans p-value ni
recommandation automatique. Elles exigent au moins 10 modèles appariés et
des variables non constantes. Les fenêtres chevauchantes, modèles partageant
des cibles et épisodes temporels ne sont pas indépendants. Ni une AUC élevée,
ni une concentration ajustée des signaux ne démontre une causalité. Les
performances Train non persistées, la généralisation Forward des candidats
non simulés et les conditions de marché jamais observées restent inconnues.
Une preuve de biais de sélection ou l’effet causal d’un critère nécessiteraient
un protocole prospectif ou des simulations supplémentaires, hors de ce travail.

## ZIP compact et performance

L’export réutilise `_Evidence` et la publication ZIP du module Forward existant.
Il conserve les octets originaux et chemins sous `runs/<id>/`, déduplique les
révisions SPY communes et ajoute `export_manifest.json` (tailles, empreintes,
sources, absences). Il inclut les agrégats, décisions, fenêtres, classements,
petits artefacts du préfiltre, seuils et métadonnées des étapes/ancêtres/Forward.
Il exclut prédictions brutes, observations Forward brutes, caches, snapshots
binaires, boosters et partitions UI déjà représentées dans les agrégats.
Le volume dépend surtout des agrégats modèle/fenêtre/contexte et des fenêtres
WF ; il n’est pas fixé arbitrairement. Aucun recalcul ni téléchargement à l’export.

L’UI lit quatre tableaux compacts au premier rendu, met les lectures vérifiées
en cache et charge les candidats/détails sur demande. Seuls les tableaux du Forward choisi sont chargés. Le ZIP est différé au
clic. Le test `test_streamlit_5000_candidates_performance` utilise le vrai
moteur Streamlit AppTest avec 5 000 candidats synthétiques, sans entraîner ni
simuler. Mesures du 8 octobre 2026 sur ce poste : premier rendu 0,154 s,
médiane de cinq rerendus 0,050 s, ouverture des détails 0,070 s, changement de
modèle 0,058 s ; six lectures CSV au total, 30 accès au cache. Une mesure à
froid du chargement initial de Streamlit a donné 0,711 s. Ce sont des temps
serveur locaux, sans réseau ni navigateur ; ils ne garantissent pas la latence
de volumes historiques arbitraires ni le coût du post-traitement scientifique.

La validation utilise uniquement des séries et artefacts synthétiques ainsi
que des lectures de formats existants. Aucun End-to-End ni Forward nouveau
n’a été lancé pour développer ou tester cette fonctionnalité.


Les acquisitions SPY et leurs divergences sont désormais archivées avant
validation. L’acquisition E2E unique, les contrôles de précision et le
comportement non bloquant sont décrits dans
[le protocole de contexte](market_context_protocol.md#acquisition-unique-et-extensions-auditables).
