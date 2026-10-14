# Registre des stratégies scientifiques

Une **stratégie** est une approche ; une **expérience** évalue une question ;
un **run** est une exécution avec données/configuration figées. Les runs et chiffres
restent dans le [registre des expériences](scientific_experiments.md) ; les priorités
sont dans la [roadmap scientifique](scientific_roadmap.md).

Ce registre réutilise l’audit des 350 runs et ses 18 investigations, sans nouvel audit.
15 approches proviennent de l'audit initial; STR-016 ajoute la prévision du
rendement continu, autorisée pour un diagnostic le 13 octobre 2026; STR-017
consigne la piste de momentum intraday à la demande de l'utilisateur.
Statuts autorisés : **À évaluer**, **En évaluation**,
**Retenue provisoirement**, **Validée**, **Rejetée**, **Remplacée**. Aucune n’est
ici déclarée Validée, Rejetée ou Remplacée sans preuve/décision correspondante.
Les variantes de réglage restent regroupées. En évaluation inclut une interprétation
inachevée de résultats passés ; cela n’annonce aucun run actif.

Les décisions ci-dessous conservent les orientations historiques ou la nécessité
d’examiner les preuves ; elles n’autorisent ni lancement ni changement de protocole.
Une préférence provisoire ne prouve pas la généralisation.

<a id="str-001"></a>

## STR-001 — Single Origin

**Statut : En évaluation.** Sélectionner à un seul cutoff ; référence de comparaison de sensibilité temporelle.

Expériences : [EXP-002](scientific_experiments.md#exp-002), [EXP-003](scientific_experiments.md#exp-003). Comparaisons historiques désormais rapportées comme réalisées : stabilité de population et décomposition J→J+1 en EXP-002/EXP-003 ; sources primaires non retrouvées.

**Décision et justification :** Poursuivre l’examen des preuves ; aucune supériorité établie.
**Nouvelle évaluation :** Sources retrouvées ou comparaison supplémentaire avec question nouvelle.

<a id="str-002"></a>

## STR-002 — Temporal Consensus

**Statut : Retenue provisoirement.** Réduire l’instabilité par occurrences dans les sélections finales de plusieurs origines.

Expériences : [EXP-002](scientific_experiments.md#exp-002), [EXP-003](scientific_experiments.md#exp-003), [EXP-007](scientific_experiments.md#exp-007). Préférence historique favorable rapportée par l’utilisateur ; consensus exécuté, mais preuves comparatives manquantes et Forward négatif.

**Décision et justification :** Retenir comme préférence méthodologique historique provisoire, sans avantage Forward validé.
**Nouvelle évaluation :** Comparaison indépendante ou contradiction mieux étayée ; pas de répétition faute d’archives.

<a id="str-003"></a>

## STR-003 — Temporal Stability

**Statut : En évaluation.** Agréger les rangs/scores de plusieurs origines, puis appliquer Top N et corrélation finale ; méthode distincte du consensus d’occurrences.

Expériences : [EXP-002](scientific_experiments.md#exp-002), [EXP-003](scientific_experiments.md#exp-003). Comparaisons antérieures contre Single Origin rapportées comme réalisées et non concluantes ; résultats comparatifs originaux à retrouver.

**Décision et justification :** Poursuivre la récupération des preuves, sans classement nouveau.
**Nouvelle évaluation :** Pas de répétition automatique : critère méthodologique nouveau ou preuve supplémentaire justifiée ; rechercher d’abord les sources.

<a id="str-004"></a>

## STR-004 — Géométrie WF : expanding / rolling

**Statut : Retenue provisoirement.** Comparer apprentissage croissant et fenêtre glissante pour stabiliser la validation.

Expériences : [EXP-003](scientific_experiments.md#exp-003), [EXP-004](scientific_experiments.md#exp-004). Expanding retenu historiquement ; comparaison chiffrée non retrouvée. Rolling reste disponible.

**Décision et justification :** Retenir expanding selon la décision existante ; ne pas déclarer rolling rejeté.
**Nouvelle évaluation :** Preuve comparative nouvelle pertinente pour la généralisation.

<a id="str-005"></a>

## STR-005 — Préfiltre AUC et stabilité

**Statut : En évaluation.** Réduire le coût de découverte par sélection univariée, seuils et déredondance ; variantes regroupées.

Expériences : [EXP-001](scientific_experiments.md#exp-001), [EXP-002](scientific_experiments.md#exp-002), [EXP-007](scientific_experiments.md#exp-007), [EXP-014](scientific_experiments.md#exp-014), [EXP-018](scientific_experiments.md#exp-018). Sélections réalisées, effets confondus et biais de chevauchement possible ; aucun compromis coût/Forward validé. Décision historique EXP-001 : variante Worst AUC 0,30 rejetée, plancher 0,35 conservé ; motif chiffré du rejet non retrouvé, sans rejet de toute la stratégie.

**Décision et justification :** Poursuivre après stabilisation du WF ; ne pas présumer que le consensus corrige le biais.
**Nouvelle évaluation :** Protocole distinguant population figée et nouvelle découverte, bénéfice attendu explicite.

<a id="str-006"></a>

## STR-006 — Sources de variables et richesse des combinaisons

**Statut : En évaluation.** Comparer cible seule, externes et combinaison, avec profondeur/univers contrôlés.

Expériences : [EXP-010](scientific_experiments.md#exp-010), [EXP-011](scientific_experiments.md#exp-011), [EXP-012](scientific_experiments.md#exp-012), [EXP-013](scientific_experiments.md#exp-013), [EXP-018](scientific_experiments.md#exp-018). Familles exécutées et désormais appariées en EXP-010 : aucun gain probabiliste moyen des externes/mixte face à la constante sur les 20 cibles examinées ; rares gagnants rétrospectifs non validés par un sélecteur. Richesse des profondeurs encore non évaluée à conditions égales.

**Décision et justification :** Poursuivre la contribution additionnelle ; aucune famille rejetée universellement.
**Nouvelle évaluation :** Analyse des artefacts communs ou validation indépendante justifiée.

Étude de formulation [EXP-019](scientific_experiments.md#exp-019) terminée le
12 octobre 2026 : les composantes overnight passées sont présentes dans les
snapshots mais omises des prédicteurs; intérêt scientifique plausible, efficacité
non évaluée. Le diagnostic logistique antérieur reste derrière la constante et
la sélection récente ne retient aucune relation. Statut **En évaluation** conservé.
L'ablation [EXP-020](scientific_experiments.md#exp-020), autorisée séparément et
terminée, dégrade la log loss face au contrôle et à la constante dans les trois
segments principaux. Aucun apport démontré pour les retards overnight étudiés;
pas de rejet universel de l'information overnight. Cette comparaison est close,
sans adoption de variables ni changement de cible/horizon ou de priorité.

Complément [EXP-018](scientific_experiments.md#exp-018), comparaison logistique
Up 1 % / 0,5 % sur les mêmes 20 titres : les deux définitions restent derrière
leur constante train propre sur les 174 séances principales et les trois segments.
Pas d'avantage moyen établi des retards externes avec la cible plus fréquente;
statut **En évaluation** conservé, sans rejet universel ni adoption de seuil.

<a id="str-007"></a>

Compl?ment [EXP-024](scientific_experiments.md#exp-024) termin? : ?tendre les
retards externes de J?3 ? J?5 avec SPY 21 d?grade la log loss face au contr?le
J?3 align? sur les p?riodes principale, ant?rieure et compl?te. Ne pas adopter
cette configuration; aucune extension automatique. Ni optimum J?3 ni rejet
universel d'une m?moire plus longue d?montr?s.


Compl?ment [EXP-025](scientific_experiments.md#exp-025) termin? : retirer J?3 et
conserver J?1/J?2 avec SPY 21 d?grade ?galement la log loss face ? J?1 ? J?3 sur
les m?mes supports. Crit?res d'amplitude non atteints; cette variante n'est pas
retenue. Les deux diagnostics de profondeur ne valident pas un optimum J?3.

## STR-007 — Probabilité constante alignée

**Statut : Retenue provisoirement.** Référence méthodologique, pas stratégie de trading à faire gagner : prévalence des labels du train commun, figée pendant le test ; mêmes observations et calendriers que les modèles évalués.

Expériences : [EXP-009](scientific_experiments.md#exp-009), [EXP-010](scientific_experiments.md#exp-010), [EXP-011](scientific_experiments.md#exp-011). Alignement réalisé ; référence devant les variantes cible seule testées.

**Décision et justification :** Retenir comme contrôle disponible ; la référence précise du protocole MVP reste à définir.
**Nouvelle évaluation :** Calendriers/masques incompatibles imposant un contrôle ou une autre référence motivée.

<a id="str-008"></a>

## STR-008 — Volatilité passée de la cible

**Statut : En évaluation.** Ajouter la volatilité ajustée disponible à J−1 sans modifier les combinaisons.

Expériences : [EXP-012](scientific_experiments.md#exp-012). N=21 exécuté : pertes Up légèrement dégradées, Down améliorées ; aucun confirmé Holdout.

**Décision et justification :** Poursuivre l’interprétation, sans retenir un gain Up ni optimiser N après observation.
**Nouvelle évaluation :** Appariement complet ou hypothèse nouvelle ; pas de relance identique automatique.

<a id="str-009"></a>

## STR-009 — XGBoost : fixe, régularisation et tours chronologiques

**Statut : En évaluation.** Régulariser et sélectionner les tours sur validation interne antérieure au test.

Expériences : [EXP-001](scientific_experiments.md#exp-001), [EXP-007](scientific_experiments.md#exp-007), [EXP-008](scientific_experiments.md#exp-008), [EXP-009](scientific_experiments.md#exp-009). Fixe favorisé descriptivement Up ; chronologique améliore Down sans battre la constante ; explorations non concluantes.

**Décision et justification :** Poursuivre selon contribution et coût ; pas de remplacement scientifique du fixe décidé.
**Nouvelle évaluation :** Preuve indépendante ou changement justifié de question/politique.

<a id="str-010"></a>

## STR-010 — Holdout puis Forward figé

**Statut : Retenue provisoirement.** Évaluer après cutoff avec candidats, paramètres et seuils figés, sans entraînement Forward.

Expériences : [EXP-005](scientific_experiments.md#exp-005), [EXP-007](scientific_experiments.md#exp-007), [EXP-016](scientific_experiments.md#exp-016). Parcours implémenté et exécuté ; Forward bruts négatifs sur les configurations étudiées.

**Décision et justification :** Retenir le parcours d’évaluation existant ; avantage prédictif non validé.
**Nouvelle évaluation :** Nouvelle population/protocole motivé et période indépendante des choix.

<a id="str-011"></a>

## STR-011 — Séparation causale de la sélection et des tests

**Statut : À évaluer.** Séparer le processus de découverte/sélection/qualification
des évaluations de généralisation; ce statut n'autorise aucune exécution.

Expériences : [EXP-014](scientific_experiments.md#exp-014). Chevauchement
préfiltre/WF identifié; impact quantitatif non mesuré.

**Décision utilisateur du 13 octobre 2026 :** le préfiltre et le WF font partie
du même processus de qualification; leur chevauchement est intentionnel et
accepté. La [convention permanente](scientific_roadmap.md#convention-scientifique-permanente--préfiltre-et-walk-forward)
définit les quatre conditions, la place du Holdout/Forward et les seuls motifs
de réouverture. Déplacer le préfiltre vers des données plus anciennes pour
supprimer ce chevauchement n'est pas nécessaire. Les recommandations antérieures
en ce sens sont remplacées; aucun protocole nouveau n'est autorisé.

<a id="str-012"></a>

## STR-012 — Modèles figés / réentraînés

**Statut : En évaluation.** Comparer maintien des modèles figés et mises à jour sur les mêmes modèles et dates.

Expériences : [EXP-016](scientific_experiments.md#exp-016), [EXP-017](scientific_experiments.md#exp-017). Simulations sauvegardées positives, rétrospectives et non complètement appariées.

**Décision et justification :** Poursuivre après les fondamentaux ; efficacité du réentraînement non établie.
**Nouvelle évaluation :** Population/provenance réconciliée et question de durée de vie validable.

Protocole futur convenu, **non exécuté comme comparaison contrôlée** : partir de
plusieurs vrais modèles de référence (idéalement 5–10 si disponibles), exactement
les mêmes modèles, versions, données et dates ; créer des variantes jumelées avec
au minimum contrôle figé et réentraînement. Cadences éventuelles : quotidien,
hebdomadaire, jamais. Même séances et observations disponibles Point-in-Time,
avec limites fournisseur tracées. Conserver séparément prédictions, signaux,
précision, AUC, rendement, P&L, drawdown et stabilité. Isoler le réentraînement de
la redécouverte : aucune substitution de candidats entre variantes. Les simulations
rétrospectives EXP-017 ne constituent pas encore ce protocole.

La durée de vie (EXP-016) étudie la dégradation et la persistance de l’avantage
sur **1–21, 22–42, 43–63, 64–84 et 85–126 séances**, ainsi que les conditions de
remplacement. Ces intervalles ne se confondent ni avec les cumuls aux jalons,
ni avec la redécouverte périodique ; les horizons sans données restent non évalués.

<a id="str-013"></a>

## STR-013 — Redécouverte périodique et renouvellement

**Statut : En évaluation.** Découvrir à plusieurs origines puis renouveler la population quand sa valeur décline.

Expériences : [EXP-006](scientific_experiments.md#exp-006), [EXP-016](scientific_experiments.md#exp-016). Gates parfois positifs sans candidats communs ; aucune preuve de renouvellement stable.

**Décision et justification :** Poursuivre après stabilisation ; garanties PIT et règle de remplacement ouvertes.
**Nouvelle évaluation :** Avantage initial établi et protocole de redécouverte indépendant.

Protocole futur EXP-006 : plusieurs cutoffs indépendants des décisions de sélection
et d’optimisation autant que possible, configuration stabilisée, snapshots figés,
même découverte/qualification et périodes Forward comparables. Mesurer séparément
nouveaux candidats, stabilité des populations, performance sur données futures,
durée de vie et renouvellement. Tracer les limites de disponibilité historique
fournisseur ; ne pas présumer des archives PIT. Stabilité d’appartenance,
prédictivité et rentabilité restent trois questions distinctes.

<a id="str-014"></a>

## STR-014 — Contexte et régimes de marché

**Statut : À évaluer.** Interpréter les performances selon contexte SPY et
évaluer son ajout comme variables continues communes; aucun modèle par régime
n'est adopté.

Expériences : [EXP-015](scientific_experiments.md#exp-015), diagnostic descriptif,
et [EXP-022](scientific_experiments.md#exp-022) / [EXP-023](scientific_experiments.md#exp-023),
ajout prédictif SPY 5 puis 21 séances / volatilité 21 séances sur la population figée du WF récent, autorisé le
13 octobre 2026. Aucune prédictivité par régime validée.

**Décision et justification :** EXP-022/EXP-023 terminés : petits gains récents, mais
critères de poursuite non atteints et ajout derrière les externes seuls sur
l'historique complet. Ne pas adopter les configurations SPY 5 ou 21 / volatilité 21;
aucune variante supplémentaire justifiée par ce résultat, sans rejet universel
du contexte. [Rapport](../../reports/adhoc/spy_context_5_20261013/README.md).
À 21, le gain additionnel satisfait le critère statistique interne (p Holm 0,023),
mais reste sous 1 % et non maintenu sur la période antérieure;
[rapport EXP-023](../../reports/adhoc/spy_context_21_20261013/README.md).
La variante [EXP-024](scientific_experiments.md#exp-024) J?1 ? J?5 avec le m?me
SPY 21 ne satisfait pas davantage les crit?res; ajout des retards J?4/J?5 non retenu.
[EXP-025](scientific_experiments.md#exp-025), seulement J?1/J?2 + SPY 21, reste
aussi derri?re J?1 ? J?3 sur les m?mes supports; aucun gain d'amplitude suffisant.
L'exploration de modèles distincts par régime reste reportée. Les rendements
SPY clôture→clôture figés utilisés ici ne sont pas assimilés aux prix ajustés
du diagnostic descriptif existant. Comparer externes + contexte aux externes,
au contexte seul et à la constante sur les mêmes supports.

<a id="str-015"></a>

## STR-015 — Qualification et calibration des seuils

**Statut : Retenue provisoirement.** Choisir les seuils en développement puis appliquer des règles figées au Holdout.

Expériences : [EXP-005](scientific_experiments.md#exp-005), [EXP-006](scientific_experiments.md#exp-006), [EXP-018](scientific_experiments.md#exp-018). Calibrations exécutées ; optimum universel et maintien Forward non démontrés. EXP-005 conserve les décisions historiques (robust signals 6, précision Holdout 55 %, minimum de qualification 25), avec leur périmètre incomplet et les divergences des snapshots/defaults.

**Décision et justification :** Retenir le contrat actuel ; conserver les choix historiques comme tels sans les appliquer rétroactivement ni les déclarer optimaux.
**Nouvelle évaluation :** Analyse contrôlée ou changement de cible autorisé, sans optimisation Holdout.

<a id="str-016"></a>

## STR-016 — Rendement intrajournalier attendu par régression ridge

**Statut : Rejetée.** Décision limitée à la configuration ridge/ensemble uniforme
évaluée en EXP-021, pas à toute prévision du rendement continu. Prévoir directement le rendement ouverture→clôture
avec les retards intrajournaliers externes; combiner uniformément les prévisions
par cible et appliquer une règle long/comptant figée.

Expérience : [EXP-021](scientific_experiments.md#exp-021). Diagnostic unique
autorisé le 13 octobre 2026; modèle, règle économique et critères figés dans le
[manifeste](../../reports/adhoc/ridge_returns_20261013/manifest.json) avant les fits.
La probabilité Up ≥ 1 % ne décrit pas l'amplitude des gains et pertes; tester
une moyenne conditionnelle est une hypothèse distincte, sans nouvelle information
ajoutée aux variables et sans efficacité présumée.

**Périmètre :** calcul ad hoc sur archives, aucun ajout au pipeline ou au catalogue
de modèles. La combinaison uniforme est fixée avant le calcul; elle ne sélectionne
pas les meilleures relations après observation. Les périodes déjà consultées
permettent un diagnostic historique, sans confirmation indépendante Forward.

**Résultat et décision :** diagnostic terminé; erreur quadratique derrière la
moyenne train dans les trois segments, avantage économique non significatif et
négatif après retrait du meilleur segment. Ne pas retenir cette configuration,
ne pas l'intégrer et ne pas poursuivre des variantes sans élément nouveau.
[Rapport et contrôles](../../reports/adhoc/ridge_returns_20261013/README.md).

<a id="str-017"></a>

## STR-017 — Momentum intraday

**Statut : À évaluer.** Hypothèse : le mouvement de SPY entre la clôture de J−1
et 10 h de J renseigne sur son rendement de 15 h 30 à la clôture de J, lors des
séances normales. Un ajustement tardif ou un rééquilibrage des investisseurs
pourrait relier ces mouvements; ce mécanisme n'est pas validé dans RStock.
Cette information de la séance en cours est absente des snapshots quotidiens
et distincte des retards overnight passés déjà étudiés négativement en EXP-020.

**Source scientifique :** Gao, Han, Li et Zhou, *Market intraday momentum*,
Journal of Financial Economics (2018), DOI `10.1016/j.jfineco.2018.05.009`.
[Article primaire](https://www.sciencedirect.com/science/article/pii/S0304405X18301351).
Les résultats historiques sur ETF, y compris hors échantillon, ne prouvent ni
le maintien de l'effet aujourd'hui ni une transmission de SPY vers les autres
cibles. Un test SPY seul vérifierait le mécanisme; il ne démontrerait pas une
relation entre symboles, objectif de RStock.

**Première intervention proposée :** étude ciblée des données horodatées,
historique accessible, fournisseur, prix d'entrée utilisables, calendriers
(notamment séances écourtées) et coût, puis protocole minimal avec comparateurs
moyenne train et exposition systématique sur la même période. Aucun test ne
peut utiliser le prix d'ouverture comme entrée après avoir observé un mouvement
qui n'est connu qu'à 10 h. Le chargement actuel normalise les prix par jour;
les snapshots disponibles ne permettent pas cette évaluation intraday.

| Effort de préparation | Calcul machine | Valeur scientifique attendue | Confiance |
|---|---|---|---|
| 2 points | Aucun entraînement ni calcul scientifique | Moyenne | Moyenne |

**Gouvernance :** ajout documentaire autorisé le 13 octobre 2026. Approche à
évaluer, sans priorité d'exécution adoptée, acquisition, entraînement ou
développement autorisé; aucune nouvelle expérience ou run créé. L'effort
chiffre seulement l'étude, pas une intégration ou une acquisition dont le coût
reste inconnu. Ne pas passer au test si les données, leur coût ou les prix
d'exécution ne permettent pas une évaluation crédible. Le choix d'une extension
inter-symboles n'est pas adopté automatiquement.

## Préférence historique pour le consensus

L’utilisateur rapporte que Temporal Consensus semblait préférable dans certaines
comparaisons historiques. Cette observation est conservée en STR-002/EXP-002 avec son
origine ; les mesures de stabilité sont désormais rapportées en EXP-002, mais les rapports,
paramètres, attribution aux méthodes et sources primaires restent à retrouver. Elle ne permet pas de déclarer Single Origin ou Temporal Stability
rejetés. Les exécutions consensus retrouvées et les résultats négatifs EXP-007 sont
distincts de cette préférence. Aucune preuve d’avantage Forward indépendant
n’est établie ; ne pas recréer une expérience uniquement faute d’archives.
