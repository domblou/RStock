# Registre des stratégies scientifiques

Une **stratégie** est une approche ; une **expérience** évalue une question ;
un **run** est une exécution avec données/configuration figées. Les runs et chiffres
restent dans le [registre des expériences](scientific_experiments.md) ; les priorités
sont dans la [roadmap scientifique](scientific_roadmap.md).

Ce registre réutilise l’audit des 350 runs et ses 18 investigations, sans nouvel audit.
15 approches sont identifiées. Statuts autorisés : **À évaluer**, **En évaluation**,
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

<a id="str-007"></a>

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

**Statut : À évaluer.** Réduire le biais en séparant données de découverte/sélection et évaluation.

Expériences : [EXP-014](scientific_experiments.md#exp-014). Chevauchement identifié ; réduction du biais et impact non quantifiés.

**Décision et justification :** Poursuivre la définition méthodologique, sans prétendre qu’une correction est déjà prouvée.
**Nouvelle évaluation :** Protocole autorisé qui mesure le biais et préserve une évaluation indépendante.

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

**Statut : À évaluer.** Interpréter les performances selon contexte SPY et éventuellement adapter les modèles.

Expériences : [EXP-015](scientific_experiments.md#exp-015). Diagnostic descriptif partiel ; aucune prédictivité par régime validée.

**Décision et justification :** Poursuivre ultérieurement, exploration de modèles par régime reportée.
**Nouvelle évaluation :** Fondamentaux stabilisés et hypothèse nouvelle ; diagnostics seuls insuffisants.

<a id="str-015"></a>

## STR-015 — Qualification et calibration des seuils

**Statut : Retenue provisoirement.** Choisir les seuils en développement puis appliquer des règles figées au Holdout.

Expériences : [EXP-005](scientific_experiments.md#exp-005), [EXP-006](scientific_experiments.md#exp-006), [EXP-018](scientific_experiments.md#exp-018). Calibrations exécutées ; optimum universel et maintien Forward non démontrés. EXP-005 conserve les décisions historiques (robust signals 6, précision Holdout 55 %, minimum de qualification 25), avec leur périmètre incomplet et les divergences des snapshots/defaults.

**Décision et justification :** Retenir le contrat actuel ; conserver les choix historiques comme tels sans les appliquer rétroactivement ni les déclarer optimaux.
**Nouvelle évaluation :** Analyse contrôlée ou changement de cible autorisé, sans optimisation Holdout.

## Préférence historique pour le consensus

L’utilisateur rapporte que Temporal Consensus semblait préférable dans certaines
comparaisons historiques. Cette observation est conservée en STR-002/EXP-002 avec son
origine ; les mesures de stabilité sont désormais rapportées en EXP-002, mais les rapports,
paramètres, attribution aux méthodes et sources primaires restent à retrouver. Elle ne permet pas de déclarer Single Origin ou Temporal Stability
rejetés. Les exécutions consensus retrouvées et les résultats négatifs EXP-007 sont
distincts de cette préférence. Aucune preuve d’avantage Forward indépendant
n’est établie ; ne pas recréer une expérience uniquement faute d’archives.
