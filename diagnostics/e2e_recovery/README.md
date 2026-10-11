Intervention sur la reprise de l'E2E `20261010T120328_c809cd1e25`.

Les modèles, les paramètres scientifiques, les snapshots et les artefacts des
runs historiques ont été conservés. Le run historique n'a pas été relancé.

Le serveur `multiprocessing.Manager` d'annulation est remplacé par un événement
partagé via `spawn`. Chaque worker confirme son initialisation avant le début
des tâches. L'attente de confirmation est limitée à 60 secondes. Les erreurs
et annulations arrêtent uniquement les processus appartenant au pool. La cause
exacte du blocage historique reste indémontrable sans sa pile d'exécution.

La reprise conserve désormais l'empreinte du JSON historique de chaque enfant.
Reconstruire un objet équivalent ajoutait des champs de sérialisation récents et
changeait l'empreinte des enfants XGBoost et paramètres de seuils. Le contrôle
avant reprise vérifie les snapshots et les empreintes des artefacts terminés.
Les probabilités de calibration sont maintenant sauvegardées par lots ; leur
ordre et leurs valeurs restent inchangés.

`readiness.json` rapporte une vérification sur les entrées figées réelles :
WF et XGBoost réutilisables, empreintes intactes, dataset vérifié, et 3 464 lignes
de probabilités exactement identiques entre calcul séquentiel et quatre workers
sur quatre combinaisons. Ces calculs de contrôle ne publient aucun résultat dans
les runs existants.

Le profilage du diagnostic de contexte porte sur huit combinaisons réelles
(3 464 observations). Les copies de colonnes inutilisées et les calculs répétés
de couverture ont été supprimés ; les conversions numériques sont partagées.
Le temps sous cProfile passe de 6,547 à 1,932 secondes, soit environ 3,4 fois
plus vite. Les métriques et synthèses sont comparées à la référence avant
optimisation avec une tolérance numérique de 1e-12. Cette mesure sur échantillon
ne constitue pas une mesure de la durée complète des 3 800 combinaisons.
Le diagnostic possède maintenant une phase visible, une progression et une
mesure de durée ; il reste synchrone dans le pipeline.

Pour la calibration interrompue, la durée observée est 22 802,38 secondes
(6 h 20 min 02 s). La détection intervient 24 160,81 secondes plus tard
(6 h 42 min 40 s). La durée historique de 46 963,19 secondes est conservée
dans les données persistantes. L'interface distingue ces mesures. Le temps
observé inclut l'attente bloquée ; il ne représente pas du temps CPU actif ni
l'heure exacte de disparition du processus.

Après chargement du code corrigé, utiliser « Reprendre le run » sur cet E2E.
Les étapes WF et XGBoost terminées seront validées et réutilisées ; le même
enfant de calibration de paramètres de seuils sera repris. Aucun lot de
probabilités n'était sauvegardé lors de l'interruption historique : cette
génération doit donc recommencer, avec ses paramètres figés.

Vérification reproductible, sans modifier les runs :

```powershell
python -m scripts.verify_e2e_recovery runs/20261010T120328_c809cd1e25 --output diagnostics/e2e_recovery
python -m scripts.profile_market_context runs/20261010T120330_7853894bf7 --sets 8 --output diagnostics/e2e_recovery/context_after --compare diagnostics/e2e_recovery/context_before/metrics.pkl
```
