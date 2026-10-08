# Optimisation Historique — lot 1

Implémentation du chargement et de la stabilité de la grille ; aucune modification du code de suppression. Les changements préexistants du workspace ont été conservés à l’identique.

## Implémentation

- Index en mémoire avec lectures différées : statuts et visibilité pour le catalogue ; stockage/configuration pour les filtres nécessaires ; résumés uniquement pour les lignes affichées et les relations consultées.
- Cache partagé et borné à 4096 fichiers. Identité du fichier vérifiée à chaque consultation (mtime, ctime, taille et inode), y compris entre deux instances de service. Projection des champs d’affichage après la désérialisation historique existante. Les objets retournés sont indépendants du cache ; une lecture concurrente instable n’est pas mise en cache.
- Aucune écriture d’index, de configuration ou de snapshot scientifique. Les pages détaillées et les comparaisons continuent à utiliser leurs API existantes.
- Interactions Historique isolées dans un fragment Streamlit sans timer. Bouton Actualiser. Le suivi actif et la réconciliation d’interruption conservent leurs fonctions existantes.
- Rendus identiques et acquittements de sélection : pas de reconstruction du tableau. La barre de recherche est conservée. Tri, focus et défilement conservés lors des mises à jour utiles ; sélection conservée entre les pages et retirée quand un filtre exclut le run.

## Rafraîchissements

Les sources inutiles confirmées étaient le rerun de toute l’application lors des interactions de la liste et la reconstruction du DOM à chaque message de rendu, même identique. La liste Historique n’appelle pas directement les fragments de suivi à deux secondes. Ces mécanismes de suivi n’ont pas été changés ; une cause supplémentaire propre à une session active n’est pas prouvée par cette analyse statique.

## Mesures

Corpus existant : 562 runs terminaux. Onglet expérimental, filtre stockage Complet, première page de 25 lignes (215 runs retenus). Trois répétitions par mode. Le benchmark compare l’ancien chemin de chargement conservé au nouvel index ; le cache applicatif est vidé pour chaque mesure à froid, sans vider le cache disque Windows.

| Mesure | Ancien chemin | Index à froid | Index en cache |
| --- | ---: | ---: | ---: |
| Temps médian | 3.294 s | 1.109 s | 0.631 s |
| Résumés lus | 373 | 25 | 0 |
| Données JSON lues | 154,16 Mo | 16,36 Mo | 0 Mo |

Gain : 2.97× à froid, 5.22× en cache. Le cache continue à vérifier les attributs des fichiers : zéro lecture JSON ne signifie pas zéro accès disque.

Ces temps couvrent l’index, les filtres, la pagination et le formatage des lignes. Ils excluent le navigateur, le réseau, les catalogues de modèles/univers et la réconciliation du dépôt. Le benchmark utilise un dépôt en lecture seule et compare les valeurs affichées pour les deux premières pages. Les durées varient avec la charge de la machine. Les données brutes sont dans benchmark.json.

Reproduction :

```powershell
python scripts/benchmark_history.py --output reports/history-lot1/benchmark.json
```

## Validation

298 tests réussis dans 14 fichiers (25,50 s). Couverture : index, cache, compatibilité historique, filtres et pagination, intégration Streamlit, grilles partagées et historique, analyses/validation, navigation, suivi Surveillance et application/jobs.

Le test Edge observe dix rendus identiques et vérifie l’absence de reconstruction des lignes, le maintien de la recherche et du curseur de saisie, de la sélection, du tri, du focus et du défilement. Il vérifie également une modification de statut et l’insertion d’une ligne.

Le test de comparaison préexistant attendait une limite de cinq runs alors que l’implémentation existante et l’interface autorisent six. Son assertion est alignée sur six, avec vérification du refus de sept ; le comportement de comparaison reste inchangé.

Commande de validation :

```powershell
python -m pytest tests/test_history_index.py tests/test_history_navigation_integration.py tests/test_history_grid_stability.py tests/test_history_ui.py tests/test_history_grid.py tests/test_history_analysis.py tests/test_history_validation.py tests/test_grid.py tests/test_grid_dataframe.py tests/test_grid_streamlit_integration.py tests/test_grid_migration_browser.py tests/test_streamlit_navigation.py tests/test_surveillance_refresh.py tests/test_application.py -q -o addopts= --basetemp=.pytest-history-lot1-complete
```
