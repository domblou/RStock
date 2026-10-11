# Optimisation des dependances sans base de donnees

## Perimetre

Index persistant independant de l'Historique, stocke dans `.rstock-dependency-index/<identifiant-du-repertoire-runs>/index.json`. Aucune base de donnees, aucun nouveau champ de configuration. Les artefacts scientifiques restent la reference et ne sont pas reecrits par l'indexation.

L'index conserve les relations extraites de chaque JSON/CSV, sa signature de fichier et une version de schema. Chaque operation redetecte les fichiers des runs, de Production et des simulations. Seuls les fichiers nouveaux, modifies ou trop recents sont reanalyses. Les ajouts et retraits de fichiers dans les repertoires sont egalement verifies, independamment de la precision des horodatages Windows.

Les sources modifiees depuis moins de deux secondes sont relues et leur analyse n'est pas persistee. Une ancienne entree correspondante est invalidee. Les CSV parcourus partiellement ne sont jamais consideres comme completement indexes.

La publication est atomique et protegee par le verrou du graphe. L'etat persistant est recharge avant fusion pour eviter d'ecraser une publication intervenue depuis la creation de l'objet. Un index absent, incompatible ou corrompu est reconstruit depuis les sources. Une indisponibilite d'ecriture du cache degrade les performances sans autoriser une suppression sur la seule foi du cache.

## Integration et securite

La confirmation utilise les controles existants de statut, de workers, de propriete et d'empreinte du perimetre. Les dependances sont revalidees au moment de l'execution. La quarantaine, les journaux de reprise et le nettoyage physique sont conserves. Les mecanismes de suivi des jobs ne sont pas modifies.

Le bouton affiche un indicateur de verification, puis la confirmation dans le meme rendu Streamlit, sans rerun supplementaire de toute la page.

Des changements paralleles a la selection de la grille Historique etaient presents dans le workspace. Ils ont ete conserves; la validation ci-dessous couvre aussi leur integration avec les boutons de suppression.

## Mesures

Voir `benchmark-final.json`. Comparaison avec le fichier `run_delete.py` du commit `16f20d3fada3ac178e8cf6c7085ecc411b9a1628`, sur les memes sources locales. Le script mesure la decouverte complete des sources, l'analyse des dependances et la synchronisation de l'index; il ne supprime aucun run. Une mesure par scenario: ces valeurs sont indicatives, pas un percentile de production.

| Scenario | Duree | Lectures de fichiers | JSON/CSV reanalyses |
|---|---:|---:|---:|
| Avant | 48.49 s | 9348 | 9348 |
| Premier remplissage | 46.82 s | 9349 | 9348 |
| Apres rechargement du cache memoire | 4.71 s | 3 | 0 |
| Index reutilise | 4.61 s | 2 | 0 |

9 348 sources scientifiques (7 664 JSON, 1 684 CSV). Les 2 a 3 lectures residuelles incluent l'index derive et le registre Production. Aucun JSON/CSV scientifique inchange n'est reanalyse dans les scenarios de reutilisation. Gain observe: environ 10,5 fois, soit 90,5 % de temps en moins pour ce parcours complet.

Le premier remplissage conserve le cout d'une analyse complete. Le parcours des chemins et la verification des metadonnees restent necessaires a chaque operation. Ces temps ne mesurent pas le nettoyage physique des fichiers ni la duree totale de suppression d'un run.

## Validation

403 tests reussis, 1 ignore, en 33,33 secondes. Resultats JUnit dans `tests.xml`. Le test ignore necessite la creation de liens symboliques, indisponible dans cet environnement Windows.

Couverture: reutilisation apres rechargement du processus, reanalyse ciblee, nouvelles dependances JSON/CSV/checkpoints et Production/simulations apres apercu, modification de references existantes, corruption et incompatibilite de l'index, interruption de publication, fusion avec l'etat persistant courant, CSV partiels, mutation pendant lecture, ajout pendant parcours, invalidation et reconstruction, confirmation simple et multiple sans rerun supplementaire, regressions Historique/grille/suppression/reprise et suivi des jobs.

## Reconstruction et limites

Commande: `python scripts/rebuild_dependency_index.py --runs-root runs`.

La reconstruction forcee est a employer apres une restauration ou un import externe qui preserve deliberement toutes les metadonnees de fichiers tout en changeant leur contenu. L'index utilise des signatures de metadonnees, pas un hash integral du contenu a chaque verification; rehacher tous les fichiers annulerait le gain mesure.

L'index JSON est publie en entier lorsqu'il change. Cette solution evite la relecture des artefacts volumineux mais conserve un cout proportionnel au nombre de chemins et a la taille de l'index. Une base de donnees pourra etre reevaluee si ces couts deviennent mesurables a plus grande echelle; elle n'est pas requise pour les gains observes ici.
