# MVP volatilité cible — configurations préparées, aucune expérience lancée

> Ce dossier décrit la préparation historique, qui n’avait créé aucun job.
> Le dérivé `20261011T130921_392c045368` a ensuite été exécuté. Ses résultats et
> limites sont consignés en EXP-012 du [registre scientifique](../../rstock/docs/scientific_experiments.md).

Source vérifiée : `20261011T115541_2a0e278563`, 20 titres, cutoff `2026-07-06`,
trois retards intraday, profondeur 2, eta 0,20, 20 tours fixes, seed 1234,
lambda 5, expanding initial 252, test/pas 63, holdout séparé 63 activé.
Tous les autres paramètres proviennent du snapshot source.

- `A_constant.json` : constante autonome ; Comparer reconstruira la référence
  alignée sur les apprentissages et blocs test communs aux variantes B/C.
- `B_control.json` : référence `target_only` sans volatilité, résultats historiques
  réutilisables si l'alignement des apprentissages/tests est confirmé.
- `C_volatility_21.json` : `target_only`, mêmes retards + volatilité cible N=21.
- `readiness.json` : vérification des empreintes historiques inchangées et
  de la disponibilité des observations pour chaque titre.

Les clôtures ajustées supplémentaires sont figées dans l'artefact d'entrée
référencé par C sous `data/metadata/target_volatility_sources/`.
Pour les 20 titres : 748 observations utilisables avant/après, aucun changement
du masque d'apprentissage/test. Aucun job créé ou lancé par la préparation.

La variable utilise N rendements logarithmiques valides se terminant à J−1,
écart-type `ddof=1` annualisé par `sqrt(252)`, sur des séances consécutives.
Les fichiers de configuration ne sont pas des résultats d'entraînement.
Les données sont rétrospectives (`retrospective_not_pit`) : leur empreinte
garantit la reproductibilité, pas une disponibilité historique point-in-time.

Pour lancer ultérieurement C depuis l'application : ouvrir le run source,
Créer une expérience dérivée, activer « Ajouter la volatilité du titre cible »,
laisser « Nombre de séances » à 21 et tous les autres paramètres inchangés.
Comparer ensuite A/B/C avec les diagnostics d'alignement existants. Ne changer
ni la définition ni N en fonction des résultats. Une validation sur de futurs
labels inconnus reste une étape distincte à planifier avant leur observation.

Préparation reproductible, sans entraînement :

```powershell
python -m scripts.prepare_target_volatility_mvp --output diagnostics/target_volatility_mvp_20261011
```
