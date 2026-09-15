## Compatibilité historique des configurations

Tout nouveau champ ajouté à `RStockConfig` doit définir explicitement
son comportement lorsqu’il est absent d’un snapshot historique.

- Ne jamais appliquer silencieusement un default moderne si cela change
  la sémantique d’un ancien run.
- Une valeur explicitement enregistrée dans le snapshot est toujours prioritaire.
- Les nouveaux runs utilisent les defaults actuels.
- Si l’absence historique d’un champ correspond à une valeur différente
  du default moderne, ajouter une compatibilité rétroactive explicite.
- Ajouter un test couvrant la désérialisation et/ou la duplication du run historique.

## Reproductibilité et checkpoints

- Un état persistant ne doit jamais être écrasé à partir d’un objet en mémoire potentiellement périmé.
- Avant toute écriture finale d’un manifest/checkpoint, recharger ou réconcilier l’état persistant si plusieurs composants peuvent l’avoir modifié.
- Les mécanismes de reprise doivent être couverts par des tests d’interruption et de réconciliation.

## Compatibilité historique des configurations

- Tout nouveau champ ajouté à `RStockConfig` doit définir explicitement son comportement lorsqu’il est absent d’un snapshot historique.
- Ne jamais appliquer silencieusement un default moderne si cela change la sémantique d’un ancien run.
- Ajouter un test de désérialisation ou de duplication pour toute compatibilité rétroactive.
