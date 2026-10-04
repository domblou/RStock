"""Scientific identity is independent of the ordered execution Set."""

import pandas as pd
import pytest

from rstock.combinations import (
    canonical_combination_id,
    canonical_combination_id_from_set,
    symbol_set_id,
)
from rstock.application.auto_promotion import AutoPromotionRunner


@pytest.mark.parametrize("predictors", [
    ("TMO",),
    ("TMO", "MMM"),
    ("TMO", "MMM", "CAT"),
])
def test_canonical_identity_ignores_predictor_order_at_each_depth(predictors):
    assert canonical_combination_id("VLO", "Up", predictors) == canonical_combination_id(
        "VLO", "Up", tuple(reversed(predictors))
    )


def test_canonical_identity_distinguishes_target_direction_and_members():
    identity = canonical_combination_id("VLO", "Up", ("TMO", "MMM"))
    assert identity == '["VLO","Up","MMM","TMO"]'
    assert identity != canonical_combination_id("VLO", "Down", ("TMO", "MMM"))
    assert identity != canonical_combination_id("TMO", "Up", ("VLO", "MMM"))
    assert identity != canonical_combination_id("VLO", "Up", ("TMO", "CAT"))


def test_ordered_execution_set_is_unchanged():
    first = symbol_set_id(pd.Series({"V0": "VLO", "V1": "TMO", "V2": "MMM"}))
    second = symbol_set_id(pd.Series({"V0": "VLO", "V1": "MMM", "V2": "TMO"}))
    assert first != second
    assert canonical_combination_id_from_set(first, "Up") == canonical_combination_id_from_set(second, "Up")


def test_historical_set_is_resolved_at_read_time_without_migration():
    assert canonical_combination_id_from_set("VLO<-TMO+MMM", "Up") == (
        canonical_combination_id_from_set('["VLO","MMM","TMO"]', "Up")
    )
    assert canonical_combination_id_from_set("unparseable", "Up") is None
    assert canonical_combination_id_from_set("unparseable", "Up", target="VLO", predictors='["MMM","TMO"]') == (
        canonical_combination_id("VLO", "Up", ("TMO", "MMM"))
    )
    assert canonical_combination_id_from_set(
        "unparseable", "Up", target="VLO", predictors="TMO + MMM"
    ) == canonical_combination_id("VLO", "Up", ("MMM", "TMO"))


def test_promotion_does_not_silently_choose_between_reordered_candidates():
    runner = AutoPromotionRunner.__new__(AutoPromotionRunner)
    runner._source_values = lambda: ({}, pd.DataFrame([
        {"Statut promotion": "Candidat", "Combinaison": '["VLO","TMO","MMM"]'},
        {"Statut promotion": "Candidat", "Combinaison": '["VLO","MMM","TMO"]'},
    ]))
    with pytest.raises(ValueError, match="Ambiguous candidate representations"):
        runner.source_candidates()
