"""Daily model inference and prediction-file persistence."""

from __future__ import annotations

import warnings
from pathlib import Path

import pandas as pd

from .calendars import next_market_session
from .config import RStockConfig
from .evaluation import binary_predictions
from .features import prepare_prediction_row
from .persistence import iter_model_metadata, load_booster


def survey_symbols(survey: pd.DataFrame) -> list[str]:
    columns = sorted(
        (name for name in survey if name.startswith("V")), key=lambda name: int(name[1:])
    )
    values = pd.unique(survey[columns].to_numpy().ravel())
    return [str(value) for value in values if not pd.isna(value)]


def predict_saved_models(
    prepared: pd.DataFrame,
    survey: pd.DataFrame,
    config: RStockConfig,
    *,
    models_directory: Path | None = None,
    observed_dates_by_symbol: dict[str, pd.DatetimeIndex] | None = None,
) -> pd.DataFrame:
    """Predict each target's next observed or calendar-defined market session."""

    import xgboost as xgb

    survey_errors = survey.set_index("Set")["Err"].to_dict() if not survey.empty else {}
    rows: list[dict[str, object]] = []
    max_features = config.permutation_depth

    for model_path, metadata in iter_model_metadata(models_directory or config.models_path):
        outcome_column = f"{metadata.observation}.UPDW"
        if outcome_column not in prepared:
            warnings.warn(
                f"Skipping {metadata.set_name}; no observations for {metadata.observation}",
                RuntimeWarning,
                stacklevel=2,
            )
            continue
        observations = prepared[outcome_column].dropna()
        if observations.empty:
            continue
        as_of_date = observations.index.max()
        target_date = next_market_session(
            as_of_date,
            metadata.market_calendar,
            observed_dates=(observed_dates_by_symbol or {}).get(metadata.observation, ()),
        )
        current = prepare_prediction_row(
            prepared, as_of_date=as_of_date, target_date=target_date
        )
        missing = [name for name in metadata.predictor_columns if name not in current]
        incomplete = current[metadata.predictor_columns].isna().any(axis=None) if not missing else True
        if missing or incomplete:
            warnings.warn(
                f"Skipping {metadata.set_name}; predictors are missing or incomplete",
                RuntimeWarning,
                stacklevel=2,
            )
            continue
        booster = load_booster(model_path)
        matrix = xgb.DMatrix(
            current[metadata.predictor_columns], feature_names=metadata.predictor_columns
        )
        probability = booster.predict(matrix)
        predicted = int(binary_predictions(probability, config.prediction_threshold)[0])
        row: dict[str, object] = {
            "Date": target_date,
            "AsOfDate": pd.Timestamp(as_of_date).normalize(),
            "MarketCalendar": metadata.market_calendar,
            "Set": metadata.set_name,
            "Observation": metadata.observation,
        }
        for index in range(max_features):
            row[f"Feature{index + 1}"] = (
                metadata.features[index] if index < len(metadata.features) else None
            )
        row.update(
            {
                "BinaryPrediction": predicted,
                "BinaryResult": -1,
                "SuccessfulPrediction": 0,
                "Err": float(survey_errors.get(metadata.set_name, metadata.error)),
            }
        )
        rows.append(row)
    return pd.DataFrame(rows)


def append_predictions(current: pd.DataFrame, path: Path) -> pd.DataFrame:
    if path.exists():
        previous = pd.read_csv(path)
        for column in ("Date", "AsOfDate"):
            previous[column] = pd.to_datetime(previous[column], errors="raise").dt.normalize()
        return pd.concat([previous, current], ignore_index=True)
    return current.copy()


def write_predictions(predictions: pd.DataFrame, path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    predictions.to_csv(path, index=False)
