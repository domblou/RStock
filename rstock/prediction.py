"""Daily model inference and prediction-file persistence."""

from __future__ import annotations

import warnings
from pathlib import Path

import pandas as pd

from .config import RStockConfig
from .evaluation import binary_predictions
from .features import prepare_prediction_row
from .persistence import iter_model_metadata, load_booster


def survey_symbols(survey: pd.DataFrame) -> list[str]:
    columns = sorted(
        (name for name in survey if name.startswith("V")), key=lambda name: int(name[1:])
    )
    values = pd.unique(survey[columns].to_numpy().ravel())
    return [str(value) for value in values if not pd.isna(value) and str(value) != "<NA>"]


def predict_saved_models(
    prepared: pd.DataFrame,
    survey: pd.DataFrame,
    config: RStockConfig,
    *,
    models_directory: Path | None = None,
) -> pd.DataFrame:
    """Predict the next calendar day with every persisted model bundle."""

    import xgboost as xgb

    current = prepare_prediction_row(prepared)
    prediction_date = (current.index[0] + pd.Timedelta(days=1)).date().isoformat()
    survey_errors = survey.set_index("Set")["Err"].to_dict() if not survey.empty else {}
    rows: list[dict[str, object]] = []
    max_features = config.permutation_depth

    for model_path, metadata in iter_model_metadata(models_directory or config.models_path):
        missing = [name for name in metadata.predictor_columns if name not in current]
        if missing:
            warnings.warn(
                f"Skipping {metadata.set_name}; missing predictors: {', '.join(missing)}",
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
            "Date": prediction_date,
            "Set": metadata.set_name,
            "Observation": metadata.observation,
        }
        for index in range(max_features):
            row[f"Feature{index + 1}"] = (
                metadata.features[index] if index < len(metadata.features) else "NA"
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
        previous = pd.read_csv(path, dtype=str)
        return pd.concat([previous, current], ignore_index=True)
    return current.copy()


def write_predictions(predictions: pd.DataFrame, path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    predictions.to_csv(path, index=False)
