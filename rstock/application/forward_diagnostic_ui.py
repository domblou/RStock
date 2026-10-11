"""Read-only views of the committed Forward diagnostic and shared T0 reference."""

from __future__ import annotations

from pathlib import Path

import pandas as pd

from .forward_diagnostic import (
    ORIGINS, forward_comparison_table, load_forward_diagnostic, materialize_forward_diagnostic,
)


HELP = {
    "Précision": "Succès de la cible Up parmi les signaux combinés ; indisponible sans signal.",
    "AUC": "Discrimination sur toutes les observations évaluables ; nécessite les deux classes.",
    "Brier": "Erreur quadratique des probabilités sur toutes les observations. Plus faible est meilleur ; dépend aussi de la prévalence.",
    "Rendement": "Rendement intraday moyen des signaux, sans frais.",
    "Taux de signaux": "Signaux divisés par observations évaluables ; compare des durées différentes.",
    "Prévalence": "Fréquence de la cible positive parmi les observations évaluables.",
}


def _table(st, render, frame: pd.DataFrame) -> None:
    render(frame, hide_index=True, width="stretch", column_config={
        name: st.column_config.Column(help=HELP.get(name, f"{name} : diagnostic descriptif, sans règle de qualification."))
        for name in frame.columns
    })


def load_view(st, output: Path):
    try:
        reference, metrics, bins = load_forward_diagnostic(output)
    except (OSError, ValueError, KeyError) as error:
        st.error(f"Diagnostic Forward indisponible : {error}")
        return {}, pd.DataFrame(), pd.DataFrame()
    if not reference and (output / "forward_observations.csv").is_file():
        st.info("Diagnostic T0 → Forward non matérialisé pour ce run historique.")
        if st.button("Construire le diagnostic depuis les artefacts conservés", key=f"forward-diagnostic-build-{output.parent.name}"):
            try:
                materialize_forward_diagnostic(output)
                st.rerun()
            except (OSError, ValueError, KeyError) as error:
                st.error(f"Construction impossible avec les artefacts conservés : {error}")
    return reference, metrics, bins


def render_summary(st, render, reference, metrics) -> None:
    if not reference:
        return
    st.caption("Diagnostic T0 → Forward : référence de qualification ; le booster Forward a été réentraîné à T0. Les deltas économiques utilisent la règle Up/Down combinée.")
    models = reference["models"]
    available = sum(m["holdout_comparable"]["availability"] == "available" for m in models if m["forward_available"])
    st.caption(f"Référence Holdout comparable : {available}/{sum(m['forward_available'] for m in models)} modèles simulés. Les données absentes restent indisponibles.")
    if reference.get("parent_e2e_run_id"):
        rows = []
        for origin in ("common", "additional", "removed", "unavailable"):
            group = [m for m in models if m["origin"] == origin]
            if group:
                rows.append({"Origine": ORIGINS[origin], "Modèles": len(group),
                             "Forward": "Non observé dans ce dérivé" if origin == "removed" else "Simulé"})
        _table(st, render, pd.DataFrame(rows))
        st.caption("« Supplémentaire » signifie admis par le dérivé ; les paramètres modifiés et motifs sont conservés. Un candidat retiré ne possède aucune performance Forward dans ce dérivé.")
        st.caption("Un bon Forward supplémentaire est un indice de faux négatif du parent ; une mauvaise trajectoire admise est un indice de faux positif. Aucune conclusion automatique de qualification.")


def render_model(st, render, reference, metrics, bins, model_id: str, run_id: str) -> None:
    if not reference:
        return
    model = next((m for m in reference["models"] if m["source_model_id"] == model_id), None)
    if model is None:
        return
    st.subheader("Diagnostic T0 → Forward")
    st.caption(f"Origine : {ORIGINS[model['origin']]} · Identité scientifique : {model['canonical_combination_id']}")
    baseline = model["holdout_comparable"]
    wf = model["wf"]
    qualification = model["holdout_qualification"]
    kind = st.radio("Périodes du diagnostic", ("Depuis T0", "Par intervalle"), horizontal=True, key=f"diagnostic-kind-{run_id}-{model_id}")
    selected = metrics.loc[metrics["source_model_id"].eq(model_id) & metrics["period_kind"].eq("cumulative" if kind == "Depuis T0" else "interval") & metrics["horizon"].isin([21, 42, 63])]
    definitions = [("Précision", "precision", "AggregatePrecision", "Precision"),
                   ("AUC", "auc", "ROCAUCMedian", "ROCAUC"), ("Brier", "brier", None, None),
                   ("Rendement", "mean_return", None, "DirectionalReturnMean"),
                   ("Taux de signaux", "signal_rate", None, "SignalProportion"),
                   ("Prévalence", "prevalence", "MeanPrevalence", "Prevalence")]
    rows = []
    for label, name, wf_name, qualification_name in definitions:
        row = {"Mesure": label, "WF T0 (qualification)": wf.get(wf_name),
               "Holdout T0 (qualification)": qualification.get(qualification_name),
               "Holdout T0 comparable": baseline.get(name)}
        for _, value in selected.iterrows():
            h = int(value["horizon"])
            row[f"+{h}"] = value.get(name)
            row[f"Δ +{h}"] = value.get(f"delta_{name}")
        rows.append(row)
    _table(st, render, pd.DataFrame(rows))
    st.caption(f"Référence : {baseline['availability']} · Holdout : {baseline.get('observations', '—')} observations, {baseline.get('signals', '—')} signaux. Δ = Forward − Holdout comparable ; un Brier croissant est défavorable. L’AUC WF est une médiane de fenêtres.")
    if selected.empty:
        st.info("Aucun horizon +21/+42/+63 entièrement atteint.")
        return
    columns = ["horizon", "evaluated_observations", "excluded_observations", "signals", "positives", "negatives", "auc_status", "recall", "f1", "mean_probability", "signal_mean_probability", "probability_bias", "active_signal_sessions", "largest_session_signal_share", "signal_share_population", "target_signal_share_population"]
    _table(st, render, selected[[c for c in columns if c in selected]])
    st.caption("À +21, les effectifs peuvent être faibles. Une AUC est indisponible avec une seule classe ; une précision est indisponible sans signal.")
    horizon = st.selectbox("Horizon de calibration", selected["horizon"].astype(int).tolist(), key=f"diagnostic-horizon-{run_id}-{model_id}")
    forward_bins = bins.loc[bins["source_model_id"].eq(model_id) & bins["period_kind"].eq("cumulative" if kind == "Depuis T0" else "interval") & bins["horizon"].eq(horizon)].copy()
    t0_bins = pd.DataFrame(baseline.get("probability_bins", []))
    forward_bins["Période"] = f"+{horizon}"
    if not t0_bins.empty:
        t0_bins["Période"] = "Holdout T0"
    table = pd.concat([t0_bins, forward_bins], ignore_index=True)
    columns = ["Période", "lower", "upper", "observations", "signals", "mean_probability", "observed_frequency", "signal_precision", "signal_mean_return"]
    _table(st, render, table.reindex(columns=columns))
    import altair as alt
    st.altair_chart(alt.Chart(table.dropna(subset=["mean_probability", "observed_frequency"])).mark_line(point=True).encode(
        x=alt.X("mean_probability:Q", title="Probabilité moyenne", scale=alt.Scale(domain=[0, 1])),
        y=alt.Y("observed_frequency:Q", title="Fréquence observée", scale=alt.Scale(domain=[0, 1])), color="Période:N",
        tooltip=["Période", "observations", "signals", "mean_probability", "observed_frequency"]), use_container_width=True)
    st.altair_chart(alt.Chart(table.dropna(subset=["signal_mean_return"])).mark_bar().encode(
        x=alt.X("lower:O", title="Borne inférieure de probabilité"), y=alt.Y("signal_mean_return:Q", title="Rendement moyen sur signaux"),
        color="Période:N", xOffset="Période:N", tooltip=["Période", "signals", "signal_mean_return"]), use_container_width=True)


def render_population(st, render, reference, metrics, run_id: str) -> None:
    if not reference or metrics.empty:
        return
    st.subheader("Qualité T0 et trajectoires Forward")
    horizons = sorted(metrics.loc[metrics["period_kind"].eq("cumulative") & metrics["horizon"].isin([21, 42, 63]), "horizon"].unique())
    if not horizons:
        return
    horizon = st.selectbox("Horizon de comparaison T0", horizons, key=f"diagnostic-population-{run_id}")
    selected = metrics.loc[metrics["period_kind"].eq("cumulative") & metrics["horizon"].eq(horizon)].copy()
    selected["Origine"] = selected["origin"].map(ORIGINS)
    grouped = selected.groupby("Origine", dropna=False).agg(Modèles=("source_model_id", "size"),
        Signaux=("signals", "sum"), **{"AUC médiane": ("auc", "median"), "Brier médian": ("brier", "median"),
                                     "Précision médiane": ("precision", "median"), "Rendement médian": ("mean_return", "median")}).reset_index()
    _table(st, render, grouped)
    def share(column):
        value = selected[column].max()
        return "—" if pd.isna(value) else f"{value:.1%}"
    st.caption(f"Concentration des signaux : premier modèle {share('signal_share_population')} · première cible {share('target_signal_share_population')} · première séance {share('population_largest_session_signal_share')}.")
    feature = st.selectbox("Mesure T0", ["t0_auc", "t0_precision", "wf_auc_median", "t0_brier", "t0_mean_return"], key=f"diagnostic-x-{run_id}")
    outcome = st.selectbox("Mesure Forward", ["mean_return", "precision", "auc", "brier"], key=f"diagnostic-y-{run_id}")
    import altair as alt
    chart = selected.dropna(subset=[feature, outcome])
    st.altair_chart(alt.Chart(chart).mark_circle(size=70).encode(x=alt.X(f"{feature}:Q"), y=alt.Y(f"{outcome}:Q"), color="Origine:N",
        tooltip=["source_model_id", "target", "Origine", "signals", feature, outcome]), use_container_width=True)
    correlation = chart[feature].corr(chart[outcome], method="spearman") if len(chart) > 2 else float("nan")
    st.caption(f"Association descriptive de rang : {correlation:.3f} · {len(chart)} modèles comparables. Modèles corrélés et sélection à T0 : cette association ne démontre pas une causalité.")
    _table(st, render, selected[["source_model_id", "target", "Origine", "signals", feature, outcome, "delta_precision", "delta_auc", "delta_brier", "delta_mean_return"]].loc[:, lambda f: ~f.columns.duplicated()])
    removed = [m for m in reference["models"] if m["origin"] == "removed"]
    if removed:
        _table(st, render, pd.DataFrame([{"Identité": m["canonical_combination_id"], "AUC WF T0": m["wf"].get("ROCAUCMedian"),
            "AUC Holdout T0": m["holdout_comparable"].get("auc"), "Précision T0": m["holdout_comparable"].get("precision"),
            "Brier T0": m["holdout_comparable"].get("brier"), "Rendement T0": m["holdout_comparable"].get("mean_return"),
            "Signaux T0": m["holdout_comparable"].get("signals"), "Référence T0": m["holdout_comparable"]["availability"],
            "Forward": "Indisponible dans ce dérivé"} for m in removed]))
        st.caption("Comparer au Forward du parent via l’outil Comparer lorsque celui-ci existe ; aucune performance n’est imputée aux candidats retirés.")


def render_comparison(st, render, runs_root: Path, run_ids: list[str]) -> None:
    try:
        table = forward_comparison_table(runs_root, run_ids)
    except (OSError, ValueError, KeyError) as error:
        st.error(f"Comparaison Forward indisponible : {error}")
        return
    st.caption("Une ligne par modèle et horizon cumulatif +21/+42/+63. Identité scientifique commune ≠ même booster ; les dates T0 et les couvertures restent visibles.")
    columns = ["run_id", "cutoff_t0", "canonical_combination_id", "target", "origin", "horizon", "availability", "t0_availability", "evaluated_observations", "excluded_observations", "signals", "t0_auc", "auc", "delta_auc", "t0_precision", "precision", "delta_precision", "t0_brier", "brier", "delta_brier", "t0_mean_return", "mean_return", "delta_mean_return", "pnl", "drawdown"]
    _table(st, render, table.reindex(columns=columns))
    st.download_button("Exporter la comparaison Forward", table.to_csv(index=False).encode("utf-8"), file_name="forward_comparison.csv", mime="text/csv")
