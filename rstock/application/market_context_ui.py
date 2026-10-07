"""Read-only views of persisted market context diagnostics."""
from pathlib import Path
import pandas as pd

from .market_context import AXES, DIAGNOSTIC_MANIFEST
from .market_context_runtime import load_context_diagnostic


def render_context_diagnostic(st, output: Path, runs: Path):
    if not (output / DIAGNOSTIC_MANIFEST).exists():
        return
    with st.expander("Contexte de marché SPY — diagnostic descriptif", expanded=False):
        try:
            manifest, context, metrics, robustness = load_context_diagnostic(output, runs)
        except (ValueError, OSError, KeyError) as exc:
            st.warning(f"Diagnostic de contexte indisponible : {exc}")
            return
        if manifest.get("status") != "available":
            st.info(f"Diagnostic indisponible : {manifest.get('reason', 'données absentes')}")
            return
        st.caption(f"Protocole {'standard' if manifest['standard_protocol'] else 'personnalisé'} : {manifest['protocol_id']} · SPY ajusté · contexte à J−1")
        st.caption("Les terciles sont descriptifs et ne définissent pas des régimes économiques. Ces mesures ne modifient pas la qualification.")
        protocol = manifest["protocol"]
        st.caption(f"Tendance {protocol['trend_sessions']} séances · drawdown {protocol['drawdown_sessions']} · volatilité {protocol['volatility_sessions']}")
        axis = st.selectbox("Variable de contexte", list(AXES), key=f"context-axis-{output.parent.name}")
        timeline = context.copy()
        timeline["session_date"] = pd.to_datetime(timeline.session_date)
        st.line_chart(timeline.set_index("session_date")[[axis]])
        tables = [(manifest["stage"], metrics, robustness)]
        from .forward_diagnostic import _json, digest
        origins = {}
        reference = manifest.get("t0_reference")
        if reference:
            path = (runs / reference["path"]).resolve()
            if path.is_relative_to(runs.resolve()) and path.exists() and digest(path) == reference["sha256"]:
                origins = {model["canonical_combination_id"]: model.get("origin", "unavailable")
                           for model in _json(path).get("models", [])}
        # Reuse compact upstream aggregates, never read/recompute predictions in UI.
        for stage, reference in manifest.get("stage_references", {}).items():
            path = (runs / reference["path"]).resolve()
            if not path.is_relative_to(runs.resolve()) or not path.exists() or digest(path) != reference["sha256"]:
                st.caption(f"{stage} : agrégats T0 indisponibles ou modifiés.")
                continue
            try:
                other, _, table, summary = load_context_diagnostic(path.parent, runs)
                if other.get("protocol_id") == manifest["protocol_id"]:
                    tables.append((stage, table, summary))
                else:
                    st.caption(f"{stage} : protocole différent, comparaison indisponible.")
            except (ValueError, OSError, KeyError):
                st.caption(f"{stage} : agrégats indisponibles.")
        for stage, table, summary in tables:
            st.write(stage)
            if origins:
                table = table.assign(qualification_origin=table.model_key.map(origins).fillna("unavailable"))
                summary = summary.assign(qualification_origin=summary.model_key.map(origins).fillna("unavailable"))
            st.dataframe(table.loc[table.axis.eq(axis)], hide_index=True)
            if not summary.empty:
                st.dataframe(summary.loc[summary.axis.eq(axis)], hide_index=True)
        st.caption("AUC étayée : ≥30 observations et ≥10 dans chaque classe ; signaux étayés : ≥10. Robustesse : trois bandes étayées et au moins deux fenêtres en WF. Les fenêtres et épisodes ne sont pas indépendants.")
