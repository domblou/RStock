"""History selection protocol: session state is authoritative.

Consume the widget value *before* rendering. Browser events describe the visible
page, not the entire selection. Revisions reject replayed events; contexts reject
events from an obsolete page/filter. Server versions order render responses.
"""
from .history_ui import reconcile_history_selection


def prepare_history_selection(previous, state, event, visible_ids, filtered_ids):
    state = dict(state or {})
    visible_ids, filtered_ids = list(visible_ids), list(filtered_ids)
    revisions = dict(state.get("revisions", {}))
    if isinstance(event, dict):
        client, revision = event.get("client_id"), event.get("revision")
        ids = event.get("selected_ids")
        if (isinstance(client, str) and client
                and isinstance(revision, int) and not isinstance(revision, bool)
                and revision > revisions.get(client, 0)
                and event.get("context") == state.get("context")
                and isinstance(ids, list) and all(isinstance(item, str) for item in ids)):
            previous = reconcile_history_selection(
                previous, state["visible_ids"], ids, filtered_ids,
            )
            revisions[client] = revision
            state["ack"] = {"client_id": client, "revision": revision}
    if isinstance(previous, str):
        previous = [previous]
    allowed = set(filtered_ids)
    selected = list(dict.fromkeys(item for item in previous if item in allowed))
    if (visible_ids != state.get("visible_ids")
            or allowed != set(state.get("filtered_ids", []))):
        state["context"] = state.get("context", 0) + 1
    state.update(visible_ids=visible_ids, filtered_ids=filtered_ids,
                 revisions=revisions, version=state.get("version", 0) + 1)
    sync = {field: state.get(field) for field in ("context", "version", "ack")}
    return selected, state, sync
