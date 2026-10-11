"""Ordering and page reconciliation of the History selection protocol."""
from rstock.application.history_selection import prepare_history_selection


def event(state, revision, ids, client="browser"):
    return dict(client_id=client, revision=revision, context=state["context"], selected_ids=ids)


def test_run_2_then_run_1_is_confirmed_before_render_and_replays_cannot_undo_it():
    population = ["run-1", "run-2"]
    selected, state, _ = prepare_history_selection([], None, None, population, population)
    first = event(state, 1, ["run-2"])
    selected, state, sync = prepare_history_selection(selected, state, first, population, population)
    assert selected == ["run-2"]
    assert sync["ack"] == {"client_id": "browser", "revision": 1}
    second = event(state, 2, ["run-2", "run-1"])
    selected, state, _ = prepare_history_selection(selected, state, second, population, population)
    assert selected == ["run-2", "run-1"]
    for message in (first, second, None):
        selected, state, _ = prepare_history_selection(selected, state, message, population, population)
        assert selected == ["run-2", "run-1"]


def test_coalesced_rapid_clicks_deselection_and_remounted_browser():
    ids = ["a", "b"]
    selected, state, _ = prepare_history_selection([], None, None, ids, ids)
    selected, state, _ = prepare_history_selection(selected, state, event(state, 10, ids), ids, ids)
    assert selected == ids
    old = event(state, 9, ["a"])
    selected, state, _ = prepare_history_selection(selected, state, event(state, 11, []), ids, ids)
    selected, state, _ = prepare_history_selection(selected, state, old, ids, ids)
    assert selected == []
    selected, state, sync = prepare_history_selection(selected, state, event(state, 1, ["b"], "new-browser"), ids, ids)
    assert selected == ["b"]
    assert sync["ack"] == {"client_id": "new-browser", "revision": 1}


def test_page_transition_consumes_old_page_click_once_and_preserves_off_page_ids():
    selected, state, _ = prepare_history_selection([], None, None, ["a"], ["a", "b"])
    old = event(state, 1, ["a"])
    selected, state, _ = prepare_history_selection(selected, state, old, ["b"], ["a", "b"])
    assert selected == ["a"]
    obsolete = dict(old, revision=20, selected_ids=[])
    selected, state, _ = prepare_history_selection(selected, state, event(state, 2, ["b"]), ["b"], ["a", "b"])
    selected, state, _ = prepare_history_selection(selected, state, obsolete, ["b"], ["a", "b"])
    assert selected == ["a", "b"]
    selected, state, _ = prepare_history_selection(selected, state, event(state, 3, []), ["b"], ["a", "b"])
    assert selected == ["a"]
    selected, state, _ = prepare_history_selection(selected, state, None, ["a"], ["a", "b"])
    assert selected == ["a"]
    selected, state, _ = prepare_history_selection(selected, state, None, ["b"], ["b"])
    assert selected == []
    selected, state, _ = prepare_history_selection(selected, state, None, [], [])
    assert selected == []
