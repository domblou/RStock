from rstock.application.surveillance_refresh import surveillance_refresh_decision


def test_active_operational_job_enables_polling():
    decision = surveillance_refresh_decision([
        {"job_type": "daily_prediction", "status": "running"},
    ], polling=False)

    assert decision.poll is True
    assert decision.final_rerun is False


def test_completed_operational_job_requests_one_final_rerun_then_stops():
    completed = [{"job_type": "realized_validation", "status": "completed"}]

    final_poll = surveillance_refresh_decision(completed, polling=True)
    static_page = surveillance_refresh_decision(completed, polling=False)

    assert final_poll.poll is False
    assert final_poll.final_rerun is True
    assert static_page.poll is False
    assert static_page.final_rerun is False


def test_non_operational_or_failed_jobs_do_not_keep_surveillance_polling():
    decision = surveillance_refresh_decision([
        {"job_type": "walk_forward", "status": "running"},
        {"job_type": "operational_run", "status": "failed"},
    ], polling=False)

    assert decision.poll is False
    assert decision.final_rerun is False
