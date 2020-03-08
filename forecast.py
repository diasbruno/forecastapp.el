import json
from flask import Flask
app = Flask(__name__)


PROJECTS = [{
    "id": 1,
    "company_project_id": 1,
    "name": "Frontend project",
    "stage": "PLANNING",
    "status": "GREEN",
    "status_description": "",
    "description": "",
    "color": "#FF7C75",
    "estimation_units": "HOURS",
    "minutes_per_estimation_point": 480,
    "budget": 1234.56,
    "billable": True,
    "use_sprints": True,
    "sprint_length": 14,
    "start_date": "2017-01-01",
    "end_date": "2018-01-01",
    "task_levels": 1,
    "client": 1,
    "rate_card": 1,
    "remaining_auto_calculated": False,
    "use_project_allocations": True,
    "labels": [1, 2],
    "external_refs": [],
    "created_by": 1,
    "updated_by": 1,
    "created_at": "2017-01-14T18:46:56Z",
    "updated_at": "2017-01-14T18:47:58Z"
}, {
    "id": 2,
    "company_project_id": 2,
    "name": "Backend project",
    "stage": "PLANNING",
    "status": "GREEN",
    "status_description": "",
    "description": "",
    "color": "#FF7C75",
    "estimation_units": "HOURS",
    "minutes_per_estimation_point": 480,
    "budget": 1234.56,
    "billable": True,
    "use_sprints": True,
    "sprint_length": 14,
    "start_date": "2017-01-01",
    "end_date": "2018-01-01",
    "task_levels": 1,
    "client": 1,
    "rate_card": 1,
    "remaining_auto_calculated": False,
    "use_project_allocations": True,
    "labels": [1, 2],
    "external_refs": [],
    "created_by": 1,
    "updated_by": 1,
    "created_at": "2017-01-14T18:46:56Z",
    "updated_at": "2017-01-14T18:47:58Z"
}]

@app.route('/v1/projects')
def get_projects():
    return json.dumps(PROJECTS)


@app.route('/v1/projects/<int:pid>')
def get_project(pid):
    project = None
    for p in PROJECTS:
        if p['id'] == pid:
            project = p
            break
    return json.dumps(project)


if __name__ == '__main__':
    app.run()
