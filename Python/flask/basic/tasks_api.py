from flask import current_app, Blueprint, jsonify, request

tasks = Blueprint('tasks', __name__)


@tasks.record  # on application reg
def record(state):
    assert 'sqlalchemy' in state.app.extensions, "SQLAlchemy required"


@tasks.route('/')
def listall():
    Task = current_app.config['tasks.db']
    tasks = Task.query.order_by(Task.priority.desc()).all()
    return jsonify([x.todict() for x in tasks])


@tasks.route('/<task>', methods=['GET', 'POST', 'DELETE'])
@tasks.route('/<task>/<int:priority>', methods=['GET', 'POST', 'DELETE'])
def single(task=None, priority=0):
    if task is None or task.strip() == "":
        return make_response('Name required', 400)

    task = task.strip()
    Task = current_app.config['tasks.db']
    session = current_app.extensions['sqlalchemy'].db.session
    if request.method == 'GET':  # fetch individual Task
        target = Task.query.filter_by(name=task).first_or_404()
        return jsonify({'status': 'SUCCESS', 'result': target.todict()})

    elif request.method == 'POST':  # make/update Task
        newtask = Task(name=task, priority=priority)
        existing = Task.query.filter_by(name=task).first()
        if existing is not None:
            session.delete(existing)
        session.add(newtask)
        session.commit()

    elif request.method == 'DELETE':
        existing = Task.query.filter_by(name=task).first_or_404()
        session.delete(existing)
        session.commit()

    return jsonify({'status': "SUCCESS"})
