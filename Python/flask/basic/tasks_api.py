from flask import current_app, Blueprint, jsonify, request

tasks = Blueprint('tasks', __name__)


@tasks.record  # on application reg
def record(state):
    assert 'sqlalchemy' in state.app.extensions, "SQLAlchemy required"
    for k,v in state.app.config['models'].items():
        # mass `global k; k = v` call
        globals()[k] = v


@tasks.route('/')
def listall():
    tasks = Task.query.order_by(Task.priority.desc()).all()
    return jsonify([x.todict() for x in tasks])


@tasks.route('/<name>', methods=['GET', 'POST', 'DELETE'])
@tasks.route('/<name>/<int:priority>', methods=['GET', 'POST', 'DELETE'])
def single(name="", priority=0):
    name = name.strip()
    if name == "":
        return make_response('Name required', 400)

    session = current_app.extensions['sqlalchemy'].db.session
    if request.method == 'GET':  # fetch individual Task
        target = Task.query.filter_by(name=name).first_or_404()
        return jsonify({'status': 'SUCCESS', 'result': target.todict()})

    elif request.method == 'POST':  # make/update Task
        existing = Task.query.filter_by(name=name).first()
        if existing is None:
            newtask = Task(name=name, priority=priority)
            session.add(newtask)
        else:
            existing.priority = priority
        session.commit()

    elif request.method == 'DELETE':
        existing = Task.query.filter_by(name=name).first_or_404()
        session.delete(existing)
        session.commit()

    return jsonify({'status': "SUCCESS"})
