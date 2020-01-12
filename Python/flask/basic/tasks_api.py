from flask import Response, current_app, Blueprint, jsonify, request
from flask.views import MethodView

tasks = Blueprint('tasks', __name__)


@tasks.record  # on application reg
def record(state):
    assert 'sqlalchemy' in state.app.extensions, "SQLAlchemy required"
    for k,v in state.app.config['models'].items():
        # mass `global k; k = v` call
        globals()[k] = v

class Taskapi(MethodView):
    def get(self, *, name = None):
        # full enumeration
        if name is None or name.strip() == "":
            tasks = Task.query.order_by(Task.priority.desc()).all()
            return jsonify([x.todict() for x in tasks])
        else: # individual fetch
            task = Task.query.filter_by(name=name).first_or_404()
            return jsonify(task.todict())

    def post(self, *, name, priority = 0):
        if name == "":
            return response("Name required", 404)

        session = current_app.extensions['sqlalchemy'].db.session
        existing = Task.query.filter_by(name=name).first()
        if existing is None:
            session.add(Task(name, priority))
        else:
            existing.priority = priority
        session.commit()
        return Response("OK", 200)

    def put(self, *args, **kwargs):
        return self.post(*args, **kwargs)

    def delete(self, *, name):
        session = current_app.extensions['sqlalchemy'].db.session
        task = Task.query.filter_by(name=name).first_or_404()
        session.delete(task)
        session.commit()
        return Response("OK", 200)


view = Taskapi.as_view('taskapi')
insert = ['POST', 'PUT']
tasks.add_url_rule('/', view_func=view, methods=['GET'])
tasks.add_url_rule('/<name>', view_func=view, methods=['GET', *insert, 'DELETE'])
tasks.add_url_rule('/<name>/<priority>', view_func=view, methods=insert)
