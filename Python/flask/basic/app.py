from flask import Flask, jsonify, url_for, redirect
from flask_sqlalchemy import SQLAlchemy

from os import environ

import models
from tasks_api import tasks

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:////tmp/test.db'
db = SQLAlchemy(app)

# For blueprint use
app.config['models'] = models.init(db)


try:
    assert environ['FLASK_ENV'] == 'development'
    db.create_all()
    from werkzeug.debug import DebuggedApplication
    app.config.from_object(__name__)
    app.wsgi_app = DebuggedApplication(app.wsgi_app, evalex=True)
except KeyError:
    pass
except AssertionError:
    pass

app.register_blueprint(tasks, url_prefix='/tasks')


@app.route('/')
def hello_json():
    return jsonify(['a', 1, 2])


@app.route('/redir')
def passover():
    # Redirection example
    return redirect(url_for('sum_route', args='2/2'))


@app.route('/sum/')
@app.route('/sum/<path:args>')  # 'path' converter forms variadic route
def sum_route(args=None):
    if args is None:
        return jsonify(result=0)
    xs = list([0])
    for x in args.split('/'):
        try:
            xs.append(int(x))
        except ValueError:
            pass
    return jsonify(result=sum(xs))


@app.route('/throw/')
@app.route('/throw/<dothrow>')  # <dothrow> parameter
def _debug(dothrow='false'):
    assert dothrow.lower() == 'false', "You've thrown!"
    return "You chose not to throw!"
