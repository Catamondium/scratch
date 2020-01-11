#!envrun

from flask import Flask, jsonify
from os import environ

app = Flask(__name__)

try:
    assert environ['FLASK_ENV'] == 'development'
    from werkzeug.debug import DebuggedApplication
    app.config.from_object(__name__)
    app.wsgi_app = DebuggedApplication(app.wsgi_app, evalex=True)
except KeyError:
    pass
except AssertionError:
    pass

@app.route('/')
def hello_json():
    return jsonify(['a', 1, 2])

@app.route('/throw')
@app.route('/throw/<dothrow>')
def _debug(dothrow='false'):
    assert dothrow.lower() == 'false', "You've thrown!"
    return "You chose not to throw!"

