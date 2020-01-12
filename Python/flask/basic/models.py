def init(db):
    class Task(db.Model):
        name = db.Column(db.String(255), primary_key=True)
        priority = db.Column(db.Integer)

        def __init__(self, name, priority):
            self.name = name
            self.priority = priority

        def __repr__(self):
            return f"<Task {self.name}>"

        def todict(self):
            return {'name': self.name, 'priority': self.priority}

    flocals = locals().items() # Freeze locals
    return {k: v for k, v in flocals if isinstance(v, type)}
