#!/usr/bin/env python3
"""
Cmd library extensions
"""

from inspect import signature, Parameter, _empty


def perr(msg: str = "An error occured"):
    """
    Prints string when an exception occurs, passes exception
    """
    from functools import wraps

    def outer(f):
        @wraps(f)
        def deco(*args, **kwargs):
            try:
                return f(*args, **kwargs)
            except:
                print(msg)
        return deco
    return outer


def lextype(param: Parameter) -> callable:
    """
    Capture type from Parameter param
    """
    if param.kind in (Parameter.POSITIONAL_ONLY, Parameter.POSITIONAL_OR_KEYWORD):
        return param.annotation
    elif param.kind == Parameter.VAR_POSITIONAL and param.annotation != _empty:
        if param.annotation.__name__ == 'Tuple':
            # Ellipsis varargs
            return param.annotation.__args__[0]
        else:
            return param.annotation
    return lambda x: x


def lexed(f):
    """
    apropos: allows method do_* = f, to be written as a normal method
    instead of f(self, line)

    Cmd do_* lexing decorator
    tokenizes input line, then constructs according to annotations
    the result is passed onto method 'f'
    ignores surplus arguments post-tokenization
    """
    from functools import wraps
    from inspect import signature, Parameter, _empty
    import shlex

    sig = signature(f)
    @wraps(f)
    def deco(obj, line):
        toks = shlex.split(line)[::-1]
        args = ()
        for name, param in sig.parameters.items():
            if name == 'self':
                # Exclude obj, handled eariler
                continue

            if param.kind == Parameter.POSITIONAL_ONLY:
                args += (param.annotation(toks.pop()),)
            elif param.kind == Parameter.POSITIONAL_OR_KEYWORD:
                if toks:
                    args += (param.annotation(toks.pop()),)
                else:
                    args += (param.default,)
            elif param.kind == Parameter.VAR_POSITIONAL:
                ctor = lextype(param)
                args += tuple(map(ctor, toks[::-1]))
        return f(obj, *args)
    return deco
