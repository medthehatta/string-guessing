#!/usr/bin/env python


"""My bottle app."""

import json
import os
import shutil

from bottle import default_app
from bottle import route
from bottle import run
from bottle import request
from bottle import response

from string_guessing import game_json
from string_guessing import random_words


#
# Constants
#

GAME_DIR = "/games"


#
# Helpers
#


def _regarding(subject, type=None, comment=None, data=None):
    res = {
        "subject": subject,
        "type": type,
        "comment": comment,
        "data": data,
    }
    return {k: v for (k, v) in res.items() if v is not None}


def _ok(data):
    return {"success": True, "data": data}


def _fail(data, reason="(no reason given)"):
    return {"success": False, "reason": reason, "data": data}


#
# Routes
#


def enable_cors(fn):
    def _enable_cors(*args, **kwargs):
        response.headers['Access-Control-Allow-Origin'] = '*'
        response.headers['Access-Control-Allow-Methods'] = 'GET, POST, PUT, OPTIONS'
        response.headers['Access-Control-Allow-Headers'] = 'Origin, Accept, Content-Type, X-Requested-With, X-CSRF-Token'

        if request.method != 'OPTIONS':
            return fn(*args, **kwargs)

    return _enable_cors


@route("/cors", method=["GET", "OPTIONS"])
@enable_cors
def cors():
    response.headers['Content-Type'] = "application/json"
    return "[1]"


@route("/")
@enable_cors
def index():
    return _ok("Server ok.")


@route("/games/", method="GET")
@enable_cors
def get_games():

    files = [
        (file_, os.path.join(GAME_DIR, file_))
        for file_ in os.listdir(GAME_DIR)
    ]

    res = [
        name for (name, path) in files
        if all([
            os.path.isdir(path),
            not path.startswith("."),
            not path.startswith("_"),
            not path.endswith("_"),
        ])
    ]

    return _ok(res)


@route("/games/<id_>", method="GET")
@enable_cors
def get_game(id_):
    try:
        with open(os.path.join(GAME_DIR, id_, "game.json")) as f:
            return json.load(f)
    except OSError:
        response.status = 404
        return _fail(_regarding(id_), "Game not found.")


@route("/games/<id_>", method="DELETE")
@enable_cors
def delete_game(id_):
    path = os.path.join(GAME_DIR, id_)

    # Make sure we aren't destroying things
    assert path.startswith("/games")

    if not os.path.isdir(path):
        response.status = 404
        return _fail(
            _regarding(id_),
            "Could not delete game as it was not found.",
        )

    else:
        shutil.rmtree(path)
        return _ok(_regarding(id_, comment="Deleted."))


@route("/games/", method="POST")
@enable_cors
def post_game():
    id_ = random_words(3)

    try:
        body = json.load(request.body)
    except json.JSONDecodeError:
        body = {}

    game = game_json(
        alphabet=body.get("alphabet", "ABCD"),
        length=body.get("length", 5),
        num_samples=body.get("samples", 5),
        num_contracts=body.get("contracts", 5),
    )

    os.makedirs(os.path.join(GAME_DIR, id_))
    with open(os.path.join(GAME_DIR, id_, "game.json"), "w") as f:
        json.dump(game, f)

    (scheme, host, path, _, _) = request.urlparts
    response.status = 201
    response.set_header("Location", f"{scheme}://{host}/{path}/{id_}")
    return _ok(_regarding(id_, data=game, comment="Game created."))


@route("/games/", method="OPTIONS")
@enable_cors
def post_game_cors():
    return "[1]"


@route("/scores/", method="GET")
@enable_cors
def get_scores():
    try:
        with open(os.path.join(GAME_DIR, "scores.log"), "r") as f:
            lines = f.readlines()
        lexed = (
            line.strip().split(" ", 1) for line in lines
            if line.strip()
        )
        best = {}
        for (a, b) in lexed:
            best[a] = max(best.get(a, 0), int(b))
        results = list(sorted(best.items(), key=lambda x: x[1], reverse=True))
        ret = {a: b for (a, b) in results}
        return _ok(_regarding("scores", data=ret))

    except OSError:
        response.status = 500
        return _fail(
            _regarding("scores"),
            reason="Failed to retrieve the scores.",
        )


@route("/scores/<id_>", method="POST")
@enable_cors
def post_score(id_):
    try:
        body = json.load(request.body)
    except json.JSONDecodeError:
        body = {"score": 0}

    score = body.get("score", 0)

    try:
        with open(os.path.join(GAME_DIR, "scores.log"), "a") as f:
            f.write(f"{id_} {score}\n")
        return _ok(_regarding(id_, data={"score": score}))
    except OSError:
        response.status = 500
        return _fail(
            {"id": id_, "score": score},
            reason="Failed to save the score for this game.",
        )


#
# Entry point
#


# Running from the module in development mode
if __name__ == "__main__":
    run(host="localhost", port=8081)

# Running on a real webserver
else:
    application = default_app()
