#!/usr/bin/env python


"""Main app entry point."""


import json
import os
from typing import Dict
from uuid import uuid1

from fastapi import FastAPI
from fastapi import HTTPException
from pydantic import BaseModel

from string_guessing import game_json

app = FastAPI()


ROOT_PATH = "/data"


#
# Specific endpoints
#


class GameDescription(BaseModel):
    """Request to create a new game."""
    alphabet: str = "ABCD"
    length: int = 5
    samples: int = 5
    contracts: int = 10


class ScoreBody(BaseModel):
    """Request to add a new score."""
    score: int = 10


@app.post("/games/")
def post_game(body: GameDescription):
    """Create a new game based on the provided parameters."""
    id_ = str(uuid1())

    game = game_json(
        alphabet=body.alphabet,
        length=body.length,
        num_samples=body.samples,
        num_contracts=body.contracts,
    )

    post_item(f"games/{id_}", game)

    return {"id": id_}


@app.get("/scores/")
def get_scores():
    """Retrieve the high scores."""
    abs_path = _correct_path("games/scores.log")

    try:
        with open(abs_path, "r") as f:
            lines = f.readlines()
    except OSError:
        lines = []

    lexed = (
        line.strip().split(" ", 1) for line in lines
        if line.strip()
    )
    best = {}
    for (a, b) in lexed:
        best[a] = max(best.get(a, 0), int(b))
    results = list(sorted(best.items(), key=lambda x: x[1], reverse=True))
    return {a: b for (a, b) in results}


@app.post("/scores/{id_}")
def post_score(id_: str, body: ScoreBody):
    """Post a new score."""
    score = body.score
    abs_path = _correct_path("games/scores.log")

    try:
        with open(abs_path, "a") as f:
            f.write(f"{id_} {score}\n")
        return {id_: score}
    except OSError:
        raise HTTPException(
            status_code=500,
            detail="Failed to save the score for this game.",
        )


#
# Basic JSON file endpoint handling
#


def _correct_path(path):
    """Check that the path is valid and fix its root."""
    path = path.strip()

    if ".." in path:
        raise HTTPException(status_code=400, detail="Paths must be absolute")

    if not os.path.isdir(ROOT_PATH):
        try:
            os.makedirs(ROOT_PATH, exist_ok=True)
        except FileExistsError:
            raise HTTPException(
                status_code=500, detail="Bad document root",
            )

    # If the path is entirely slashes, it is referring to root
    if path.count("/") == len(path):
        joined = ROOT_PATH
    else:
        joined = os.path.join(ROOT_PATH, path)

    return joined


@app.get("/")
def get_root():
    """GET the root."""
    return get_item("/")


@app.get("/{file_path:path}")
def get_item(file_path: str):
    """GET an item."""
    abs_path = _correct_path(file_path)

    if os.path.isdir(abs_path):
        return {file_path: os.listdir(abs_path)}

    elif os.path.isfile(abs_path):
        with open(abs_path, "r") as f:
            try:
                return json.load(f)
            except json.JSONDecodeError:
                raise HTTPException(
                    status_code=500,
                    detail="Path does not refer to a JSON document",
                )

    else:
        raise HTTPException(status_code=404, detail="Path not found")


@app.post("/{file_path:path}")
def post_item(file_path: str, body: Dict):
    """POST an item."""
    abs_path = _correct_path(file_path)
    dirname = os.path.dirname(abs_path)

    if not os.path.isdir(dirname):
        try:
            os.makedirs(dirname, exist_ok=True)
        except FileExistsError:
            raise HTTPException(
                status_code=500,
                detail="Path refers to a directory which is invalid",
            )

    with open(abs_path, "w") as f:
        json.dump(body, f)


@app.delete("/{file_path:path}")
def delete_item(file_path: str):
    """DELETE an item."""
    abs_path = _correct_path(file_path)

    if os.path.exists(abs_path):
        os.remove(abs_path)

    else:
        raise HTTPException(status_code=404, detail="Path not found")
