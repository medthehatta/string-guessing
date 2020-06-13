#!/usr/bin/env python


"""Main app entry point."""


import os
import json


from typing import Dict


from fastapi import FastAPI, HTTPException

app = FastAPI()


ROOT_PATH = "/data"


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
                status_code=500,
                detail="Bad document root",
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

    print(f"GET {abs_path}")

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
        return json.dump(body, f)


@app.delete("/{file_path:path}")
def delete_item(file_path: str):
    """DELETE an item."""
    abs_path = _correct_path(file_path)

    if os.path.exists(abs_path):
        os.remove(abs_path)

    else:
        raise HTTPException(status_code=404, detail="Path not found")
