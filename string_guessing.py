#!/usr/bin/env python


import itertools
import json
import os
import random
import re
import shutil
from collections import Counter
from collections import defaultdict
from functools import reduce
from functools import partial
from types import SimpleNamespace
from math import log

import click
import diceware
from cytoolz import curry
from cytoolz import merge
from cytoolz import juxt

#
# Constants
#


ALPHABET = "ABCD"
SEQUENCE_LENGTH = 5


#
# Generic helpers
#


class Seqs:

    def __init__(self, alphabet, length):
        self.alphabet = alphabet
        self.length = length

    def make(self):
        return "".join(
            random.choice(self.alphabet)
            for _ in range(self.length)
        ).upper()

    def hist(self, *funcs):
        alphabet = self.alphabet
        length = self.length
        seqs = ("".join(p) for p in itertools.product(*[alphabet]*length))
        num = len(alphabet)**length
        f = juxt(*funcs)
        counts = Counter(f(seq) for seq in seqs)
        return {k: v / num for (k, v) in counts.items()}

    def value(self, *funcs):
        restriction = self.hist(*funcs).values()
        log_geo_avg = sum(-log(r) for r in restriction) / len(restriction)
        return round(10*log_geo_avg)

    def compare_values(self, *funcs):
        vals_each = [self.value(func) for func in funcs]
        vals_together = self.value(*funcs)
        efficiency = vals_together/sum(vals_each)
        return {
            "each": vals_each,
            "together": vals_together,
            "efficiency": efficiency,
        }


def rx(s):
    return re.compile(s)


def flatten_list(seq):
    return list(itertools.chain.from_iterable(seq))


#
# Predicate helpers
#


def and_(*funcs):
    def true(*args, **kwargs):
        return True

    def _(seq):
        return reduce(lambda acc, f: acc and f(seq), funcs, true,)

    return _


def or_(*funcs):
    def _(seq):
        return reduce(lambda f1, f2: f1(seq) or f2(seq), funcs)

    return _


def not_(func):
    def _(seq):
        return not func(seq)

    return _


#
# Analysis helpers
#


#
# Measurement helpers
#


@curry
def count_of(sub, seq):
    return len(re.findall(sub.upper(), seq.upper()))


@curry
def has_repeat_at_least(length, seq):
    return count_of(repeat_of(length), seq)


def distinct_repeat_groups(seq):
    return list(set(re.findall(repeat_of(2), seq)))


def most_frequent_char(seq):
    (char, _) = most_frequent_no_ties(seq)
    return char


def repeat_of(n):
    return "(.)" + r"\1" * (n - 1)


def most_frequent_no_ties(seq):

    counts = Counter(seq)

    most = None
    most_count = 0
    for (k, count) in counts.items():

        # Drop duplicate counts
        if count == most_count:
            most = None
            most_count = 0

        elif count > most_count:
            most = k
            most_count = count

    return (most, most_count)


#
# Measurement predicates
#


def more_than_two_repeaters(seq):
    return len(distinct_repeat_groups(seq.upper())) >= 2


@curry
def predominantly(chars, seq):
    (found_char, _) = most_frequent_no_ties(seq.upper())
    return found_char.upper() in chars


@curry
def at_least(n, sub, seq):
    return count_of(sub, seq) >= n


@curry
def fewer_than(n, sub, seq):
    return count_of(sub, seq) < n


@curry
def starts_with(sub, seq):
    return seq.upper().startswith(sub.upper())


@curry
def ends_with(sub, seq):
    return seq.upper().endswith(sub.upper())


@curry
def has_absence_of(sub, seq):
    return count_of(sub, seq) == 0


@curry
def at_positions(positions, char, seq):
    return len([seq[i] for i in positions if seq[i].upper() == char.upper()])


#
# Interactive stuff
#


def price_chart(seqs):
    print(f"""
A1 ... {seqs.value(at_positions([0], "A"))}
A2 ... {seqs.value(at_positions([0, 1], "A"))}
A3 ... {seqs.value(at_positions([0, 1, 2], "A"))}
AB ... {seqs.value(count_of("AB"))}
AA ... {seqs.value(count_of("AA"))}
A  ... {seqs.value(count_of("A"))}
"""
)


def posc(length, positions, char):
    res = ["."] * length
    for pos in positions:
        res[pos] = char
    return "".join(res)


def measurements(x, seqs):
    alphabet = seqs.alphabet
    length = seqs.length
    tri_positions = sum(
        [
            [[i, i + 3, i + 4] for i in range(length - 4 - 1)],
            [[i, i + 2, i + 4] for i in range(length - 4 - 1)],
            [[i, i + 1, i + 2] for i in range(length - 2 - 1)],
            [[0, length // 2, -1]],
        ],
        [],
    )

    res = defaultdict(dict)

    nonzeros = []
    zeros = []

    for char in alphabet:
        res[f"{char}"] = count_of(char, x)
        for char2 in alphabet:
            res[f"{char}{char2}"] = count_of(f"{char}{char2}", x)
        for (i, j, k) in tri_positions:
            res[posc(length, [i, j, k], char)] = at_positions([i, j, k], char, x)

    zeros = [k for (k, v) in res.items() if not k.startswith("_") and v == 0]
    nonzeros = [k for (k, v) in res.items() if not k.startswith("_") and v != 0]

    res["_stats_"] = {
        "zeros": {z: True for z in zeros},
        "nonzeros": {z: True for z in nonzeros},
        "nonzero %": round(100 * len(nonzeros) / (len(nonzeros) + len(zeros)), 2),
    }
    return res


#
# HTML stuff
#


button_container_tpl = """
<div class="menu-container">
    <div class="button-container">
{links}
    </div>
</div>
"""

html_tpl = """
<html>
<head>
<style>
.button-container {{
    width: 100%;
    overflow-y: auto;
}}

.button-container > a {{
    display: flex;
    align-items: center;
    justify-content: center;
    width: 150px;
    height: 150px;
    line-height: 30px;
    float: left;
    background: lightgray;
    margin: 2px;
    text-align: center;
    text-decoration: none;
    font-weight: bold;
    font-family: sans;
    font-size: 24;
}}

p {{
    width: 100%;
    height: 50px;
    float: left;
    text-align: center;
    line-height: 50px;
    font-weight: bold;
    text-decoration: none;
    background: lightblue;
}}
</style>
</head>

<body>
{}
<p><a href="./index.html">home</a></p>
<p><a href="./answers.html">answers</a></p>
</body>
</html>
"""


def html(data):
    return html_tpl.format(data)


def wrap_to_html(data):
    return "<h1>{}</h1>".format(json.dumps(data))


def buttons_html(buttons):
    split_buttons = [el if isinstance(el, tuple) else (el, el) for el in buttons]
    full_link_text = "\n".join(
        f'<a href="./{link}.html" class="button">{name}</a>'
        for (link, name) in split_buttons
    )
    return html(button_container_tpl.format(links=full_link_text))


def write_html(prefix, name, data):
    with open(os.path.join(prefix, name + ".html"), "w") as f:
        f.write(data)


def apply_html_files(prefix, data_dict, overwrite=False):
    if os.path.isdir(prefix) and overwrite:
        if prefix.count(os.path.sep) < 3:
            raise RuntimeError('Not willing to delete prefix "{}"'.format(prefix))
        shutil.rmtree(prefix)

    elif os.path.isdir(prefix) and not overwrite:
        raise RuntimeError('Prefix already exists: "{}"'.format(prefix))

    os.makedirs(prefix)

    for (name, data) in data_dict.items():
        write_html(prefix, str(name), data)


def make_buttons(tree, index=""):
    current_index = index if index else "index"

    def index_for(x):
        return f"{index}.{x}" if index else f"{x}"

    # Base case
    if not isinstance(tree, dict):
        return {current_index: html(wrap_to_html(tree))}

    # Otherwise...
    button_list = [(index_for(i), str(k)) for (i, k) in enumerate(tree.keys())]
    index_entry = {current_index: buttons_html(button_list)}
    return merge(
        index_entry,
        *[
            make_buttons(v, index=index_for(i))
            for (i, (k, v)) in enumerate(tree.items())
        ],
    )


def make_game(prefix, game_name, alphabet, length):
    seqs = Seqs(alphabet, length)
    q = seqs.make()
    measures = measurements(q, seqs)
    apply_html_files(
        f"{prefix}/{game_name}", make_buttons(measures), overwrite=True,
    )
    write_html(f"{prefix}/{game_name}", "answers", html(wrap_to_html(q)))
    return (f"{prefix}/{game_name}/index.html", measures)


#
# Entry point
#


@click.command()
@click.option("--series-name")
@click.option("--num-games", default=1)
@click.option("--alphabet", default="ABCD")
@click.option("--length", type=int, default=5)
@click.option("--upload/--no-upload")
@click.argument("rootdir")
def main(
    rootdir,
    series_name,
    alphabet='ABCD',
    length=5,
    num_games=1,
    upload=False,
):
    rootdir = rootdir.rstrip("/")
    diceware_settings = SimpleNamespace(
        num=2,
        infile=None,
        delimiter="-",
        specials=0,
        randomsource="system",
        caps=False,
        wordlist="en",
    )
    series_name = series_name or diceware.get_passphrase(diceware_settings)
    for _ in range(num_games):
        game_name = diceware.get_passphrase(diceware_settings)
        (path, game) = make_game(
            f"{rootdir}/{series_name}",
            game_name,
            alphabet,
            length,
        )
        click.echo(f"{path} ({game['_stats_']['nonzero %']} %)")

    if upload:
        title = "string-guessing"
        tarball = f"{title}-{series_name}"
        print("Upload to mancer requested, uploading...")
        print("Compressing...")
        os.system(
            f"tar -C {rootdir} -cvJf {rootdir}/{tarball}.tar.xz {series_name} 2>&1"
        )
        print("Uploading package...")
        os.system(f"ssh med@mancer.in mkdir -p /var/www/files/{title}")
        os.system(
            f"scp -v {rootdir}/{tarball}.tar.xz med@mancer.in:/var/www/files/{title} 2>&1"
        )
        print("Unpacking package...")
        os.system(
            f"ssh med@mancer.in tar -C /var/www/files/{title} -xvJf /var/www/files/{title}/{tarball}.tar.xz 2>&1"
        )



if __name__ == "__main__":
    main()


"""
ectoplasm
cold
heat
photographable - visible
photographable - infrared
"photographable" - radio
magnetic signature
audio (waveforms "evps")
"fog" readings
footprints

elemental resonance
emotional resonance
animal sacrifice
seance
hired psychic
smell
mirrors
writing on walls
portals
historical data
"""
