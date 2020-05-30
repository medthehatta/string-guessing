#!/usr/bin/env python


import itertools
import json
import os
import random
import re
import subprocess
import tempfile
from collections import Counter
from collections import defaultdict
from functools import reduce
from math import log
from types import SimpleNamespace

import click
import diceware
from cytoolz import curry
from cytoolz import dissoc
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
        return "".join(random.choice(self.alphabet) for _ in range(self.length)).upper()

    def hist(self, *funcs):
        alphabet = self.alphabet
        length = self.length
        seqs = ("".join(p) for p in itertools.product(*[alphabet] * length))
        num = len(alphabet) ** length
        f = juxt(*funcs)
        counts = Counter(f(seq) for seq in seqs)
        return {k: v / num for (k, v) in counts.items()}

    def value(self, *funcs):
        restriction = self.hist(*funcs).values()
        log_geo_avg = sum(-log(r) for r in restriction) / len(restriction)
        return round(10 * log_geo_avg)

    def compare_values(self, *funcs):
        vals_each = [self.value(func) for func in funcs]
        vals_together = self.value(*funcs)
        efficiency = vals_together / sum(vals_each)
        return {
            "each": vals_each,
            "together": vals_together,
            "efficiency": efficiency,
        }


def rx(s):
    return re.compile(s)


def flatten_list(seq):
    return list(itertools.chain.from_iterable(seq))


def random_word():
    return (
        subprocess.check_output(
            f"sort -R {diceware.get_wordlist_path('en')} | head -n1", shell=True,
        )
        .decode("utf-8")
        .strip()
    )


def random_words(num):
    return "-".join(random_word() for _ in range(num))


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
    print(
        f"""
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


@curry
def sweep_offsets(kernel, max_):
    def _componentwise(f, a, b):
        return [f(*ab) for ab in zip(a, b)]

    klen = len(kernel)
    gen = (
        _componentwise(lambda x, y: x + y, kernel, [i] * klen)
        for i in itertools.count()
    )
    return list(itertools.takewhile(lambda x: x[-1] <= max_, gen))


def measurements(x, seqs):
    alphabet = seqs.alphabet
    length = seqs.length
    sweep = sweep_offsets(max_=length - 1)

    tri_positions = flatten_list(
        [sweep([0, 1, 2]), sweep([0, 2, 4]), sweep([0, 3, 4]), [[0, length // 2, -1]],]
    )

    res = {}

    for char in alphabet:
        res[f"{char}"] = count_of(char, x)
        for char2 in alphabet:
            res[f"{char}{char2}"] = count_of(f"{char}{char2}", x)
        for (i, j, k) in tri_positions:
            res[posc(length, [i, j, k], char)] = at_positions([i, j, k], char, x)

    return res


def game_json(alphabet, length, num_samples):
    seqs = Seqs(alphabet, length)
    words = (random_word() for _ in itertools.count())
    samples = [seqs.make() for _ in range(num_samples)]
    qs = dict(zip(words, samples))
    ms = {k: dissoc(dict(measurements(v, seqs)), "_stats_") for (k, v) in qs.items()}
    data = {
        "game": "0",
        "samples": list(qs.keys()),
        "tests": list(list(ms.values())[0].keys()),
        "answers": qs,
        "measures": ms,
    }
    return data


#
# Harnesses
#


def emit_json(alphabet, length, num_samples, json_file):
    game = game_json(alphabet, length, num_samples)
    json_file.write(json.dumps(game))


def prepare_stage(json_data):
    stage_dir = tempfile.mkdtemp()
    os.system(f"cp -r client/out/* {stage_dir}")
    with open(f"{stage_dir}/game.json", "w") as f:
        f.write(json.dumps(json_data))
    os.system(f"chmod -R 775 {stage_dir}")
    return stage_dir


#
# Entry point
#


@click.group()
def main():
    pass


@main.command('single')
@click.option("--num-samples", default=5)
@click.option("--alphabet", default="ABCD")
@click.option("--length", type=int, default=5)
@click.argument("output", type=click.File("w"))
def single(alphabet, length, num_samples, output):
    emit_json(alphabet, length, num_samples, output)


@main.command('upload')
@click.option("--num-samples", default=5)
@click.option("--alphabet", default="ABCD")
@click.option("--length", type=int, default=5)
@click.option("--series-name", default=None)
@click.option("--num-games", default=5)
def upload(alphabet, length, num_samples, series_name, num_games):
    games = [
        game_json(alphabet, length, num_samples)
        for _ in range(num_games)
    ]
    stage_dirs = [prepare_stage(game) for game in games]
    series_name = series_name or random_words(2)
    for stage_dir in stage_dirs:
        game_name = random_words(2)
        remote_prefix = "/var/www/files/string-guessing"
        remote_path = f"{series_name}/{game_name}"
        os.system(f"ssh med@mancer.in mkdir -p {remote_prefix}/{remote_path}")
        os.system(
            f'bash -c "'
            f'scp -q -r '
            f'{stage_dir}/* '
            f'\\"med@mancer.in:{remote_prefix}/{remote_path}/\\"'
            f'"'
        )
        os.system(f"ssh med@mancer.in rm {remote_prefix}/latest")
        os.system(
            f"ssh med@mancer.in "
            f"ln -sf {remote_prefix}/{series_name} {remote_prefix}/latest"
        )
        os.system(f"rm -rf {stage_dir}")
        print(stage_dir)
    click.echo(f"https://files.mancer.in/string-guessing/{series_name}")


if __name__ == "__main__":
    main()
