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
from math import sqrt
from math import isclose
from types import SimpleNamespace

import click
import diceware
from cytoolz import curry
from cytoolz import dissoc
from cytoolz import juxt
from cytoolz import sliding_window
from cytoolz import take
from cytoolz import partition

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


def and_(f1, f2):
    def _and_(seq):
        return f1(seq) and f2(seq)
    _and_._text = f"{f1._text} and {f2._text}"
    return _and_


#
# Measurement helpers
#


def count_of(sub):
    def _count_of(seq):
        return len(re.findall(sub.upper(), seq.upper()))
    _count_of._text = f"{sub.upper()}"
    return _count_of


def count_of_exact(sub):
    def _count_of_exact(seq):
        subs = ["".join(entry) for entry in sliding_window(len(sub), seq)]
        return subs.count(sub)
    _count_of_exact._text = f"{sub.upper()}"
    return _count_of_exact


@curry
def at_positions(positions, char):
    def _at_positions(seq):
        return len([seq[i] for i in positions if seq[i].upper() == char.upper()])

    # FIXME: length hardcoded here because FUCK passing it through would suck
    length = 5
    pos_description = ["."] * length
    for pos in positions:
        pos_description[pos] = char
    _at_positions._text = "".join(pos_description)
    return _at_positions


#
# Interactive stuff
#


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

    callables = flatten_list([
        [count_of(char) for char in alphabet],
        [count_of_exact(f"{char1}{char2}") for char1 in alphabet for char2 in alphabet],
        [at_positions([i, j, k], char) for (i, j, k) in tri_positions for char in alphabet],
    ])

    return {c._text: c(x) for c in callables}


def _random_comparison(max_, *tests):
    a = random.choice(range(max_))

    @curry
    def greater_than(f, y):
        def _greater_than(seq):
            return f(seq) >= y
        _greater_than._text = f"{f._text}≥{y}"
        return _greater_than

    @curry
    def less_than(f, y):
        def _less_than(seq):
            return f(seq) <= y
        _less_than._text = f"{f._text}≤{y}"
        return _less_than

    @curry
    def equal_to(f, y):
        def _equal_to(seq):
            return f(seq) == y
        _equal_to._text = f"{f._text}={y}"
        return _equal_to

    test = random.choice(tests)
    compare = random.choice([less_than, greater_than, equal_to])
    return compare(test, a)


def random_contracts(seqs):
    alphabet = seqs.alphabet
    length = seqs.length
    sweep = sweep_offsets(max_=length - 1)

    tri_positions = flatten_list(
        [sweep([0, 1, 2]), sweep([0, 2, 4]), sweep([0, 3, 4]), [[0, length // 2, -1]],]
    )

    callables = flatten_list([
        [count_of(char) for char in alphabet],
        [count_of_exact(f"{char1}{char2}") for char1 in alphabet for char2 in alphabet],
        [at_positions([i, j, k], char) for (i, j, k) in tri_positions for char in alphabet],
    ])

    while True:
        if random.random() < 0.8:
            yield _random_comparison(length, *callables)
        else:
            yield and_(
                _random_comparison(length, *callables),
                _random_comparison(length, *callables),
            )


def average(lst):
    lst = list(lst)
    return sum(lst) / len(lst)


def std(lst):
    avg = average(lst)
    var = sum((x - avg)**2 for x in lst)
    return sqrt(var/len(lst))


def truth_frequencies(contracts, values):
    interactions = [
        (val, contract, contract(val))
        for val in values for contract in contracts
    ]

    truth_freq_by_val = \
        Counter(val for (val, contract, p) in interactions if p)

    truth_freq_by_contract = \
        Counter(contract for (val, contract, p) in interactions if p)

    return (truth_freq_by_val, truth_freq_by_contract)


def evaluate_candidate_contracts(contracts, values):
    (truth_freq_by_val, truth_freq_by_contract) = \
        truth_frequencies(contracts, values)

    no_universal_values = \
        not any(v == len(contracts) for v in truth_freq_by_val.values())

    all_vals_can_score = all(
        val in truth_freq_by_val
        for val in values
    )

    all_contracts_can_score = all(
        contract in truth_freq_by_contract
        for contract in contracts
    )

    std_vals_small = (
        len(truth_freq_by_val.values()) > 0 and
        std(truth_freq_by_val.values()) < len(contracts) / 3
    )

    std_contracts_small = (
        len(truth_freq_by_contract.values()) > 0 and
        std(truth_freq_by_contract.values()) < len(values) / 3
    )

    return all([
        all_vals_can_score,
        all_contracts_can_score,
        no_universal_values,
        std_vals_small,
        std_contracts_small,
    ])


def select_reasonable_contracts(seqs, qs, num_contracts):
    num_samples = len(qs)

    candidate_contracts = partition(num_contracts, random_contracts(seqs))
    # We accept only sets of contracts which are actually interesting to play.
    #
    # `evaluate_candidate_contracts` checks all the conditions except for one
    # which fails frequently and which we need to modify gently instead of
    # rejection sampling: we need to attempt to replace contracts which can be
    # fulfulled by ANY of the samples.
    #
    for contracts in candidate_contracts:
        (truth_by_val, truth_by_contract) = \
            truth_frequencies(contracts, qs.values())
        if not evaluate_candidate_contracts(contracts, qs.values()):
            continue
        contracts_matching_all = [
            contract for contract in contracts
            if truth_by_contract[contract] == num_samples
        ]
        if not contracts_matching_all:
            return contracts
        adjusted = []
        while contracts_matching_all:
            old_adjusted = adjusted
            adjusted = [
                (
                    next(random_contracts(seqs))
                    if contract in contracts_matching_all else
                    contract
                )
                for contract in contracts
            ]
            # If we accidentally invalidated `evaluate_candidate_contracts`
            # with our adjustment, but the adjustment removed any contracts
            # matching all, we're stuck because this loop is only trying to
            # adjust for contracts matching all.  We need to break out and get
            # a fresh set of candidates.
            if old_adjusted == adjusted:
                break
            (truth_by_val, truth_by_contract) = \
                truth_frequencies(adjusted, qs.values())
            if not evaluate_candidate_contracts(contracts, qs.values()):
                continue
            contracts_matching_all = [
                contract for contract in adjusted
                if truth_by_contract[contract] == num_samples
            ]
        # If we did not break, we made it out of the loop successfully.  Return
        # the working adjusted value
        else:
            return adjusted
        # If we got out of the loop by breaking, it means we got stuck.  Try
        # again with a fresh set of candidates
        continue


def game_json(alphabet, length, num_samples, num_contracts):
    seqs = Seqs(alphabet, length)
    words = (f"sample{i}" for i in itertools.count(1))
    samples = [seqs.make() for _ in range(num_samples)]
    qs = dict(zip(words, samples))
    ms = {k: measurements(v, seqs) for (k, v) in qs.items()}
    selected_contracts = select_reasonable_contracts(seqs, qs, num_contracts)
    cs = {
        k: {contract._text: contract(v) for contract in selected_contracts}
        for (k, v) in qs.items()
    }
    data = {
        "game": "0",
        "samples": list(qs.keys()),
        "tests": list(list(ms.values())[0].keys()),
        "answers": qs,
        "measures": ms,
        "contracts": cs,
    }
    return data


#
# Harnesses
#


def emit_json(alphabet, length, num_samples, num_contracts, json_file):
    game = game_json(alphabet, length, num_samples, num_contracts)
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
@click.option("--num-contracts", default=5)
@click.option("--alphabet", default="ABCD")
@click.option("--length", type=int, default=5)
@click.argument("output", type=click.File("w"))
def single(alphabet, length, num_samples, num_contracts, output):
    emit_json(alphabet, length, num_samples, num_contracts, output)


@main.command('upload')
@click.option("--num-samples", default=5)
@click.option("--num-contracts", default=5)
@click.option("--alphabet", default="ABCD")
@click.option("--length", type=int, default=5)
@click.option("--series-name", default=None)
@click.option("--num-games", default=5)
def upload(alphabet, length, num_samples, num_contracts, series_name, num_games):
    games = [
        game_json(alphabet, length, num_samples, num_contracts)
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
