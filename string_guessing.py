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


def trace(*xs):
    for x in xs:
        print(x)
    return xs


class GameDefinition:

    def __init__(self, alphabet, length, num_samples, num_contracts):
        self.alphabet = alphabet
        self.length = length
        self.num_samples = num_samples
        self.num_contracts = num_contracts

    def make_sample(self):
        return "".join(
            random.choice(self.alphabet)
            for _ in range(self.length)
        ).upper()

    def sample_generator(self):
        while True:
            yield self.make_sample()

    def sample_set_generator(self):
        return partition(self.num_samples, self.sample_generator())

    def make_contract(self):
        callables = self.available_callables()

        if random.random() < 0.8:
            cmp_against = random.choice(range(self.length))
            return random_comparison(*callables)(cmp_against)

        else:
            cmp_against1 = random.choice(range(self.length))
            cmp_against2 = random.choice(range(self.length))
            return and_(
                random_comparison(*callables)(cmp_against1),
                random_comparison(*callables)(cmp_against2),
            )

    def contract_generator(self):
        while True:
            yield self.make_contract()

    def contract_set_generator(self):
        return partition(self.num_contracts, self.contract_generator())

    def paired_contracts_samples(self):

        def _adjust(cs):
            (contracts, samples) = cs
            contracts_fixed = fix_contract_set(
                contracts,
                samples,
                self.make_contract,
            )
            samples_fixed = fix_sample_set(
                samples,
                contracts_fixed,
                self.make_sample,
            )
            return (contracts_fixed, samples_fixed)

        initial_contract_set = next(self.contract_set_generator())
        initial_sample_set = next(self.sample_set_generator())

        return iterate_until_stable(
            _adjust,
            (initial_contract_set, initial_sample_set),
        )

    def available_callables(self):
        alphabet = self.alphabet
        length = self.length
        sweep = sweep_offsets(max_=length - 1)

        tri_positions = flatten_list(
            [
                sweep([0, 1, 2]),
                sweep([0, 2, 4]),
                sweep([0, 3, 4]),
                [[0, length // 2, -1]],
            ]
        )

        return flatten_list([
            [count_of(char) for char in alphabet],
            [
                count_of_exact(f"{char1}{char2}")
                for char1 in alphabet
                for char2 in alphabet
            ],
            [
                at_positions(length, [i, j, k], char)
                for (i, j, k) in tri_positions
                for char in alphabet
            ],
        ])

    def random_measurements(self, available_pct=100):
        callables = self.available_callables()
        selected = []
        for callable_ in callables:
            if 100*random.random() < available_pct:
                selected.append(callable_)
        return selected


class GameAnalyzer:

    def __init__(self, alphabet, length, num_samples, num_contracts):
        self.alphabet = alphabet
        self.length = length

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


def replace_unacceptable(unacceptable, lst, element_maker):
    while any(unacceptable(element) for element in lst):
        lst = [
            element_maker(element) if unacceptable(element) else element
            for element in lst
        ]
    return lst


def fix_sample_set(sample_set, contract_set, sample_maker):

    def unacceptable(sample):
        return any([
            all(c(sample) is True for c in contract_set),
            not any(c(sample) is True for c in contract_set),
        ])

    return replace_unacceptable(
        unacceptable,
        sample_set,
        lambda x: sample_maker(),
    )


def fix_contract_set(contract_set, sample_set, contract_maker):

    def unacceptable(contract):
        return any([
            all(contract(s) is True for s in sample_set),
            not any(contract(s) is True for s in sample_set),
        ])

    return replace_unacceptable(
        unacceptable,
        contract_set,
        lambda x: contract_maker(),
    )


def fix_contracts_samples(contract_set, sample_set, contract_maker, sample_maker):

    def _adjust(cs):
        (contracts, samples) = cs
        contracts_fixed = fix_contract_set(contracts, samples, contract_maker)
        samples_fixed = fix_sample_set(samples, contracts_fixed, sample_maker)
        return (contracts_fixed, samples_fixed)

    return iterate_until_stable(_adjust, (contract_set, sample_set))


def random_comparison(*tests):

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
    return compare(test)


def iterate_until_stable(func, initial):
    class _Uninitialized:
        pass

    oldvalue = _Uninitialized
    value = initial

    while value != oldvalue:
        oldvalue = value
        value = func(value)

    return value


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
def at_positions(length, positions, char):
    def _at_positions(seq):
        return len([seq[i] for i in positions if seq[i].upper() == char.upper()])

    pos_description = ["."] * length
    for pos in positions:
        pos_description[pos] = char
    _at_positions._text = "".join(pos_description)
    return _at_positions


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


#
# Interactive stuff
#


def game_json(alphabet, length, num_samples, num_contracts):
    setup = GameDefinition(alphabet, length, num_samples, num_contracts)
    (contract_set, sample_set) = setup.paired_contracts_samples()
    measurement_set = setup.random_measurements()

    sample_names = (f"sample{i}" for i in itertools.count(1))
    named_samples = list(zip(sample_names, sample_set))

    answers = {
        sample_name: sample
        for (sample_name, sample) in named_samples
    }

    measurement_values = {
        sample_name: {
            measurement._text: measurement(sample)
            for measurement in measurement_set
        }
        for (sample_name, sample) in named_samples
    }

    contract_values = {
        sample_name: {
            contract._text: contract(sample)
            for contract in contract_set
        }
        for (sample_name, sample) in named_samples
    }

    return {
        "game": "0",
        "answers": answers,
        "measures": measurement_values,
        "contracts": contract_values,
    }


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
