#!/usr/bin/env python

import string_guessing as sg
from cytoolz import partition
from collections import namedtuple
from cytoolz import take as take_


Result = namedtuple(
    'Result',
    (
        'samples contracts easycontract opsample '
        'hardcontract trashsample'
    ),
)


def log(x):
    print(x)
    return x


def take(n, seq):
    return list(take_(n, seq))


def genlen(seq):
    count = 0
    for s in seq:
        count += 1
    return count


def count_truthy(seq):
    return genlen(s for s in seq if s)


def contract_generator(setup):
    return sg.random_contracts(setup)


def sample_generator(setup):
    while True:
        yield setup.make()


def test_contracts_samples(contract_set, sample_set):
    return Result(
        sample_set,
        contract_set,
        count_contract_matching_all_samples(contract_set, sample_set),
        count_sample_matching_all_contracts(sample_set, contract_set),
        count_contract_matching_no_samples(contract_set, sample_set),
        count_sample_matching_no_contracts(sample_set, contract_set),
    )


def fix_contracts_for_result(result, contract_maker):
    return test_contracts_samples(
        contract_set=fix_contracts(result.contracts, result.samples, contract_maker),
        sample_set=result.samples,
    )


def fix_contracts(contract_set, sample_set, contract_maker):
    def unacceptable(contract):
        return any([
            contract_matches_all_samples(contract, sample_set),
            contract_matches_no_samples(contract, sample_set),
        ])

    while (
        count_contract_matching_all_samples(contract_set, sample_set) > 0 or
        count_contract_matching_no_samples(contract_set, sample_set) > 0
    ):
        contract_set = [
            (contract_maker() if unacceptable(contract) else contract)
            for contract in contract_set
        ]
    return contract_set


def fix_samples_for_result(result, sample_maker):
    return test_contracts_samples(
        contract_set=result.contracts,
        sample_set=fix_samples(result.samples, result.contracts, sample_maker),
    )


#def fix_samples(sample_set, contract_set, sample_maker):
#    def unacceptable(sample):
#        return any([
#            sample_matches_all_contracts(sample, contract_set),
#            sample_matches_no_contracts(sample, contract_set),
#        ])
#
#    while (
#        count_sample_matching_all_contracts(sample_set, contract_set) > 0 or
#        count_sample_matching_no_contracts(sample_set, contract_set) > 0
#    ):
#        sample_set = [
#            (sample_maker() if unacceptable(sample) else sample)
#            for sample in sample_set
#        ]
#    return sample_set


def fix_samples(sample_set, contract_set, sample_maker):

    def unacceptable(sample):
        return any([
            sample_matches_all_contracts(sample, contract_set),
            sample_matches_no_contracts(sample, contract_set),
        ])

    return replace_unacceptable(
        unacceptable,
        sample_set,
        sample_maker,
    )


def replace_unacceptable(unacceptable, series, element_maker):
    while any(unacceptable(element) for element in series):
        series = [
            element_maker() if unacceptable(element) else element
            for element in series
        ]
    return series


def contract_matches_all_samples(contract, sample_set):
    return all(contract(s) for s in sample_set)


def sample_matches_all_contracts(sample, contract_set):
    return all(c(sample) for c in contract_set)


def contract_matches_no_samples(contract, sample_set):
    return not any(contract(s) for s in sample_set)


def sample_matches_no_contracts(sample, contract_set):
    return not any(c(sample) for c in contract_set)


def count_contract_matching_all_samples(contract_set, sample_set):
    return count_truthy(contract_matches_all_samples(c, sample_set) for c in contract_set)


def count_sample_matching_all_contracts(sample_set, contract_set):
    return count_truthy(sample_matches_all_contracts(s, contract_set) for s in sample_set)


def count_contract_matching_no_samples(contract_set, sample_set):
    return count_truthy(contract_matches_no_samples(c, sample_set) for c in contract_set)


def count_sample_matching_no_contracts(sample_set, contract_set):
    return count_truthy(sample_matches_no_contracts(s, contract_set) for s in sample_set)




SETUP = sg.Seqs('ABCD', 5)

contract_gen = contract_generator(SETUP)
contract_maker = lambda: next(contract_gen)
sample_gen = sample_generator(SETUP)
sample_maker = lambda: next(sample_gen)


def contracts_for_samples(num_contracts, sample_set):
    contract_set_gen = partition(num_contracts, contract_gen)
    return fix_contracts(
        next(contract_set_gen),
        sample_set,
        lambda: next(contract_gen),
    )


def samples_for_contracts(num_samples, contract_set):
    sample_set_gen = partition(num_samples, sample_gen)
    return fix_samples(
        next(sample_set_gen),
        contract_set,
        lambda: next(sample_gen),
    )


def result_gen():
    while True:
        yield test_contracts_samples(
            next(partition(5, contract_gen)),
            next(partition(5, sample_gen)),
        )


def refine_contract_samples(contract_set, sample_set):
    oldresult = None
    result = test_contracts_samples(contract_set, sample_set)
    while result != oldresult:
        oldresult = result
        samples_fixed = fix_samples_for_result(result, sample_maker)
        contracts_fixed = fix_contracts_for_result(samples_fixed, contract_maker)
        result = contracts_fixed
    return result


def iterate_until_stable(func, initial):
    class _Uninitialized:
        pass

    oldvalue = _Uninitialized
    value = initial

    while value != oldvalue:
        oldvalue = value
        value = func(value)

    return value
