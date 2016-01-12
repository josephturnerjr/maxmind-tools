from maxmind.city import GeoLiteCityLookup as GeoLookup
from maxmind.asn import ASNLookup
from maxmind import ipv4_to_int, valid_ipv4
import os
import json
import time
from random import randint
from itertools import product

def lookup_ipv4(ipv4_addr, lookups):
    ip = ipv4_to_int(ipv4_addr)
    results = {}
    for lookup in lookups:
        result = lookup.lookup(ip)
        if result:
            results.update(result.as_dict())
    return results


def timeit(lookups):
    iters = 1000000
    errors = 0
    a = time.time()
    for _ in xrange(iters):
        addr = ".".join([str(randint(0,255)) for _ in range(4)])
        ret = lookup_ipv4(addr, lookups)
    elapsed = time.time() - a
    print "%s iterations: %s seconds (%s iterations / sec) with %s errors" % (iters, elapsed, iters / elapsed, errors)

def input(lookups):
    while True:
        addr = raw_input("ip: ")
        if valid_ipv4(addr):
            print lookup_ipv4(addr, lookups)
        else:
            print "Invalid IP!"

def test_some(lookups):
    octets = 1
    tests = 10000
    iters = 0
    a = time.time()
    for addr_parts in product(range(256), repeat=octets):
        print addr_parts
        prefix = map(str, addr_parts)
        for _ in xrange(tests):
            addr = ".".join(prefix + [str(randint(0,255)) for _ in range(4-octets)])
            ret = lookup_ipv4(addr, lookups)
            iters += 1
    elapsed = time.time() - a
    print "%s iterations: %s seconds (%s iterations / sec) with %s errors" % (iters, elapsed, iters / elapsed, errors)

def test_all(lookups):
    for addr_parts in product(range(256), repeat=4):
        addr = ".".join(map(str, addr_parts))
        ret = lookup_ipv4(addr, lookups)
        iters += 1
    elapsed = time.time() - a
    print "%s iterations: %s seconds (%s iterations / sec) with %s errors" % (iters, elapsed, iters / elapsed, errors)


def get_city_lookup():
    directory=".data/GeoLite2-City-CSV_20151103"
    locs = os.path.join(directory, "GeoLite2-City-Locations-en.csv")
    blocks = os.path.join(directory, "GeoLite2-City-Blocks-IPv4.csv")
    return GeoLookup(locs, blocks)

def get_asn_lookup():
    directory = ".data"
    asns = os.path.join(directory, "GeoIPASNum2.csv")
    return ASNLookup(asns) 

if __name__ == "__main__":
    asn_lookup = get_asn_lookup()
    city_lookup = get_city_lookup()
    lookups = [city_lookup, asn_lookup]
    #test_some(lookup)
    timeit(lookups)
    input(lookups)
