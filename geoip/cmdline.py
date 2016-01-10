from geo.maxmind import create_geoip_lookup
from geo import location_to_json, ip_to_int
import os
import json
import time
from random import randint
from itertools import product

def lookup_ip(addr, lookup):
    ip = ip_to_int(addr)
    location = lookup.lookup(ip)
    if location:
        return json.dumps(location.as_dict())
    else:
        return None

if __name__ == "__main__":
    directory=".data/GeoLite2-City-CSV_20151103"
    locs = os.path.join(directory, "GeoLite2-City-Locations-en.csv")
    blocks = os.path.join(directory, "GeoLite2-City-Blocks-IPv4.csv")

    lookup = create_geoip_lookup(locs, blocks)
    iters = 0
    errors = 0
    a = time.time()
    while True:
        addr = raw_input("ip: ")
        print lookup_ip(addr, lookup)

    #for addr_parts in product(range(256), repeat=4):
    #    addr = ".".join(map(str, addr_parts))
    #    ret = lookup_ip(addr, lookup)
    #    iters += 1
    #elapsed = time.time() - a
    #print "%s iterations: %s seconds (%s iterations / sec) with %s errors" % (iters, elapsed, iters / elapsed, errors)
