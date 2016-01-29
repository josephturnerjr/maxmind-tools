from flask import Flask, jsonify, abort
from maxmind.city import GeoLiteCityLookup as GeoLookup
from maxmind.asn import ASNLookup
from maxmind import ipv4_to_int, valid_ipv4
import os.path

app = Flask(__name__)

def get_city_lookup():
    directory=".data/GeoCity"
    locs = os.path.join(directory, "GeoLite2-City-Locations-en.csv")
    blocks = os.path.join(directory, "GeoLite2-City-Blocks-IPv4.csv")
    return GeoLookup(locs, blocks)

def get_asn_lookup():
    directory = ".data"
    asns = os.path.join(directory, "GeoASN.csv")
    return ASNLookup(asns) 


asn_lookup = get_asn_lookup()
city_lookup = get_city_lookup()
lookups = [city_lookup, asn_lookup]


def lookup_ipv4(ipv4_addr):
    ip = ipv4_to_int(ipv4_addr)
    results = {}
    for lookup in lookups:
        result = lookup.lookup(ip)
        if result:
            results.update(result.as_dict())
    return results

@app.route("/<ip>")
def lookup(ip):
    if valid_ipv4(ip):
        return jsonify(**lookup_ipv4(ip))
    else:
        abort(400)

if __name__ == "__main__":
    app.run(host='0.0.0.0')
