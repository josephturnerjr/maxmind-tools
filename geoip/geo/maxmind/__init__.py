import csv 
from geo import Location, ipv4_to_int, IPRangeLookup, IPRangeLocation, IPRangeASN
import re


class Locations(object):
    def __init__(self):
        self.locations = {}

    def register_location(self, geo_id, *args):
        location = Location(*args)
        self.locations[geo_id] = location

    def get_location(self, location_id):
        return self.locations.get(location_id)

    def __repr__(self):
        return "Locations<%s registered>" % len(self.locations)

def create_asn_lookup(asn_filename):
    asns = parse_asns(asn_filename)
    return IPRangeLookup(asns)
    
def create_geoip_lookup(locs_filename, blocks_filename):
    locations = parse_locations(locs_filename)
    iprs = parse_blocks(blocks_filename)
    for geo_id, ipr in iprs:
        ipr.location = locations.get_location(geo_id)
    return IPRangeLookup([x[1] for x in iprs])

def parse_asns(filename):
    with open(filename) as f:
        reader = csv.reader(f)
        reader.next()
        return map(line_to_asnrange, reader)

asn_owner_regex = re.compile("(?P<asn>AS[0-9]*) ?(?P<owner>.*)")

def line_to_asnrange(line):
    start, end, asn_owner = line
    start = int(start)
    end = int(end)
    match = asn_owner_regex.match(asn_owner)
    asn, owner = match.groups()
    if not owner:
        owner = None
    return IPRangeASN(start, end, owner, asn)

        

def parse_locations(filename):
    locations = Locations()
    with open(filename) as f:
        reader = csv.reader(f)
        reader.next()
        map(lambda x: line_to_location(locations, x), reader)
    return locations

def line_to_location(locations, line):
    geoname_id,_,continent_code,continent_name,country_iso_code,country_name,subdivision_1_iso_code,subdivision_1_name,subdivision_2_iso_code,subdivision_2_name,city_name,_,_ = line 
    locations.register_location(geoname_id, continent_code, continent_name, country_iso_code, country_name, subdivision_1_iso_code, subdivision_1_name,subdivision_2_iso_code,subdivision_2_name,city_name)

def parse_blocks(filename):
    with open(filename) as f:
        reader = csv.reader(f)
        reader.next()
        return map(line_to_iprangeloc, reader)

def line_to_iprangeloc(line):
    network,geoname_id,registered_country_geoname_id,represented_country_geoname_id,is_anonymous_proxy,is_satellite_provider,postal_code,latitude,longitude = line
    actual_id = geoname_id or registered_country_geoname_id or None
    try:
        lat, lon = float(latitude), float(longitude)
    except:
        lat = lon = None
    return [actual_id, IPRangeLocation(parse_network(network), lat, lon)]

def parse_network(network):
    mask, bitrange = network.split("/")
    return get_range(ipv4_to_int(mask), int(bitrange))

def get_range(mask, bitrange):
    return (mask, mask + (0xffffffff >> bitrange))
