import csv 
from geo import ipv4_to_int, GeoIPLookup, IPRangeLocation, IPRangeLocations, Locations
    
def create_geoip_lookup(locs_filename, blocks_filename):
    return GeoIPLookup(parse_locations(locs_filename),
                       parse_blocks(blocks_filename))

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
        return IPRangeLocations(map(line_to_iprangeloc, reader))

def line_to_iprangeloc(line):
    network,geoname_id,registered_country_geoname_id,represented_country_geoname_id,is_anonymous_proxy,is_satellite_provider,postal_code,latitude,longitude = line
    if not geoname_id and not registered_country_geoname_id:
        print line
    return IPRangeLocation(parse_network(network),
                           geoname_id,
                           registered_country_geoname_id,
                           represented_country_geoname_id,
                           int(is_anonymous_proxy),
                           int(is_satellite_provider),
                           latitude, longitude)
    

def parse_network(network):
    mask, bitrange = network.split("/")
    return get_range(ipv4_to_int(mask), int(bitrange))

def get_range(mask, bitrange):
    return (mask, mask + (0xffffffff >> bitrange))
