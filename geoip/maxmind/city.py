from maxmind import IPRange, IPRangeLookup, parse_network
import csv


class GeoLiteCityLookup(IPRangeLookup):
    def __init__(self, locs_filename, blocks_filename):
        locations = self.parse_locations(locs_filename)
        iprs = self.parse_blocks(blocks_filename)
        for geo_id, ipr in iprs:
            ipr.location = locations.get_location(geo_id)
        super(GeoLiteCityLookup, self).__init__([x[1] for x in iprs])

    def parse_locations(self, filename):
        locations = Locations()
        with open(filename) as f:
            reader = csv.reader(f)
            reader.next()
            map(lambda x: self.line_to_location(locations, x), reader)
        return locations

    def line_to_location(self, locations, line):
        line = map(lambda val: val.decode('latin1'), line)
        (geoname_id, _, continent_code, continent_name, country_iso_code,
         country_name, subdivision_1_iso_code, subdivision_1_name,
         subdivision_2_iso_code, subdivision_2_name, city_name, _, _) = line 
        locations.register_location(geoname_id, continent_code, continent_name,
                                    country_iso_code, country_name,
                                    subdivision_1_iso_code, subdivision_1_name,
                                    subdivision_2_iso_code,subdivision_2_name,city_name)

    def parse_blocks(self, filename):
        with open(filename) as f:
            reader = csv.reader(f)
            reader.next()
            return map(self.line_to_iprangeloc, reader)

    def line_to_iprangeloc(self, line):
        line = map(lambda val: val.decode('latin1'), line)
        network,geoname_id,registered_country_geoname_id,represented_country_geoname_id,is_anonymous_proxy,is_satellite_provider,postal_code,latitude,longitude = line
        actual_id = geoname_id or registered_country_geoname_id or None
        try:
            lat, lon = float(latitude), float(longitude)
        except:
            lat = lon = None
        return [actual_id, IPRangeLocation(parse_network(network), lat, lon)]


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


class IPRangeLocation(IPRange):
    __slots__ = ["location", "lat", "lon"]

    def __init__(self, rng, lat, lon):
        super(IPRangeLocation, self).__init__(*rng)
        self.lat = lat
        self.lon = lon
        self.location = None

    def as_dict(self):
        seed = {}
        ext = {}
        if self.lat and self.lon:
            seed = dict(latitude=self.lat, longitude=self.lon)
        if self.location:
            ext = self.location.as_dict()
        return dict(seed, **ext)


class Location(object):
    __slots__ = ["continent", "country", "r1", "r2", "city"]

    def __init__(self, continent_code, continent_name, country_code, country_name, r1_code, r1_name, r2_code, r2_name, city):
        self.continent = (continent_code, continent_name)
        self.country = (country_code, country_name)
        self.r1 = (r1_code, r1_name)
        self.r2 = (r2_code, r2_name)
        self.city = city

    def __repr__(self):
        return "Location <%s > %s > %s > %s > %s>" % (self.continent[1], self.country[0], self.r1[0], self.r2[0], self.city)

    def as_dict(self):
        return {"continent": self.continent,
                "country": self.country,
                "r1": self.r1,
                "r2": self.r2,
                "city": self.city}

