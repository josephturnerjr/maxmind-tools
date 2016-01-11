import copy 
import re


valid_ipv4_regex = re.compile(r"^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$")

def valid_ipv4(addr):
    return bool(valid_ipv4_regex.match(addr))


def int_to_ipv4(i):
    return ".".join(["%i" % ((i & (0xff000000 >> x)) >> (24 - x)) for x in range(0, 32, 8)])


def ipv4_to_int(mask):
    return reduce(lambda total, byte: (total << 8) + byte,
                  map(int, mask.split(".")), 0)


class GeoIPLookup(object):
    def __init__(self, location_ranges):
        self.ranges = IPRangeLocations(location_ranges)

    def lookup(self, ip):
        return self.ranges.find_ip_range(ip)


class IPRangeLocations(object):
    def __init__(self, ranges):
        self.ranges = ranges

    def find_ip_range(self, search_ip):
        lo = 0
        hi = len(self.ranges)
        while lo < hi:
            mid = (lo + hi) // 2
            if self.ranges[mid].in_range(search_ip):
                return self.ranges[mid]
            elif self.ranges[mid].start < search_ip:
                lo = mid + 1
            else:
                hi = mid


class IPRangeLocation(object):
    __slots__ = ["start", "end", "location", "lat", "lon"]

    def __init__(self, rng, lat, lon):
        self.start, self.end = rng
        self.lat = lat
        self.lon = lon
        self.location = None

    def in_range(self, ip):
        return self.start <= ip <= self.end

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
